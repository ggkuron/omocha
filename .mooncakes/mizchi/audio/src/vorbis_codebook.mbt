///|
/// A Vorbis codebook for Huffman decoding and VQ lookup.
struct VorbisCodebook {
  entries : Int
  dimensions : Int
  // Flat decode tree: positive values are internal nodes (left child index),
  // negative values are leaves: -(entry_number + 1)
  decode_tree : Array[Int]
  // VQ lookup: multiplicands[entry * dimensions + dim]
  multiplicands : Array[Float]
  min_value : Float
  delta_value : Float
  sequence_p : Bool
  lookup_type : Int
} derive(Show)

///|
/// Decode a Huffman symbol from the bitstream using the codebook.
fn codebook_decode(
  book : VorbisCodebook,
  br : BitReader,
) -> Int raise AudioError {
  let mut node = 0
  while true {
    if node < 0 || node >= book.decode_tree.length() {
      raise AudioError::DecodeFailed("Vorbis: codebook tree traversal error")
    }
    let val = book.decode_tree[node]
    if val < 0 {
      // Leaf node
      return -(val + 1)
    }
    let bit = read_bit(br)
    if bit {
      node = val + 1 // right child
    } else {
      node = val // left child
    }
  }
  raise AudioError::DecodeFailed("Vorbis: codebook decode failed")
}

///|
/// Decode a VQ vector from the codebook.
fn codebook_decode_vector(
  book : VorbisCodebook,
  br : BitReader,
  output : Array[Float],
  offset : Int,
) -> Unit raise AudioError {
  let entry = codebook_decode(book, br)
  if book.lookup_type == 0 {
    return
  }
  for i in 0..<book.dimensions {
    let idx = entry * book.dimensions + i
    if idx < book.multiplicands.length() {
      let val = book.multiplicands[idx]
      output[offset + i] = output[offset + i] + val
    }
  }
}

///|
/// Parse a Vorbis codebook from the bitstream.
fn parse_codebook(br : BitReader) -> VorbisCodebook raise AudioError {
  // Sync pattern: 0x564342
  let sync = read_bits(br, 24)
  if sync != 0x564342 {
    raise AudioError::DecodeFailed("Vorbis: invalid codebook sync")
  }
  let dimensions = read_bits(br, 16)
  let entries = read_bits(br, 24)
  // Read codeword lengths
  let lengths : Array[Int] = Array::make(entries, 0)
  let ordered = read_bit(br)
  if ordered {
    let mut current_entry = 0
    let mut current_length = read_bits(br, 5) + 1
    while current_entry < entries {
      let number = read_bits(br, ilog(entries - current_entry))
      for _ in 0..<number {
        if current_entry < entries {
          lengths[current_entry] = current_length
          current_entry = current_entry + 1
        }
      }
      current_length = current_length + 1
    }
  } else {
    let sparse = read_bit(br)
    for i in 0..<entries {
      if sparse {
        let flag = read_bit(br)
        if flag {
          lengths[i] = read_bits(br, 5) + 1
        } else {
          lengths[i] = 0 // unused entry
        }
      } else {
        lengths[i] = read_bits(br, 5) + 1
      }
    }
  }
  // Build Huffman decode tree
  let decode_tree = build_huffman_tree(entries, lengths)
  // Read VQ lookup
  let lookup_type = read_bits(br, 4)
  let mut min_value : Float = 0.0
  let mut delta_value : Float = 0.0
  let mut sequence_p = false
  let multiplicands : Array[Float] = []
  if lookup_type == 1 || lookup_type == 2 {
    min_value = float32_unpack(read_bits(br, 32))
    delta_value = float32_unpack(read_bits(br, 32))
    let value_bits = read_bits(br, 4) + 1
    sequence_p = read_bit(br)
    let lookup_values = if lookup_type == 1 {
      lookup1_values(entries, dimensions)
    } else {
      entries * dimensions
    }
    let raw_multiplicands : Array[Int] = []
    for _ in 0..<lookup_values {
      raw_multiplicands.push(read_bits(br, value_bits))
    }
    // Expand lookup table into per-entry VQ vectors
    expand_lookup(
      lookup_type, entries, dimensions, min_value, delta_value, sequence_p, raw_multiplicands,
      multiplicands,
    )
  }
  VorbisCodebook::{
    entries,
    dimensions,
    decode_tree,
    multiplicands,
    min_value,
    delta_value,
    sequence_p,
    lookup_type,
  }
}

///|
/// Build a canonical Huffman decode tree from codeword lengths.
/// Returns a flat array where:
/// - Positive values point to the left child index
/// - Negative values are leaves: -(entry + 1)
fn build_huffman_tree(entries : Int, lengths : Array[Int]) -> Array[Int] {
  // Find max length
  let mut max_len = 0
  for i in 0..<entries {
    if lengths[i] > max_len {
      max_len = lengths[i]
    }
  }
  if max_len == 0 {
    return [-(0 + 1)] // single entry tree
  }
  // Allocate tree nodes
  let tree : Array[Int] = []
  // Root node at index 0
  tree.push(0) // placeholder
  tree.push(0) // placeholder for right child
  // Assign canonical Huffman codes
  // Sort entries by length, then by entry number
  let sorted : Array[(Int, Int)] = [] // (entry, length)
  for i in 0..<entries {
    if lengths[i] > 0 {
      sorted.push((i, lengths[i]))
    }
  }
  sorted.sort_by(fn(a, b) {
    if a.1 != b.1 {
      a.1.compare(b.1)
    } else {
      a.0.compare(b.0)
    }
  })
  // Insert entries into tree
  for pair in sorted {
    let entry = pair.0
    let length = pair.1
    insert_into_tree(tree, entry, length)
  }
  tree
}

///|
/// Insert an entry with given code length into the Huffman tree.
fn insert_into_tree(tree : Array[Int], entry : Int, length : Int) -> Unit {
  let mut node = 0
  for depth in 0..<length {
    let is_last = depth == length - 1
    let val = tree[node]
    if val < 0 {
      // Already a leaf, tree is corrupt but handle gracefully
      return
    }
    // Try to go left (0) first, then right (1)
    if is_last {
      // Insert as leaf
      // Find an empty child
      if tree[node] == 0 && node != 0 {
        // This node hasn't been expanded yet
        tree[node] = -(entry + 1)
        return
      }
      // Check if left child is available
      let left = tree[node]
      if left == 0 || (left > 0 && left < tree.length()) {
        if tree[node] == 0 && depth == 0 {
          // Expand root
          let child_idx = tree.length()
          tree.push(0) // left child
          tree.push(0) // right child
          tree[node] = child_idx
          tree[child_idx] = -(entry + 1)
          return
        }
      }
      // Navigate down to find empty slot
      if val == 0 {
        tree[node] = -(entry + 1)
        return
      }
      // Go to left child
      if tree[val] == 0 {
        tree[val] = -(entry + 1)
        return
      }
      // Go to right child
      if tree[val + 1] == 0 {
        tree[val + 1] = -(entry + 1)
        return
      }
      // Internal node - expand if needed
    } else if val == 0 {
      let child_idx = tree.length()
      tree.push(0)
      tree.push(0)
      tree[node] = child_idx
      node = child_idx
    } else if val > 0 {
      node = val
    } else {
      return // leaf in the way
    }
  }
}

///|
/// Compute lookup1_values: floor(entries^(1/dimensions)).
fn lookup1_values(entries : Int, dimensions : Int) -> Int {
  if dimensions == 0 {
    return 0
  }
  let mut r = 0
  while true {
    let next = r + 1
    let mut power = 1
    for _ in 0..<dimensions {
      power = power * next
      if power > entries {
        return r
      }
    }
    if power > entries {
      return r
    }
    r = next
  }
  r
}

///|
/// Expand VQ lookup table into per-entry multiplicand vectors.
fn expand_lookup(
  lookup_type : Int,
  entries : Int,
  dimensions : Int,
  min_value : Float,
  delta_value : Float,
  sequence_p : Bool,
  raw : Array[Int],
  output : Array[Float],
) -> Unit {
  if lookup_type == 1 {
    let lookup_values = raw.length()
    for entry in 0..<entries {
      let mut last : Float = 0.0
      let mut index_divisor = 1
      for dim in 0..<dimensions {
        let multiplicand_offset = entry / index_divisor % lookup_values
        let val = min_value +
          Float::from_int(raw[multiplicand_offset]) * delta_value
        let result = if sequence_p { val + last } else { val }
        output.push(result)
        if sequence_p {
          last = result
        }
        index_divisor = index_divisor * lookup_values
        ignore(dim)
      }
    }
  } else {
    // lookup_type == 2
    for entry in 0..<entries {
      let mut last : Float = 0.0
      for dim in 0..<dimensions {
        let raw_idx = entry * dimensions + dim
        let val = if raw_idx < raw.length() {
          min_value + Float::from_int(raw[raw_idx]) * delta_value
        } else {
          min_value
        }
        let result = if sequence_p { val + last } else { val }
        output.push(result)
        if sequence_p {
          last = result
        }
      }
    }
  }
}
