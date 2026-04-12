///|
/// Decode residue for the given channels.
fn decode_residue(
  br : BitReader,
  residue : VorbisResidueConfig,
  codebooks : Array[VorbisCodebook],
  n : Int,
  ch : Int,
  do_not_decode : Array[Bool],
) -> Array[Array[Float]] raise AudioError {
  let output : Array[Array[Float]] = []
  for _ in 0..<ch {
    output.push(Array::make(n, (0.0 : Float)))
  }
  let actual_size = residue.end_ - residue.begin
  if actual_size <= 0 || residue.partition_size <= 0 {
    return output
  }
  let classwords = codebooks[residue.classbook].dimensions
  let n_to_decode = actual_size / residue.partition_size
  if n_to_decode <= 0 {
    return output
  }
  if residue.residue_type == 2 {
    // Type 2: interleaved channels
    let mut actual_ch = 0
    for c in 0..<ch {
      if not(do_not_decode[c]) {
        actual_ch = actual_ch + 1
      }
    }
    if actual_ch == 0 {
      return output
    }
    let interleave_size = n * actual_ch
    let interleaved : Array[Float] = Array::make(interleave_size, (0.0 : Float))
    decode_residue_partition(
      br,
      residue,
      codebooks,
      classwords,
      interleaved,
      interleave_size / residue.partition_size,
    )
    // De-interleave
    let mut ch_idx = 0
    for c in 0..<ch {
      if not(do_not_decode[c]) {
        for i in 0..<n {
          let idx = i * actual_ch + ch_idx
          if idx < interleaved.length() {
            output[c][i] = interleaved[idx]
          }
        }
        ch_idx = ch_idx + 1
      }
    }
  } else {
    // Type 0 or 1: per-channel
    for c in 0..<ch {
      if not(do_not_decode[c]) {
        decode_residue_partition(
          br,
          residue,
          codebooks,
          classwords,
          output[c],
          n_to_decode,
        )
      }
    }
  }
  output
}

///|
/// Decode residue partitions into the output vector.
fn decode_residue_partition(
  br : BitReader,
  residue : VorbisResidueConfig,
  codebooks : Array[VorbisCodebook],
  classwords : Int,
  output : Array[Float],
  n_to_decode : Int,
) -> Unit raise AudioError {
  let mut partition_count = 0
  while partition_count < n_to_decode {
    // Decode classification
    if residue.classbook >= codebooks.length() {
      break
    }
    let temp = codebook_decode(codebooks[residue.classbook], br)
    let mut t = temp
    let classifications : Array[Int] = Array::make(classwords, 0)
    for j in (classwords - 1)..<=0 {
      if j < 0 {
        break
      }
      if residue.classifications > 0 {
        classifications[j] = t % residue.classifications
        t = t / residue.classifications
      }
    }
    for j in 0..<classwords {
      if partition_count >= n_to_decode {
        break
      }
      let vq_class = classifications[j]
      // Read pass 0 (simplified - only pass 0 for basic decoding)
      if vq_class < residue.cascade.length() &&
        (residue.cascade[vq_class] & 1) != 0 {
        if vq_class < residue.books.length() {
          let book_idx = residue.books[vq_class][0]
          if book_idx >= 0 && book_idx < codebooks.length() {
            let book = codebooks[book_idx]
            let offset = residue.begin +
              partition_count * residue.partition_size
            let vectors_in_partition = residue.partition_size / book.dimensions
            for _ in 0..<vectors_in_partition {
              codebook_decode_vector(book, br, output, offset)
            }
          }
        }
      }
      partition_count = partition_count + 1
    }
  }
}
