///|
/// Create a new asset cache with given limits.
pub fn new_asset_cache(max_bytes : Int, max_entries : Int) -> AssetCache {
  AssetCache::{
    entries: {},
    total_bytes: 0,
    max_bytes,
    max_entries,
    access_counter: 0,
  }
}

///|
/// Get a cached audio buffer by key. Returns None if not found.
pub fn cache_get(cache : AssetCache, key : String) -> AudioBuffer? {
  match cache.entries.get(key) {
    Some(entry) => {
      cache.access_counter = cache.access_counter + 1
      entry.last_access = cache.access_counter
      Some(entry.buffer)
    }
    None => None
  }
}

///|
/// Put an audio buffer into the cache. Evicts entries if needed.
pub fn cache_put(
  cache : AssetCache,
  key : String,
  buffer : AudioBuffer,
  policy? : CachePolicy = Normal,
) -> Unit {
  let byte_size = buffer.data.length() * 4 // Float = 4 bytes
  // If key already exists, remove old entry first
  match cache.entries.get(key) {
    Some(old) => {
      cache.total_bytes = cache.total_bytes - old.byte_size
      cache.entries.remove(key)
    }
    None => ()
  }
  // Evict until we have space
  while cache.total_bytes + byte_size > cache.max_bytes ||
        cache.entries.length() >= cache.max_entries {
    if not(evict_one(cache)) {
      break
    }
  }
  cache.access_counter = cache.access_counter + 1
  let entry = CacheEntry::{
    buffer,
    policy,
    byte_size,
    last_access: cache.access_counter,
  }
  cache.entries[key] = entry
  cache.total_bytes = cache.total_bytes + byte_size
}

///|
/// Evict a specific key from the cache. Returns true if found and removed.
pub fn cache_evict(cache : AssetCache, key : String) -> Bool {
  match cache.entries.get(key) {
    Some(entry) => {
      cache.total_bytes = cache.total_bytes - entry.byte_size
      cache.entries.remove(key)
      true
    }
    None => false
  }
}

///|
/// Clear all entries from the cache.
pub fn cache_clear(cache : AssetCache) -> Unit {
  cache.entries.clear()
  cache.total_bytes = 0
}

///|
/// Evict one entry based on LRU policy. StreamPrefer evicted first, then Normal.
/// AlwaysCache entries are never evicted. Returns true if an entry was evicted.
fn evict_one(cache : AssetCache) -> Bool {
  // Find best candidate: StreamPrefer first, then Normal, by oldest access
  let mut best_key : String? = None
  let mut best_access = 2147483647
  let mut best_priority = 0 // 0=none, 1=Normal, 2=StreamPrefer
  for key, entry in cache.entries {
    let priority = match entry.policy {
      StreamPrefer => 2
      Normal => 1
      AlwaysCache => 0
    }
    if priority == 0 {
      continue
    }
    if priority > best_priority ||
      (priority == best_priority && entry.last_access < best_access) {
      best_key = Some(key)
      best_access = entry.last_access
      best_priority = priority
    }
  }
  match best_key {
    Some(key) => cache_evict(cache, key)
    None => false
  }
}
