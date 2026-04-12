///|
test "asset cache basic put and get" {
  let cache = @audio.new_asset_cache(10000, 10)
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  @audio.cache_put(cache, "test.wav", buf)
  let result = @audio.cache_get(cache, "test.wav")
  assert_true(result is Some(_))
  let result2 = @audio.cache_get(cache, "missing.wav")
  assert_true(result2 is None)
}

///|
test "asset cache eviction by max_entries" {
  let cache = @audio.new_asset_cache(100000, 3)
  let buf1 = @audio.new_audio_buffer(1, 44100, 10)
  let buf2 = @audio.new_audio_buffer(1, 44100, 10)
  let buf3 = @audio.new_audio_buffer(1, 44100, 10)
  let buf4 = @audio.new_audio_buffer(1, 44100, 10)
  @audio.cache_put(cache, "a", buf1)
  @audio.cache_put(cache, "b", buf2)
  @audio.cache_put(cache, "c", buf3)
  // Adding 4th should evict oldest (a)
  @audio.cache_put(cache, "d", buf4)
  assert_true(@audio.cache_get(cache, "a") is None)
  assert_true(@audio.cache_get(cache, "d") is Some(_))
}

///|
test "asset cache eviction by max_bytes" {
  // Each 100-frame mono buffer = 100 * 4 = 400 bytes
  let cache = @audio.new_asset_cache(1000, 100)
  let buf1 = @audio.new_audio_buffer(1, 44100, 100)
  let buf2 = @audio.new_audio_buffer(1, 44100, 100)
  @audio.cache_put(cache, "a", buf1)
  @audio.cache_put(cache, "b", buf2)
  // Total = 800. Adding another 400 = 1200 > 1000, should evict
  let buf3 = @audio.new_audio_buffer(1, 44100, 100)
  @audio.cache_put(cache, "c", buf3)
  assert_true(@audio.cache_get(cache, "a") is None)
  assert_true(@audio.cache_get(cache, "c") is Some(_))
}

///|
test "asset cache AlwaysCache not evicted" {
  let cache = @audio.new_asset_cache(100000, 3)
  let buf1 = @audio.new_audio_buffer(1, 44100, 10)
  let buf2 = @audio.new_audio_buffer(1, 44100, 10)
  let buf3 = @audio.new_audio_buffer(1, 44100, 10)
  let buf4 = @audio.new_audio_buffer(1, 44100, 10)
  @audio.cache_put(
    cache,
    "always",
    buf1,
    policy=@audio.CachePolicy::AlwaysCache,
  )
  @audio.cache_put(cache, "normal1", buf2)
  @audio.cache_put(cache, "normal2", buf3)
  // Should evict normal1 (oldest Normal), not "always"
  @audio.cache_put(cache, "new", buf4)
  assert_true(@audio.cache_get(cache, "always") is Some(_))
  assert_true(@audio.cache_get(cache, "normal1") is None)
}

///|
test "asset cache StreamPrefer evicted first" {
  let cache = @audio.new_asset_cache(100000, 3)
  let buf1 = @audio.new_audio_buffer(1, 44100, 10)
  let buf2 = @audio.new_audio_buffer(1, 44100, 10)
  let buf3 = @audio.new_audio_buffer(1, 44100, 10)
  let buf4 = @audio.new_audio_buffer(1, 44100, 10)
  @audio.cache_put(cache, "normal", buf1, policy=@audio.CachePolicy::Normal)
  @audio.cache_put(
    cache,
    "stream",
    buf2,
    policy=@audio.CachePolicy::StreamPrefer,
  )
  @audio.cache_put(cache, "normal2", buf3)
  // Should evict "stream" first (StreamPrefer has higher eviction priority)
  @audio.cache_put(cache, "new", buf4)
  assert_true(@audio.cache_get(cache, "stream") is None)
  assert_true(@audio.cache_get(cache, "normal") is Some(_))
}

///|
test "asset cache explicit evict" {
  let cache = @audio.new_asset_cache(10000, 10)
  let buf = @audio.new_audio_buffer(1, 44100, 100)
  @audio.cache_put(cache, "test", buf)
  assert_true(@audio.cache_evict(cache, "test"))
  assert_true(@audio.cache_get(cache, "test") is None)
  assert_true(not(@audio.cache_evict(cache, "test")))
}

///|
test "asset cache clear" {
  let cache = @audio.new_asset_cache(10000, 10)
  let buf1 = @audio.new_audio_buffer(1, 44100, 10)
  let buf2 = @audio.new_audio_buffer(1, 44100, 10)
  @audio.cache_put(cache, "a", buf1)
  @audio.cache_put(cache, "b", buf2)
  @audio.cache_clear(cache)
  assert_true(@audio.cache_get(cache, "a") is None)
  assert_true(@audio.cache_get(cache, "b") is None)
  assert_true(cache.total_bytes == 0)
}

///|
test "asset cache update existing key" {
  let cache = @audio.new_asset_cache(10000, 10)
  let buf1 = @audio.new_audio_buffer(1, 44100, 50)
  let buf2 = @audio.new_audio_buffer(1, 44100, 100)
  @audio.cache_put(cache, "test", buf1)
  let bytes_before = cache.total_bytes
  @audio.cache_put(cache, "test", buf2)
  // Should have updated the entry, total_bytes should reflect new size
  assert_true(cache.total_bytes > bytes_before)
  let result = @audio.cache_get(cache, "test")
  match result {
    Some(b) => assert_true(b.data.length() == 100)
    None => assert_true(false)
  }
}
