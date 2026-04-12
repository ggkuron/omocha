///|
test "package name benchmark" (b : @bench.T) {
  b.bench(() => ignore(package_name()))
}
