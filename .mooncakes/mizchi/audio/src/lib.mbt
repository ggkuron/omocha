///|
/// Project marker.
fn package_name() -> String {
  "mizchi/audio-mpt"
}

///|
test "package name" {
  inspect(package_name(), content="mizchi/audio-mpt")
}
