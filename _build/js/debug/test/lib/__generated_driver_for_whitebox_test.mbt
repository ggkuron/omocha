// #region Define traits with default implementations

///|
/// is_being_cancelled may be unused if there's no async test
#warnings("-unused_value")
trait MoonBit_Async_Test_Driver {
  run_async_tests(@moonbitlang/core/prelude.Array[async () -> Unit]) -> Unit = _
  is_being_cancelled() -> Bool = _
}

///|
impl MoonBit_Async_Test_Driver with run_async_tests(_) {
  ()
}

///|
impl MoonBit_Async_Test_Driver with is_being_cancelled() {
  false
}

///|
trait MoonBit_Test_Driver {
  /// returns if the test is found and executed
  run_test(
    @moonbitlang/core/prelude.Array[async () -> Unit],
    String,
    Int,
    (String, String, Bool) -> Unit,
    (Error) -> String,
  ) -> Bool raise MoonBitTestDriverInternalSkipTest = _
}

///|
impl MoonBit_Test_Driver with run_test(_, _, _, _, _) {
  false
}

// #endregion
// #region Define abstract types for namespace with implementation declarations

///|
priv type MoonBit_Test_Driver_Internal_No_Args

///|
impl MoonBit_Test_Driver for MoonBit_Test_Driver_Internal_No_Args

///|
priv type MoonBit_Test_Driver_Internal_With_Args

///|
impl MoonBit_Test_Driver for MoonBit_Test_Driver_Internal_With_Args

///|
priv type MoonBit_Test_Driver_Internal_Async_No_Args

///|
impl MoonBit_Test_Driver for MoonBit_Test_Driver_Internal_Async_No_Args

///|
priv type MoonBit_Test_Driver_Internal_Async_With_Args

///|
impl MoonBit_Test_Driver for MoonBit_Test_Driver_Internal_Async_With_Args

///|
priv type MoonBit_Test_Driver_Internal_With_Bench_Args

///|
impl MoonBit_Test_Driver for MoonBit_Test_Driver_Internal_With_Bench_Args

///|
priv type MoonBit_Async_Test_Driver_Impl

///|
impl MoonBit_Async_Test_Driver for MoonBit_Async_Test_Driver_Impl

// #endregion

// #region Utility Types

///|
/// Maybe unused if there's no tests
#warnings("-unused_type_declaration")
priv struct MoonBitTestDriverInternalTestMap[F](
  @moonbitlang/core/prelude.Map[
    String,
    @moonbitlang/core/prelude.Map[
      Int,
      (F, @moonbitlang/core/prelude.Array[String]),
    ],
  ]
)

///|
priv suberror MoonBitTestDriverInternalSkipTest {
  MoonBitTestDriverInternalSkipTest(name~ : String)
}

// #endregion
///|
fn moonbit_test_driver_internal_error_to_string(x : Error) -> String = "%error.to_string"

///|
#cfg(target="native")
fn string_get(s : String, index : Int) -> Int = "%string_get"

///|
#cfg(not(target="js"))
fn moonbit_unsafe_char_from_int(x : Int) -> Char = "%identity"

///|
#cfg(target="native")
const MOONBIT_TEST_DRIVER_INTERNAL_IS_NATIVE = true

///|
#cfg(not(target="native"))
const MOONBIT_TEST_DRIVER_INTERNAL_IS_NATIVE = false

// =========================================================
// ================== BEGIN WASM SPECIFIC ==================
// =========================================================

///|
#cfg(any(target="wasm", target="wasm-gc"))
fn moonbit_test_driver_internal_get_file_name(
  file_name : MoonbitTestDriverInternalExternString,
) -> String {
  moonbit_test_driver_internal_string_from_extern(file_name)
}

///|
#cfg(any(target="wasm", target="wasm-gc"))
#external
type MoonbitTestDriverInternalStringReadHandle

///|
#cfg(any(target="wasm", target="wasm-gc"))
#external
type MoonbitTestDriverInternalExternString

///|
#cfg(any(target="wasm", target="wasm-gc"))
fn moonbit_test_driver_internal_begin_read_string(
  s : MoonbitTestDriverInternalExternString,
) -> MoonbitTestDriverInternalStringReadHandle = "__moonbit_fs_unstable" "begin_read_string"

///|
#cfg(any(target="wasm", target="wasm-gc"))
fn moonbit_test_driver_internal_string_read_char(
  handle : MoonbitTestDriverInternalStringReadHandle,
) -> Int = "__moonbit_fs_unstable" "string_read_char"

///|
#cfg(any(target="wasm", target="wasm-gc"))
fn moonbit_test_driver_internal_finish_read_string(
  handle : MoonbitTestDriverInternalStringReadHandle,
) = "__moonbit_fs_unstable" "finish_read_string"

///|
#cfg(any(target="wasm", target="wasm-gc"))
fn fixedarray_to_bytes(arr : FixedArray[Byte]) -> Bytes = "%identity"

///|
#cfg(any(target="wasm", target="wasm-gc"))
fn moonbit_test_driver_internal_string_from_extern(
  e : MoonbitTestDriverInternalExternString,
) -> String {
  let buf = FixedArray::make(512, Byte::default())
  let mut len = 0
  let handle = moonbit_test_driver_internal_begin_read_string(e)
  while true {
    let ch = moonbit_test_driver_internal_string_read_char(handle)
    if ch == -1 {
      break
    } else {
      let written_bytes_len = buf.set_utf16le_char(
        len,
        moonbit_unsafe_char_from_int(ch),
      )
      len += written_bytes_len
    }
  }
  moonbit_test_driver_internal_finish_read_string(handle)
  fixedarray_to_bytes(buf).to_unchecked_string(offset=0, length=len).to_string()
}

// =========================================================
// =================== END WASM SPECIFIC ===================
// =========================================================

// =========================================================
// ================= BEGIN NATIVE SPECIFIC =================
// =========================================================

///|
#cfg(target="native")
fn moonbit_test_driver_internal_native_parse_args() -> @moonbitlang/core/prelude.Array[
  (String, Int),
] {
  fn moonbit_test_driver_internal_parse_int_(s : String) -> Int {
    let mut res = 0
    let len = s.length()
    for i = 0; i < len; i = i + 1 {
      res = res * 10 + (string_get(s, i) - '0')
    }
    res
  }

  fn moonbit_test_driver_internal_utf8_bytes_to_mbt_string(
    bytes : Bytes,
  ) -> String {
    let res = @moonbitlang/core/prelude.StringBuilder::new()
    let len = bytes.length()
    let mut i = 0
    while i < len {
      let mut c = bytes[i].to_int()
      if c < 0x80 {
        res.write_char(moonbit_unsafe_char_from_int(c))
        i += 1
      } else if c < 0xE0 {
        if i + 1 >= len {
          break
        }
        c = ((c & 0x1F) << 6) | (bytes[i + 1].to_int() & 0x3F)
        res.write_char(moonbit_unsafe_char_from_int(c))
        i += 2
      } else if c < 0xF0 {
        if i + 2 >= len {
          break
        }
        c = ((c & 0x0F) << 12) |
          ((bytes[i + 1].to_int() & 0x3F) << 6) |
          (bytes[i + 2].to_int() & 0x3F)
        res.write_char(moonbit_unsafe_char_from_int(c))
        i += 3
      } else {
        if i + 3 >= len {
          break
        }
        c = ((c & 0x07) << 18) |
          ((bytes[i + 1].to_int() & 0x3F) << 12) |
          ((bytes[i + 2].to_int() & 0x3F) << 6) |
          (bytes[i + 3].to_int() & 0x3F)
        c -= 0x10000
        res.write_char(moonbit_unsafe_char_from_int((c >> 10) + 0xD800))
        res.write_char(moonbit_unsafe_char_from_int((c & 0x3FF) + 0xDC00))
        i += 4
      }
    }
    res.to_string()
  }

  fn moonbit_test_driver_internal_get_cli_args_internal() -> @moonbitlang/core/prelude.Array[
    String,
  ] {
    let tmp = moonbit_test_driver_internal_get_cli_args_ffi()
    let res = @moonbitlang/core/prelude.Array::new(capacity=tmp.length())
    for i = 0; i < tmp.length(); i = i + 1 {
      res.push(moonbit_test_driver_internal_utf8_bytes_to_mbt_string(tmp[i]))
    }
    res
  }

  fn moonbit_test_driver_internal_split_mbt_string(
    s : String,
    sep : Char,
  ) -> @moonbitlang/core/prelude.Array[String] {
    let res = []
    let mut i = 0
    let mut start = 0
    while i < s.length() {
      if moonbit_unsafe_char_from_int(string_get(s, i)) == sep {
        res.push(s.unsafe_substring(start~, end=i))
        start = i + 1
      }
      i += 1
    }
    if start < s.length() {
      res.push(s.unsafe_substring(start~, end=s.length()))
    }
    res
  }

  let file_and_index = []
  let cli_args = moonbit_test_driver_internal_get_cli_args_internal()
  let test_args = moonbit_test_driver_internal_split_mbt_string(
    cli_args[1],
    '/',
  )
  for arg in test_args {
    let file_and_range = moonbit_test_driver_internal_split_mbt_string(arg, ':')
    let file = file_and_range[0]
    let range = file_and_range[1]
    let start_and_end = moonbit_test_driver_internal_split_mbt_string(
      range, '-',
    )
    let start = moonbit_test_driver_internal_parse_int_(start_and_end[0])
    let end = moonbit_test_driver_internal_parse_int_(start_and_end[1])
    for i = start; i < end; i = i + 1 {
      file_and_index.push((file, i))
    }
  }
  file_and_index
}

///|
#cfg(target="native")
fn moonbit_test_driver_internal_get_cli_args_ffi() -> FixedArray[Bytes] = "$moonbit.get_cli_args"

// =========================================================
// ================== END NATIVE SPECIFIC ==================
// =========================================================

// =========================================================
// -================== BEGIN JS SPECIFIC ===================
// =========================================================

///|
#cfg(target="js")
#external
type MoonBitTestDriverInternalTestEntry

///|
#cfg(target="js")
extern "js" fn MoonBitTestDriverInternalTestEntry::filename(
  self : Self,
) -> String =
  #| (entry) => entry[0]

///|
#cfg(target="js")
extern "js" fn MoonBitTestDriverInternalTestEntry::index(self : Self) -> Int =
  #| (entry) => entry[1]

///|
pub(all) suberror MoonBitTestDriverInternalJsError {
  MoonBitTestDriverInternalJsError(String)
}

///|
/// Maybe unused if there's no tests
#cfg(target="js")
#warnings("-unused_value")
fn moonbit_test_driver_internal_catch_error(
  f : () -> Unit raise,
  on_ok~ : () -> Unit,
  on_err~ : (Error) -> Unit,
) -> Unit {
  moonbit_test_driver_internal_js_catch(
    on_err=err_str => on_err(MoonBitTestDriverInternalJsError(err_str)),
    () => try f() catch {
      err => on_err(err)
    } noraise {
      _ => on_ok()
    },
  )
}

///|
#cfg(target="js")
extern "js" fn moonbit_test_driver_internal_js_catch(
  f : () -> Unit,
  on_err~ : (String) -> Unit,
) =
  #| (f, on_err) => {
  #|   try {
  #|     f()
  #|   } catch (err) {
  #|     const msg = err.stack.toString()
  #|     on_err(msg)
  #|   }
  #| }

///|
/// Maybe unused if there's no tests
#cfg(not(target="js"))
#warnings("-unused_value")
fn moonbit_test_driver_internal_catch_error(
  f : () -> Unit raise,
  on_ok~ : () -> Unit,
  on_err~ : (Error) -> Unit,
) -> Unit {
  try f() catch {
    err => on_err(err)
  } noraise {
    _ => on_ok()
  }
}

// =========================================================
// -=================== END JS SPECIFIC ====================
// =========================================================
// This file is introduced when tests without args is needed

///|
let moonbit_test_driver_internal_no_args_tests : MoonBitTestDriverInternalTestMap[
  () -> Unit raise,
] = {
  "logic_wbtest.mbt": {
    0: (__test_6c6f6769635f7762746573742e6d6274_0, ["start_phase: ドローが実行される - hands+1"]),
    1: (__test_6c6f6769635f7762746573742e6d6274_1, ["start_phase: ドローが実行される - stack-1"]),
    2: (__test_6c6f6769635f7762746573742e6d6274_2, ["start_phase: ログに 'Start Phase: Own' を出力"]),
    3: (__test_6c6f6769635f7762746573742e6d6274_3, ["start_phase: Rival 側 - ログに 'Start Phase: Rival' を出力"]),
    4: (__test_6c6f6769635f7762746573742e6d6274_4, ["evol_phase: ログに 'Evol Phase: Own' を出力"]),
    5: (__test_6c6f6769635f7762746573742e6d6274_5, ["action_phase: ログに 'Action Phase: Own' を出力"]),
    6: (__test_6c6f6769635f7762746573742e6d6274_6, ["attack_phase: ログに 'Attack Phase: Own' を出力"]),
    7: (__test_6c6f6769635f7762746573742e6d6274_7, ["end_phase: ログに 'End Phase: Own' を出力"]),
    8: (__test_6c6f6769635f7762746573742e6d6274_8, ["run_turn: ログが5件出力される"]),
    9: (__test_6c6f6769635f7762746573742e6d6274_9, ["run_turn: ログの順番が正しい (Start→Evol→Action→Attack→End)"]),
    10: (__test_6c6f6769635f7762746573742e6d6274_10, ["run_turn: start_phase によるドローが反映される"]),
    11: (__test_6c6f6769635f7762746573742e6d6274_11, ["run_turn: rival フィールドは変化しない (Own のターン)"]),
    12: (__test_6c6f6769635f7762746573742e6d6274_12, ["next_turn: Own → Rival"]),
    13: (__test_6c6f6769635f7762746573742e6d6274_13, ["next_turn: Rival → Own"]),
    14: (__test_6c6f6769635f7762746573742e6d6274_14, ["apply_turn: ログが state.log に追加される"]),
  },
  "field_selection_wbtest.mbt": {
    0: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_0, ["next_selection: SideSelect(Own) → FieldSelect(Own, BattleZone)"]),
    1: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_1, ["next_selection: SideSelect(Rival) → FieldSelect(Rival, BattleZone)"]),
    2: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_2, ["next_selection: FieldSelect(Own, BattleZone) → SlotSelect(Own, BattleZone, OriginSlot)"]),
    3: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_3, ["next_selection: FieldSelect(Rival, AuraZone) は描画対象のまま維持"]),
    4: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_4, ["next_selection: FieldSelect(Own, GuardZone) は描画対象のまま維持"]),
    5: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_5, ["next_selection: SlotSelect は最深レベル - 変化なし"]),
    6: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_6, ["prev_selection: SlotSelect → FieldSelect (一段上)"]),
    7: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_7, ["prev_selection: SlotSelect(Rival, AuraZone, FrontL) → FieldSelect(Rival, AuraZone)"]),
    8: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_8, ["prev_selection: FieldSelect → SideSelect (一段上)"]),
    9: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_9, ["prev_selection: FieldSelect(Rival, GuardZone) → SideSelect(Rival)"]),
    10: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_10, ["prev_selection: SideSelect(Own) は最上レベル - 変化なし"]),
    11: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_11, ["prev_selection: SideSelect(Rival) は最上レベル - 変化なし"]),
    12: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_12, ["next/prev 往復対称性: SideSelect(Own)"]),
    13: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_13, ["next/prev 往復対称性: FieldSelect(Own, BattleZone)"]),
    14: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_14, ["is_assigned: OriginSlot は常に true"]),
    15: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_15, ["is_assigned: own の衛星スロットは全て false"]),
    16: (__test_6669656c645f73656c656374696f6e5f7762746573742e6d6274_16, ["is_assigned: rival frontL/frontR/backL は true, backR は false"]),
  },
  "field_play_success_wbtest.mbt": {
    0: (__test_6669656c645f706c61795f737563636573735f7762746573742e6d6274_0, ["play_card: 全スロット Empty のとき FrontL に先頭 Satellite を配置する"]),
    1: (__test_6669656c645f706c61795f737563636573735f7762746573742e6d6274_1, ["play_card: FrontL のみ Empty のとき FrontL に配置する"]),
    2: (__test_6669656c645f706c61795f737563636573735f7762746573742e6d6274_2, ["play_card: FrontL/FrontR Assigned のとき BackL に配置する"]),
    3: (__test_6669656c645f706c61795f737563636573735f7762746573742e6d6274_3, ["play_card: FrontL/FrontR/BackL Assigned のとき BackR に配置する"]),
  },
  "field_play_state_wbtest.mbt": {
    0: (__test_6669656c645f706c61795f73746174655f7762746573742e6d6274_0, ["apply_play_own_card places own hand head into FrontL"]),
    1: (__test_6669656c645f706c61795f73746174655f7762746573742e6d6274_1, ["apply_play_own_card leaves rival field unchanged"]),
    2: (__test_6669656c645f706c61795f73746174655f7762746573742e6d6274_2, ["apply_play_own_card preserves turn phase and log"]),
  },
  "field_play_helpers_wbtest.mbt": {
  },
  "field_play_edge_wbtest.mbt": {
    0: (__test_6669656c645f706c61795f656467655f7762746573742e6d6274_0, ["play_card: 全スロット Assigned のとき Field は変化しない"]),
    1: (__test_6669656c645f706c61795f656467655f7762746573742e6d6274_1, ["play_card: hands が空のとき Field は変化しない"]),
    2: (__test_6669656c645f706c61795f656467655f7762746573742e6d6274_2, ["play_card: hands[0] が Satellite でない場合 Field は変化しない"]),
    3: (__test_6669656c645f706c61795f656467655f7762746573742e6d6274_3, ["play_card: 配置後 hands[0] が除去され残りの手札が保持される"]),
    4: (__test_6669656c645f706c61795f656467655f7762746573742e6d6274_4, ["play_card: origin カードは変化しない"]),
    5: (__test_6669656c645f706c61795f656467655f7762746573742e6d6274_5, ["play_card: FrontL が Sealed のとき FrontR に配置する"]),
  },
  "field_level_wbtest.mbt": {
    0: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_0, ["next_in_level: SideSelect(Own) → SideSelect(Rival)"]),
    1: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_1, ["next_in_level: SideSelect(Rival) → SideSelect(Own) - wrap around"]),
    2: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_2, ["next_in_level: FieldSelect BattleZone → AuraZone"]),
    3: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_3, ["next_in_level: FieldSelect GuardZone → BattleZone - wrap around"]),
    4: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_4, ["next_in_level: SlotSelect rival OriginSlot → FrontL"]),
    5: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_5, ["next_in_level: SlotSelect rival FrontL → FrontR"]),
    6: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_6, ["next_in_level: SlotSelect rival FrontR → BackL"]),
    7: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_7, ["next_in_level: SlotSelect rival BackL → OriginSlot - BackR(Empty) をスキップ"]),
    8: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_8, ["next_in_level: own 全スロット Empty - OriginSlot のまま"]),
    9: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_9, ["prev_in_level: SideSelect(Rival) → SideSelect(Own)"]),
    10: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_10, ["prev_in_level: SideSelect(Own) → SideSelect(Rival) - wrap around"]),
    11: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_11, ["prev_in_level: FieldSelect AuraZone → BattleZone"]),
    12: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_12, ["prev_in_level: FieldSelect BattleZone → GuardZone - wrap around"]),
    13: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_13, ["prev_in_level: SlotSelect rival OriginSlot → BackL - BackR(Empty) をスキップ"]),
    14: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_14, ["prev_in_level: SlotSelect rival FrontL → OriginSlot"]),
    15: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_15, ["prev_in_level: SlotSelect rival BackL → FrontR"]),
    16: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_16, ["nextInLevel/prevInLevel 対称性: SideSelect(Own)"]),
    17: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_17, ["nextInLevel/prevInLevel 対称性: FieldSelect(Own, BattleZone)"]),
    18: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_18, ["nextInLevel/prevInLevel 対称性: SlotSelect rival OriginSlot"]),
    19: (__test_6669656c645f6c6576656c5f7762746573742e6d6274_19, ["nextInLevel/prevInLevel 対称性: own stack あり FieldSelect"]),
  },
  "field_draw_wbtest.mbt": {
    0: (__test_6669656c645f647261775f7762746573742e6d6274_0, ["draw_one_card: 通常ドロー - hands が1枚増え stack が1枚減る"]),
    1: (__test_6669656c645f647261775f7762746573742e6d6274_1, ["draw_one_card: 空 stack では変化なし"]),
  },
}


///|
impl MoonBit_Test_Driver for MoonBit_Test_Driver_Internal_No_Args with run_test(
  _,
  filename,
  index,
  handle_result,
  error_to_string,
) {
  if moonbit_test_driver_internal_no_args_tests.0.get(filename)
    is Some(index_map) &&
    index_map.get(index) is Some((f, attrs)) {
    let name = if attrs is [name, ..] { name } else { "" }
    if MOONBIT_TEST_DRIVER_INTERNAL_IS_NATIVE {
      for attr in attrs {
        if attr is [.. "panic", ..] {
          raise MoonBitTestDriverInternalSkipTest(name~)
        }
      }
    }
    moonbit_test_driver_internal_catch_error(
      f,
      on_ok=() => handle_result(name, "", false),
      on_err=err => handle_result(name, error_to_string(err), false),
    )
    true
  } else {
    false
  }
}
///|
#cfg(target="native")
#test_entry
fn main {
  let async_tests = []
  for arg in moonbit_test_driver_internal_native_parse_args() {
    moonbit_test_driver_internal_do_execute(async_tests, arg.0, arg.1)
  }
  MoonBit_Async_Test_Driver_Impl::run_async_tests(async_tests)
}

///|
#cfg(not(target="native"))
#test_entry
fn main {
  ()
}

///|
#cfg(target="js")
pub fn moonbit_test_driver_internal_execute(
  tests : Array[MoonBitTestDriverInternalTestEntry],
) -> Unit {
  let async_tests = []
  for i in 0..<tests.length() {
    let entry = tests[i]
    moonbit_test_driver_internal_do_execute(
      async_tests,
      entry.filename(),
      entry.index(),
    )
  }
  MoonBit_Async_Test_Driver_Impl::run_async_tests(async_tests)
}

///|
#cfg(any(target="wasm", target="wasm-gc"))
pub fn moonbit_test_driver_internal_execute(
  filename : MoonbitTestDriverInternalExternString,
  index : Int,
) -> Unit {
  let filename = moonbit_test_driver_internal_get_file_name(filename)
  let async_tests = []
  moonbit_test_driver_internal_do_execute(async_tests, filename, index)
  MoonBit_Async_Test_Driver_Impl::run_async_tests(async_tests)
}

///|
#cfg(not(target="native"))
pub fn moonbit_test_driver_finish() -> Unit {
  
}

///|
fn moonbit_test_driver_internal_do_execute(
  async_tests : Array[async () -> Unit],
  filename : String,
  index : Int,
) -> Unit {
  // These two are internalized to avoid unused warnings

  fn handle_result(testname : String, message : String, skipped : Bool) {
    // In WASM, this function is called by the external test driver, but here
    // in native mode there's nothing that calls it, so we call it manually.
    if not(skipped) && MOONBIT_TEST_DRIVER_INTERNAL_IS_NATIVE {
      
    }
    let file_name = filename.escape()
    let test_name = testname.escape()
    let message = message.escape()
    @moonbitlang/core/prelude.println("----- BEGIN MOON TEST RESULT -----")
    @moonbitlang/core/prelude.println(
      "{\"package\": \"astrata/astrata/lib\", \"filename\": \{file_name}, \"index\": \"\{index}\", \"test_name\": \{test_name}, \"message\": \{message}}",
    )
    @moonbitlang/core/prelude.println("----- END MOON TEST RESULT -----")
  }

  fn error_to_string(err : Error) -> String {
    match err {
      @moonbitlang/core/prelude.Failure::Failure(e)
      | @moonbitlang/core/prelude.InspectError::InspectError(e)
      | @moonbitlang/core/prelude.SnapshotError::SnapshotError(e)
      | MoonBitTestDriverInternalJsError(e) => e
      e => moonbit_test_driver_internal_error_to_string(e)
    }
  }

  try {
    if !(MoonBit_Test_Driver_Internal_No_Args::run_test(
        async_tests, filename, index, handle_result, error_to_string,
      ) ||
      MoonBit_Test_Driver_Internal_With_Args::run_test(
        async_tests, filename, index, handle_result, error_to_string,
      ) ||
      MoonBit_Test_Driver_Internal_Async_No_Args::run_test(
        async_tests, filename, index, handle_result, error_to_string,
      ) ||
      MoonBit_Test_Driver_Internal_Async_With_Args::run_test(
        async_tests, filename, index, handle_result, error_to_string,
      ) ||
      MoonBit_Test_Driver_Internal_With_Bench_Args::run_test(
        async_tests, filename, index, handle_result, error_to_string,
      )) {
      raise MoonBitTestDriverInternalSkipTest(name="")
    }
  } catch {
    err => {
      guard err is MoonBitTestDriverInternalSkipTest(name~)
      handle_result(name, "skipped test", true)
    }
  }
}
