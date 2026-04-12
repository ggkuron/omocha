// ターンロジック ホワイトボックステスト
//
// 対応Haskellテスト: test/Astrata/LogicSpec.hs
// 対象実装: logic.mbt (start_phase, evol_phase, action_phase, attack_phase,
//                       end_phase, run_turn)

// ─────────────────────────────────────────────────
// テスト用ヘルパー
// ─────────────────────────────────────────────────

// own の stack に1枚を積んだバトルフィールドを返す
//
// start_phase がドローを実行するには own.stack が空でない必要がある。
///|
fn test_bf_for_logic() -> BattleField {
  make_battle_field_with_own_stack()
}

// ─────────────────────────────────────────────────
// start_phase テスト
// ─────────────────────────────────────────────────

// Given: own に stack が1枚ある
// When:  start_phase(bf, Own) を実行
// Then:  own.hands が1枚増え、ログに "Start Phase" が含まれる

///|
test "start_phase: ドローが実行される - hands+1" {
  let bf = test_bf_for_logic()
  let hands_before = bf.own.hands.length()
  let (result_bf, _logs) = start_phase(bf, Own)
  assert_eq(result_bf.own.hands.length(), hands_before + 1)
}

///|
test "start_phase: ドローが実行される - stack-1" {
  let bf = test_bf_for_logic()
  let stack_before = bf.own.stack.length()
  let (result_bf, _logs) = start_phase(bf, Own)
  assert_eq(result_bf.own.stack.length(), stack_before - 1)
}

///|
test "start_phase: ログに 'Start Phase: Own' を出力" {
  let bf = test_bf_for_logic()
  let (_result_bf, logs) = start_phase(bf, Own)
  assert_eq(logs.length(), 1)
  assert_eq(logs[0], "Start Phase: Own")
}

// Given: rival側で start_phase を実行
// When:  start_phase(bf, Rival)
// Then:  rival フィールドは変化なし（rival.stack が空のため）、ログは "Start Phase: Rival"

///|
test "start_phase: Rival 側 - ログに 'Start Phase: Rival' を出力" {
  let bf = test_bf_for_logic()
  let (_result_bf, logs) = start_phase(bf, Rival)
  assert_eq(logs.length(), 1)
  assert_eq(logs[0], "Start Phase: Rival")
}

// ─────────────────────────────────────────────────
// 各フェーズのログ出力テスト
// ─────────────────────────────────────────────────

///|
test "evol_phase: ログに 'Evol Phase: Own' を出力" {
  let bf = test_bf_for_logic()
  let (_result_bf, logs) = evol_phase(bf, Own)
  assert_eq(logs.length(), 1)
  assert_eq(logs[0], "Evol Phase: Own")
}

///|
test "action_phase: ログに 'Action Phase: Own' を出力" {
  let bf = test_bf_for_logic()
  let (_result_bf, logs) = action_phase(bf, Own)
  assert_eq(logs.length(), 1)
  assert_eq(logs[0], "Action Phase: Own")
}

///|
test "attack_phase: ログに 'Attack Phase: Own' を出力" {
  let bf = test_bf_for_logic()
  let (_result_bf, logs) = attack_phase(bf, Own)
  assert_eq(logs.length(), 1)
  assert_eq(logs[0], "Attack Phase: Own")
}

///|
test "end_phase: ログに 'End Phase: Own' を出力" {
  let bf = test_bf_for_logic()
  let (_result_bf, logs) = end_phase(bf, Own)
  assert_eq(logs.length(), 1)
  assert_eq(logs[0], "End Phase: Own")
}

// ─────────────────────────────────────────────────
// run_turn テスト
// ─────────────────────────────────────────────────

// Given: stack に1枚ある
// When:  run_turn(bf, Own)
// Then:  ログが5件（5フェーズ分）、順番通り

///|
test "run_turn: ログが5件出力される" {
  let bf = test_bf_for_logic()
  let (_result_bf, logs) = run_turn(bf, Own)
  assert_eq(logs.length(), 5)
}

///|
test "run_turn: ログの順番が正しい (Start→Evol→Action→Attack→End)" {
  let bf = test_bf_for_logic()
  let (_result_bf, logs) = run_turn(bf, Own)
  assert_eq(logs[0], "Start Phase: Own")
  assert_eq(logs[1], "Evol Phase: Own")
  assert_eq(logs[2], "Action Phase: Own")
  assert_eq(logs[3], "Attack Phase: Own")
  assert_eq(logs[4], "End Phase: Own")
}

// Given: start_phase でドローが実行される
// When:  run_turn(bf, Own)
// Then:  own.hands が1枚増える

///|
test "run_turn: start_phase によるドローが反映される" {
  let bf = test_bf_for_logic()
  let hands_before = bf.own.hands.length()
  let (result_bf, _logs) = run_turn(bf, Own)
  assert_eq(result_bf.own.hands.length(), hands_before + 1)
}

// rival フィールドは run_turn(_, Own) では変化しない

///|
test "run_turn: rival フィールドは変化しない (Own のターン)" {
  let bf = test_bf_for_logic()
  let rival_hands_before = bf.rival.hands.length()
  let (result_bf, _logs) = run_turn(bf, Own)
  assert_eq(result_bf.rival.hands.length(), rival_hands_before)
}

// ─────────────────────────────────────────────────
// next_turn / apply_turn テスト
// ─────────────────────────────────────────────────

// Given: Own ターン
// When:  next_turn
// Then:  Rival ターンに切り替わる

///|
test "next_turn: Own → Rival" {
  let state : GameState = {
    battlefield: initial_battle_field,
    turn: Own,
    phase: StartPhase,
    log: Array::new(),
  }
  let next = next_turn(state)
  assert_eq(next.turn, Rival)
}

// Given: Rival ターン
// When:  next_turn
// Then:  Own ターンに切り替わる

///|
test "next_turn: Rival → Own" {
  let state : GameState = {
    battlefield: initial_battle_field,
    turn: Rival,
    phase: StartPhase,
    log: Array::new(),
  }
  let next = next_turn(state)
  assert_eq(next.turn, Own)
}

// apply_turn はターンを進め、ログを状態に蓄積する

///|
test "apply_turn: ログが state.log に追加される" {
  let bf = test_bf_for_logic()
  let state : GameState = {
    battlefield: bf,
    turn: Own,
    phase: StartPhase,
    log: Array::new(),
  }
  let result = apply_turn(state)
  // run_turn は5件のログを出力するはず
  assert_eq(result.log.length(), 5)
}
