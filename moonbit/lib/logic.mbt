pub fn start_phase(
  bf : BattleField,
  side : FieldSide,
) -> (BattleField, Array[String]) {
  let field = field_for(bf, side)
  let new_field = draw_one_card(field)
  let new_bf = set_field(bf, side, new_field)
  let log = ["Start Phase: \{side}"]
  (new_bf, log)
}
pub fn evol_phase(
  bf : BattleField,
  side : FieldSide,
) -> (BattleField, Array[String]) {
  (bf, ["Evol Phase: \{side}"])
}
pub fn action_phase(
  bf : BattleField,
  side : FieldSide,
) -> (BattleField, Array[String]) {
  (bf, ["Action Phase: \{side}"])
}
pub fn attack_phase(
  bf : BattleField,
  side : FieldSide,
) -> (BattleField, Array[String]) {
  (bf, ["Attack Phase: \{side}"])
}
pub fn end_phase(
  bf : BattleField,
  side : FieldSide,
) -> (BattleField, Array[String]) {
  (bf, ["End Phase: \{side}"])
}
pub fn run_turn(
  bf : BattleField,
  side : FieldSide,
) -> (BattleField, Array[String]) {
  let (bf1, log1) = start_phase(bf, side)
  let (bf2, log2) = evol_phase(bf1, side)
  let (bf3, log3) = action_phase(bf2, side)
  let (bf4, log4) = attack_phase(bf3, side)
  let (bf5, log5) = end_phase(bf4, side)
  let all_logs = concat_logs([log1, log2, log3, log4, log5])
  (bf5, all_logs)
}
pub fn next_turn(state : GameState) -> GameState {
  let next_side = match state.turn {
    Own => Rival
    Rival => Own
  }
  {
    battlefield: state.battlefield,
    turn: next_side,
    phase: state.phase,
    log: state.log,
  }
}
pub fn apply_turn(state : GameState) -> GameState {
  let (new_bf, turn_logs) = run_turn(state.battlefield, state.turn)
  let new_log = state.log.copy()
  for entry in turn_logs {
    new_log.push(entry)
  }
  next_turn({
    battlefield: new_bf,
    turn: state.turn,
    phase: state.phase,
    log: new_log,
  })
}
fn set_field(bf : BattleField, side : FieldSide, field : Field) -> BattleField {
  match side {
    Own => { own: field, rival: bf.rival }
    Rival => { own: bf.own, rival: field }
  }
}
fn concat_logs(logs : Array[Array[String]]) -> Array[String] {
  let result : Array[String] = []
  for segment in logs {
    for entry in segment {
      result.push(entry)
    }
  }
  result
}
