///|
struct Game {
  mut state : @lib.GameState
  mut selection : @lib.Selection
  input : @inpututil.InputHelper
}

///|
fn Game::new() -> Game {
  {
    state: @lib.initial_game_state(),
    selection: @lib.SideSelect(@lib.Own),
    input: @inpututil.new_input_helper(),
  }
}

///|
impl @scene.SceneGame for Game with update_game(self, input) {
  @inpututil.update_input_helper(self.input, input)
  let bf = self.state.battlefield
  let sel = self.selection
  if @inpututil.is_key_just_pressed(
      self.input.key_state,
      @inpututil.key_left_web,
    ) {
    self.selection = @lib.prev_selection(sel)
  }
  if @inpututil.is_key_just_pressed(
      self.input.key_state,
      @inpututil.key_right_web,
    ) {
    self.selection = @lib.next_selection(sel)
  }
  if @inpututil.is_up_just_pressed(self.input) {
    self.selection = @lib.prev_in_level(bf, sel)
  }
  if @inpututil.is_down_just_pressed(self.input) {
    self.selection = @lib.next_in_level(bf, sel)
  }
  if @inpututil.is_confirm_just_pressed(self.input) {
    self.state = @lib.apply_play_own_card(self.state)
  }
  if @inpututil.is_key_just_pressed(self.input.key_state, @inpututil.key_e) {
    self.state = @lib.apply_turn(self.state)
  }
}

///|
impl @scene.SceneGame for Game with view_game(self) {
  view_game_state(self.state, self.selection)
}
