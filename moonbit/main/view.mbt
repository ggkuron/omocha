let screen_w : Int = 800
let screen_h : Int = 600
let screen_wf : Double = 800.0
let card_w : Double = 80.0
let card_h : Double = 110.0
let c_bg_rival : Int = 0x0f3460
let c_bg_own : Int = 0x16213e
let c_bg_hud : Int = 0x0a0a1a
let c_origin : Int = 0xf0a500
let c_satellite : Int = 0x4ecdc4
let c_empty_slot : Int = 0x2d2d5a
let c_sealed_slot : Int = 0x555577
let c_hand : Int = 0x6c8ebf
let c_text : Int = 0xffffff
let c_turn_own : Int = 0x4caf50
let c_turn_rival : Int = 0xe53935
let c_highlight : Int = 0xffff00
fn is_slot_selected(
  selection : @lib.Selection,
  side : @lib.FieldSide,
  zone : @lib.FieldZone,
  slot : @lib.BattleFieldSlot,
) -> Bool {
  match selection {
    @lib.SlotSelect(s, z, sl) => s == side && z == zone && sl == slot
    _ => false
  }
}
fn view_game_state(
  state : @lib.GameState,
  selection : @lib.Selection,
) -> @scene.SceneNode {
  let bf = state.battlefield
  @scene.fragment([
    @scene.rect(x=0.0, y=0.0, w=screen_wf, h=260.0, fill=c_bg_rival),
    @scene.rect(x=0.0, y=260.0, w=screen_wf, h=80.0, fill=c_bg_hud),
    @scene.rect(x=0.0, y=340.0, w=screen_wf, h=260.0, fill=c_bg_own),
    view_rival_field(bf.rival, selection),
    view_hud(state),
    view_own_field(bf.own, selection),
  ])
}
fn view_rival_field(
  field : @lib.Field,
  selection : @lib.Selection,
) -> @scene.SceneNode {
  let side = @lib.Rival
  @scene.fragment([
    @scene.label(x=10.0, y=15.0, content="RIVAL", color=c_text, scale=0.8),
    view_hands(field.hands, selection, side, y=10.0),
    view_slot_row(
      field.back_l,
      field.back_r,
      y=100.0,
      hl_left=is_slot_selected(selection, side, @lib.BattleZone, @lib.BackL),
      hl_right=is_slot_selected(selection, side, @lib.BattleZone, @lib.BackR),
    ),
    view_slot_row(
      field.front_l,
      field.front_r,
      y=165.0,
      hl_left=is_slot_selected(selection, side, @lib.BattleZone, @lib.FrontL),
      hl_right=is_slot_selected(selection, side, @lib.BattleZone, @lib.FrontR),
    ),
    view_origin(
      field.origin,
      x=360.0,
      y=140.0,
      highlighted=is_slot_selected(
        selection,
        side,
        @lib.BattleZone,
        @lib.OriginSlot,
      ),
    ),
  ])
}
fn view_own_field(
  field : @lib.Field,
  selection : @lib.Selection,
) -> @scene.SceneNode {
  let side = @lib.Own
  @scene.fragment([
    @scene.label(x=10.0, y=355.0, content="OWN", color=c_text, scale=0.8),
    view_origin(
      field.origin,
      x=360.0,
      y=345.0,
      highlighted=is_slot_selected(
        selection,
        side,
        @lib.BattleZone,
        @lib.OriginSlot,
      ),
    ),
    view_slot_row(
      field.front_l,
      field.front_r,
      y=400.0,
      hl_left=is_slot_selected(selection, side, @lib.BattleZone, @lib.FrontL),
      hl_right=is_slot_selected(selection, side, @lib.BattleZone, @lib.FrontR),
    ),
    view_slot_row(
      field.back_l,
      field.back_r,
      y=458.0,
      hl_left=is_slot_selected(selection, side, @lib.BattleZone, @lib.BackL),
      hl_right=is_slot_selected(selection, side, @lib.BattleZone, @lib.BackR),
    ),
    view_hands(field.hands, selection, side, y=490.0),
  ])
}
fn view_slot_row(
  left : @lib.SatelliteSlot,
  right : @lib.SatelliteSlot,
  y~ : Double,
  hl_left? : Bool = false,
  hl_right? : Bool = false,
) -> @scene.SceneNode {
  let center_x = 400.0
  let gap = 10.0
  @scene.fragment([
    view_satellite_slot(
      left,
      x=center_x - card_w - gap,
      y~,
      highlighted=hl_left,
    ),
    view_satellite_slot(right, x=center_x + gap, y~, highlighted=hl_right),
  ])
}
fn view_satellite_slot(
  slot : @lib.SatelliteSlot,
  x~ : Double,
  y~ : Double,
  highlighted? : Bool = false,
) -> @scene.SceneNode {
  let base = match slot {
    @lib.Empty =>
      @scene.rect(x~, y~, w=card_w, h=card_h, fill=c_empty_slot, alpha=0.7)
    @lib.Sealed =>
      @scene.rect(x~, y~, w=card_w, h=card_h, fill=c_sealed_slot, alpha=0.8)
    @lib.Assigned(inst) =>
      @scene.fragment([
        @scene.rect(x~, y~, w=card_w, h=card_h, fill=c_satellite),
        @scene.label(
          x=x + 4.0,
          y=y + 4.0,
          content=inst.card.prop.name,
          color=0x000000,
          scale=0.55,
        ),
        @scene.label(
          x=x + 4.0,
          y=y + card_h - 20.0,
          content="ATK:\{inst.card.battler.atk} HP:\{inst.battle.current_hp}",
          color=0x000000,
          scale=0.5,
        ),
      ])
  }
  view_with_highlight(base, x~, y~, highlighted~)
}
fn view_origin(
  origin : @lib.OriginInstance,
  x~ : Double,
  y~ : Double,
  highlighted? : Bool = false,
) -> @scene.SceneNode {
  let base = @scene.fragment([
    @scene.rect(x~, y~, w=card_w, h=card_h, fill=c_origin),
    @scene.label(
      x=x + 4.0,
      y=y + 4.0,
      content="ORIGIN",
      color=0x000000,
      scale=0.55,
    ),
    @scene.label(
      x=x + 4.0,
      y=y + card_h - 20.0,
      content="HP:\{origin.state.current_hp}/\{origin.card.max_hp}",
      color=0x000000,
      scale=0.5,
    ),
  ])
  view_with_highlight(base, x~, y~, highlighted~)
}
fn view_hands(
  hands : Array[@lib.HandCard],
  selection : @lib.Selection,
  side : @lib.FieldSide,
  y~ : Double,
) -> @scene.SceneNode {
  let zone_highlighted = match selection {
    @lib.FieldSelect(s, @lib.GuardZone) => s == side
    _ => false
  }
  let n = hands.length()
  if n == 0 {
    return @scene.fragment([])
  }
  let gap = 5.0
  let total_w = card_w * n.to_double() + gap * (n - 1).to_double()
  let start_x = (screen_wf - total_w) / 2.0
  let nodes : Array[@scene.SceneNode] = Array::new()
  for i in 0..<n {
    let x = start_x + (card_w + gap) * i.to_double()
    nodes.push(view_hand_card(hands[i], x~, y~, highlighted=zone_highlighted))
  }
  @scene.fragment(nodes)
}
fn view_hand_card(
  card : @lib.HandCard,
  x~ : Double,
  y~ : Double,
  highlighted? : Bool = false,
) -> @scene.SceneNode {
  let name = match card {
    @lib.HSatellite(inst) => inst.card.prop.name
    @lib.HPortion(inst) => inst.card.prop.name
    @lib.HGuard(inst) => inst.card.prop.name
  }
  let base = @scene.fragment([
    @scene.rect(x~, y~, w=card_w, h=card_h, fill=c_hand),
    @scene.label(x=x + 4.0, y=y + 4.0, content=name, color=0x000000, scale=0.5),
  ])
  view_with_highlight(base, x~, y~, highlighted~)
}
fn view_with_highlight(
  base : @scene.SceneNode,
  x~ : Double,
  y~ : Double,
  highlighted~ : Bool,
) -> @scene.SceneNode {
  if highlighted {
    @scene.fragment([
      base,
      @scene.rect_outline(
        x~,
        y~,
        w=card_w,
        h=card_h,
        color=c_highlight,
        line_width=3.0,
      ),
    ])
  } else {
    base
  }
}
fn view_hud(state : @lib.GameState) -> @scene.SceneNode {
  let turn_color = match state.turn {
    @lib.Own => c_turn_own
    @lib.Rival => c_turn_rival
  }
  let turn_label = match state.turn {
    @lib.Own => "YOUR TURN"
    @lib.Rival => "RIVAL TURN"
  }
  @scene.fragment([
    @scene.rect(x=290.0, y=265.0, w=220.0, h=70.0, fill=turn_color),
    @scene.label(x=315.0, y=278.0, content=turn_label, color=c_text, scale=1.0),
    @scene.label(
      x=315.0,
      y=308.0,
      content="\{state.phase}",
      color=c_text,
      scale=0.7,
    ),
    @scene.label(
      x=530.0,
      y=290.0,
      content="End Turn [E]",
      color=c_text,
      scale=0.7,
    ),
  ])
}
