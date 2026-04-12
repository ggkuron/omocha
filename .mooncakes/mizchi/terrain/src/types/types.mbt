///|
pub(all) enum Tile {
  Floor
  Wall
  Door
  LockedDoor
  Key
  Start
  Goal
  Corridor
  Water
  Empty
} derive(Eq, Show)

///|
pub fn Tile::to_char(self : Tile) -> Char {
  match self {
    Floor => '.'
    Wall => '#'
    Door => '+'
    LockedDoor => 'L'
    Key => 'K'
    Start => 'S'
    Goal => 'G'
    Corridor => ','
    Water => '~'
    Empty => ' '
  }
}

///|
pub(all) struct Point {
  x : Int
  y : Int
} derive(Eq, Show, Compare)

///|
pub(all) struct Rect {
  x : Int
  y : Int
  w : Int
  h : Int
} derive(Eq, Show)

///|
pub fn Rect::center(self : Rect) -> Point {
  { x: self.x + self.w / 2, y: self.y + self.h / 2 }
}

///|
pub fn Rect::contains(self : Rect, p : Point) -> Bool {
  p.x >= self.x && p.x < self.x + self.w && p.y >= self.y && p.y < self.y + self.h
}

///|
pub fn Rect::intersects(self : Rect, other : Rect) -> Bool {
  self.x < other.x + other.w &&
  self.x + self.w > other.x &&
  self.y < other.y + other.h &&
  self.y + self.h > other.y
}

///|
pub(all) struct Room {
  rect : Rect
  id : Int
} derive(Show)

///|
pub(all) struct Edge {
  from : Int
  to : Int
} derive(Show)

///|
pub(all) enum NodeKind {
  StartNode
  GoalNode
  RoomNode
  KeyNode
  LockNode
} derive(Eq, Show)

///|
pub(all) struct GraphNode {
  id : Int
  kind : NodeKind
  children : Array[Int]
} derive(Show)
