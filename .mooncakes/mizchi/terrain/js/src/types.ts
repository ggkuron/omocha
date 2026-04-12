export const enum Tile {
  Floor = 0,
  Wall = 1,
  Door = 2,
  LockedDoor = 3,
  Key = 4,
  Start = 5,
  Goal = 6,
  Corridor = 7,
  Water = 8,
  Empty = 9,
}

export type RenderMode = "grid2d" | "hex" | "grid3d" | "quarter";

export interface GridMetrics {
  open_ratio: number;
  room_count: number;
  connected: number;
  largest_component: number;
  dead_end_count: number;
}

export interface HexInfo {
  cols: number;
  rows: number;
  orientation: number;
}

export interface Grid3DInfo {
  width: number;
  height: number;
  depth: number;
}

export interface GenerateResult {
  ascii: string;
  cells: number[];
  width: number;
  height: number;
  metrics: GridMetrics;
  renderMode: RenderMode;
  hexInfo?: HexInfo;
  grid3dInfo?: Grid3DInfo;
  heights?: number[];
  maxHeight?: number;
}

export interface AlgorithmParam {
  name: string;
  min: number;
  max: number;
  default: number;
  step: number;
}

export interface AlgorithmDef {
  id: number;
  name: string;
  category: string;
  params: AlgorithmParam[];
}
