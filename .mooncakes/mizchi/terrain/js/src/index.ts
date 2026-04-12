import "../terrain.js";
import type { GenerateResult, GridMetrics, HexInfo, Grid3DInfo, RenderMode } from "./types.js";

declare const globalThis: {
  __terrain_generate: (algo: number, seed: number, w: number, h: number) => string;
  __terrain_cells: string;
  __terrain_metrics: string;
  __terrain_render_mode: string;
  __terrain_params: number[] | undefined;
  __terrain_hex_info: HexInfo | undefined;
  __terrain_3d_info: Grid3DInfo | undefined;
  __terrain_heights: string | undefined;
  __terrain_max_height: number | undefined;
};

export function generate(
  algoId: number,
  seed: number,
  width: number,
  height: number,
  params?: number[],
): GenerateResult {
  globalThis.__terrain_params = params;
  const ascii = globalThis.__terrain_generate(algoId, seed, width, height);

  const cellsCsv = globalThis.__terrain_cells ?? "";
  const cells = cellsCsv ? cellsCsv.split(",").map(Number) : [];

  const metrics: GridMetrics = globalThis.__terrain_metrics
    ? JSON.parse(globalThis.__terrain_metrics)
    : { open_ratio: 0, room_count: 0, connected: 0, largest_component: 0, dead_end_count: 0 };

  const renderMode = (globalThis.__terrain_render_mode ?? "grid2d") as RenderMode;

  const result: GenerateResult = {
    ascii,
    cells,
    width,
    height,
    metrics,
    renderMode,
  };

  if (globalThis.__terrain_hex_info) {
    result.hexInfo = globalThis.__terrain_hex_info;
  }
  if (globalThis.__terrain_3d_info) {
    result.grid3dInfo = globalThis.__terrain_3d_info;
  }
  if (globalThis.__terrain_heights) {
    result.heights = globalThis.__terrain_heights.split(",").map(Number);
    result.maxHeight = globalThis.__terrain_max_height;
  }

  return result;
}

export { ALGORITHMS } from "./algorithms.js";
export type {
  Tile,
  RenderMode,
  GridMetrics,
  HexInfo,
  Grid3DInfo,
  GenerateResult,
  AlgorithmParam,
  AlgorithmDef,
} from "./types.js";
