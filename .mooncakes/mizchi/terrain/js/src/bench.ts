import { generate } from "./index.js";
import { ALGORITHMS } from "./algorithms.js";

const SEEDS = 10;
const WIDTH = 40;
const HEIGHT = 25;

interface BenchResult {
  id: number;
  name: string;
  category: string;
  mean: number;
  min: number;
  max: number;
}

const results: BenchResult[] = [];

for (const algo of ALGORITHMS) {
  const times: number[] = [];
  for (let seed = 0; seed < SEEDS; seed++) {
    const defaults = algo.params.map(p => p.default);
    const start = performance.now();
    generate(algo.id, seed, WIDTH, HEIGHT, defaults);
    const elapsed = performance.now() - start;
    times.push(elapsed);
  }
  const mean = times.reduce((a, b) => a + b, 0) / times.length;
  const min = Math.min(...times);
  const max = Math.max(...times);
  results.push({ id: algo.id, name: algo.name, category: algo.category, mean, min, max });
}

results.sort((a, b) => b.mean - a.mean);

console.log(`\nBenchmark: ${ALGORITHMS.length} algorithms x ${SEEDS} seeds (${WIDTH}x${HEIGHT})\n`);
console.log("  ID | Mean (ms) | Min (ms) | Max (ms) | Algorithm");
console.log("-----|-----------|----------|----------|----------");
for (const r of results) {
  const id = String(r.id).padStart(4);
  const mean = r.mean.toFixed(2).padStart(9);
  const min = r.min.toFixed(2).padStart(8);
  const max = r.max.toFixed(2).padStart(8);
  console.log(`${id} |${mean} |${min} |${max} | [${r.category}] ${r.name}`);
}

const totalMean = results.reduce((s, r) => s + r.mean, 0);
console.log(`\nTotal mean: ${totalMean.toFixed(2)} ms`);
