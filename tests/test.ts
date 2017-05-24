/// <reference path="../types/assembly.d.ts" />

export function add(a: int, b: float): float {
  return (a as float) + b + 1e-06;
}

function start(): void {
}

function bin(): int {
  return ~1;
}
