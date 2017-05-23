/// <reference path="../types/assembly.d.ts" />

declare function alert(message: IntPtr<ushort>): void;

export function add(a: int, b: float): byte {
  return a + b;
}

function start(): void {
}