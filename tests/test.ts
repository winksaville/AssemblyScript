/// <reference path="../types/assembly.d.ts" />

declare function alert(message: IntPtr<ushort>): void;

export function add(a: int, b: int): int {
  return 12 + a + b;
}

function start<T>(): void {
}