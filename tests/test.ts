/// <reference path="../types/assembly.d.ts" />

declare function window$alert(message: Ptr<ushort>): void;

export function add(a: int, b: int): byte {
  return (a as short) + b;
}

function start(): void {
}