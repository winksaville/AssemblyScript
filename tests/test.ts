/// <reference path="../assembly.d.ts" />

import { Hello } from "./import";

export function add(a: int, b: double): short {
  a++;
  a--;
  ++a;
  --a;
  return (a + (b as int)) as short;
}

function start(): void {
  // wasm start function
}