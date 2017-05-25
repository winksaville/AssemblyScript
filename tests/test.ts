/// <reference path="../assembly.d.ts" />

import { Hello } from "./import";

export function add(a: int, b: double): short {
  return (a + (b as int)) as short;
}
