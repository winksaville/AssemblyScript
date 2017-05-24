/// <reference path="../assembly.d.ts" />

export function add(a: int, b: double): short {
  return (a + (b as int)) as short;
}
