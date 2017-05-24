/// <reference path="../types/assembly.d.ts" />

declare function window$alert(message: Ptr<ushort>): void;

enum MyEnum {
  ONE = 1,
  TWO = 2
}

class MyClass {
  hi(): void {

  }
  static hlo(): int {
    return 1;
  }
}

export function add(a: int, b: float): byte {
  return (a + (b as int)) as byte;
}

function start(): void {
}