/// <reference path="../assembly.d.ts" />

class Hello {
  world(param: int): int {
    this.anotherVoid();
    Hello.anotherStaticVoid();
    let a: int = 1;
    while (a) {
      return a;
    }
  }

  anotherVoid(): void {
  }

  static anotherStaticVoid(): void {
  }
}

export function add(a: int, b: double): short {
  return (a + (b as int)) as short;
}
