export class Hello {
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