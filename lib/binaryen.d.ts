declare module binaryen {

  const none: Type;
  const i32: Type;
  const i64: Type;
  const f32: Type;
  const f64: Type;

  class Module {

    addFunctionType(name: string, resultType: Type, paramTypes: Type[]): Signature;
    addFunction(name: string, functionType: number, varTypes: number[], body: number[]): number;
    addImport(internalName: string, externalModuleName: string, externalBaseName: string, functionType?: number): void;
    addExport(internalName: string, externalName: string): void;
    setFunctionTable(funcs: number[]): void;
    setMemory(initial: number, maximum: number, exportName?: string, segments?: number[]);
    setStart(start: number): void;

    emitBinary(): Uint8Array;
    emitText(): string;
    validate(): void;
    optimize(): void;
    autoDrop(): void;
    interpret(): void;
    dispose(): void;

    i32: {
      load(offset: number, align: number, ptr: number): OpCode,
      load8_s(offset: number, align: number, ptr: number): OpCode,
      load8_u(offset: number, align: number, ptr: number): OpCode
      load16_s(offset: number, align: number, ptr: number): OpCode,
      load16_u(offset: number, align: number, ptr: number): OpCode,
      store(offset: number, align: number, ptr: number, value: Expression): OpCode,
      store8(offset: number, align: number, ptr: number, value: Expression): OpCode,
      store16(offset: number, align: number, ptr: number, value: Expression): OpCode,
      const(value: number): OpCode;
      clz(value: Expression): OpCode;
      ctz(value: Expression): OpCode;
      popcnt(value: Expression): OpCode;
      eqz(value: Expression): OpCode;
      trunc_s: {
        f32(value: Expression): OpCode;
        f64(value: Expression): OpCode;
      };
      trunc_u: {
        f32(value: Expression): OpCode;
        f64(value: Expression): OpCode;
      }
      reinterpret(value: Expression): OpCode;
      wrap(value: Expression): OpCode;
      add(left: Expression, right: Expression): OpCode;
      sub(left: Expression, right: Expression): OpCode;
      mul(left: Expression, right: Expression): OpCode;
      div_s(left: Expression, right: Expression): OpCode;
      div_u(left: Expression, right: Expression): OpCode;
      rem_s(left: Expression, right: Expression): OpCode;
      rem_u(left: Expression, right: Expression): OpCode;
      and(left: Expression, right: Expression): OpCode;
      or(left: Expression, right: Expression): OpCode;
      xor(left: Expression, right: Expression): OpCode;
      shl(left: Expression, right: Expression): OpCode;
      shr_u(left: Expression, right: Expression): OpCode;
      shr_s(left: Expression, right: Expression): OpCode;
      rotl(left: Expression, right: Expression): OpCode;
      rotr(left: Expression, right: Expression): OpCode;
      eq(left: Expression, right: Expression): OpCode;
      ne(left: Expression, right: Expression): OpCode;
      lt_s(left: Expression, right: Expression): OpCode;
      lt_u(left: Expression, right: Expression): OpCode;
      le_s(left: Expression, right: Expression): OpCode;
      le_u(left: Expression, right: Expression): OpCode;
      gt_s(left: Expression, right: Expression): OpCode;
      gt_u(left: Expression, right: Expression): OpCode;
      ge_s(left: Expression, right: Expression): OpCode;
      ge_u(left: Expression, right: Expression): OpCode;
    }

    i64: {
      load(offset: number, align: number, ptr: number): OpCode,
      load8_s(offset: number, align: number, ptr: number): OpCode,
      load8_u(offset: number, align: number, ptr: number): OpCode
      load16_s(offset: number, align: number, ptr: number): OpCode,
      load16_u(offset: number, align: number, ptr: number): OpCode,
      load32_s(offset: number, align: number, ptr: number): OpCode,
      load32_u(offset: number, align: number, ptr: number): OpCode,
      store(offset: number, align: number, ptr: number, value: Expression): OpCode,
      store8(offset: number, align: number, ptr: number, value: Expression): OpCode,
      store16(offset: number, align: number, ptr: number, value: Expression): OpCode,
      store32(offset: number, align: number, ptr: number, value: Expression): OpCode,
      const(low: number, right: number): OpCode;
      clz(value: Expression): OpCode;
      ctz(value: Expression): OpCode;
      popcnt(value: Expression): OpCode;
      eqz(value: Expression): OpCode;
      trunc_s: {
        f32(value: Expression): OpCode;
        f64(value: Expression): OpCode;
      };
      trunc_u: {
        f32(value: Expression): OpCode;
        f64(value: Expression): OpCode;
      }
      reinterpret(value: Expression): OpCode;
      extend_s(value: Expression): OpCode;
      extend_u(value: Expression): OpCode;
      wrap(value: Expression): OpCode;
      add(left: Expression, right: Expression): OpCode;
      sub(left: Expression, right: Expression): OpCode;
      mul(left: Expression, right: Expression): OpCode;
      div_s(left: Expression, right: Expression): OpCode;
      div_u(left: Expression, right: Expression): OpCode;
      rem_s(left: Expression, right: Expression): OpCode;
      rem_u(left: Expression, right: Expression): OpCode;
      and(left: Expression, right: Expression): OpCode;
      or(left: Expression, right: Expression): OpCode;
      xor(left: Expression, right: Expression): OpCode;
      shl(left: Expression, right: Expression): OpCode;
      shr_u(left: Expression, right: Expression): OpCode;
      shr_s(left: Expression, right: Expression): OpCode;
      rotl(left: Expression, right: Expression): OpCode;
      rotr(left: Expression, right: Expression): OpCode;
      eq(left: Expression, right: Expression): OpCode;
      ne(left: Expression, right: Expression): OpCode;
      lt_s(left: Expression, right: Expression): OpCode;
      lt_u(left: Expression, right: Expression): OpCode;
      le_s(left: Expression, right: Expression): OpCode;
      le_u(left: Expression, right: Expression): OpCode;
      gt_s(left: Expression, right: Expression): OpCode;
      gt_u(left: Expression, right: Expression): OpCode;
      ge_s(left: Expression, right: Expression): OpCode;
      ge_u(left: Expression, right: Expression): OpCode;
    }

    // TODO: f32, f64

    block(label: string, children: Statement[]): OpCode;
    if(condition: Expression, ifTrue: Statement, ifFalse: Statement): OpCode;
    loop(label: string, body: Statement): OpCode;
    break(label: string, condition: Expression, value?: Expression): OpCode;
    switch(labels: string[], defaultLabel: string, condition: Expression, value?: Expression): OpCode;
    call(name: string, operands: Expression[], type: Type): OpCode;
    callIndirect(target: Expression, operands: Expression[], type: Type): OpCode;
    getLocal(index: number, type: Type): OpCode;
    setLocal(index: number, value: Expression): OpCode;
    teeLocal(index: number, value: Expression): OpCode;
    select(condition: Expression, ifTrue: Expression, ifFalse: Expression): OpCode;
    drop(value: Expression): OpCode;
    return(value?: Expression): OpCode;
    nop(): OpCode;
    unreachable(): OpCode;
  }

  // these are actually pointers internally but exist for the sake of clarity
  type Type = number;
  type OpCode = number;
  type Statement = number;
  type Expression = number;
  type Signature = number;
  type Function = number;
}

export = binaryen;
