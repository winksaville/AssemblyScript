import * as binaryen from "../lib/binaryen";

export {
  Module as WasmModule,
  Signature as WasmSignature,
  Statement as WasmStatement,
  Expression as WasmExpression,
  I32Expression as WasmI32Expression,
  I64Expression as WasmI64Expression,
  F32Expression as WasmF32Expression,
  F64Expression as WasmF64Expression
} from "../lib/binaryen";

export enum WasmTypeKind {
  byte,
  sbyte,
  short,
  ushort,
  int,
  uint,
  long,
  ulong,
  bool,
  float,
  double,
  uintptr,
  void
}

export class WasmType {
  kind: WasmTypeKind;
  size: number;
  underlyingType: WasmType;

  constructor(kind: WasmTypeKind, size: number, underlyingType: WasmType = null) {
    this.kind = kind;
    this.size = size;
    this.underlyingType = underlyingType;
  }

  get isInteger(): boolean {
    switch (this.kind) {
      case WasmTypeKind.byte:
      case WasmTypeKind.sbyte:
      case WasmTypeKind.short:
      case WasmTypeKind.ushort:
      case WasmTypeKind.int:
      case WasmTypeKind.uint:
      case WasmTypeKind.long:
      case WasmTypeKind.ulong:
      case WasmTypeKind.bool:
      case WasmTypeKind.uintptr:
        return true;
    }
    return false;
  }

  get isFloat(): boolean {
    switch (this.kind) {
      case WasmTypeKind.float:
      case WasmTypeKind.double:
        return true;
    }
    return false;
  }

  get isSigned(): boolean {
    switch (this.kind) {
      case WasmTypeKind.sbyte:
      case WasmTypeKind.short:
      case WasmTypeKind.int:
      case WasmTypeKind.long:
        return true;
    }
    return false;
  }

  get isByte(): boolean {
    switch (this.kind) {
      case WasmTypeKind.byte:
      case WasmTypeKind.sbyte:
        return true;
    }
    return false;
  }

  get isShort(): boolean {
    switch (this.kind) {
      case WasmTypeKind.short:
      case WasmTypeKind.ushort:
        return true;
    }
    return false;
  }

  get isInt(): boolean {
    switch (this.kind) {
      case WasmTypeKind.int:
      case WasmTypeKind.uint:
        return true;
      case WasmTypeKind.uintptr:
        return this.size === 4;
    }
    return false;
  }

  get isLong(): boolean {
    switch (this.kind) {
      case WasmTypeKind.long:
      case WasmTypeKind.ulong:
        return true;
      case WasmTypeKind.uintptr:
        return this.size === 8;
    }
    return false;
  }

  withUnderlyingType(underlyingType: WasmType): WasmType {
    if (underlyingType == null)
      throw Error("underlying type must be specified");

    if (this.kind != WasmTypeKind.uintptr)
      throw Error("only pointers can have an underlying type");

    const type = new WasmType(this.kind, this.size);
    type.underlyingType = underlyingType;
    return type;
  }

  toSignatureIdentifier(uintptrType: WasmType): string {
    switch (this.kind) {

      case WasmTypeKind.byte:
      case WasmTypeKind.short:
      case WasmTypeKind.ushort:
      case WasmTypeKind.int:
      case WasmTypeKind.uint:
      case WasmTypeKind.bool:
        return "i";

      case WasmTypeKind.long:
      case WasmTypeKind.ulong:
        return "I";

      case WasmTypeKind.float:
        return "f";

      case WasmTypeKind.double:
        return "F";

      case WasmTypeKind.uintptr:
        return uintptrType.size == 4 ? "i" : "I";

      case WasmTypeKind.void:
        return "v";

    }
    throw Error("unexpected type");
  }

  toBinaryenType(uintptrType: WasmType): any {
    switch (this.kind) {

      case WasmTypeKind.byte:
      case WasmTypeKind.short:
      case WasmTypeKind.ushort:
      case WasmTypeKind.int:
      case WasmTypeKind.uint:
      case WasmTypeKind.bool:
        return binaryen.i32;

      case WasmTypeKind.long:
      case WasmTypeKind.ulong:
        return binaryen.i64;

      case WasmTypeKind.float:
        return binaryen.f32;

      case WasmTypeKind.double:
        return binaryen.f64;

      case WasmTypeKind.uintptr:
        return uintptrType.size == 4 ? binaryen.i32 : binaryen.i64;

      case WasmTypeKind.void:
        return binaryen.none;

    }
    throw Error("unexpected type");
  }

  toString(): string {
    return WasmTypeKind[this.kind];
  }
}

export enum WasmFunctionFlags {
  none   = 0,
  import = 1 << 0,
  export = 1 << 1
}

export interface WasmFunction {
  name: string,
  parameters: WasmType[],
  returnType: WasmType,
  flags: WasmFunctionFlags,
  signature: binaryen.Signature
}

export interface WasmVariable {
  index: number,
  type: WasmType
}

export interface WasmConstant {
  type: WasmType,
  value: any
}
