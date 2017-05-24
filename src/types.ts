import {
  WasmTypeKind,
  WasmType
} from "./wasm";

export const byteType      = new WasmType(WasmTypeKind.byte   , 1);
export const sbyteType     = new WasmType(WasmTypeKind.sbyte  , 1);
export const shortType     = new WasmType(WasmTypeKind.short  , 2);
export const ushortType    = new WasmType(WasmTypeKind.ushort , 2);
export const intType       = new WasmType(WasmTypeKind.int    , 4);
export const uintType      = new WasmType(WasmTypeKind.uint   , 4);
export const longType      = new WasmType(WasmTypeKind.long   , 8);
export const ulongType     = new WasmType(WasmTypeKind.ulong  , 8);
export const boolType      = new WasmType(WasmTypeKind.bool   , 4);
export const floatType     = new WasmType(WasmTypeKind.float  , 4);
export const doubleType    = new WasmType(WasmTypeKind.double , 8);
export const uintptrType32 = new WasmType(WasmTypeKind.uintptr, 4);
export const uintptrType64 = new WasmType(WasmTypeKind.uintptr, 8);
export const voidType      = new WasmType(WasmTypeKind.void   , 0);
