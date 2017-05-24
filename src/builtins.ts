import {
  Compiler,
  byteType,
  sbyteType,
  shortType,
  ushortType,
  intType,
  uintType,
  longType,
  ulongType,
  uintptrType32,
  uintptrType64,
  boolType,
  floatType,
  doubleType
} from "./compiler";

import {
  WasmModule,
  WasmExpression,
  WasmType
} from "./wasm";

export function abs(compiler: Compiler, node: ts.Expression, expr: WasmExpression): WasmExpression {
  switch ((<any>node).wasmType) {

    case floatType:
      return compiler.module.f32.abs(expr);

    case doubleType:
      return compiler.module.f64.abs(expr);
  }
  throw Error("unsupported operation");
}

export function ceil(compiler: Compiler, node: ts.Expression, expr: WasmExpression): WasmExpression {
  switch ((<any>node).wasmType) {

    case floatType:
      return compiler.module.f32.ceil(expr);

    case doubleType:
      return compiler.module.f64.ceil(expr);
  }
  throw Error("unsupported operation");
}

export function floor(compiler: Compiler, node: ts.Expression, expr: WasmExpression): WasmExpression {
  switch ((<any>node).wasmType) {

    case floatType:
      return compiler.module.f32.floor(expr);

    case doubleType:
      return compiler.module.f64.floor(expr);
  }
  throw Error("unsupported operation");
}

export function sqrt(compiler: Compiler, node: ts.Expression, expr: WasmExpression): WasmExpression {
  switch ((<any>node).wasmType) {

    case floatType:
      return compiler.module.f32.sqrt(expr);

    case doubleType:
      return compiler.module.f64.sqrt(expr);
  }
  throw Error("unsupported operation");
}

export function trunc(compiler: Compiler, node: ts.Expression, expr: WasmExpression): WasmExpression {
  switch ((<any>node).wasmType) {

    case floatType:
      return compiler.module.f32.trunc(expr);

    case doubleType:
      return compiler.module.f64.trunc(expr);
  }
  throw Error("unsupported operation");
}

export function nearest(compiler: Compiler, node: ts.Expression, expr: WasmExpression): WasmExpression {
  switch ((<any>node).wasmType) {

    case floatType:
      return compiler.module.f32.nearest(expr);

    case doubleType:
      return compiler.module.f64.nearest(expr);
  }
  throw Error("unsupported operation");
}

export function min(compiler: Compiler, node: ts.Expression[], expr: WasmExpression[]): WasmExpression {
  if ((<any>node[0]).wasmType === (<any>node[1]).wasmType) {
    switch ((<any>node[0]).wasmType) {

      case floatType:
        return compiler.module.f32.min(expr[0], expr[1]);

      case doubleType:
        return compiler.module.f64.min(expr[0], expr[1]);
    }
  }
  throw Error("unsupported operation");
}

export function max(compiler: Compiler, node: ts.Expression, expr: WasmExpression[]): WasmExpression {
  if ((<any>node[0]).wasmType === (<any>node[1]).wasmType) {
    switch ((<any>node[0]).wasmType) {

      case floatType:
        return compiler.module.f32.max(expr[0], expr[1]);

      case doubleType:
        return compiler.module.f64.max(expr[0], expr[1]);
    }
  }
  throw Error("unsupported operation");
}
