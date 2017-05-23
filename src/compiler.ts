/// <reference types="node" />

import * as ts from "byots";
import * as binaryen from "../lib/binaryen";
import * as Long from "long";
import { formatDiagnostics, formatDiagnosticsWithColorAndContext } from "./util/diagnostics";

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
}

export const byteType   = new WasmType(WasmTypeKind.byte   , 1);
export const sbyteType  = new WasmType(WasmTypeKind.sbyte  , 1);
export const shortType  = new WasmType(WasmTypeKind.short  , 2);
export const ushortType = new WasmType(WasmTypeKind.ushort , 2);
export const intType    = new WasmType(WasmTypeKind.int    , 4);
export const uintType   = new WasmType(WasmTypeKind.uint   , 4);
export const longType   = new WasmType(WasmTypeKind.long   , 8);
export const ulongType  = new WasmType(WasmTypeKind.ulong  , 8);
export const boolType   = new WasmType(WasmTypeKind.bool   , 4);
export const floatType  = new WasmType(WasmTypeKind.float  , 4);
export const doubleType = new WasmType(WasmTypeKind.double , 8);
export const voidType   = new WasmType(WasmTypeKind.void   , 0);

enum WasmFunctionFlags {
  none   = 0,
  import = 1 << 0,
  export = 1 << 1
}

interface WasmFunction {
  name: string,
  parameters: WasmType[],
  returnType: WasmType,
  flags: WasmFunctionFlags,
  signature: binaryen.Signature
}

interface WasmVariable {
  index: number,
  type: WasmType
}

interface WasmConstant {
  type: WasmType,
  value: any
}

export class Compiler {
  program: ts.Program;
  checker: ts.TypeChecker;
  diagnostics: ts.DiagnosticCollection;
  uintptrType: WasmType;
  module: binaryen.Module;
  signatures: { [key: string]: binaryen.Signature } = {};
  constants: { [key: string]: WasmConstant } = {};

  static compile(filename: string): binaryen.Module {
    let program = ts.createProgram([ filename ], {
      target: ts.ScriptTarget.Latest,
      module: ts.ModuleKind.None,
      noLib: true,
      experimentalDecorators: true
    });

    let compiler = new Compiler(program);

    compiler.initialize();

    // bail out if there were initialization errors
    let diagnostics = compiler.diagnostics.getDiagnostics();
    for (let diagnostic of diagnostics) {
      if (diagnostic.category === ts.DiagnosticCategory.Error)
        return null;
    }

    compiler.compile();

    // bail out if there were compilation errors
    diagnostics = compiler.diagnostics.getDiagnostics();
    for (let diagnostic of diagnostics) {
      if (diagnostic.category === ts.DiagnosticCategory.Error)
        return null;
    }

    console.log(compiler.constants);

    return compiler.module;
  }

  constructor(program: ts.Program, uintptrSize = 4) {
    if (uintptrSize !== 4 && uintptrSize !== 8)
      throw Error("unsupported uintptrSize");

    this.program = program;
    this.checker = program.getDiagnosticsProducingTypeChecker();
    this.diagnostics = ts.createDiagnosticCollection();
    this.module = new binaryen.Module();
    this.uintptrType = new WasmType(WasmTypeKind.uintptr, uintptrSize);
  }

  createDiagnosticForNode(node: ts.Node, category: ts.DiagnosticCategory, message: string, arg1?: string) {
    let realMessage = message;
    if (arg1 != null)
      realMessage += ": " + arg1;
    var diagnostic = ts.createDiagnosticForNode(node, {
      key: message.toLowerCase().replace(/\s+/g, "_").replace(/[^\w]/g, ""),
      category: category,
      code: <any>"-AS",
      message: realMessage
    });
    this.printDiagnostic(diagnostic);
    return diagnostic;
  }

  printDiagnostic(diagnostic: ts.Diagnostic): void {
    if (diagnostic.category == ts.DiagnosticCategory.Message)
      process.stderr.write(formatDiagnostics([ diagnostic ]));
    else
      process.stderr.write(formatDiagnosticsWithColorAndContext([ diagnostic ]) + "\n");
  }

  info(node: ts.Node, message: string, arg1?: string): void {
    this.diagnostics.add(this.createDiagnosticForNode(node, ts.DiagnosticCategory.Message, message, arg1));
  }

  warn(node: ts.Node, message: string, arg1?: string): void {
    this.diagnostics.add(this.createDiagnosticForNode(node, ts.DiagnosticCategory.Warning, message, arg1));
  }

  error(node: ts.Node, message: string, arg1?: string): void {
    this.diagnostics.add(this.createDiagnosticForNode(node, ts.DiagnosticCategory.Error, message, arg1));
  }

  initialize(): void {
    const compiler = this;

    this.module.setMemory(256, 0, "memory", []); // "unexpected true: memory max >= initial" (but the result is correct: growable)

    // TODO: it seem that binaryen.js doesn't support importing memory yet

    for (let file of this.program.getSourceFiles()) {
      if (file.isDeclarationFile) continue;
      this.info(file, "initializing file", file.fileName);
      ts.forEachChild(file, visit);
    }

    function visit(node: ts.Node) {
      switch (node.kind) {
        case ts.SyntaxKind.VariableStatement:
          compiler.initializeVariable(<ts.VariableStatement>node);
          break;
        case ts.SyntaxKind.FunctionDeclaration:
          compiler.initializeFunction(<ts.FunctionDeclaration>node);
          break;
        case ts.SyntaxKind.ClassDeclaration:
          compiler.initializeClass(<ts.ClassDeclaration>node);
          break;
        case ts.SyntaxKind.EnumDeclaration:
          compiler.initializeEnum(<ts.EnumDeclaration>node);
          break;
        case ts.SyntaxKind.EndOfFileToken:
          break;
        default:
          compiler.error(node, "unsupported top-level node", ts.SyntaxKind[node.kind]);
      }
    }
  }

  initializeVariable(node: ts.VariableStatement): void {
    // if constant, create a global
    if ((node.flags & ts.NodeFlags.Const) == 0)
      this.error(node, "global variables must be constant");
    // otherwise create in memory
  }

  initializeFunction(node: ts.FunctionDeclaration): void {
    const name = node.symbol.name;

    this.info(node, "initializing function", name);
    if (node.typeParameters && node.typeParameters.length !== 0)
      this.error(node.typeParameters[0], "type parameters are not supported yet");

    var parameters: WasmType[] = [];
    var signatureIdentifiers: string[] = [];
    var signatureTypes: number[] = [];

    node.parameters.forEach(parameter => {
      const name = parameter.symbol.name;
      const type = this.resolveType(parameter.type);
      parameters.push(type);
      signatureIdentifiers.push(type.toSignatureIdentifier(this.uintptrType));
      signatureTypes.push(type.toBinaryenType(this.uintptrType));
    });

    const returnType = this.resolveType(node.type, true);
    signatureIdentifiers.push(returnType.toSignatureIdentifier(this.uintptrType));

    const signatureKey = signatureIdentifiers.join("");
    let signature = this.signatures[signatureKey];
    if (!signature)
      signature = this.signatures[signatureKey] = this.module.addFunctionType(signatureKey, returnType.toBinaryenType(this.uintptrType), signatureTypes);
    let flags = 0;
    if (node.modifiers)
      node.modifiers.forEach(token => {
        switch (token.kind) {
          case ts.SyntaxKind.DeclareKeyword:
            flags |= WasmFunctionFlags.import;
            break;
          case ts.SyntaxKind.ExportKeyword:
            flags |= WasmFunctionFlags.export;
            break;
        }
      });

    (<any>node).wasmFunction = {
      name: name,
      parameters: parameters,
      returnType: returnType,
      flags: flags,
      signature: signature
    };
  }

  initializeClass(node: ts.ClassDeclaration): void {
    this.info(node, "initializing class", node.symbol.name);
    this.error(node, "classes are not supported yet");
    ts.forEachChild(node, visit);
    function visit(node: ts.Node): void {
    }
  }

  initializeEnum(node: ts.EnumDeclaration): void {
    this.info(node, "initializing enum", node.symbol.name);

    const compiler = this;
    const name = node.symbol.name;

    ts.forEachChild(node, visit);

    function visit(node: ts.Node): void {
      switch (node.kind) {

        case ts.SyntaxKind.Identifier:
          break;

        case ts.SyntaxKind.EnumMember:
        {
          var member = <ts.EnumMember>node;
          compiler.constants[name + "$" + member.symbol.name] = {
            type: intType,
            value: compiler.checker.getConstantValue(member)
          };
          break;
        }

        default:
          throw compiler.error(node, "unsupported enum node kind", ts.SyntaxKind[node.kind]);

      }
    }
  }

  compile(): void {
    const compiler = this;

    for (let file of this.program.getSourceFiles()) {
      if (file.isDeclarationFile) continue;
      this.info(file, "compiling file", file.fileName);
      ts.forEachChild(file, visit);
    }

    function visit(node: ts.Node) {
      switch (node.kind) {

        case ts.SyntaxKind.VariableStatement:
          compiler.compileVariable(<ts.VariableStatement>node);
          break;

        case ts.SyntaxKind.FunctionDeclaration:
          compiler.compileFunction(<ts.FunctionDeclaration>node);
          break;

        case ts.SyntaxKind.ClassDeclaration:
          compiler.compileClass(<ts.ClassDeclaration>node);
          break;

        case ts.SyntaxKind.EnumDeclaration:
          // converted to constants when initialized
          break;

        case ts.SyntaxKind.EndOfFileToken:
          break;

        // default:
          // already reported by initialize

      }
    }
  }

  compileVariable(node: ts.VariableStatement): void {
    this.info(node, "compiling variable", node.symbol.name);
  }

  compileFunction(node: ts.FunctionDeclaration): void {
    const wasmFunction: WasmFunction = (<any>node).wasmFunction;
    const name = node.symbol.name;
    this.info(node, "compiling function", name);

    if ((wasmFunction.flags & WasmFunctionFlags.import) != 0) {
      let moduleName = "env";
      let baseName = name;
      var idx = name.indexOf("$");
      if (idx > 0) {
        moduleName = name.substring(0, idx);
        baseName = name.substring(idx + 1);
      }
      this.module.addImport(name, moduleName, baseName, wasmFunction.signature);
      return;
    }

    const compiler = this;
    const body = [];
    const vars: { [key: string]: WasmVariable } = {};

    node.parameters.forEach((parameter, i) => {
      vars[parameter.symbol.name] = {
        index: i,
        type: wasmFunction.parameters[i]
      }
    });

    ts.forEachChild(node.body, visit);

    function visit(node: ts.Node) {
      switch (node.kind) {
        case ts.SyntaxKind.ReturnStatement:
        {
          const expr = <ts.ParenthesizedExpression>node;
          if (wasmFunction.returnType === voidType) {
            if (expr.getChildCount() > 1) // return keyword
              compiler.error(expr, "void function cannot return a value", wasmFunction.name);
            body.push(compiler.module.return());
          } else {
            if (expr.getChildCount() < 2) // return keyword + expression
              compiler.error(expr, "function must return a value", wasmFunction.name);
            body.push(compiler.module.return(compileExpression(<ts.Expression>node.getChildAt(1), wasmFunction.returnType)));
          }
          break;
        }
        default:
          throw Error("unsupported body node kind: " + ts.SyntaxKind[node.kind]);
      }
    }

    function convertValueIfNecessary(expr: binaryen.Expression, fromType: WasmType, toType: WasmType, explicit: boolean = false) {
      if (fromType.kind === toType.kind)
        return expr;

      if (fromType === floatType) {

        switch (toType) {

          case byteType:
          case ushortType:
          case uintType:
          case boolType:
            return convertValueIfNecessary(compiler.module.i32.trunc_u.f32(expr), intType, toType);

          case sbyteType:
          case shortType:
          case intType:
            return convertValueIfNecessary(compiler.module.i32.trunc_s.f32(expr), intType, toType);

          case ulongType:
            return compiler.module.i64.trunc_u.f32(expr);

          case longType:
            return compiler.module.i64.trunc_s.f32(expr);

          case doubleType:
            return compiler.module.f64.promote(expr);

          case this.uintptrType:
            if (this.uintptrType.size === 4)
              return compiler.module.i32.trunc_u.f32(expr);
            else
              return compiler.module.i64.trunc_u.f32(expr);

        }

      } else if (fromType === doubleType) {

        switch (toType) {

          case byteType:
          case ushortType:
          case uintType:
          case boolType:
            return convertValueIfNecessary(compiler.module.i32.trunc_u.f64(expr), intType, toType);

          case sbyteType:
          case shortType:
          case intType:
            return convertValueIfNecessary(compiler.module.i32.trunc_s.f64(expr), intType, toType);

          case ulongType:
            return compiler.module.i64.trunc_u.f64(expr);

          case longType:
            return compiler.module.i64.trunc_s.f64(expr);

          case floatType:
            return compiler.module.f32.demote(expr);

          case this.uintptrType:
            if (this.uintptrType.size === 4)
              return compiler.module.i32.trunc_u.f64(expr);
            else
              return compiler.module.i64.trunc_u.f64(expr);

        }

      } else if (fromType.size <= 4 && toType.size === 8) {

        if (toType.isSigned) {

          const shift = 32 - (toType.size << 3);
          return compiler.module.i64.extend_s(
            compiler.module.i32.shl(
              compiler.module.i32.shr_s(
                expr,
                compiler.module.i32.const(shift)
              ),
              compiler.module.i32.const(shift)
            )
          );

        } else {

          return compiler.module.i64.extend_u(
            compiler.module.i32.and(
              expr,
              compiler.module.i32.const((toType.size << 8) - 1)
            )
          );

        }

      } else if (fromType.size === 8 && toType.size <= 4) {

        if (toType.isSigned) {

          const shift = 64 - (toType.size << 3);
          const longShift = Long.fromNumber(shift);
          return compiler.module.i32.wrap(
            compiler.module.i64.shl(
              compiler.module.i64.shr_s(
                expr,
                compiler.module.i64.const(longShift.low, longShift.high)
              ),
              compiler.module.i64.const(longShift.low, longShift.high)
            )
          );

        } else {

          const longMask = Long.fromNumber((toType.size << 8) - 1);
          return compiler.module.i32.wrap(
            compiler.module.i64.and(
              expr,
              compiler.module.i64.const(longMask.low, longMask.high)
            )
          );

        }

      } else { // some i32 type to another

        if (fromType.size < toType.size)
          return expr;

        if (toType.isSigned) {
          const shift = 32 - (toType.size << 3);
          return compiler.module.i32.shl(
            compiler.module.i32.shr_s(
              expr,
              compiler.module.i32.const(shift)
            ),
            compiler.module.i32.const(shift)
          );
        } else {
          return compiler.module.i32.and(
            expr,
            compiler.module.i32.const((toType.size << 8) - 1)
          );
        }

      }

      throw Error("unexpected conversion");
    }

    function compileExpression(node: ts.Expression, type: WasmType): binaryen.Expression {
      switch (node.kind) {

        case ts.SyntaxKind.ParenthesizedExpression:
          return compileExpression((<ts.ParenthesizedExpression>node).expression, type);

        case ts.SyntaxKind.AsExpression:
        {
          const expr = <ts.AsExpression>node;
          const exprType = compiler.resolveType(expr.type);
          return convertValueIfNecessary(compileExpression(expr.expression, exprType), exprType, type, true);
        }

        case ts.SyntaxKind.BinaryExpression:
        {
          const expr = <ts.BinaryExpression>node;
          const left = compileExpression(expr.left, type);
          const right = compileExpression(expr.right, type);

          if (type === floatType) {

            switch (expr.operatorToken.kind) {

              case ts.SyntaxKind.PlusToken:
                return compiler.module.f32.add(left, right);

              case ts.SyntaxKind.MinusToken:
                return compiler.module.f32.sub(left, right);

              case ts.SyntaxKind.AsteriskToken:
                return compiler.module.f32.mul(left, right);

              case ts.SyntaxKind.SlashToken:
                return compiler.module.f32.div(left, right);

            }

          } else if (type === doubleType) {

            switch (expr.operatorToken.kind) {

              case ts.SyntaxKind.PlusToken:
                return compiler.module.f64.add(left, right);

              case ts.SyntaxKind.MinusToken:
                return compiler.module.f64.sub(left, right);

              case ts.SyntaxKind.AsteriskToken:
                return compiler.module.f64.mul(left, right);

              case ts.SyntaxKind.SlashToken:
                return compiler.module.f64.div(left, right);

            }

          } else if (type == longType || type === ulongType || (type === compiler.uintptrType && compiler.uintptrType.size === 8)) {

            switch (expr.operatorToken.kind) {

              case ts.SyntaxKind.PlusToken:
                return compiler.module.i64.add(left, right);

              case ts.SyntaxKind.MinusToken:
                return compiler.module.i64.sub(left, right);

              case ts.SyntaxKind.AsteriskToken:
                return compiler.module.i64.mul(left, right);

              case ts.SyntaxKind.SlashToken:
                if (type.isSigned)
                  return compiler.module.i64.div_s(left, right);
                else
                  return compiler.module.i64.div_u(left, right);

              case ts.SyntaxKind.PercentToken:
                if (type.isSigned)
                  return compiler.module.i64.rem_s(left, right);
                else
                  return compiler.module.i64.rem_u(left, right);

              case ts.SyntaxKind.AmpersandToken:
                return compiler.module.i64.and(left, right);

              case ts.SyntaxKind.BarToken:
                return compiler.module.i64.or(left, right);

              case ts.SyntaxKind.CaretToken:
                return compiler.module.i64.xor(left, right);

              case ts.SyntaxKind.LessThanLessThanToken:
                return compiler.module.i64.shl(left, right);

              case ts.SyntaxKind.GreaterThanGreaterThanToken:
                if (type.isSigned)
                  return compiler.module.i64.shr_s(left, right);
                else
                  return compiler.module.i64.shr_u(left, right);

            }

          } else { // some i32 type

            switch (expr.operatorToken.kind) {

              case ts.SyntaxKind.PlusToken:
                return convertValueIfNecessary(compiler.module.i32.add(left, right), intType, type);

              case ts.SyntaxKind.MinusToken:
                return convertValueIfNecessary(compiler.module.i32.sub(left, right), intType, type);

              case ts.SyntaxKind.AsteriskToken:
                return convertValueIfNecessary(compiler.module.i32.mul(left, right), intType, type);

              case ts.SyntaxKind.SlashToken:
                if (type.isSigned)
                  return convertValueIfNecessary(compiler.module.i32.div_s(left, right), intType, type);
                else
                  return convertValueIfNecessary(compiler.module.i32.div_u(left, right), intType, type);

              case ts.SyntaxKind.PercentToken:
                if (type.isSigned)
                  return convertValueIfNecessary(compiler.module.i32.rem_s(left, right), intType, type);
                else
                  return convertValueIfNecessary(compiler.module.i32.rem_u(left, right), intType, type);

              case ts.SyntaxKind.AmpersandToken:
                return convertValueIfNecessary(compiler.module.i32.and(left, right), intType, type);

              case ts.SyntaxKind.BarToken:
                return convertValueIfNecessary(compiler.module.i32.or(left, right), intType, type);

              case ts.SyntaxKind.CaretToken:
                return convertValueIfNecessary(compiler.module.i32.xor(left, right), intType, type);

              case ts.SyntaxKind.LessThanLessThanToken:
                return convertValueIfNecessary(compiler.module.i32.shl(left, right), intType, type);

              case ts.SyntaxKind.GreaterThanGreaterThanToken:
                if (type.isSigned)
                  return convertValueIfNecessary(compiler.module.i32.shr_s(left, right), intType, type);
                else
                  return convertValueIfNecessary(compiler.module.i32.shr_u(left, right), intType, type);

            }
          }

          throw Error("unsupported operator token kind: " + ts.SyntaxKind[expr.operatorToken.kind]);
        }

        case ts.SyntaxKind.FirstLiteralToken:
        {
          let text = (<ts.LiteralExpression>node).text;
          let radix: number;

          if (/^[1-9][0-9]*$/.test(text)) {
            radix = 10;
          } else if (/^0[xX][0-9A-Fa-f]+$/.test(text)) {
            radix = 16;
            text = text.substring(2);
          } else {
            compiler.error(node, "unsupported literal", text);
            text = "0";
          }

          let long: Long;
          switch (type) {

            case floatType:
              return compiler.module.f32.const(parseFloat(text));

            case doubleType:
              return compiler.module.f64.const(parseFloat(text));

            case byteType:
            case sbyteType:
            case shortType:
            case ushortType:
            case intType:
            case uintType:
              return compiler.module.i32.const(parseInt(text, radix) & ((type.size << 8) - 1));

            case longType:
            case ulongType:
              long = Long.fromString(text, type === ulongType, radix);
              return compiler.module.i64.const(long.low, long.high);

            case boolType:
              return compiler.module.i32.const(parseInt(text, radix) !== 0 ? 1 : 0);

            case this.uintptrType:
              if (this.uintptrType.size === 8) {
                long = Long.fromString(text, true, radix);
                return compiler.module.i64.const(long.low, long.high);
              }
              return compiler.module.i32.const(parseInt(text, radix) & ((type.size << 8) - 1));
          }
        }

        case ts.SyntaxKind.Identifier:
        {
          const id = <ts.Identifier>node;
          const local = vars[id.text];

          if (local == null)
            throw Error("undefined local variable: " + id.text);

          return convertValueIfNecessary(compiler.module.getLocal(local.index, local.type.toBinaryenType(compiler.uintptrType)), local.type, type);
        }

        case ts.SyntaxKind.PropertyAccessExpression:
        {
          const expr = <ts.PropertyAccessExpression>node;

          if (expr.expression.kind === ts.SyntaxKind.Identifier) {
            const name = (<ts.Identifier>expr.expression).text;

            if (expr.name.kind === ts.SyntaxKind.Identifier) {
              const prop = (<ts.Identifier>expr.name).text;
              const constant = compiler.constants[name + "$" + prop];
              let long: Long;
              if (constant) {
                switch (constant.type) {

                  case byteType:
                  case sbyteType:
                  case shortType:
                  case ushortType:
                  case intType:
                  case uintType:
                    return convertValueIfNecessary(compiler.module.i32.const(constant.value), intType, type);

                  case longType:
                  case ulongType:
                    long = Long.fromValue(constant.value);
                    return convertValueIfNecessary(compiler.module.i64.const(long.low, long.high), longType, type);

                  case compiler.uintptrType:
                    if (compiler.uintptrType.size === 4)
                      return convertValueIfNecessary(compiler.module.i32.const(constant.value), intType, type);
                    else {
                      long = Long.fromValue(constant.value);
                      return convertValueIfNecessary(compiler.module.i64.const(long.low, long.high), longType, type);
                    }
                }
              }
            }
          }
          throw Error("unsupported property access");
        }

        default:
          throw Error("unsupported expression node kind: " + ts.SyntaxKind[node.kind]);
      }
    }

    if (body.length == 0)
      body.push(this.module.return());

    const func = this.module.addFunction(name, wasmFunction.signature, [], body);

    if ((node.modifierFlagsCache & ts.ModifierFlags.Export) != 0)
      this.module.addExport(name, name);

    if (name === "start")
      this.module.setStart(func);
  }

  compileClass(node: ts.ClassDeclaration): void {
    this.info(node, "compiling class", node.symbol.name);
  }

  resolveType(type: ts.TypeNode, acceptVoid: boolean = false): WasmType {
    const text = type.getText();

    switch (text) {
      case "byte": return byteType;
      case "short": return shortType;
      case "ushort": return ushortType;
      case "int": return intType;
      case "uint": return uintType;
      case "long": return longType;
      case "ulong": return ulongType;
      case "bool": return boolType;
      case "float": return floatType;
      case "double": return doubleType;
      case "void": if (acceptVoid) return voidType;
    }

    if (type.kind == ts.SyntaxKind.TypeReference) {
      var reference = <ts.TypeReferenceNode>type;
      switch (reference.typeName.getText()) {
        case "Ptr":
          if (reference.typeArguments.length !== 1)
            throw Error("unexpected number of type parameters on Ptr<T>");
          if (reference.typeArguments[0].kind !== ts.SyntaxKind.TypeReference)
            throw Error("unexpected type parameter on Ptr<T>");
          return this.uintptrType.withUnderlyingType(this.resolveType(<ts.TypeReferenceNode>reference.typeArguments[0]));
      }
    }

    this.error(type, "unsupported type", text);
  }
}

// TOOD: remove this
if (process.argv.length > 2)
  console.log("\n" + Compiler.compile(process.argv[2]).emitText());
