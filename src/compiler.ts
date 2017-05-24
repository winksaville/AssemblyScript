import * as ts from "byots";
import * as binaryen from "../lib/binaryen";
import * as Long from "long";
import { formatDiagnostics, formatDiagnosticsWithColorAndContext } from "./diagnostics";
import { Profiler } from "./profiler";

import {
  WasmTypeKind,
  WasmType,
  WasmFunctionFlags,
  WasmFunction,
  WasmVariable,
  WasmConstant
} from "./wasm";

const byteType      = new WasmType(WasmTypeKind.byte   , 1);
const sbyteType     = new WasmType(WasmTypeKind.sbyte  , 1);
const shortType     = new WasmType(WasmTypeKind.short  , 2);
const ushortType    = new WasmType(WasmTypeKind.ushort , 2);
const intType       = new WasmType(WasmTypeKind.int    , 4);
const uintType      = new WasmType(WasmTypeKind.uint   , 4);
const longType      = new WasmType(WasmTypeKind.long   , 8);
const ulongType     = new WasmType(WasmTypeKind.ulong  , 8);
const boolType      = new WasmType(WasmTypeKind.bool   , 4);
const floatType     = new WasmType(WasmTypeKind.float  , 4);
const doubleType    = new WasmType(WasmTypeKind.double , 8);
const voidType      = new WasmType(WasmTypeKind.void   , 0);
const uintptrType32 = new WasmType(WasmTypeKind.uintptr, 4);
const uintptrType64 = new WasmType(WasmTypeKind.uintptr, 8);

function isExport(node: ts.Node): boolean {
  return (node.modifierFlagsCache & ts.ModifierFlags.Export) !== 0;
}

function isImport(node: ts.Node): boolean {
  if (node.modifiers) // TODO: isn't there a flag for that?
    for (let modifier of node.modifiers)
      if (modifier.kind === ts.SyntaxKind.DeclareKeyword)
        return true;
  return false;
}

export class Compiler {
  program: ts.Program;
  checker: ts.TypeChecker;
  diagnostics: ts.DiagnosticCollection;
  uintptrType: WasmType;
  module: binaryen.Module;
  signatures: { [key: string]: binaryen.Signature } = {};
  constants: { [key: string]: WasmConstant } = {};
  profiler = new Profiler();

  static compile(filename: string): binaryen.Module {
    let program = ts.createProgram([ __dirname + "/../types/assembly.d.ts", filename ], {
      target: ts.ScriptTarget.Latest,
      module: ts.ModuleKind.None,
      noLib: true,
      experimentalDecorators: true,
      types: []
    });

    let compiler = new Compiler(program);

    // bail out if there were 'pre emit' errors
    for (let diagnostic of ts.getPreEmitDiagnostics(compiler.program)) {
      compiler.printDiagnostic(diagnostic);
      if (diagnostic.category === ts.DiagnosticCategory.Error)
        return null;
    }

    compiler.profiler.start("initialize");
    compiler.initialize();
    process.stderr.write("initialization took " + compiler.profiler.end("initialize").toFixed(3) + " ms\n");

    // bail out if there were initialization errors
    let diagnostics = compiler.diagnostics.getDiagnostics();
    for (let diagnostic of diagnostics) {
      if (diagnostic.category === ts.DiagnosticCategory.Error)
        return null;
    }

    compiler.profiler.start("compile");
    compiler.compile();
    process.stderr.write("compilation took " + compiler.profiler.end("compile").toFixed(3) + " ms\n");

    // bail out if there were compilation errors
    diagnostics = compiler.diagnostics.getDiagnostics();
    for (let diagnostic of diagnostics) {
      if (diagnostic.category === ts.DiagnosticCategory.Error)
        return null;
    }

    return compiler.module;
  }

  constructor(program: ts.Program, uintptrSize = 4) {
    if (uintptrSize !== 4 && uintptrSize !== 8)
      throw Error("unsupported uintptrSize");

    this.program = program;
    this.checker = program.getDiagnosticsProducingTypeChecker();
    this.diagnostics = ts.createDiagnosticCollection();
    this.module = new binaryen.Module();
    this.uintptrType = uintptrSize === 4 ? uintptrType32 : uintptrType64;
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
    // TODO: it seems that binaryen.js does not support globals, yet

    if ((node.flags & ts.NodeFlags.Const) == 0)
      this.error(node, "global variables must be constant");
    // otherwise create in memory
  }

  private _initializeFunction(node: ts.FunctionDeclaration | ts.MethodDeclaration, parent?: ts.ClassDeclaration, isInstance: boolean = false): void {
    const name = node.symbol.name;

    if (node.typeParameters && node.typeParameters.length !== 0)
      this.error(node.typeParameters[0], "type parameters are not supported yet");

    var parameters: WasmType[] = [];
    var signatureIdentifiers: string[] = [];
    var signatureTypes: number[] = [];

    if (parent && isInstance) {
      const thisType = this.uintptrType; // TODO: underlyingType
      parameters.push(thisType);
      signatureIdentifiers.push(thisType.toSignatureIdentifier(this.uintptrType));
      signatureTypes.push(thisType.toBinaryenType(this.uintptrType));
    }

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

    if (isExport(node))
      flags |= WasmFunctionFlags.export;

    if (isImport(node))
      flags |= WasmFunctionFlags.import;

    (<any>node).wasmFunction = {
      name: parent ? parent.symbol.name + "$" + name : name,
      parameters: parameters,
      returnType: returnType,
      flags: flags,
      signature: signature
    };
  }

  initializeFunction(node: ts.FunctionDeclaration): void {
    this._initializeFunction(node);
  }

  initializeClass(node: ts.ClassDeclaration): void {
    const compiler = this;
    const clazz = node;
    const name = node.symbol.name;

    ts.forEachChild(node, visit);

    function visit(node: ts.Node): void {
      switch (node.kind) {

        case ts.SyntaxKind.Identifier:
          break;

        case ts.SyntaxKind.MethodDeclaration:
          if (isExport(node))
            compiler.error(node, "class methods cannot be exports");
          if (isImport(node))
            compiler.error(node, "class methods cannot be imports");
          compiler._initializeFunction(<ts.MethodDeclaration>node, clazz, (node.modifierFlagsCache & ts.ModifierFlags.Static) === 0);
          break;

        default:
          throw Error("unsupported class member kind: " + ts.SyntaxKind[node.kind]);

      }
    }
  }

  initializeEnum(node: ts.EnumDeclaration): void {
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

        // default:
        // already reported by initialize

      }
    }
  }

  compileVariable(node: ts.VariableStatement): void {
    // TODO
  }

  private _compileFunction(node: ts.FunctionDeclaration | ts.MethodDeclaration) {
    const wasmFunction: WasmFunction = (<any>node).wasmFunction;
    const compiler = this;
    const body = [];
    const locals: { [key: string]: WasmVariable } = {};

    node.parameters.forEach((parameter, i) => {
      locals[parameter.symbol.name] = {
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
            body.push(compiler.module.return(compileExpression(compiler, locals, <ts.Expression>node.getChildAt(1), wasmFunction.returnType)));
          }
          break;
        }

        default:
          throw Error("unsupported function body node: " + ts.SyntaxKind[node.kind]);
      }
    }

    if (body.length == 0)
      body.push(this.module.return());

    return this.module.addFunction(wasmFunction.name, wasmFunction.signature, [], body);
  }

  compileFunction(node: ts.FunctionDeclaration): void {
    const wasmFunction: WasmFunction = (<any>node).wasmFunction;
    const name = node.symbol.name;

    if ((wasmFunction.flags & WasmFunctionFlags.import) != 0) {
      let moduleName: string;
      let baseName: string;
      var idx = name.indexOf("$");
      if (idx > 0) {
        moduleName = name.substring(0, idx);
        baseName = name.substring(idx + 1);
      } else {
        moduleName = "env";
        baseName = name;
      }
      this.module.addImport(name, moduleName, baseName, wasmFunction.signature);
      return;
    }

    const func = this._compileFunction(node);

    if ((node.modifierFlagsCache & ts.ModifierFlags.Export) != 0)
      this.module.addExport(name, name);

    if (name === "start")
      this.module.setStart(func);
  }

  compileClass(node: ts.ClassDeclaration): void {
    const compiler = this;
    const clazz = node;
    const name = node.symbol.name;

    ts.forEachChild(node, visit);

    function visit(node: ts.Node) {
      switch (node.kind) {

        case ts.SyntaxKind.MethodDeclaration:
          compiler._compileFunction(<ts.MethodDeclaration>node);
          break;

        // default:
        // already reported by initialize
      }
    }
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
      case "uintptr": return this.uintptrType;
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

function convertValueIfNecessary(compiler: Compiler, expr: binaryen.Expression, fromType: WasmType, toType: WasmType, explicit: boolean = false) {
  if (fromType.kind === toType.kind)
    return expr;

  if (fromType === floatType) {

    switch (toType) {

      case byteType:
      case ushortType:
      case uintType:
      case boolType:
        return convertValueIfNecessary(compiler, compiler.module.i32.trunc_u.f32(expr), intType, toType);

      case sbyteType:
      case shortType:
      case intType:
        return convertValueIfNecessary(compiler, compiler.module.i32.trunc_s.f32(expr), intType, toType);

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
        return convertValueIfNecessary(compiler, compiler.module.i32.trunc_u.f64(expr), intType, toType);

      case sbyteType:
      case shortType:
      case intType:
        return convertValueIfNecessary(compiler, compiler.module.i32.trunc_s.f64(expr), intType, toType);

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
}

function compileExpression(compiler: Compiler, locals: { [key: string]: WasmVariable }, node: ts.Expression, type?: WasmType): binaryen.Expression {
  switch (node.kind) {

    case ts.SyntaxKind.ParenthesizedExpression:
      return compileExpression(compiler, locals, (<ts.ParenthesizedExpression>node).expression, type);

    case ts.SyntaxKind.AsExpression:
    {
      const expr = <ts.AsExpression>node;
      const exprType = compiler.resolveType(expr.type);
      return convertValueIfNecessary(compiler, compileExpression(compiler, locals, expr.expression, exprType), exprType, type, true);
    }

    case ts.SyntaxKind.BinaryExpression:
    {
      const expr = <ts.BinaryExpression>node;
      const left = compileExpression(compiler, locals, expr.left, type);
      const right = compileExpression(compiler, locals, expr.right, type);

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
            return convertValueIfNecessary(compiler, compiler.module.i32.add(left, right), intType, type);

          case ts.SyntaxKind.MinusToken:
            return convertValueIfNecessary(compiler, compiler.module.i32.sub(left, right), intType, type);

          case ts.SyntaxKind.AsteriskToken:
            return convertValueIfNecessary(compiler, compiler.module.i32.mul(left, right), intType, type);

          case ts.SyntaxKind.SlashToken:
            if (type.isSigned)
              return convertValueIfNecessary(compiler, compiler.module.i32.div_s(left, right), intType, type);
            else
              return convertValueIfNecessary(compiler, compiler.module.i32.div_u(left, right), intType, type);

          case ts.SyntaxKind.PercentToken:
            if (type.isSigned)
              return convertValueIfNecessary(compiler, compiler.module.i32.rem_s(left, right), intType, type);
            else
              return convertValueIfNecessary(compiler, compiler.module.i32.rem_u(left, right), intType, type);

          case ts.SyntaxKind.AmpersandToken:
            return convertValueIfNecessary(compiler, compiler.module.i32.and(left, right), intType, type);

          case ts.SyntaxKind.BarToken:
            return convertValueIfNecessary(compiler, compiler.module.i32.or(left, right), intType, type);

          case ts.SyntaxKind.CaretToken:
            return convertValueIfNecessary(compiler, compiler.module.i32.xor(left, right), intType, type);

          case ts.SyntaxKind.LessThanLessThanToken:
            return convertValueIfNecessary(compiler, compiler.module.i32.shl(left, right), intType, type);

          case ts.SyntaxKind.GreaterThanGreaterThanToken:
            if (type.isSigned)
              return convertValueIfNecessary(compiler, compiler.module.i32.shr_s(left, right), intType, type);
            else
              return convertValueIfNecessary(compiler, compiler.module.i32.shr_u(left, right), intType, type);

        }
      }

      throw Error("unsupported operator token: " + ts.SyntaxKind[expr.operatorToken.kind]);
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
      const ident = <ts.Identifier>node;
      const local = locals[ident.text];

      if (local == null)
        throw Error("undefined local variable: " + ident.text);

      return convertValueIfNecessary(compiler, compiler.module.getLocal(local.index, local.type.toBinaryenType(compiler.uintptrType)), local.type, type);
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
                return convertValueIfNecessary(compiler, compiler.module.i32.const(constant.value), intType, type);

              case longType:
              case ulongType:
                long = Long.fromValue(constant.value);
                return convertValueIfNecessary(compiler, compiler.module.i64.const(long.low, long.high), longType, type);

              case compiler.uintptrType:
                if (compiler.uintptrType.size === 4)
                  return convertValueIfNecessary(compiler, compiler.module.i32.const(constant.value), intType, type);
                else {
                  long = Long.fromValue(constant.value);
                  return convertValueIfNecessary(compiler, compiler.module.i64.const(long.low, long.high), longType, type);
                }
            }
          }
        }
      }

      throw Error("unsupported property access");
    }

    default:
      throw Error("unsupported expression node: " + ts.SyntaxKind[node.kind]);
  }
}
