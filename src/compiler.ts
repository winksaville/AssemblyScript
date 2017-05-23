/// <reference types="node" />

import * as ts from "byots";
import * as binaryen from "../lib/binaryen";
import { formatDiagnostics, formatDiagnosticsWithColorAndContext } from "./util/diagnostics";

export enum WasmTypeKind {
  byte,
  short,
  ushort,
  int,
  uint,
  long,
  ulong,
  bool,
  float,
  double,
  intptr,
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

  withUnderlyingType(underlyingType: WasmType): WasmType {
    if (underlyingType == null)
      throw Error("underlying type must be specified");
    if (this.kind != WasmTypeKind.intptr)
      throw Error("only pointers can have an underlying type");
    const type = new WasmType(this.kind, this.size);
    type.underlyingType = underlyingType;
    return type;
  }

  toSignatureIdentifier(intptrType: WasmType): string {
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
      case WasmTypeKind.intptr:
        return intptrType.size == 4 ? "i" : "I";
      case WasmTypeKind.void:
        return "v";
    }
    throw Error("unexpected type");
  }

  toBinaryenType(intptrType: WasmType): any {
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
      case WasmTypeKind.intptr:
        return intptrType.size == 4 ? binaryen.i32 : binaryen.i64;
      case WasmTypeKind.void:
        return binaryen.none;
    }
    throw Error("unexpected type");
  }
}

export const byteType   = new WasmType(WasmTypeKind.byte   , 1);
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
  parameters: WasmType[],
  returnType: WasmType,
  flags: WasmFunctionFlags,
  signature: binaryen.Signature
}

interface WasmVariable {
  type: WasmType,
  value: number
}

export class Compiler {
  program: ts.Program;
  checker: ts.TypeChecker;
  diagnostics: ts.DiagnosticCollection;
  intptrType: WasmType;
  module: binaryen.Module;
  signatures: { [key: string]: binaryen.Signature };

  static compile(filename: string) {
    let program = ts.createProgram([ filename ], {
      target: ts.ScriptTarget.Latest,
      module: ts.ModuleKind.None,
      noLib: true,
      experimentalDecorators: true
    });
    let compiler = new Compiler(program);
    compiler.initialize();
    compiler.compile();

    console.log("\n" + compiler.module.emitText()); // For now
  }

  constructor(program: ts.Program, intptrSize = 4) {
    if (intptrSize !== 4 && intptrSize !== 8)
      throw Error("unsupported intptrSize");
    this.program = program;
    this.checker = program.getDiagnosticsProducingTypeChecker();
    this.diagnostics = ts.createDiagnosticCollection();
    this.module = new binaryen.Module();
    this.signatures = {};
    this.intptrType = new WasmType(WasmTypeKind.intptr, intptrSize);
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

    this.module.setMemory(256, 0, "memory", []);
    // this.module.addImport("memory", "env", "memory", <any>"memory"); // ?

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
    this.info(node, "initializing function", node.symbol.name);
    if (node.typeParameters && node.typeParameters.length !== 0)
      this.error(node.typeParameters[0], "type parameters are not supported yet");

    var parameters: WasmType[] = [];
    var signatureIdentifiers: string[] = [];
    var signatureTypes: number[] = [];

    node.parameters.forEach(parameter => {
      const name = parameter.symbol.name;
      const type = this.resolveType(parameter.type);
      parameters.push(type);
      signatureIdentifiers.push(type.toSignatureIdentifier(this.intptrType));
      signatureTypes.push(type.toBinaryenType(this.intptrType));
    });

    const returnType = this.resolveType(node.type, true);
    signatureIdentifiers.push(returnType.toSignatureIdentifier(this.intptrType));

    const signatureKey = signatureIdentifiers.join("");
    let signature = this.signatures[signatureKey];
    if (!signature)
      signature = this.signatures[signatureKey] = this.module.addFunctionType(signatureKey, returnType.toBinaryenType(this.intptrType), signatureTypes);
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
        case ts.SyntaxKind.EndOfFileToken:
          break;
        // otherwise already reported by initialize
      }
    }
  }

  compileVariable(node: ts.VariableStatement): void {
    this.info(node, "compiling variable", node.symbol.name);
  }

  compileFunction(node: ts.FunctionDeclaration): void {
    const wasmFunction = (<any>node).wasmFunction;
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
    const vars = {};

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
          body.push(compiler.module.return(wasmFunction.returnType !== voidType ? visitExpression(<ts.Expression>node.getChildAt(1)) : null));
          break;
        default:
          throw Error("unsupported body node kind: " + ts.SyntaxKind[node.kind]);
      }
    }

    function visitExpression(node: ts.Expression): number {
      switch (node.kind) {
        case ts.SyntaxKind.BinaryExpression:
        {
          const expr = <ts.BinaryExpression>node;
          const left = visitExpression(expr.left);
          const right = visitExpression(expr.right);

          switch (expr.operatorToken.kind) {
            case ts.SyntaxKind.PlusToken:
              return compiler.module.i32.add(left, right);
            case ts.SyntaxKind.MinusToken:
              return compiler.module.i32.sub(left, right);
            default:
              throw Error("unsupported operator token kind: " + ts.SyntaxKind[expr.operatorToken.kind]);
          }
        }
        case ts.SyntaxKind.FirstLiteralToken:
        {
          const text = (<ts.LiteralExpression>node).text;
          if (/^[1-9][0-9]+$/.test(text)) {
            return compiler.module.i32.const(parseInt(text, 10));
          } else {
            throw Error("unsupported literal: " + text);
          }
        }
        case ts.SyntaxKind.Identifier:
        {
          const id = <ts.Identifier>node;
          const local = vars[id.text];
          if (local == null)
            throw Error("undefined local variable: " + id.text);
          return compiler.module.getLocal(local.index, local.type);
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
      case "byte":
        return byteType;
      case "short":
        return shortType;
      case "ushort":
        return ushortType;
      case "int":
        return intType;
      case "uint":
        return uintType;
      case "long":
        return longType;
      case "ulong":
        return ulongType;
      case "bool":
        return boolType;
      case "float":
        return floatType;
      case "double":
        return doubleType;
      case "void":
        if (acceptVoid)
          return voidType;
    }

    if (type.kind == ts.SyntaxKind.TypeReference) {
      var reference = <ts.TypeReferenceNode>type;
      switch (reference.typeName.getText()) {
        case "IntPtr":
          if (reference.typeArguments.length !== 1)
            throw Error("unexpected number of type parameters on IntPtr<T>");
          if (reference.typeArguments[0].kind !== ts.SyntaxKind.TypeReference)
            throw Error("unexpected type parameter on IntPtr<T>");
          return this.intptrType.withUnderlyingType(this.resolveType(<ts.TypeReferenceNode>reference.typeArguments[0]));
      }
    }

    this.error(type, "unsupported type", text);
  }
}

// TOOD: remove this
if (process.argv.length > 2)
  Compiler.compile(process.argv[2]);
