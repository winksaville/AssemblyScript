import * as ts from "byots";
import * as Long from "long";
import * as assert from "assert";
import { Profiler } from "./profiler";

import {
  formatDiagnostics,
  formatDiagnosticsWithColorAndContext,
  createDiagnosticForNode,
  printDiagnostic
} from "./diagnostics";

import {
  WasmModule,
  WasmSignature,
  WasmStatement,
  WasmExpression,
  WasmTypeKind,
  WasmType,
  WasmFunctionFlags,
  WasmFunction,
  WasmVariable,
  WasmConstant
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
export const voidType      = new WasmType(WasmTypeKind.void   , 0);
export const uintptrType32 = new WasmType(WasmTypeKind.uintptr, 4);
export const uintptrType64 = new WasmType(WasmTypeKind.uintptr, 8);

const MEM_MAX_32 = (1 << 16) - 1; // 65535 (pageSize) * 65535 (n) ^= 4GB

function isExport(node: ts.Node): boolean {
  return (node.modifierFlagsCache & ts.ModifierFlags.Export) !== 0;
}

function isImport(node: ts.Node): boolean {
  if (node.modifiers) // TODO: isn't there a flag for that?
    for (let i = 0, k = node.modifiers.length; i < k; ++i)
      if (node.modifiers[i].kind === ts.SyntaxKind.DeclareKeyword)
        return true;
  return false;
}

export class Compiler {
  program: ts.Program;
  checker: ts.TypeChecker;
  diagnostics: ts.DiagnosticCollection;
  uintptrType: WasmType;
  module: WasmModule;
  signatures: { [key: string]: WasmSignature } = {};
  constants: { [key: string]: WasmConstant } = {};
  profiler = new Profiler();
  currentFunction: WasmFunction;
  currentLocals: { [key: string]: WasmVariable };
  currentBlockIndex: -1;

  static compile(filename: string): WasmModule {
    let program = ts.createProgram([ __dirname + "/../types/assembly.d.ts", filename ], {
      target: ts.ScriptTarget.Latest,
      module: ts.ModuleKind.None,
      noLib: true,
      experimentalDecorators: true,
      types: []
    });

    let compiler = new Compiler(program);

    // bail out if there were 'pre emit' errors
    let diagnostics = ts.getPreEmitDiagnostics(compiler.program);
    for (let i = 0, k = diagnostics.length; i < k; ++i) {
      printDiagnostic(diagnostics[i]);
      if (diagnostics[i].category === ts.DiagnosticCategory.Error)
        return null;
    }

    compiler.profiler.start("initialize");
    compiler.initialize();
    process.stderr.write("initialization took " + compiler.profiler.end("initialize").toFixed(3) + " ms\n");

    // bail out if there were initialization errors
    diagnostics = compiler.diagnostics.getDiagnostics();
    for (let i = 0, k = diagnostics.length; i < k; ++i) {
      if (diagnostics[i].category === ts.DiagnosticCategory.Error)
        return null;
    }

    compiler.profiler.start("compile");
    compiler.compile();
    process.stderr.write("compilation took " + compiler.profiler.end("compile").toFixed(3) + " ms\n");

    // bail out if there were compilation errors
    diagnostics = compiler.diagnostics.getDiagnostics();
    for (let i = 0, k = diagnostics.length; i < k; ++i) {
      if (diagnostics[i].category === ts.DiagnosticCategory.Error)
        return null;
    }

    // compiler.module.validate();

    return compiler.module;
  }

  constructor(program: ts.Program, uintptrSize = 4) {
    if (uintptrSize !== 4 && uintptrSize !== 8)
      throw Error("unsupported uintptrSize");

    this.program = program;
    this.checker = program.getDiagnosticsProducingTypeChecker();
    this.diagnostics = ts.createDiagnosticCollection();
    this.module = new WasmModule();
    this.uintptrType = uintptrSize === 4 ? uintptrType32 : uintptrType64;
  }

  info(node: ts.Node, message: string, arg1?: string): void {
    const diagnostic = createDiagnosticForNode(node, ts.DiagnosticCategory.Message, message, arg1);
    this.diagnostics.add(diagnostic);
    printDiagnostic(diagnostic);
  }

  warn(node: ts.Node, message: string, arg1?: string): void {
    const diagnostic = createDiagnosticForNode(node, ts.DiagnosticCategory.Warning, message, arg1);
    this.diagnostics.add(diagnostic);
    printDiagnostic(diagnostic);
  }

  error(node: ts.Node, message: string, arg1?: string): void {
    const diagnostic = createDiagnosticForNode(node, ts.DiagnosticCategory.Error, message, arg1);
    this.diagnostics.add(diagnostic);
    printDiagnostic(diagnostic);
  }

  initialize(): void {
    const compiler = this;

    this.module.setMemory(256, MEM_MAX_32, "memory", []);
    // TODO: it seem that binaryen.js doesn't support importing memory yet

    const sourceFiles = this.program.getSourceFiles();
    for (let i = 0, k = sourceFiles.length, file; i < k; ++i) {

      if ((file = sourceFiles[i]).isDeclarationFile)
        continue;

      for (let i = 0, k = file.statements.length, statement; i < k; ++i) {
        switch ((statement = file.statements[i]).kind) {

          case ts.SyntaxKind.VariableStatement:
            compiler.initializeVariable(<ts.VariableStatement>statement);
            break;

          case ts.SyntaxKind.FunctionDeclaration:
            compiler.initializeFunction(<ts.FunctionDeclaration>statement);
            break;

          case ts.SyntaxKind.ClassDeclaration:
            compiler.initializeClass(<ts.ClassDeclaration>statement);
            break;

          case ts.SyntaxKind.EnumDeclaration:
            compiler.initializeEnum(<ts.EnumDeclaration>statement);
            break;

          case ts.SyntaxKind.EndOfFileToken:
            break;

          default:
            throw Error("unsupported top-level node: " + ts.SyntaxKind[statement.kind]);
        }
      }
    }
  }

  initializeVariable(node: ts.VariableStatement): void {
    // TODO: it seems that binaryen.js does not support globals, yet
  }

  private _initializeFunction(node: ts.FunctionDeclaration | ts.MethodDeclaration, parent?: ts.ClassDeclaration, isInstance: boolean = false): void {
    const name = node.symbol.name;

    if (node.typeParameters && node.typeParameters.length !== 0)
      this.error(node.typeParameters[0], "Type parameters are not supported yet");

    let parameters: WasmType[];
    let signatureIdentifiers: string[]; // including return type
    let signatureTypes: number[]; // excluding return type
    let index = 0;

    if (parent && isInstance) {
      parameters = new Array(node.parameters.length + 1);
      signatureIdentifiers = new Array(parameters.length + 1);
      signatureTypes = new Array(parameters.length);

      const thisType = this.uintptrType; // TODO: underlyingType
      parameters[0] = thisType;
      signatureIdentifiers[0] = thisType.toSignatureIdentifier(this.uintptrType);
      signatureTypes[0] = thisType.toBinaryenType(this.uintptrType);

      index = 1;
    } else {
      parameters = new Array(node.parameters.length);
      signatureIdentifiers = new Array(parameters.length + 1);
      signatureTypes = new Array(parameters.length);
    }

    for (let i = 0, k = node.parameters.length; i < k; ++i) {
      const type = this.resolveType(node.parameters[i].type);
      parameters[index] = type;
      signatureIdentifiers[index] = type.toSignatureIdentifier(this.uintptrType);
      signatureTypes[index++] = type.toBinaryenType(this.uintptrType);
    }

    const returnType = this.resolveType(node.type, true);
    signatureIdentifiers[index] = returnType.toSignatureIdentifier(this.uintptrType);

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
    const name = node.symbol.name;

    for (let i = 0, k = node.members.length, member; i < k; ++i) {
      switch ((member = node.members[i]).kind) {

        case ts.SyntaxKind.Identifier:
          break;

        case ts.SyntaxKind.MethodDeclaration:
          if (isExport(node))
            this.error(node, "Class methods cannot be exports");
          if (isImport(node))
            this.error(node, "Class methods cannot be imports");
          this._initializeFunction(<ts.MethodDeclaration>member, node, (node.modifierFlagsCache & ts.ModifierFlags.Static) === 0);
          break;

        default:
          this.error(node, "Unsupported class member", ts.SyntaxKind[node.kind]);

      }
    }
  }

  initializeEnum(node: ts.EnumDeclaration): void {
    const name = node.symbol.name;

    for (let i = 0, k = node.members.length, member; i < k; ++i) {
      this.constants[name + "$" + node.members[i].symbol.name] = {
        type: intType,
        value: this.checker.getConstantValue(member)
      };
    }
  }

  compile(): void {
    this.module.autoDrop();

    const sourceFiles = this.program.getSourceFiles();
    for (let i = 0, k = sourceFiles.length; i < k; ++i) {

      if (sourceFiles[i].isDeclarationFile)
        continue;

      const statements = sourceFiles[i].statements;
      for (let j = 0, l = statements.length, statement; j < l; ++j) {
        switch ((statement = statements[j]).kind) {

          case ts.SyntaxKind.VariableStatement:
            this.compileVariable(<ts.VariableStatement>statement);
            break;

          case ts.SyntaxKind.FunctionDeclaration:
            this.compileFunction(<ts.FunctionDeclaration>statement);
            break;

          case ts.SyntaxKind.ClassDeclaration:
            this.compileClass(<ts.ClassDeclaration>statement);
            break;

          // otherwise already reported by initialize
        }
      }
    }
  }

  compileVariable(node: ts.VariableStatement): void {
    // TODO
  }

  private _compileFunction(node: ts.FunctionDeclaration | ts.MethodDeclaration) {
    const wasmFunction: WasmFunction = (<any>node).wasmFunction;
    const compiler = this;
    const locals: { [key: string]: WasmVariable } = {};
    const op = this.module;

    for (let i = 0, k = node.parameters.length; i < k; ++i) {
      locals[node.parameters[i].symbol.name] = {
        index: i,
        type: wasmFunction.parameters[i]
      };
    }

    this.currentFunction = wasmFunction;
    this.currentLocals = locals;
    this.currentBlockIndex = -1;

    const body: WasmStatement[] = new Array(node.body.statements.length);
    let index = 0;

    for (let i = 0, k = node.body.statements.length; i < k; ++i)
      body[i] = compiler.compileStatement(node.body.statements[i]);

    return this.module.addFunction(wasmFunction.name, wasmFunction.signature, [], op.block("", body));
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

    for (let i = 0, k = node.members.length, member; i < k; ++i) {
      switch ((member = node.members[i]).kind) {

        case ts.SyntaxKind.MethodDeclaration:
          compiler._compileFunction(<ts.MethodDeclaration>member);
          break;

        // default:
        // already reported by initialize
      }
    }
  }

  compileStatement(node: ts.Statement): WasmStatement {
    const op = this.module;

    switch (node.kind) {

      case ts.SyntaxKind.IfStatement:
      {
        const stmt = <ts.IfStatement>node;
        return op.if(
          this.convertValue(stmt.expression, this.compileExpression(stmt.expression, intType), (<any>stmt.expression).wasmType, intType, true),
          this.compileStatement(stmt.thenStatement),
          stmt.elseStatement ? this.compileStatement(stmt.elseStatement) : undefined
        );
      }

      /* case ts.SyntaxKind.SwitchStatement:
      {
        const stmt = <ts.SwitchStatement>node;
        const blocks: WasmStatement[] = new Array(stmt.caseBlock.clauses.length);
        const labels: string[] = new Array(blocks.length);
        let hasDefault = false;
        stmt.caseBlock.clauses.forEach((clause, i) => {
          let label: string;
          if (clause.kind == ts.SyntaxKind.DefaultClause) {
            if (hasDefault)
              this.error(clause, "A switch statement cannot have multiple default branches");
            hasDefault = true;
            label = "default";
          } else {
            label = "case" + i;
          }
          labels[i] = label;
          blocks[i] = op.block(label, clause.statements.map(stmt => this.compileStatement(stmt)));
        });
        return op.block("break", [
          op.switch(labels, hasDefault ? "default" : "break", this.compileExpression(stmt.expression, intType))
        ].concat(blocks));
      } */

      case ts.SyntaxKind.WhileStatement:
      {
        ++this.currentBlockIndex;
        const stmt = <ts.WhileStatement>node;
        return op.loop("break$" + this.currentBlockIndex, op.block("continue$" + this.currentBlockIndex, [
          op.break("break$" + this.currentBlockIndex, op.i32.eqz(this.convertValue(stmt.expression, this.compileExpression(stmt.expression, intType), (<any>stmt.expression).wasmType, intType, true))),
          this.compileStatement(stmt.statement)
        ]));
      }

      case ts.SyntaxKind.DoStatement:
      {
        ++this.currentBlockIndex;
        const stmt = <ts.WhileStatement>node;
        return op.loop("break$" + this.currentBlockIndex, op.block("continue$" + this.currentBlockIndex, [
          this.compileStatement(stmt.statement),
          op.break("break$" + this.currentBlockIndex, op.i32.eqz(this.convertValue(stmt.expression, this.compileExpression(stmt.expression, intType), (<any>stmt.expression).wasmType, intType, true)))
        ]));
      }

      case ts.SyntaxKind.Block:
      {
        const stmt = <ts.Block>node;
        if (stmt.statements.length === 0)
          return op.nop();
        else if (stmt.statements.length === 1)
          return this.compileStatement(stmt.statements[0]);
        else {
          const children: WasmStatement[] = new Array(stmt.statements.length);
          for (let i = 0, k = children.length; i < k; ++i)
            children[i] = this.compileStatement(stmt.statements[i]);
          return op.block("", children);
        }
      }

      case ts.SyntaxKind.ContinueStatement:
        return op.break("continue$" + this.currentBlockIndex);

      case ts.SyntaxKind.BreakStatement:
        return op.break("break$" + this.currentBlockIndex);

      case ts.SyntaxKind.ReturnStatement:
      {
        const stmt = <ts.ReturnStatement>node;

        if (this.currentFunction.returnType === voidType) {

          if (stmt.getChildCount() > 1) // return keyword
            this.error(stmt, "A function without a return type cannot return a value", this.currentFunction.name);
          return op.return();

        } else {

          if (stmt.getChildCount() < 2) // return keyword + expression
            this.error(stmt, "A function with a return type must return a value", this.currentFunction.name);
          const expr = <ts.Expression>stmt.getChildAt(1);
          return op.return(
            this.convertValue(
              expr,
              this.compileExpression(expr, this.currentFunction.returnType),
              (<any>expr).wasmType,
              this.currentFunction.returnType,
              false
            )
          );
        }
      }

      default:
        this.error(node, "Unsupported statement node", ts.SyntaxKind[node.kind]);
    }
  }

  compileExpression(node: ts.Expression, contextualType: WasmType): WasmExpression {
    const op = this.module;

    // remember to always set 'wasmType' on 'node' here

    switch (node.kind) {

      case ts.SyntaxKind.ParenthesizedExpression:
      {
        const expr = (<ts.ParenthesizedExpression>node).expression;
        const compiled = this.compileExpression(expr, contextualType);
        (<any>node).wasmType = (<any>expr).wasmType;
        return compiled;
      }

      case ts.SyntaxKind.AsExpression:
      {
        const expr = <ts.AsExpression>node;
        const asType = this.resolveType(expr.type);
        (<any>node).wasmType = asType;
        return this.convertValue(node, this.compileExpression(expr.expression, contextualType), (<any>expr.expression).wasmType, asType, true);
      }

      case ts.SyntaxKind.BinaryExpression:
      {
        const expr = <ts.BinaryExpression>node;
        let left = this.compileExpression(expr.left, contextualType);
        let right = this.compileExpression(expr.right, contextualType);
        let leftType: WasmType = (<any>expr.left).wasmType;
        let rightType: WasmType = (<any>expr.right).wasmType;
        let resultType: WasmType;

        if (leftType.isFloat) {
          if (rightType.isFloat)
            resultType = leftType.size > rightType.size ? leftType : rightType;
          else
            resultType = leftType;
        } else if (rightType.isFloat) {
          resultType = rightType;
        } else {
          resultType = leftType.size > rightType.size ? leftType : rightType;
        }

        // compile again with contextual result type so that literals are properly coerced
        if (leftType !== resultType)
          left = this.convertValue(expr.left, this.compileExpression(expr.left, resultType), leftType, resultType, false);
        if (rightType !== resultType)
          right = this.convertValue(expr.right, this.compileExpression(expr.right, resultType), rightType, resultType, false);

        if (resultType === floatType) {

          (<any>expr).wasmType = floatType;

          switch (expr.operatorToken.kind) {

            case ts.SyntaxKind.PlusToken:
              return op.f32.add(left, right);

            case ts.SyntaxKind.MinusToken:
              return op.f32.sub(left, right);

            case ts.SyntaxKind.AsteriskToken:
              return op.f32.mul(left, right);

            case ts.SyntaxKind.SlashToken:
              return op.f32.div(left, right);

            case ts.SyntaxKind.EqualsEqualsToken:
              return op.f32.eq(left, right);

            case ts.SyntaxKind.ExclamationEqualsToken:
              return op.f32.ne(left, right);

            case ts.SyntaxKind.GreaterThanToken:
              return op.f32.gt(left, right);

            case ts.SyntaxKind.GreaterThanEqualsToken:
              return op.f32.ge(left, right);

            case ts.SyntaxKind.LessThanToken:
              return op.f32.lt(left, right);

            case ts.SyntaxKind.LessThanEqualsToken:
              return op.f32.le(left, right);

          }

        } else if (resultType === doubleType) {

          (<any>expr).wasmType = doubleType;

          switch (expr.operatorToken.kind) {

            case ts.SyntaxKind.PlusToken:
              return op.f64.add(left, right);

            case ts.SyntaxKind.MinusToken:
              return op.f64.sub(left, right);

            case ts.SyntaxKind.AsteriskToken:
              return op.f64.mul(left, right);

            case ts.SyntaxKind.SlashToken:
              return op.f64.div(left, right);

            case ts.SyntaxKind.EqualsEqualsToken:
              return op.f64.eq(left, right);

            case ts.SyntaxKind.ExclamationEqualsToken:
              return op.f64.ne(left, right);

            case ts.SyntaxKind.GreaterThanToken:
              return op.f64.gt(left, right);

            case ts.SyntaxKind.GreaterThanEqualsToken:
              return op.f64.ge(left, right);

            case ts.SyntaxKind.LessThanToken:
              return op.f64.lt(left, right);

            case ts.SyntaxKind.LessThanEqualsToken:
              return op.f64.le(left, right);

          }

        } else if (resultType.isLong) {

          (<any>expr).wasmType = longType;

          switch (expr.operatorToken.kind) {

            case ts.SyntaxKind.PlusToken:
              return op.i64.add(left, right);

            case ts.SyntaxKind.MinusToken:
              return op.i64.sub(left, right);

            case ts.SyntaxKind.AsteriskToken:
              return op.i64.mul(left, right);

            case ts.SyntaxKind.SlashToken:
              if (resultType.isSigned)
                return op.i64.div_s(left, right);
              else
                return op.i64.div_u(left, right);

            case ts.SyntaxKind.PercentToken:
              if (resultType.isSigned)
                return op.i64.rem_s(left, right);
              else
                return op.i64.rem_u(left, right);

            case ts.SyntaxKind.AmpersandToken:
              return op.i64.and(left, right);

            case ts.SyntaxKind.BarToken:
              return op.i64.or(left, right);

            case ts.SyntaxKind.CaretToken:
              return op.i64.xor(left, right);

            case ts.SyntaxKind.LessThanLessThanToken:
              return op.i64.shl(left, right);

            case ts.SyntaxKind.GreaterThanGreaterThanToken:
              if (resultType.isSigned)
                return op.i64.shr_s(left, right);
              else
                return op.i64.shr_u(left, right);

            case ts.SyntaxKind.EqualsEqualsToken:
              return op.i64.eq(left, right);

            case ts.SyntaxKind.ExclamationEqualsToken:
              return op.i64.ne(left, right);

            case ts.SyntaxKind.GreaterThanToken:
              if (resultType.isSigned)
                return op.i64.gt_s(left, right);
              else
                return op.i64.gt_u(left, right);

            case ts.SyntaxKind.GreaterThanEqualsToken:
              if (resultType.isSigned)
                return op.i64.ge_s(left, right);
              else
                return op.i64.ge_u(left, right);

            case ts.SyntaxKind.LessThanToken:
              if (resultType.isSigned)
                return op.i64.lt_s(left, right);
              else
                return op.i64.lt_u(left, right);

            case ts.SyntaxKind.LessThanEqualsToken:
              if (resultType.isSigned)
                return op.i64.le_s(left, right);
              else
                return op.i64.le_u(left, right);

          }

        } else { // some i32 type including bool

          (<any>expr).wasmType = intType;

          switch (expr.operatorToken.kind) {

            case ts.SyntaxKind.PlusToken:
              return op.i32.add(left, right);

            case ts.SyntaxKind.MinusToken:
              return op.i32.sub(left, right);

            case ts.SyntaxKind.AsteriskToken:
              return op.i32.mul(left, right);

            case ts.SyntaxKind.SlashToken:
              if (resultType.isSigned)
                return op.i32.div_s(left, right);
              else
                return op.i32.div_u(left, right);

            case ts.SyntaxKind.PercentToken:
              if (resultType.isSigned)
                return op.i32.rem_s(left, right);
              else
                return op.i32.rem_u(left, right);

            case ts.SyntaxKind.AmpersandToken:
              return op.i32.and(left, right);

            case ts.SyntaxKind.BarToken:
              return op.i32.or(left, right);

            case ts.SyntaxKind.CaretToken:
              return op.i32.xor(left, right);

            case ts.SyntaxKind.LessThanLessThanToken:
              return op.i32.shl(left, right);

            case ts.SyntaxKind.GreaterThanGreaterThanToken:
              if (resultType.isSigned)
                return op.i32.shr_s(left, right);
              else
                return op.i32.shr_u(left, right);

            case ts.SyntaxKind.EqualsEqualsToken:
              return op.i32.eq(left, right);

            case ts.SyntaxKind.ExclamationEqualsToken:
              return op.i32.ne(left, right);

            case ts.SyntaxKind.GreaterThanToken:
              if (resultType.isSigned)
                return op.i32.gt_s(left, right);
              else
                return op.i32.gt_u(left, right);

            case ts.SyntaxKind.GreaterThanEqualsToken:
              if (resultType.isSigned)
                return op.i32.ge_s(left, right);
              else
                return op.i32.ge_u(left, right);

            case ts.SyntaxKind.LessThanToken:
              if (resultType.isSigned)
                return op.i32.lt_s(left, right);
              else
                return op.i32.lt_u(left, right);

            case ts.SyntaxKind.LessThanEqualsToken:
              if (resultType.isSigned)
                return op.i32.le_s(left, right);
              else
                return op.i32.le_u(left, right);

          }
        }

        this.error(expr.operatorToken, "Unsupported binary operator", ts.SyntaxKind[expr.operatorToken.kind]);
      }

      case ts.SyntaxKind.PrefixUnaryExpression:
      {
        const expr = <ts.PrefixUnaryExpression>node;

        switch (expr.operator) {

          case ts.SyntaxKind.ExclamationToken:
            (<any>expr).wasmType = boolType;
            return op.i32.eqz(this.compileExpression(expr.operand, boolType));

          // TODO: + and - always convert to a double like in JS - is that what we want?
          case ts.SyntaxKind.PlusToken:
            (<any>expr).wasmType = doubleType;
            return this.convertValue(expr.operand, this.compileExpression(expr.operand, contextualType), (<any>expr.operand).wasmType, doubleType, true);

          case ts.SyntaxKind.MinusToken:
            (<any>expr).wasmType = doubleType;
            return op.i32.mul(this.convertValue(expr.operand, this.compileExpression(expr.operand, contextualType), (<any>expr.operand).wasmType, doubleType, true), op.i32.const(-1));

          case ts.SyntaxKind.TildeToken:

            if (contextualType.isLong) {
              (<any>expr).wasmType = longType;
              return op.i64.xor(this.convertValue(expr.operand, this.compileExpression(expr.operand, contextualType), (<any>expr.operand).wasmType, longType, true), op.i64.const(-1, -1));
            } else {
              (<any>expr).wasmType = intType;
              return op.i32.xor(this.convertValue(expr.operand, this.compileExpression(expr.operand, contextualType), (<any>expr.operand).wasmType, intType, true), op.i32.const(-1));
            }

          default:
            this.error(expr, "Unsupported unary operator", ts.SyntaxKind[expr.operator]);
        }
      }

      case ts.SyntaxKind.FirstLiteralToken:
      {
        let text = (<ts.LiteralExpression>node).text;
        let radix: number;

        if (text === "true")
          text = "1";
        else if (text === "false")
          text = "0";

        if (/^(?:0|[1-9][0-9]*)$/.test(text)) {
          radix = 10;
        } else if (/^0[xX][0-9A-Fa-f]+$/.test(text)) {
          radix = 16;
          text = text.substring(2);
        } else if (/^(?![eE])[0-9]*(?:\.[0-9]*)?(?:[eE][+-]?[0-9]+)?$/.test(text)) {
          if (!contextualType.isFloat) { // explicit float in non-float context must be converted
            (<any>node).wasmType = doubleType;
            return op.f64.const(parseFloat(text));
          }
        } else {
          this.error(node, "Unsupported literal", text);
          text = "0";
          radix = 10;
        }

        (<any>node).wasmType = contextualType;

        let long: Long;
        switch (contextualType) {

          case floatType:
            return op.f32.const(parseFloat(text));

          case doubleType:
            return op.f64.const(parseFloat(text));

          case byteType:
          case sbyteType:
          case shortType:
          case ushortType:
          case intType:
          case uintType:
          case uintptrType32:
            return op.i32.const(parseInt(text, radix) & ((contextualType.size << 8) - 1));

          case longType:
          case ulongType:
          case uintptrType64:
            long = Long.fromString(text, contextualType === ulongType, radix);
            return op.i64.const(long.low, long.high);

          case boolType:
            return op.i32.const(parseInt(text, radix) !== 0 ? 1 : 0);
        }
      }

      case ts.SyntaxKind.Identifier:
      {
        const ident = <ts.Identifier>node;
        const local = this.currentLocals[ident.text];

        if (local == null) {
          this.error(node, "Undefined local variable", ident.text);
          return op.unreachable();
        }

        (<any>node).wasmType = local.type;

        return op.getLocal(local.index, local.type.toBinaryenType(this.uintptrType));
      }

      case ts.SyntaxKind.PropertyAccessExpression:
      {
        const expr = <ts.PropertyAccessExpression>node;

        if (expr.expression.kind === ts.SyntaxKind.Identifier) {
          const name = (<ts.Identifier>expr.expression).text;

          if (expr.name.kind === ts.SyntaxKind.Identifier) {
            const prop = (<ts.Identifier>expr.name).text;
            const constant = this.constants[name + "$" + prop];
            let long: Long;
            if (constant) {
              switch (constant.type) {

                case byteType:
                case sbyteType:
                case shortType:
                case ushortType:
                case intType:
                case uintType:
                case uintptrType32:
                  (<any>node).wasmType = intType;
                  return op.i32.const(constant.value);

                case longType:
                case ulongType:
                case uintptrType64:
                  long = Long.fromValue(constant.value);
                  (<any>node).wasmType = longType;
                  return op.i64.const(long.low, long.high);

                case floatType:
                  (<any>node).wasmType = floatType;
                  return op.f32.const(constant.value);

                case doubleType:
                  (<any>node).wasmType = doubleType;
                  return op.f64.const(constant.value);

              }
            }
          }
        }

        this.error(node, "Unsupported property access");
      }

      case ts.SyntaxKind.TrueKeyword:

        if (contextualType.isLong) {
          (<any>node).wasmType = longType;
          return op.i64.const(1, 0);
        } else {
          (<any>node).wasmType = intType;
          return op.i32.const(1);
        }

      case ts.SyntaxKind.FalseKeyword:
      case ts.SyntaxKind.NullKeyword:

        if (contextualType.isLong) {
          (<any>node).wasmType = longType;
          return op.i64.const(0, 0);
        } else {
          (<any>node).wasmType = intType;
          return op.i32.const(0);
        }

      default:
        this.error(node, "Unsupported expression node", ts.SyntaxKind[node.kind]);
    }
  }

  convertValue(node: ts.Node, expr: WasmExpression, fromType: WasmType, toType: WasmType, explicit: boolean): WasmExpression {
    if (fromType.kind === toType.kind)
      return expr;

    const $this = this;
    const op = this.module;

    function illegalImplicitConversion() {
      $this.error(node, "Cannot convert from '" + fromType + "' to '" + toType + "' without a cast");
      explicit = true; // report this only once for the topmost node
    }

    (<any>node).wasmType = toType;

    if (fromType === floatType) {

      if (!explicit && toType !== doubleType)
        illegalImplicitConversion();

      switch (toType) {

        case byteType:
        case ushortType:
        case boolType:
          return this.convertValue(node, op.i32.trunc_u.f32(expr), intType, toType, explicit);

        case uintType:
        case uintptrType32:
          return op.i32.trunc_u.f32(expr);

        case sbyteType:
        case shortType:
          return this.convertValue(node, op.i32.trunc_s.f32(expr), intType, toType, explicit);

        case intType:
          return op.i32.trunc_s.f32(expr);

        case ulongType:
        case uintptrType64:
          return op.i64.trunc_u.f32(expr);

        case longType:
          return op.i64.trunc_s.f32(expr);

        // floatType == floatType

        case doubleType:
          return op.f64.promote(expr);

      }

    } else if (fromType === doubleType) {

      if (!explicit) illegalImplicitConversion();

      switch (toType) {

        case byteType:
        case ushortType:
        case boolType:
          return this.convertValue(node, op.i32.trunc_u.f64(expr), intType, toType, explicit);

        case uintType:
        case uintptrType32:
          return op.i32.trunc_u.f64(expr);

        case sbyteType:
        case shortType:
          return this.convertValue(node, op.i32.trunc_s.f64(expr), intType, toType, explicit);

        case intType:
          return op.i32.trunc_s.f64(expr);

        case ulongType:
        case uintptrType64:
          return op.i64.trunc_u.f64(expr);

        case longType:
          return op.i64.trunc_s.f64(expr);

        case floatType:
          return op.f32.demote(expr);

        // doubleType == doubleType

      }

    } else if (toType === floatType) { // int to float

      switch (fromType) {

        case uintType:
        case uintptrType32:
          if (!explicit) illegalImplicitConversion();

        case byteType:
        case ushortType:
        case boolType:
          return op.f32.convert_u.i32(expr);

        case intType:
          if (!explicit) illegalImplicitConversion();

        case sbyteType:
        case shortType:
          return op.f32.convert_s.i32(expr);

        case ulongType:
        case uintptrType64:
          if (!explicit) illegalImplicitConversion();
          return op.f32.convert_u.i64(expr);

        case longType:
          if (!explicit) illegalImplicitConversion();
          return op.f32.convert_s.i64(expr);

      }

    } else if (toType === doubleType) { // int to double

      switch (fromType) {

        case uintType:
        case uintptrType32:
        case byteType:
        case ushortType:
        case boolType:
          return op.f64.convert_u.i32(expr);

        case intType:
        case sbyteType:
        case shortType:
          return op.f64.convert_s.i32(expr);

        case ulongType:
        case uintptrType64:
          if (!explicit) illegalImplicitConversion();
          return op.f64.convert_u.i64(expr);

        case longType:
          if (!explicit) illegalImplicitConversion();
          return op.f64.convert_s.i64(expr);

      }

    } else if (fromType.isInt && toType.isLong) {

      if (toType.isSigned)
        return op.i64.extend_s(expr);
      else
        return op.i64.extend_u(expr);

    } else if (fromType.isLong && toType.isInt) {

      if (!explicit) illegalImplicitConversion();

      expr = op.i32.wrap(expr);
      fromType = fromType.isSigned ? intType : uintType;

      // fallthrough
    }

    // int to other int

    if (fromType.size < toType.size)
      return expr;

    if (!explicit) illegalImplicitConversion();

    if (toType.isSigned) {
      return op.i32.shl(
        op.i32.shr_s(
          expr,
          op.i32.const(toType.shift32)
        ),
        op.i32.const(toType.shift32)
      );
    } else {
      return op.i32.and(
        expr,
        op.i32.const(toType.mask32)
      );
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
            throw Error("illegal number of type parameters on Ptr<T>");
          if (reference.typeArguments[0].kind !== ts.SyntaxKind.TypeReference)
            throw Error("unsupported type parameter on Ptr<T>");
          return this.uintptrType.withUnderlyingType(this.resolveType(<ts.TypeReferenceNode>reference.typeArguments[0]));
      }
    }

    throw Error("unsupported type: " + text);
  }
}
