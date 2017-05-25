/// <reference path="./require-json.d.ts" />

import { Compiler } from "../../src/compiler";
import * as fs from "fs";
import * as minimist from "minimist";
import * as pkg from "../../package.json";

const argv = minimist(process.argv.slice(2), {
  alias: {
    "out": [ "o", "outFile" ],
    "validate": [ "v" ],
    "optimize": [ "O" ],
    "text": [ "t" ]
  },
  string: [ "out" ],
  boolean: [ "text", "optimize", "validate" ]
});

const files = argv._;

if (files.length !== 1) {
  process.stderr.write([
    "Version " + (<any>pkg["version"]),
    "Syntax: asc [options] [file ...]",
    "",
    "Options:",
    " -o, --out         Specifies the output file name.",
    " -v, --validate    Validates the module.",
    " -O, --optimize    Runs optimizing binaryen IR passes.",
    " -t, --text        Emits text format instead of a binary.",
    ""
  ].join("\n"));
  process.exit(1);
}

const wasmModule = Compiler.compile(files[0]);
if (!wasmModule)
  process.exit(1);

if (argv.validate)
  wasmModule.validate();

if (argv.optimize)
  wasmModule.optimize();

if (argv.out && /\.wast$/.test(argv.out))
  argv.text = true;

let output: any = argv.out ? fs.createWriteStream(argv.out) : process.stdout;

if (argv.text)
  output.write(wasmModule.emitText(), "utf8");
else
  output.write(Buffer.from(wasmModule.emitBinary()));
