/// <reference path="./webassembly.d.ts" />

export interface ILoadOptions {
  initialMemory?: number;
  maximumMemory?: number;
  imports?: { [key: string]: any }
}

interface IModule {
  memory: WebAssembly.Memory;
  imports: { [key: string]: any };
  exports: { [key: string]: any };
  U8: Uint8Array;
  S8: Int8Array;
  U16: Uint16Array;
  S16: Int16Array;
  U32: Uint32Array;
  S32: Int32Array;
  F32: Float32Array;
  F64: Float64Array;
  currentMemory(): number;
  growMemory(numPages: number): number;
}

export function load(filename: string, options?: ILoadOptions): Promise<IModule> {
  if (!options)
    options = {};

  let imports = options.imports || {};
  let memory = <WebAssembly.Memory>imports.memory;

  if (!memory)
    memory = new WebAssembly.Memory({
      initial: options.initialMemory || 256,
      maximum: options.maximumMemory || 4294967295
    });

  let module = <IModule>{
    memory: memory,
    imports: imports,
    exports: {},
    currentMemory: function currentMemory(): number {
      return this.memory.buffer.byteLength >>> 16;
    },
    growMemory: function growMemory(numPages: number): number {
      let previousPages = this.memory.grow(numPages);
      onGrowMemory();
      return previousPages;
    }
  };

  function onGrowMemory() {
    module.U8 = new Uint8Array(memory.buffer);
    module.S8 = new Int8Array(memory.buffer);
    module.U16 = new Uint16Array(memory.buffer);
    module.S16 = new Int16Array(memory.buffer);
    module.U32 = new Uint32Array(memory.buffer);
    module.S32 = new Int32Array(memory.buffer);
    module.F32 = new Float32Array(memory.buffer);
    module.F64 = new Float64Array(memory.buffer);
  }

  return (typeof fetch === "function" && fetch || fetch_node)(filename)
    .then((result: Response) => result.arrayBuffer())
    .then((buffer: ArrayBuffer) => WebAssembly.instantiate(buffer, imports))
    .then((result: WebAssembly.ResultObject) => {
      module.exports = result.instance.exports;
      onGrowMemory();
      return <IModule>module;
    });
}

var fs: any;
function fetch_node(file: string): Promise<Response> {
  // 'Promise' only refers to a type, but is being used as a value here. - no idea why that's still a thing
  return new Promise((resolve: (result: any) => {}, reject: (error: Error) => {}) =>
    (fs || (fs = eval("equire".replace(/^/, "r"))("fs"))).readFile(file, (err: Error, data?: Buffer) => err ? reject(err) : resolve({ arrayBuffer: () => data }))
  );
}
