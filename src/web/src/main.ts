import { WASI } from "@wasmer/wasi";
import { WasmFs } from "@wasmer/wasmfs";

import prim from "../../data/lib/prim/Agda/Primitive.agda"


const makeWriter = (callback: (msg: string) => void) => (buffer: Buffer) => {
  callback(buffer.toString());
  return buffer.length;
};

(async () => {
  console.log("Downloading");
  let wasm = await WebAssembly.compileStreaming(fetch("dist/agda.wasm"));
  console.log("Initialising");

  const fs = new WasmFs();
  fs.volume.fds[1].write = makeWriter(x => {
    const msg = x.trimEnd();
    if (msg.length > 0) console.log(msg);
  });
  fs.volume.fds[2].write = makeWriter(x => console.error(x.trimEnd()));
  fs.volume.fromJSON({
    "/": null,
    "/agda": null,
    "/agda/lib": null,
    "/agda/lib/prim/Agda/Primitive.agda": prim,
    "/file.agda": `{-# OPTIONS -v10 #-}\ndata ⊤ : Set where\n  tt : ⊤\n`
  });
  console.log("Volume", fs.volume, fs.volume.toJSON());

  const wasi = new WASI({
    bindings: {
      ...WASI.defaultBindings,
      fs: fs.fs,
      path: {
        resolve: (l: string, r: string): string => {
          if (r.startsWith("/")) return r;
          return l.endsWith("/") ? `${l}${r}` : `${l}/${r}`;
        },
        relative: (l: string, r: string): string => {
          if (l == r) return "."
          console.log("resolve", l, r);
          return l + "/" + r;
        }
      }
    },
    args: ["agda"],
    env: { "Agda_datadir": "/agda" },
    preopens: { "/": "/" },
    // traceSyscalls: true,
  });

  const inst = await WebAssembly.instantiate(wasm, {
    "wasi_snapshot_preview1": wasi.wasiImport,
  });
  const instTyped = inst as typeof inst & {
    exports: {
      memory: WebAssembly.Memory,
      _initialize: () => void,
      "wizer.initialize": () => void,
      "agdaRun": (ptr: number, len: number) => void,
      "malloc": (len: number) => number,
    }
  };
  wasi.start(instTyped);

  console.log("Initialising (GHC)");

  instTyped.exports._initialize();
  instTyped.exports["wizer.initialize"]();

  console.log("Loading file");

  const { malloc, memory, agdaRun } = instTyped.exports;
  const cmd = `IOTCM "/file.agda" NonInteractive Direct (Cmd_load "/file.agda" [])\0`;
  const ptr = malloc(cmd.length);
  new TextEncoder().encodeInto(cmd, new Uint8Array(memory.buffer, ptr, cmd.length));
  agdaRun(ptr, cmd.length - 1);

  console.log("Done.");
})();
