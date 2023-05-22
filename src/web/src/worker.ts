import { WASI } from "@wasmer/wasi";
import { WasmFs } from "@wasmer/wasmfs";

import prim from "../../data/lib/prim/Agda/Primitive.agda"
import primCubical from "../../data/lib/prim/Agda/Primitive/Cubical.agda"

const makeWriter = (callback: (msg: string) => void) => (buffer: Buffer) => {
  callback(buffer.toString());
  return buffer.length;
};

const log = (message: string) => {
  console.log(message);
  postMessage({ "kind": "RunningInfo", message });
};

(async () => {
  const fs = new WasmFs();
  fs.volume.fds[1].write = makeWriter(x => {
    const msg = x.trimEnd();
    if (msg.length == 0) return;
    console.log(msg);
  });
  fs.volume.fds[2].write = makeWriter(x => console.error(x.trimEnd()));
  fs.volume.fromJSON({
    "/": null,
    "/agda": null,
    "/agda/lib": null,
    "/agda/lib/prim/Agda/Primitive.agda": prim,
    "/agda/lib/prim/Agda/Primitive/Cubical.agda": primCubical,
    // TODO: This should be driven from user input.
    "/file.agda": `data ⊤ : Set where\n  tt : ⊤\n`
  });

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

  log("Downloading and compiling")

  const module = await WebAssembly.instantiateStreaming(fetch("agda.wasm", { "cache": "no-cache" }), {
    "wasi_snapshot_preview1": wasi.wasiImport,
    "agda": {
      "interact": (addr: number, len: number) => {
        const text = new TextDecoder().decode(wasi.memory.buffer.slice(addr, addr + len));
        let message;
        try {
          message = JSON.parse(text);
        } catch (e) {
          console.error(e);
          return;
        }
        postMessage(message);
      },
    },
  });
  const instance = module.instance as typeof module & {
    exports: {
      memory: WebAssembly.Memory,
      _initialize: () => void,
      "wizer.initialize": () => void,
      "agdaRun": (ptr: number, len: number) => void,
      "malloc": (len: number) => number,
    },
  };
  wasi.start(instance);

  log("Initialising (Base)");
  instance.exports._initialize();

  log("Initialising (GHC)");
  instance.exports["wizer.initialize"]();

  log("Loading file");

  const { malloc, memory, agdaRun } = instance.exports;
  const cmd = `IOTCM "/file.agda" NonInteractive Direct (Cmd_load "/file.agda" [])\0`;
  const ptr = malloc(cmd.length);
  new TextEncoder().encodeInto(cmd, new Uint8Array(memory.buffer, ptr, cmd.length));
  agdaRun(ptr, cmd.length - 1);

  log("Done.");
})();

export default "";
