import { WASI, Fd, PreopenDirectory, File, Directory } from "@bjorn3/browser_wasi_shim";
import type { Iovec } from "@bjorn3/browser_wasi_shim/typings/wasi_defs";


const makeBuffered = (callback: (msg: string) => void): ((msg: string) => void) => {
  let buffer = "";
  return text => {
    buffer += text;

    const i = buffer.lastIndexOf("\n");
    if (i >= 0) {
      callback(buffer.substring(0, i));
      buffer = buffer.substring(i + 1);
    }
  }
}
class TextFd extends Fd {
  private readonly callback: (msg: string) => void;
  private readonly decoder: TextDecoder;
  constructor(callback: (msg: string) => void) {
    super();

    this.callback = callback;
    this.decoder = new TextDecoder();
  }
  fd_write(view8: Uint8Array, iovs: [Iovec]): { ret: number, nwritten: number } {
    let nwritten = 0;
    let text = "";
    for (let iovec of iovs) {
      text += this.decoder.decode(view8.slice(iovec.buf, iovec.buf + iovec.buf_len));
      nwritten += iovec.buf_len;
    }

    this.callback(text);
    return { ret: 0, nwritten };
  }
}

(async () => {
  console.log("Downloading");
  let wasm = await WebAssembly.compileStreaming(fetch("dist/agda.wasm"));
  console.log("Initialising");


  let w = new WASI(["agda"], [
    "Agda_datadir=/agda"
  ], [
    new Fd(),
    new TextFd(x => console.log("[Stdout] " + x)),
    new TextFd(makeBuffered(x => console.error(x))),
    new PreopenDirectory(".", {
      "test.agda": new File(new TextEncoder().encode(`data True : Set where\n  tt : True`)),
    }),
    new PreopenDirectory("/", {
      "agda": new Directory({
        "lib": new Directory({

        }),
      }),
    }),
  ]);
  let inst = await WebAssembly.instantiate(wasm, {
    // "wasi_snapshot_preview1": w.wasiImport
    "wasi_snapshot_preview1": new Proxy(w.wasiImport, {
      get(target, prop, receiver) {
        let func = Reflect.get(target, prop, receiver);
        return (...args: any[]) => {
          const res = Reflect.apply(func, receiver, args);
          console.log(prop, "(", ...args, ") => ", res);
          return res;
        };
      }
    }),
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
  w.initialize(instTyped);

  console.log("Initialising (GHC)");

  instTyped.exports["wizer.initialize"]();

  console.log("Loading file");

  const { malloc, memory, agdaRun } = instTyped.exports;
  const cmd = `IOTCM "/file.agda" NonInteractive Direct (Cmd_load "/file.agda" [])\0`;
  const ptr = malloc(cmd.length);
  new TextEncoder().encodeInto(cmd, new Uint8Array(memory.buffer, ptr, cmd.length));
  agdaRun(ptr, cmd.length - 1);

  console.log("Done.");
})();
