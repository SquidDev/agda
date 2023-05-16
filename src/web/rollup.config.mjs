import { promises as fs } from "fs";
import path from "path";

import resolve from "@rollup/plugin-node-resolve";
import typescript from "@rollup/plugin-typescript";

const out = "dist";

export default [{
  input: ["src/worker.ts"],
  output: {
    dir: out,
    format: "iife",
    generatedCode: {
      preferConst: true,
    }
  },
  context: "window",

  plugins: [
    typescript(),
    resolve({ browser: true, }),

    {
      name: "copy-cat",
      async transform(code, file) {
        // Allow loading files in /mount.
        const ext = path.extname(file);
        return ext == '.agda' ? `export default ${JSON.stringify(code)};\n` : null;
      },
    },
  ],
}, {
  input: ["src/main.ts"],
  output: {
    dir: out,
    format: "iife",
    generatedCode: {
      preferConst: true,
    }
  },
  context: "window",

  plugins: [
    typescript(),
    resolve({ browser: true, }),
  ],
}];
