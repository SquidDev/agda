import { promises as fs } from "fs";
import path from "path";

import resolve from "@rollup/plugin-node-resolve";
import typescript from "@rollup/plugin-typescript";
import url from "@rollup/plugin-url";

const out = "dist";

export default {
  input: ["src/main.ts"],
  output: {
    dir: out,
    format: "iife",
    paths: {
      "monaco-editor": "vs/editor/editor.main",
    },
    generatedCode: {
      preferConst: true,
    }
  },
  context: "window",
  external: ["monaco-editor", "require", "jszip"],

  plugins: [
    url({
      limit: 1024,
      fileName: "[name]-[hash][extname]",
      include: ["**/*.worker.js", "**/*.png"],
    }),

    typescript(),
    resolve({ browser: true, }),

    {
      name: "copy-cat",
      async writeBundle () {
        await Promise.all([
          fs.copyFile("node_modules/requirejs/require.js", `${out}/require.js`),
        ]);
      },
      async transform(code, file) {
        // Allow loading files in /mount.
        const ext = path.extname(file);
        return ext == '.agda' ? `export default ${JSON.stringify(code)};\n` : null;
      },
    },
  ],
};
