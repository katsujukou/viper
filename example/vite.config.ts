import { defineConfig, Plugin }from 'vite';
import { readFile } from "node:fs/promises";
import * as path from "node:path";
import pluginInspect from "vite-plugin-inspect";

interface Config {
  spagoDirectory: string;
  pursOutput: string;
}

export default defineConfig({
  root: "/Users/koto/Projects/viper/example",
  plugins: [
    pluginInspect(),
    ((config: Partial<Config>): Plugin => {
      let root: string;
      let spagoCacheDirectory: string;
      let spagoCacheManifest: Record<string, string>;

      const spagoDirectory = config?.spagoDirectory ?? "./.spago";
      const pursOutput = config?.pursOutput ?? "./output";

      function readModuleName (filepath) {
        // TODO ファイル名を信用するのは危ない?
        let done = false;
        let nameParts: string[] = [];
        filepath
          .split("/")
          .reverse()
          .forEach((part) => {
            if (!/^[A-Z][a-zA-Z0-9]*(\.purs)?$/.test(part)) {
              done = true;
              return;
            }
            if (!done) {
              nameParts.push(part.replace(/\.purs$/, ""));
            }
          });
        
        return nameParts.reverse().join(".");
      }
      return {
        name: "vite-plugin-purescript",
        enforce: 'pre',
        async configResolved (resolvedConfig) {
          root = resolvedConfig.root;
          spagoCacheDirectory = path.resolve(root, "node_modules/.spago");
          spagoCacheManifest = JSON.parse(await readFile(path.join(spagoCacheDirectory, 'manifest.json'), 'utf8'));
        },
        async config (config) {
          if (!config.optimizeDeps) {
            config.optimizeDeps = {};
          }
          config.optimizeDeps = {
            include: [
              ...(config.optimizeDeps?.include ?? []),
            ],
          };
        },
        async resolveId (source, importer, options) {
          if (options["scan"]) {
            if (source.endsWith(".purs")) {
              return "/Users/koto/Projects/viper/example/output/Main/index.js";
            }
            if(source.startsWith("@spago")) {
              return source.replace(/^@spago\//, path.resolve(root, "node_modules/.spago/") + "/");
            }
          }
          if (source.startsWith("@spago")) return null;
          //{
            // return source.replace("@spago", ".spago/dist");
            // const metadataJson = await readFile(path.resolve(root, "node_modules/.vite/deps/_metadata.json"), "utf8");
            // const metadata = JSON.parse(metadataJson);
            // const optimized = metadata?.optimized?.[source]?.file;
            // return optimized != undefined
            //   ? path.resolve(root, "node_modules/.vite/deps", optimized)
            //   : source;
           
          // return null;
          //}


          if (importer?.endsWith(".purs")) {
            let pursOutputModuleName: string | null = null;

            if (source.includes("./foreign.js")) {
              const foreignModulePath = importer.replace(".purs", ".js");
              return foreignModulePath.startsWith("/") ? 
                foreignModulePath :
                path.resolve(root, foreignModulePath);
            }
            else if (source.endsWith(".js")) {
              const pursOutputModuleRegex = /\.\.\/(([A-Z][a-zA-Z0-9]*\.)*[A-Z][a-zA-Z0-9]*)\/index\.js$/;
              const matched = source.match(pursOutputModuleRegex);
              if (!matched) {
                throw new Error("Failed to read module name from source path!" + source);
              }
              pursOutputModuleName = matched[1];
            }
            else if (source.endsWith(".purs")) {
              pursOutputModuleName = readModuleName(source);
            }

            if (!pursOutputModuleName) {
              throw new Error("Failed to read purs output module name");
            }
            const corefnJsonPath = path.resolve(root, pursOutput, pursOutputModuleName, "corefn.json");
            const corefnJson = JSON.parse(await readFile(corefnJsonPath, 'utf8'));
            const modulePath = corefnJson["modulePath"];
            if (modulePath.startsWith(".spago/")) {
              return modulePath.replace(/^\.spago/, path.resolve(root, spagoDirectory));
            }
            return modulePath;
          }
          return null;
        },
        async load(id) {
          if (id.endsWith(".purs")) {
            const moduleName = readModuleName(id);
            return {
              code: await readFile(path.resolve(root, pursOutput, moduleName, "index.js"), "utf8"), 
            }
          }
          if (id.startsWith("/.spago/")) {
            return {
              code: await readFile(
                id.replace(/^\/\.spago\//, path.resolve(root, "node_modules/.spago") + "/"), 'utf8'),
            }
          }
        },
      }
    })({

    })
  ]
})