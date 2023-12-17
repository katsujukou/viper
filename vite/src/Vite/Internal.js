import * as vite from 'vite';

export const unsafeShowForeign = f => { console.log(f); return `${f}` }

/**
 * @param {<T>(p: Promise<T>) => Aff<T>} toAffE
 * @param {import('vite').UserConfig} config 
 */
export function mkViteImpl(toAffE, config) {
  return function () {
    /** @var {ViteDevServer} server */
    let server;
    return {
      serve: toAffE(async () => {
        server = await vite.createServer(config);
        await server.listen();
        return toAffE(() => server.close());
      }),
    }
  };
}

export const unsafePartialConfig = function (Nothing, Just, rec, keys) {

}