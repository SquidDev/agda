#include "Main_stub.h"
#include <Rts.h>

void _agdaInteract(char *buf, uint32_t len) __attribute__((
  __import_module__("agda"),
  __import_name__("interact")
));

void agdaInteract(char *buf, uint32_t len) {
  _agdaInteract(buf, len);
}


__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
  // See https://gitlab.haskell.org/ghc/ghc-wasm-meta#using-wizer-to-pre-initialize-a-wasi-reactor-module
  char *args[] = {"agda.wasm", "+RTS", "-A128M", "-RTS", NULL};
  int argc = sizeof(args) / sizeof(args[0]) - 1;
  char **argv = args;
  hs_init_with_rtsopts(&argc, &argv);
  agdaSetup();
  // GC
  hs_perform_gc();
  hs_perform_gc();
  // Zero out unused memory
  rts_clearMemory();
}
