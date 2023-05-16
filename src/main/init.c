#include "Main_stub.h"
#include <Rts.h>

__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
  char *args[] = {"agda.wasm", "+RTS", "-A128M", "-RTS", NULL};
  int argc = sizeof(args) / sizeof(args[0]) - 1;
  char **argv = args;
  hs_init_with_rtsopts(&argc, &argv);
  agdaSetup();
  hs_perform_gc();
  hs_perform_gc();
  rts_clearMemory();
}
