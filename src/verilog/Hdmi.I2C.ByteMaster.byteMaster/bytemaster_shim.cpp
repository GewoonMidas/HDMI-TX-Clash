#include <cstdlib>

#include <verilated.h>

#include "Vbytemaster.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vbytemaster *top = new Vbytemaster;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

