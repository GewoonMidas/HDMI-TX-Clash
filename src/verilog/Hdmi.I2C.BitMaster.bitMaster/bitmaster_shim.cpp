#include <cstdlib>

#include <verilated.h>

#include "Vbitmaster.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vbitmaster *top = new Vbitmaster;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

