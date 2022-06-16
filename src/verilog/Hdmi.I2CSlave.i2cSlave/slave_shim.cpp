#include <cstdlib>

#include <verilated.h>

#include "Vslave.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vslave *top = new Vslave;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

