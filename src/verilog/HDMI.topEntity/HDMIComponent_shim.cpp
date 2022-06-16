#include <cstdlib>

#include <verilated.h>

#include "VHDMIComponent.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VHDMIComponent *top = new VHDMIComponent;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

