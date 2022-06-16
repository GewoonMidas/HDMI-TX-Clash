#include <cstdlib>

#include <verilated.h>

#include "Vsystem.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vsystem *top = new Vsystem;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

