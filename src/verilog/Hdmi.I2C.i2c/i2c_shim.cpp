#include <cstdlib>

#include <verilated.h>

#include "Vi2c.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vi2c *top = new Vi2c;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

