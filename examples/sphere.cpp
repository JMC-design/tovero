// sphere.cpp
//
// Copyright 2018 Kavalogic, Inc.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//   (1) Redistributions of source code must retain the above copyright
//   notice, this list of conditions and the following disclaimer.
//
//   (2) Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimer in
//   the documentation and/or other materials provided with the
//   distribution.
//
//   (3) The name of the author may not be used to
//   endorse or promote products derived from this software without
//   specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
// IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

// compile with:
//   g++ -std=c++11 -I/usr/local/include/libfive -o sphere sphere.cpp -L/usr/local/lib -lfive

#include <libfive/libfive.h>

float origin[] = { 0.0f, 0.0f, 0.0f };

#define tree_add(x,y) libfive_tree_binary(libfive_opcode_enum("ADD"), (x), (y))
#define tree_sub(x,y) libfive_tree_binary(libfive_opcode_enum("SUB"), (x), (y))
#define tree_sqrt(x) libfive_tree_unary(libfive_opcode_enum("SQRT"), (x))
#define tree_square(x) libfive_tree_unary(libfive_opcode_enum("SQUARE"), (x))
#define tree_x libfive_tree_x()
#define tree_y libfive_tree_y()
#define tree_z libfive_tree_z()
#define tree_const(c) libfive_tree_const((c))

libfive_tree sphere(float r, float center[3] = origin)
{
  return
    (tree_sub (tree_sqrt
               (tree_add
                (tree_add (tree_square
                           (tree_sub (tree_x, tree_const(center[0]))),
                           tree_square
                           (tree_sub (tree_y, tree_const(center[1])))),
                 tree_square
                 (tree_sub (tree_z, tree_const(center[2]))))),
               tree_const(r)));
}

void write_sphere()
{
  libfive_tree s = sphere(1.0);
  libfive_region3 bounds = libfive_tree_bounds(s);
  float resolution = 20.0f;

  libfive_tree_save_mesh(s, bounds, resolution, "sphere_cpp.stl");
}

int main(int argc, char* argv[])
{
  write_sphere();
}
