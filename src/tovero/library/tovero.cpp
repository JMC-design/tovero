// tovero.cpp
//
// Copyright 2018 Kavalogic, Inc.
//
// This file is part of Tovero Library.
//
// Tovero Library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License
// as published by the Free Software Foundation; either version 2.1 of
// the License, or (at your option) any later version.
//
// Tovero Library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.  You should have
// received a copy of the GNU Lesser General Public License along with
// Tovero Library. If not, see <http://www.gnu.org/licenses/>.

#include <libfive.h>
#include "tovero.h"
#include <iostream>
#include <fstream>
#include <cfenv>

#if ! LIBFIVE_BOUNDS_EVER_WORK
#include <limits>
#endif

namespace {
  bool tovero_library_initialized = false;
}

//
// Structures
//

//   structure destructors
void tovero_contours_delete(tovero_contours* contours)
{
  libfive_contours3_delete(contours);
}

void tovero_mesh_contiguous_delete(tovero_mesh_contiguous* mesh)
{
  libfive_mesh_delete(mesh);
}

void tovero_mesh_delimited_delete(tovero_mesh_delimited* mesh)
{
  libfive_mesh_coords_delete(mesh);
}

void tovero_pixels_delete(tovero_pixels* pixels)
{
  libfive_pixels_delete(pixels);
}

void tovero_voxel_slice_delete(tovero_voxel_slice* voxel_slice)
{
  delete [] voxel_slice->voxels;
  delete voxel_slice;
}

//
// Library initialization
//

void tovero_initialize(int disable_fpe)
{
  if (!tovero_library_initialized)
  {
    // libfive generates FPEs, which need to be
    // disabled because CL's FPE handling is
    // 'bozotic' (to be kind)...
    //    if (disable_fpe && (fedisableexcept(FE_OVERFLOW) == -1))
    if (disable_fpe && (fedisableexcept(FE_ALL_EXCEPT) == -1))
    {
      fprintf(stderr,
              "Warning: (tovero_initialize) Tovero could not disable"
              " floating point exceptions.\n");
    }
    tovero_library_initialized = true;
  }
}

int tovero_is_initialized()
{
  return tovero_library_initialized;
}

//
// Opcode
//

int tovero_opcode_enum(const char* op)
{
  return libfive_opcode_enum(op);
}

int tovero_opcode_args(int op)
{
  return libfive_opcode_args(op);
}


//
// Tree
//

typedef Kernel::Tree tovero_tree;

tovero_tree* tovero_tree_x()
{
  return libfive_tree_x();
}

tovero_tree* tovero_tree_y()
{
  return libfive_tree_y();
}

tovero_tree* tovero_tree_z()
{
  return libfive_tree_z();
}

tovero_tree* tovero_tree_var()
{
  return libfive_tree_var();
}

int tovero_tree_is_var(tovero_tree* tree)
{
  return libfive_tree_is_var(tree);
}

tovero_tree* tovero_tree_const(float f)
{
  return libfive_tree_const(f);
}

float tovero_tree_get_const(tovero_tree* tree, int* return_success)
{
  bool bool_success;
  float return_value =  libfive_tree_get_const(tree, &bool_success);
  *return_success = bool_success;
  return return_value;
}

tovero_tree *tovero_tree_constant_vars(tovero_tree *tree)
{
  return libfive_tree_constant_vars(tree);
}

tovero_tree* tovero_tree_nonary(int op)
{
  return libfive_tree_nonary(op);
}

tovero_tree* tovero_tree_unary(int op, tovero_tree* a)
{
  return libfive_tree_unary(op, a);
}

tovero_tree* tovero_tree_binary(int op,
                                tovero_tree* a,
                                tovero_tree* b)
{
  return libfive_tree_binary(op,
                             a,
                             b);
}

const void* tovero_tree_id(tovero_tree* tree)
{
  return libfive_tree_id(tree);
}

float tovero_tree_eval_f(tovero_tree* tree, const tovero_vec3* p)
{
  return libfive_tree_eval_f(tree, *p);
}

void tovero_tree_eval_r(tovero_tree* tree,
                        const tovero_region3* region,
                        tovero_interval *out_interval)
{
  *out_interval = libfive_tree_eval_r(tree, *region);
}

void tovero_tree_eval_d(tovero_tree* tree,
                        const tovero_vec3* p,
                        tovero_vec3* out_vec)
{
  *out_vec = libfive_tree_eval_d(tree, *p);
}

int tovero_tree_eq(tovero_tree* a, tovero_tree* b)
{
  return libfive_tree_eq(a, b);
}

void tovero_tree_delete(tovero_tree* ptr)
{
  libfive_tree_delete(ptr);
}

int tovero_tree_save(tovero_tree* ptr, const char* file_name)
{
  return libfive_tree_save(ptr, file_name);
}

tovero_tree* tovero_tree_load(const char* file_name)
{
  return libfive_tree_load(file_name);
}

tovero_tree* tovero_tree_remap(tovero_tree* p,
                               tovero_tree* x,
                               tovero_tree* y,
                               tovero_tree* z)
{
  return libfive_tree_remap(p,
                       x,
                       y,
                       z);
}

void tovero_tree_bounds(tovero_tree* tree,
                        tovero_region3* out_bounds)
{
#if LIBFIVE_BOUNDS_EVER_WORK
  *return_region  = libfive_tree_bounds(tree);
#else
  out_bounds->X.lower = -std::numeric_limits<float>::infinity();
  out_bounds->X.upper = std::numeric_limits<float>::infinity();
  out_bounds->Y.lower = -std::numeric_limits<float>::infinity();
  out_bounds->Y.upper = std::numeric_limits<float>::infinity();
  out_bounds->Z.lower = -std::numeric_limits<float>::infinity();
  out_bounds->Z.upper = std::numeric_limits<float>::infinity();
#endif
}

const char* tovero_tree_print(tovero_tree* tree)
{
  return libfive_tree_print(tree);
}

tovero_contours* tovero_tree_render_slice(tovero_tree* tree,
                                          const tovero_region2* region,
                                          float z,
                                          float resolution)
{
  return libfive_tree_render_slice3(tree,
                                    *region,
                                    z,
                                    resolution);
}

void tovero_tree_save_slice(tovero_tree* tree,
                            const tovero_region2* region,
                            float z,
                            float resolution,
                            const char* file_name)
{
  libfive_tree_save_slice(tree,
                          *region,
                          z,
                          resolution,
                          file_name);
}

tovero_mesh_contiguous*
  tovero_tree_render_mesh_contiguous(tovero_tree* tree,
                                     const tovero_region3* region,
                                     float resolution)
{
    return libfive_tree_render_mesh(tree,
                                    *region,
                                    resolution);
}

tovero_mesh_delimited*
  tovero_tree_render_mesh_delimited(tovero_tree* tree,
                                    const tovero_region3* region,
                                    float resolution)
{
    return libfive_tree_render_mesh_coords(tree,
                                           *region,
                                           resolution);
}

int tovero_tree_save_mesh(tovero_tree* tree,
                          const tovero_region3* region,
                          float resolution,
                          const char* file_name)
{
  return libfive_tree_save_mesh(tree,
                                *region,
                                resolution,
                                file_name);
}

tovero_pixels* tovero_tree_render_pixels(tovero_tree* tree,
                                         const tovero_region2* region,
                                         float z,
                                         float resolution)
{
  return libfive_tree_render_pixels(tree,
                                    *region,
                                    z,
                                    resolution);
}

tovero_voxel_slice* tovero_tree_render_voxel_slice(tovero_tree* tree,
                                                   const tovero_region2* region,
                                                   float z,
                                                   float resolution,
                                                   uint8_t voxel_off_value,
                                                   uint8_t voxel_on_value)
{
  tovero_pixels* pixels = libfive_tree_render_pixels(tree,
                                                     *region,
                                                     z,
                                                     resolution);

  tovero_voxel_slice* voxel_slice = new tovero_voxel_slice;
  size_t voxel_count = pixels->width * pixels->height;
  voxel_slice->voxels = new uint8_t[voxel_count];
  voxel_slice->x_dimension = pixels->width;
  voxel_slice->y_dimension = pixels->height;

  for (size_t i = 0; i < voxel_count; ++i) {
    voxel_slice->voxels[i] = pixels->pixels[i] ? voxel_on_value : voxel_off_value;
  }

  tovero_pixels_delete(pixels);

  return voxel_slice;
}

/*
 * Renderer info (libfive version)
 */

const char* tovero_renderer_version(void)
{
  return libfive_git_version();
}

const char* tovero_renderer_revision(void)
{
  return libfive_git_revision();
}
