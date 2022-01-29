/* tovero.h */

/* Copyright 2018 Kavalogic, Inc. */

/*
 * This file is part of Tovero Library.
 *
 * Tovero Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * Tovero Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.  You should have
 * received a copy of the GNU Lesser General Public License along with
 * Tovero Library. If not, see <http://www.gnu.org/licenses/>.
 *
 */

/* Tovero library functions */

#ifndef KAVALOGIC_LIB_TOVERO_TOVERO_H_
#define KAVALOGIC_LIB_TOVERO_TOVERO_H_

#include <libfive.h>

#ifdef __cplusplus

extern "C" {
typedef Kernel::Tree tovero_tree;
#else
typedef struct tovero_tree tovero_tree;
#endif

/*
 * Structures
 */

typedef libfive_interval tovero_interval;
typedef libfive_region2 tovero_region2;
typedef libfive_region3 tovero_region3;

typedef libfive_vec3 tovero_vec3;

typedef libfive_contour3 tovero_contour;
typedef libfive_contours3 tovero_contours;

typedef libfive_mesh tovero_mesh_contiguous;
typedef libfive_mesh_coords tovero_mesh_delimited;

typedef libfive_pixels tovero_pixels;

typedef struct tovero_voxel_slice {
  uint8_t *voxels;
  uint32_t x_dimension;
  uint32_t y_dimension;
} tovero_voxel_slice;

/*
 *   destructors
 */
void tovero_contours_delete(tovero_contours* contours);
void tovero_mesh_contiguous_delete(tovero_mesh_contiguous* mesh);
void tovero_mesh_delimited_delete(tovero_mesh_delimited* mesh);
void tovero_pixels_delete(tovero_pixels* pixels);
void tovero_voxel_slice_delete(tovero_voxel_slice* pixels);

/*
 *  Initialization
 */
void tovero_initialize(int disable_fpe);
int tovero_is_initialized();

/*
 * Opcode
 */
int tovero_opcode_enum(const char* op);
int tovero_opcode_args(int op);

/*
 * Tree
 */
tovero_tree* tovero_tree_x();
tovero_tree* tovero_tree_y();
tovero_tree* tovero_tree_z();

tovero_tree* tovero_tree_var();
int tovero_tree_is_var(tovero_tree* tree);

tovero_tree* tovero_tree_const(float f);
float tovero_tree_get_const(tovero_tree* tree, int* return_success);

tovero_tree *tovero_tree_constant_vars(tovero_tree *t);

tovero_tree* tovero_tree_nonary(int op);
tovero_tree* tovero_tree_unary(int op, tovero_tree* a);
tovero_tree* tovero_tree_binary(int op,
                                tovero_tree* a,
                                tovero_tree* b);

const void* tovero_tree_id(tovero_tree* tree);

float tovero_tree_eval_f(tovero_tree* tree, tovero_vec3 p);
void tovero_tree_eval_r(tovero_tree* tree,
                        const tovero_region3* region,
                        tovero_interval *out_interval);
void tovero_tree_eval_d(tovero_tree* tree,
                        const tovero_vec3* p,
                        tovero_vec3* out_vec);

int tovero_tree_eq(tovero_tree* a, tovero_tree* b);

void tovero_tree_delete(tovero_tree* ptr);

int tovero_tree_save(tovero_tree* tree, const char* file_name);
tovero_tree* tovero_tree_load(const char* file_name);

tovero_tree* tovero_tree_remap(tovero_tree* p,
                               tovero_tree* x,
                               tovero_tree* y,
                               tovero_tree* z);

void tovero_tree_bounds(tovero_tree* tree,
                        tovero_region3* out_bounds);

const char* tovero_tree_print(tovero_tree* tree);

tovero_contours* tovero_tree_render_slice(tovero_tree* tree,
                                          const tovero_region2* region,
                                          float z,
                                          float resolution);
void tovero_tree_save_slice(tovero_tree* tree,
                            const tovero_region2* region,
                            float z,
                            float resolution,
                            const char* file_name);

tovero_mesh_contiguous*
  tovero_tree_render_mesh_contiguous(tovero_tree* tree,
                                     const tovero_region3* region,
                                     float resolution);
tovero_mesh_delimited*
  tovero_tree_render_mesh_delimited(tovero_tree* tree,
                                    const tovero_region3* region,
                                    float resolution);

int tovero_tree_save_mesh(tovero_tree* tree,
                          const tovero_region3 *region,
                          float resolution,
                          const char* file_name);

tovero_pixels* tovero_tree_render_pixels(tovero_tree* tree,
                                         const tovero_region2* region,
                                         float z,
                                         float resolution);

tovero_voxel_slice* tovero_tree_render_voxel_slice(tovero_tree* tree,
                                                   const tovero_region2* region,
                                                   float z,
                                                   float resolution,
                                                   uint8_t voxel_off_value,
                                                   uint8_t voxel_on_value);

/*
 * Renderer info (libfive version)
 */

const char* tovero_renderer_version(void);
const char* tovero_renderer_revision(void);

#ifdef __cplusplus
}
#endif

#endif /* KAVALOGIC_LIB_TOVERO_TOVERO_H_ */
