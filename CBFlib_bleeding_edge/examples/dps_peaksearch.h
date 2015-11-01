/*=======================================================================
 * All files in the distribution of the DPS system are Copyright
 * 1996 by the Computational Biology group in the Department of Biological
 * Sciences at Purdue University.  All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this entire copyright notice is duplicated in all such
 * copies, and that any documentation, announcements, and other materials
 * related to such distribution and use acknowledge that the software was
 * developed by the Computational Biology group in the Department of
 * Biological Sciences at Purdue University, W. Lafayette, IN by Ingo
 * Steller and Michael G. Rossmann. No charge may be made for copies,
 * derivations, or distributions of this material without the express
 * written consent of the copyright holder.  Neither the name of the
 * University nor the names of the authors may be used to endorse or
 * promote products derived from this material without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR ANY PARTICULAR PURPOSE.
 *======================================================================*/

/*=====================================================================*
 *                                                                     *
 *                         Data Processing Suite                       *
 *                                                                     *
 *                            Utility-Library                          *
 *                                                                     *
 *                        Written by Ingo Steller                      *
 *                                                                     *
 *                         File: dps_peaksearch.c                      *
 *                                                                     *
 *=====================================================================*/

/* Modifed 3/24/98 By Andy Arvai to be a subroutine where the data
 * is passed as a pointer.
 */
/* Modified 10/24/2015 by H. J. Bernstein to use doubles, rather than
 * floats and, reject peaks with background points below half the background
 * and clean up the comments to reflect the current subroutinized version
 * built on the Chris Neilson, John Skinner version of 2015, which was
 * built on the Andy Arvai version.
 */

/* dps_peaksearch
 
 The original standalone program was used as dps_peaksearch frame.file
 This version is used as a function call:
 
 #include "dps_peaksearch.h
 
 int dps_peaksearch_cn(unsigned short *data, // The 16 bit data
                       int nx, int ny,       // The dimensions of the data
                       int npeaks_out,       // The maximum number of peaks
                       double min_isigma,    // The minimum I/sigma to accept
                       int min_spacing,      // The minimum spacing in pixels
                       DPS_Peak *pptr);      // The array of peaks
 
 */

/* This program does a peak search on a given image and returns a list of
 R, S coordinates in pixel. It uses the read_frame routine from the util
 library and a modified algorithm of Sangsoo Kim.			*/


#ifndef DPS_PEAKSEARCH_H
#define DPS_PEAKSEARCH_H

typedef struct {
    double x;
    double y;
    double isigma;
    int peakfw;  /* the full width of the peak */
    int peakfh;  /* the full height of the peak */
} DPS_Peak;

int dps_peaksearch(unsigned short *data,
                      int nx, int ny,
                      int npeaks_out,
                      double min_isigma,
                      int min_spacing,
                      DPS_Peak *pptr);

int	near_edge(unsigned short *data,
              int width, int height,
              int xpos, int ypos,
              double back, int peak,
              int bmax, int bmin,
              int stepx, int stepy,
              int *peakfw, int *peakfh);

#endif
