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
 * built on the Andy Arvai version.  Further extended 10/31/2015 by HJB to allow
 * for large fuzzy spots
 */

/* dps_peaksearch
 
 The original standalone program was used as dps_peakssearch frame.file
 This version is used as a function call:
 
 #include "dps_peaksearch.h
 
 int dps_peaksearch(unsigned short *data, // The 16 bit data
 int nx, int ny,       // The dimensions of the data
 int npeaks_out,       // The maximum number of peaks
 double min_isigma,    // The minimum I/sigma to accept
 int min_spacing,      // The minimum spacing in pixels
 DPS_Peak *pptr);      // The array of peaks
 
 */

/* This program does a peak search on a given image and returns a list of
 R, S coordinates in pixel. It uses the read_frame routine from the util
 library and a modified algorithm of Sangsoo Kim.			*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "dps_peaksearch.h"


int xirint(double x) {
    if (x == (int)x)
        return((int)x);
    if (x > 0.0)
        return((int)(x+0.5));
    else
        return((int)(x-0.5));
}


#if defined (__linux__)||defined(sun)
/* Round a double to an integer */


/* Calculate sin() and cos(). */
void
sincos(x, s, c)
double x, *s, *c;
{
    *s = sin(x);
    *c = cos(x);
}
#endif /* __linux__ */

#if defined(sgi)||defined(Alpha)||defined(hppa)||defined(convex)||defined(rs6000)


/* Calculate sin() and cos(). */
void
sincos(x, s, c)
double x, *s, *c;
{
    *s = sin(x);
    *c = cos(x);
}


/* Round a double to an integer */
double
rint(x)
register double x;
{
    if (x == (int)x)
        return(x);
    if (x > 0.0)
        return((double)((int)(x+0.5)));
    else
        return((double)((int)(x-0.5)));
}

#endif /* sgi */


int	fsign(double x)
{
    if (x >= 0.0)
        return(1);
    else
        return(-1);
}
/* Find the "center of mass" around a "spot" centered at x,y in the array
 * idata.
 *
 */

void
cmass (idata, iwidth, iheight, x, y, cm_x, cm_y, cm_nx, cm_ny)
register unsigned short  *idata;
int iwidth, iheight, x, y;
double *cm_x, *cm_y;
int cm_nx, cm_ny;
{
    register int i, j, val;
    double sum_x=0, sum_y=0, sum_z=0.0;
    int prev_x, prev_y;
    
    prev_x = x;
    prev_y = y;
    
    for(j= -cm_ny/2;j<=cm_ny/2;j++)
        for(i= -cm_nx/2;i<=cm_nx/2;i++) {
            if (((j+y) >= iheight) || ((j+y) < 0)
                || ((i+x) >= iwidth) || ((i+x) < 0))
                continue;
            val = idata[(j+y)*iwidth + (i+x)];
            sum_x += ((double)i+x)*val;
            sum_y += ((double)j+y)*val;
            sum_z += val;
        }
    if (sum_z == 0.0) {
        *cm_x = x;
        *cm_y = y;
    }
    else {
        *cm_x = sum_x / sum_z;
        *cm_y = sum_y / sum_z;
    }
    
    /* Do a second pass to get a better value */
    sum_x = sum_y = sum_z = 0;
    x = xirint(*cm_x);
    y = xirint(*cm_y);
    
    if ((x == prev_x) && (y == prev_y))
        return;
    
    for(j= -cm_ny/2;j<=cm_ny/2;j++)
        for(i= -cm_nx/2;i<=cm_nx/2;i++) {
            if (((j+y) >= iheight) || ((j+y) < 0)
                || ((i+x) >= iwidth) || ((i+x) < 0))
                continue;
            val = idata[(j+y)*iwidth + (i+x)];
            sum_x += ((double)i+x)*val;
            sum_y += ((double)j+y)*val;
            sum_z += val;
        }
    if (sum_z == 0.0) {
        *cm_x = x;
        *cm_y = y;
    }
    else {
        *cm_x = sum_x / sum_z;
        *cm_y = sum_y / sum_z;
    }
}


static	DPS_Peak *dps_peaks = NULL;

static	int	ccd_image_saturation = 0;

int dps_peaksearch(unsigned short *data, int nx, int ny,
                   int npeaks_out, double min_isigma,
                   int min_spacing, DPS_Peak *pptr, int min_value)
{
    
    int stepx;
    int stepy;
    int small_stepx;
    int small_stepy;
    double back_count;
    double spot_count;
    double cm_x, cm_y;
    int i, j; /* counter */
    int y, dy; /* more counter */
    int value; /* variable to store an actual od_value */
    int maxval;
    int bmax, bmin;
    int k, l; /* many more counter... */
    int x_max, y_max;
    int bma_x, bmi_x, bma_y, bmi_y;
    int back, spot;
    double noise_thresh = 1.0;
    double A, B, I, sigmaI;
    int overload = 55000;
    int npeaks=0;
    int maxpeaks=20480;
    int sortfunc();
    int nover;
    int nunder;
    int	nxfer;
    DPS_Peak dps_temp;
    int peakfw, peakfh;
    int next_good_y[nx];
    int collide;
    
    stepx = (min_spacing+1)/2;  /* Initial stepsize for scanning through the image */
    if (stepx < 1) stepx = 1;
    stepy = stepx;
    small_stepx = stepx;
    if (small_stepx > 3) small_stepx = 3;
    small_stepy = stepy;
    if (small_stepy > 3) small_stepy = 3;
    
    back_count = 4*stepx+4*stepy; /* Number of background pixels */
    spot_count = (2*stepx-1)*(2*stepy-1); /* Number of spot pixels (see below) */
    
    
    if (min_isigma > 0)
        noise_thresh = min_isigma;
    
    if (ccd_image_saturation > 0)
        overload = ccd_image_saturation;
    
    if (dps_peaks != NULL)
        free(dps_peaks);
    
    dps_peaks = (DPS_Peak *)malloc(maxpeaks * sizeof(DPS_Peak));
    if (dps_peaks == NULL) {
        fprintf(stderr,"error: not enough memory for dps_peaksearch.\n");
        fflush(stderr);
        return 0;
    }
    
    /* Initialize the next_good_y  array to 0 */
    for (i=0; i < nx; i++) next_good_y[i]= 0;
    
    /* The next two loops go over the whole frame with stepsize
     * small_stepx and small_stepy, but looking at boxes 2*stepx by 2*stepy.
     */
    for(j=2*stepy;j<ny-2*stepy;j=j+small_stepy) {
        for(i=2*stepx;i<nx-2*stepx;i=i+small_stepx) {
            
            /* Skip this pixel if too close to prior spots */
            
            collide = 0;
            
            for (l=i-stepx; l<=i+stepx; l++) {
                if (next_good_y[l] > j-stepy-1 ) {
                    collide = 1;
                    break;
                }
            }
            if (collide) continue;
            
            /* y hold the index of the pixel at i,j */
            y = j*nx+i;
            
            /* dx is the difference in index between i,j and
             * i, j+stepy  */
            dy = stepy*nx;
            
            value=data[y];
            
            /* Check if we have a maximum at i,j */
            if ((value > data[y+stepx]) &&
                (value > data[y-stepx]) &&
                (value > data[y+dy])    &&
                (value > data[y-dy])    &&
                (value > data[y+stepy+dy]) &&
                (value > data[y-stepy+dy]) &&
                (value > data[y+stepy-dy]) &&
                (value > data[y-stepy-dy])        ) {
                
                /* If we have a maximum, try to find the maximum
                 * in a box around i, j with size 2*stepy x 2*stepx
                 * and stepsize 1. */
                maxval=data[y];
                
                /* x_max, y_max will hold the final maximum */
                x_max=j;
                y_max=i;
                for(k=j-stepy;k<=j+stepy;k++) {
                    for(l=i-stepx;l<=i+stepx;l++) {
                        
                        /* Same as above only with stepsize 1 */
                        y = k*nx+l;
                        dy = nx;
                        value=data[y];
                        if((value >= data[y+1])  &&
                           (value >= data[y-1])  &&
                           (value >= data[y+dy]) &&
                           (value >= data[y-dy]) &&
                           (value >= maxval)         ) {
                            maxval=value;
                            x_max=l;
                            y_max=k;
                        }
                    }
                }
                
                cmass (data, nx, ny, x_max, y_max, &cm_x, &cm_y, 2*stepx+1, 2*stepy+1);
                x_max = cm_x + 0.5;
                y_max = cm_y + 0.5;
                
                /* Now we calculate the average background and
                 * spot values for this position. The box goes
                 * from i-stepy to i+stepy and j-stepx to
                 * i+stepx. The background pixels are the pixels
                 * of a one pixel frame at the border of the box,
                 * the rest are spot pixels. For stepy=stepy=3 this
                 * gives 25 spot pixels and 24 background pixels
                 */
                
                /* Borders of the box */
                bma_y = y_max+stepy;
                bmi_y = y_max-stepy;
                bma_x = x_max+stepx;
                bmi_x = x_max-stepx;
                
                if (bma_y >= ny) bma_y = ny-1;
                if (bma_x >= nx) bma_x = nx-1;
                if (bmi_y < 0) bmi_y = 0;
                if (bmi_x < 0) bmi_x = 0;
                
                back = 0;
                bmax = -1;
                bmin = 999999;
                spot = 0;
                
                nover=0;
                nunder=0;
                for(k=bmi_y;k<=bma_y;k++) {
                    for(l=bmi_x;l<=bma_x;l++) {
                        
                        /* see above */
                        y = k*nx+l;
                        value=data[y];
                        value &= 0xFFFF;
                        
                        if (value >= overload) {
                            nover++;
                        }
                        
                        if (value < min_value) {
                            nunder++;
                        }
                        
                        /* if counter at border of box, the pixel
                         * is a background pixel otherwise it is a
                         * spot pixel */
                        if ((k == bma_y) ||
                            (k == bmi_y) ||
                            (l == bma_x) ||
                            (l == bmi_x)    ) {
                            back = back + value;
                            if (value > bmax) bmax = value;
                            if (value < bmin) bmin = value;
                        }
                        else {
                            spot = spot + value;
                        }
                    }
                }
                /* If the average spot pixel value is larger by a
                 * certain factor than the average background pixel value
                 * write the reflection to the output file. Check also
                 * if the maximum value is an overload value.... */
                
                A = (double)spot;
                B = (double)back*spot_count/back_count;
                I = A - B;
                sigmaI = sqrt(A + B);
                if ((sigmaI > 0.0)
                    && (I/sigmaI > noise_thresh)
                    && (nover <= 4)
                    && (nunder < 1)
                    &&!near_edge(data, nx, ny, x_max, y_max,
                                 ((double)back)/((double)(back_count)),
                                 maxval, bmax, bmin, stepx, stepy,
                                 &peakfw, &peakfh, min_value)) {
                    dps_peaks[npeaks].x = cm_x;
                    dps_peaks[npeaks].y = cm_y;
                    dps_peaks[npeaks].isigma = I/sigmaI;
                    dps_peaks[npeaks].peakfw = peakfw;
                    dps_peaks[npeaks].peakfh = peakfh;
                    npeaks++;
                        
                    /* Update next_good_y for this peak */

                        for (l=x_max-peakfw/2; l<=x_max+peakfw/2; l++) {
                        if (next_good_y[l] < y_max+stepy+1 ) {
                            next_good_y[l] = y_max+stepy+1;
                        }
                    }


                    if (x_max+peakfw/2-small_stepx > i) i = x_max+peakfw/2-small_stepx;
                        
                    if (npeaks >= maxpeaks) {
                        maxpeaks *= 2;
                        dps_peaks = (DPS_Peak *)realloc(dps_peaks, sizeof(DPS_Peak)*maxpeaks);
                        if (dps_peaks == NULL) {
                            fprintf(stderr,"error: not enough memory for dps_peaksearch (%d spots).\n",npeaks);
                            return 0;
                        }
                    }
                        
                }
            }
        }
    }
    
    if (min_spacing > 0) {
        for(i=0;i<npeaks;i++)
            for(j=i+1;j<npeaks;j++) {
                if ((fabs(dps_peaks[i].x - dps_peaks[j].x) < (dps_peaks[i].peakfw + dps_peaks[j].peakfw)/2) &&
                    (fabs(dps_peaks[i].y - dps_peaks[j].y) < (dps_peaks[i].peakfh + dps_peaks[j].peakfh)/2)) {
                    
                    if (dps_peaks[i].isigma > dps_peaks[j].isigma)
                        dps_peaks[j].isigma = dps_peaks[j].x = dps_peaks[j].y = -9999;
                    else
                        dps_peaks[i].isigma = dps_peaks[i].x = dps_peaks[i].y = -9999;
                }
            }
    }
    qsort(dps_peaks,npeaks,sizeof(DPS_Peak),sortfunc);
    
    if ((npeaks_out <= 0) || (npeaks < npeaks_out))
        npeaks_out = npeaks;
    
    nxfer = 0;
    
    for(i=0;i<npeaks_out;i++)
    {
        if(dps_peaks[i].x < 0 || dps_peaks[i].y < 0)
            continue;
        dps_temp.x = dps_peaks[i].x;
        dps_temp.y = dps_peaks[i].y;
        dps_temp.isigma = dps_peaks[i].isigma;
        dps_temp.peakfw = dps_peaks[i].peakfw;
        dps_temp.peakfh = dps_peaks[i].peakfh;
        *pptr++ = dps_temp;
        nxfer++;
    }
    
    free(dps_peaks);
    return(nxfer);
}

/* Sort pixel value
 */
int
sortfunc(pk1, pk2)
DPS_Peak *pk1, *pk2;
{
    if (pk2->isigma > pk1->isigma)
        return 1;
    else
        if (pk2->isigma < pk1->isigma)
            return -1;
        else
            return 0;
}

/* Test if there are more than stepx+stepy pixels within the spot or the spot overlaps the edge
 */
int	near_edge(unsigned short *data,
              int width, int height,
              int xpos, int ypos,
              double back, int peak, int bmax, int bmin,
              int stepx, int stepy, int* peakfw, int* peakfh, int min_value)
{
    int x,y, is, it;
    int minstep, maxstep;
    int value;
    int smin, smax;
    double bavg;
    
    x = xpos;
    y = ypos;
    
    if (x < stepx)
        return 1;
    if (x >= width-stepx)
        return 1;
    if (y < stepy)
        return 1;
    if (y >= height-stepy)
        return 1;
    if (bmax >= peak)
        return 1;
    
    minstep = maxstep = stepx;
    if (stepy > maxstep) maxstep = stepy;
    if (stepy < minstep) minstep = stepy;
    
    
    /* Loop through bounding boxes 
       When we are is from the peak, we have 
       2*(2*is+1)+2*(2*is-1) = 8*is points */
    for (is = 1; is <= minstep; is++) {
        smax = -1;
        smin = 999999;
        bavg = 0;
        for (it = -is; it <= is; it ++) {
            value = data[width*(ypos -is)+xpos+it];
            value &= 0xFFFF;
            if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
            if (value < min_value) return 1;
            if (value > smax) smax = value;
            if (value < smin) smin = value;
            bavg += (double)value;
            value = data[width*(ypos +is)+xpos+it];
            value &= 0xFFFF;
            if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
            if (value > smax) smax = value;
            if (value < smin) smin = value;
            bavg += (double)value;
        }
        for (it = -is+1; it <= is-1; it ++) {
            value = data[width*(ypos +it)+xpos-is];
            value &= 0xFFFF;
            if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
            if (value > smax) smax = value;
            if (value < smin) smin = value;
            bavg += (double)value;
            value = data[width*(ypos +it)+xpos+is];
            value &= 0xFFFF;
            if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
            if (value > smax) smax = value;
            if (value < smin) smin = value;
            bavg += (double)value;
        }
        bavg /= (double)(8*is);
        if ((double)smin < back/2. && smin < bmin) return 1;
        if ((double)bavg >= back*1.1 ) continue;
        if (smax <= bmax) {
            *peakfw = *peakfh = is*2-1;
            return 0;
        }
    }
    for (is = minstep+1; is <= maxstep; is++) {
        smax = -1;
        smin = 999999;
        bavg = 0;
        if (stepy > stepx) {
            for (it = -minstep; it <= minstep; it ++) {
                value = data[width*(ypos -is)+xpos+it];
                value &= 0xFFFF;
                if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
                if (value > smax) smax = value;
                if (value < smin) smin = value;
                bavg += (double)value;
                value = data[width*(ypos +is)+xpos+it];
                value &= 0xFFFF;
                if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
                if (value > smax) smax = value;
                if (value < smin) smin = value;
                bavg += (double)value;
            }
            for (it = -is+1; it <= is-1; it ++) {
                value = data[width*(ypos +it)+xpos-minstep];
                value &= 0xFFFF;
                if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
                if (value > smax) smax = value;
                if (value < smin) smin = value;
                bavg += (double)value;
                value = data[width*(ypos +it)+xpos+minstep];
                value &= 0xFFFF;
                if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
                if (value > smax) smax = value;
                if (value < smin) smin = value;
                bavg += (double)value;
            }
            
        } else if (stepx < stepy) {
            
            for (it = -is; it <= is; it ++) {
                value = data[width*(ypos -minstep)+xpos+it];
                value &= 0xFFFF;
                if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
                if (value > smax) smax = value;
                if (value < smin) smin = value;
                bavg += (double)value;
                value = data[width*(ypos +minstep)+xpos+it];
                value &= 0xFFFF;
                if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
                if (value > smax) smax = value;
                if (value < smin) smin = value;
                bavg += (double)value;
            }
            for (it = -minstep+1; it <= minstep-1; it ++) {
                value = data[width*(ypos +it)+xpos-is];
                value &= 0xFFFF;
                if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
                if (value > smax) smax = value;
                if (value < smin) smin = value;
                bavg += (double)value;
                value = data[width*(ypos +it)+xpos+is];
                value &= 0xFFFF;
                if (value !=0xFFFF && (value & 0XFFFC) == 0xFFFC) return 1;
                if (value > smax) smax = value;
                if (value < smin) smin = value;
                bavg += (double)value;
            }
        }
        bavg /= (double)(4*is+4*minstep);
        if ((double)smin < back/2. && smin < bmin) return 1;
        if ((double)bavg >= back*1.1 ) continue;
        if (smax <= bmax) {
            if (stepx < stepy) {
                *peakfh = 2*minstep-1;
                *peakfw = 2*is-1;
            } else {
                *peakfw = 2*minstep-1;
                *peakfh = 2*is-1;
            }
            return 0;
        }
    }
    *peakfh = 2*stepy+1;
    *peakfw = 2*stepx+1;
    return 0;
    
 }
