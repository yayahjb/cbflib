/**********************************************************************
 *                                                                    *
 * Comparison functions for single & double precision floating point  *
 * numbers, which function correctly in the presence of NaNs &        *
 * infinities but don't account for denormalised numbers              *
 *                                                                    *
 *    J.Sloan, 06/2013                                                *
 *                                                                    *
 **********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
 *********************************************************************/

#ifndef CBF_ULP_H
#define CBF_ULP_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdint.h>

#ifndef UINT64_MAX
#define NO_UINT64_TYPE
#endif

#ifndef isinf
#define isinf32(x) (!(((*(unsigned int*)&x)&0x7fffffff)^0x7f800000))
#define isinf64(x) (!(((*(uint64_t*)&x)&0x7fffffffffffffffl)^0x7ff0000000000000l))
#else
#define isinf32(x) isinf(x)
#define isinf64(x) isinf(x)
#endif

#ifndef isnan
#define isnan32(x) (((*(unsigned int*)&x)&0x007fffff)&&!(((*(unsigned int*)&x)&0x7f800000)^0x7f800000))
#define isnan64(x) (((*(uint64_t*)&x)&0x000fffffffffffffl)&&!(((*(uint64_t*)&x)&0x7ff0000000000000l)^0x7ff0000000000000l))
#else
#define isnan32(x) isnan(x)
#define isnan64(x) isnan(x)
#endif

/** \brief Compare 32 bit IEEE754s */
unsigned int cbf_ULP32(const float a, const float b);


#ifndef NO_UINT64_TYPE

/** \brief Compare 64 bit IEEE754s */
uint64_t cbf_ULP64(const double a, const double b);

#endif

/** \brief Let people find out if the library was compiled with support for the ULP64 function */
int cbf_has_ULP64();

#ifdef __cplusplus

}

#endif

#endif
