/*
 * testcompression.c - test compression schemes
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <float.h>

#include "cbf.h"

#ifdef CBF_USE_LONG_LONG
#ifndef LLONG_MAX
#define LLONG_MAX (1ll << (sizeof(long long) * CHAR_BIT - 1))
#define ULLONG_MAX (~0ll)
#endif
#endif

#define NSTEPS 5

double urand()
{
  return rand() * 1.0 / RAND_MAX;
}

#define DATAUC 0
#define DATASC 1
#define DATAUS 2
#define DATASS 3
#define DATAUI 4
#define DATASI 5
#define DATAUL 6
#define DATASL 7
#define DATAULL 8
#define DATASLL 9
#define DATAF 10
#define DATAD 11

/*
 * Create images where spots are separated by a uniform distribution of mean
 * distance of NSTEPS/2 and have height uniformly distributed over (positive
 * part of) data range
 */
size_t createtestimage(void **data, int type, int nelem)
{
  unsigned char *ucdata;
  signed char *scdata;
  unsigned short *usdata;
  signed short *ssdata;
  unsigned int *uidata;
  signed int *sidata;
  unsigned long *uldata;
  signed long *sldata;
  CBF_ull_type *ulldata;
  CBF_sll_type *slldata;
  float *fdata;
  double *ddata;

  int i = 0;
  size_t elsize = 0;

  switch (type) {
  case DATAUC: /* unsigned char */
    ucdata = (unsigned char *)calloc(nelem, 1);
    elsize = sizeof(char);
    while (i < nelem) {
      ucdata[i] = urand() * UCHAR_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = ucdata;
    break;
  case DATASC: /* signed char */
    scdata = (signed char *)calloc(nelem, 1);
    elsize = sizeof(char);
    while (i < nelem) {
      scdata[i] = urand() * SCHAR_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = scdata;
    break;
  case DATAUS: /* unsigned short */
    usdata = (unsigned short *)calloc(nelem, sizeof(short));
    elsize = sizeof(short);
    while (i < nelem) {
      usdata[i] = urand() * USHRT_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = usdata;
    break;
  case DATASS: /* signed short */
    ssdata = (signed short *)calloc(nelem, sizeof(short));
    elsize = sizeof(short);
    while (i < nelem) {
      ssdata[i] = urand() * SHRT_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = ssdata;
    break;
  case DATAUI: /* unsigned int */
    uidata = (unsigned int *)calloc(nelem, sizeof(int));
    elsize = sizeof(int);
    while (i < nelem) {
      uidata[i] = urand() * UINT_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = uidata;
    break;
  case DATASI: /* signed int */
    sidata = (signed int *)calloc(nelem, sizeof(int));
    elsize = sizeof(int);
    while (i < nelem) {
      sidata[i] = urand() * INT_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = sidata;
    break;
  case DATAUL: /* unsigned long */
    uldata = (unsigned long *)calloc(nelem, sizeof(long));
    elsize = sizeof(long);
    while (i < nelem) {
      uldata[i] = urand() * ULONG_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = uldata;
    break;
  case DATASL: /* signed long */
    sldata = (signed long *)calloc(nelem, sizeof(long));
    elsize = sizeof(long);
    while (i < nelem) {
      sldata[i] = urand() * LONG_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = sldata;
    break;
#ifdef CBF_USE_LONG_LONG
  case DATAULL: /* unsigned long long */
    ulldata = (unsigned long long *)calloc(nelem, sizeof(long long));
    elsize = sizeof(long long);
    while (i < nelem) {
      ulldata[i] = urand() * ULLONG_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = ulldata;
    break;
  case DATASLL: /* signed long long */
    slldata = (signed long long *)calloc(nelem, sizeof(long long));
    elsize = sizeof(long long);
    while (i < nelem) {
      slldata[i] = urand() * LLONG_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = slldata;
    break;
#else
      case DATAULL: /* unsigned long long as an array */
          ulldata = (CBF_ull_type *)calloc(nelem, sizeof(CBF_ull_type));
          elsize = sizeof(CBF_ull_type);
          while (i < nelem) {
              ulldata[i].el0 = urand() * INT_MAX;
              ulldata[i].el1 = urand() * INT_MAX;
#if CBF_ULL_INTS == 4
              ulldata[i].el2 = urand() * INT_MAX;
              ulldata[i].el3 = urand() * INT_MAX;
#endif
              i += 1 + urand() * NSTEPS;
          }
          *data = ulldata;
          break;
      case DATASLL: /* signed long long */
          slldata = (CBF_sll_type *)calloc(nelem, sizeof(CBF_sll_type));
          elsize = sizeof(CBF_sll_type);
          while (i < nelem) {
              slldata[i].el0 = urand() * INT_MAX;
              slldata[i].el1 = urand() * INT_MAX;
#if CBF_ULL_INTS == 4
              slldata[i].el2 = urand() * INT_MAX;
              slldata[i].el3 = urand() * INT_MAX;
#endif
              i += 1 + urand() * NSTEPS;
          }
          *data = slldata;
          break;          
#endif
  case DATAF: /* float */
    fdata = (float *)calloc(nelem, sizeof(float));
    elsize = sizeof(float);
    while (i < nelem) {
      fdata[i] = urand() * INT_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = fdata;
    break;
  case DATAD: /* double */
    ddata = (double *)calloc(nelem, sizeof(double));
    elsize = sizeof(double);
    while (i < nelem) {
      ddata[i] = urand() * INT_MAX;
      i += 1 + urand() * NSTEPS;
    }
    *data = ddata;
    break;
  default:
    fprintf(stderr, "Unknown type number\n");
  }

  return elsize;
}

int createtestfile(const char *fn, int isreal, int rows, int columns,
    int compression, void *data, size_t elsize, int elsigned)
{
    int status;
    cbf_handle ch;
    FILE *f;
    
    
    status = cbf_make_handle(&ch);
    if (status) printf("make_handle (%d)\n", status);
    
    status = cbf_new_datablock (ch, "image_1");
    if (status) printf("new_datablock (%d)\n", status);
    
    
    status = cbf_new_category(ch, "array_data");
    if (status) printf("new_category (%d)\n", status);
    status = cbf_new_column(ch, "array_id");
    if (status) printf("new_column (%d)\n", status);
    status = cbf_set_value(ch, "image_1");
    if (status) printf("set_value (%d)\n", status);
    status = cbf_new_column(ch, "binary_id");
    if (status) printf("new_column (%d)\n", status);
    status = cbf_set_integervalue(ch, 1);
    if (status) printf("set_integervalue (%d)\n", status);
    status = cbf_new_column(ch, "data");
    if (status) printf("new_column (%d)\n", status);
    
    if (isreal) {
        status = cbf_set_realarray_wdims(ch, compression, 1, data, elsize,
                                         rows * columns, "little_endian", columns, rows, 0, 0);
        if (status) printf("set_realarray_wdims (%d)\n", status);
    } else {
        status = cbf_set_integerarray_wdims(ch, compression, 1, data, elsize,
                                            elsigned, rows * columns, "little_endian", columns, rows, 0, 0);
        if (status) printf("set_integerarray_wdims (%d)\n", status);
    }
    if (status) return status;
    
    f = fopen(fn, "wb");
    status = cbf_write_widefile(ch, f, 1, CBF, MIME_HEADERS | MSG_DIGEST, 0);
    if (status) printf("write_widefile (%d)\n", status);
    status = cbf_free_handle(ch);
    if (status) printf("free_handle (%d)\n", status);
    
    return status;
}

size_t readtestimage(const char *fn, void **data, size_t *size, int *sign)
{
    cbf_handle ch;
    int status;
    FILE *f;
    unsigned int cifcomp = 0;
    int bid = 0, els = 0, elu = 0;
    int minel = 0, maxel = 0, isre = 0;
    size_t elsize = 0, elnum = 0;
    size_t dim1 = 0, dim2 = 0, dim3 = 0, pad = 0;
    const char *byteorder = NULL;
    int isreal;
    int id;
    size_t rsize = 0;
    
    
    
    status = cbf_make_handle(&ch);
    if (status) printf("make_handle (%d)\n", status);
    
    f = fopen(fn, "rb");
    status = cbf_read_widefile(ch, f, MSG_DIGEST);
    if (status) printf("read_widefile (%d)\n", status);
    
    status = cbf_rewind_datablock(ch);
    if (status) printf("rewind_db (%d)\n", status);
    
    status = cbf_find_category(ch, "array_data");
    if (status) printf("find_cat (%d)\n", status);
    status = cbf_find_tag(ch, "_array_data.data");
    if (status) printf("find_tag (%d)\n", status);
    status = cbf_rewind_row(ch);
    if (status) printf("rewind_row (%d)\n", status);
    
    
    status = cbf_get_arrayparameters_wdims(ch, &cifcomp, &bid, &elsize,
                                           &els, &elu, &elnum,
                                           &minel, &maxel, &isre,
                                           &byteorder, &dim1, &dim2, &dim3, &pad);
    if (status) printf("get_aparams (%d) = %d, %d, %ld, %d, %d,\n",
                       status, cifcomp, bid, (long)elsize, els, elu);
    if (status) printf(" %ld, %d, %d, %d, %s,\n", (long)elnum, minel, maxel, isre,
                       (byteorder == NULL) ? "null" : byteorder);
    if (status) printf(" %ld, %ld, %ld, %ld\n", (long)dim1, (long)dim2, (long)dim3, (long)pad);
    
    isreal = (isre == 1);
    
    if (isreal) {
        if (elsize == sizeof(float) || elsize == sizeof(double)) {
            void *rdata = malloc(elsize * elnum);
            status = cbf_get_realarray(ch, &id, rdata, elsize, elnum, &rsize);
            if (status) printf("get_realarray (%d)\n", status);
            *data = rdata;
        } else {
            fprintf(stderr, "Size of element (%ld) does not match any real types\n", (long)elsize);
        }
    } else {
        if (els && elu) {
            fprintf(stderr, "Both signed and unsigned flags have been set!\n");
            return rsize;
        }
        if (elsize == sizeof(char) || elsize == sizeof(short) ||
            elsize == sizeof(int) || elsize == sizeof(long) 
#ifdef CBF_USE_LONG_LONG
            || elsize == sizeof(long long)
#else
            || elsize == sizeof(CBF_sll_type)
#endif
            ) {
            void *idata = malloc(elsize * elnum);
            status = cbf_get_integerarray(ch, &id, idata, elsize, els, elnum, &rsize);
            if (status) printf("get_integerarray (%d)\n", status);
            *data = idata;
        } else {
            fprintf(stderr, "Size of element (%ld) does not match any integer types\n", (long)elsize);
        }
    }
    
    status = cbf_free_handle(ch);
    if (status) printf("free_handle (%d)\n", status);
    
    *sign = els;
    *size = elsize;
    
    if (rsize != elnum) printf("Read %ld elements\n", (long)rsize);
    return rsize;
}

void checkdata(int type, int nelem, void *data, void *idata)
{
  unsigned char *ucdata, *ucidata;
  signed char *scdata, *scidata;
  unsigned short *usdata, *usidata;
  signed short *ssdata, *ssidata;
  unsigned int *uidata, *uiidata;
  signed int *sidata, *siidata;
  unsigned long *uldata, *ulidata;
  signed long *sldata, *slidata;
#ifdef CBF_USE_LONG_LONG
  unsigned long long *ulldata, *ullidata;
  signed long long *slldata, *sllidata;
#else
    CBF_ull_type *ulldata, *ullidata;
    CBF_sll_type *slldata, *sllidata;    
#endif
  float *fdata, *fidata;
  double *ddata, *didata;

  int i = 0;

  switch (type) {
  case DATAUC: /* unsigned char */
    ucdata = (unsigned char *)data;
    ucidata = (unsigned char *)idata;
    while (i < nelem) {
      if (ucdata[i] != ucidata[i]) {
        fprintf(stderr, "UC element %d did not match (%d != %d)\n", i, ucdata[i], ucidata[i]);
        fprintf(stderr, "Previous UC elements are    (%x    %x)\n", ucdata[i-1], ucidata[i-1]);
        fprintf(stderr, "Previous UC elements are   ~(%x    %x)\n", ~ucdata[i-1], ~ucidata[i-1]);
        return;
      }
      i++;
    }
    break;
  case DATASC: /* signed char */
    scdata = (signed char *)data;
    scidata = (signed char *)idata;
    while (i < nelem) {
      if (scdata[i] != scidata[i]) {
        fprintf(stderr, "SC element %d did not match (%d != %d)\n", i, scdata[i], scidata[i]);
        fprintf(stderr, "Previous SC elements are    (%x    %x)\n", scdata[i-1], scidata[i-1]);
        fprintf(stderr, "Previous SC elements are   ~(%x    %x)\n", ~scdata[i-1], ~scidata[i-1]);
        return;
      }
      i++;
    }
    break;
  case DATAUS: /* unsigned short */
    usdata = (unsigned short *)data;
    usidata = (unsigned short *)idata;
    while (i < nelem) {
      if (usdata[i] != usidata[i]) {
        fprintf(stderr, "US element %d did not match (%d != %d)\n", i, usdata[i], usidata[i]);
        return;
      }
      i++;
    }
    break;
  case DATASS: /* signed short */
    ssdata = (signed short *)data;
    ssidata = (signed short *)idata;
    while (i < nelem) {
      if (ssdata[i] != ssidata[i]) {
        fprintf(stderr, "SS element %d did not match (%d != %d)\n", i, ssdata[i], ssidata[i]);
        return;
      }
      i++;
    }
    break;
  case DATAUI: /* unsigned int */
    uidata = (unsigned int *)data;
    uiidata = (unsigned int *)idata;
    while (i < nelem) {
      if (uidata[i] != uiidata[i]) {
        fprintf(stderr, "UI element %d did not match (%d != %d)\n", i, uidata[i], uiidata[i]);
        return;
      }
      i++;
    }
    break;
  case DATASI: /* signed int */
    sidata = (signed int *)data;
    siidata = (signed int *)idata;
    while (i < nelem) {
      if (sidata[i] != siidata[i]) {
        fprintf(stderr, "SI element %d did not match (%d != %d)\n", i, sidata[i], siidata[i]);
        return;
      }
      i++;
    }
    break;
  case DATAUL: /* unsigned long */
    uldata = (unsigned long *)data;
    ulidata = (unsigned long *)idata;
    while (i < nelem) {
      if (uldata[i] != ulidata[i]) {
        fprintf(stderr, "UL element %d did not match (%ld != %ld)\n", i, uldata[i], ulidata[i]);
        return;
      }
      i++;
    }
    break;
  case DATASL: /* signed long */
    sldata = (signed long *)data;
    slidata = (signed long *)idata;
    while (i < nelem) {
      if (sldata[i] != slidata[i]) {
        fprintf(stderr, "SL element %d did not match (%ld != %ld)\n", i, sldata[i], slidata[i]);
        return;
      }
      i++;
    }
    break;
#ifdef CBF_USE_LONG_LONG
  case DATAULL: /* unsigned long long */
    ulldata = (unsigned long long *)data;
    ullidata = (unsigned long long *)idata;
    while (i < nelem) {
      if (ulldata[i] != ullidata[i]) {
        fprintf(stderr, "ULL element %d did not match (%lld != %lld)\n", i, ulldata[i], ullidata[i]);
        return;
      }
      i++;
    }
    break;
  case DATASLL: /* signed long long */
    slldata = (signed long long *)data;
    sllidata = (signed long long *)idata;
    while (i < nelem) {
      if (slldata[i] != sllidata[i]) {
        fprintf(stderr, "SLL element %d did not match (%lld != %lld)\n", i, slldata[i], sllidata[i]);
        return;
      }
      i++;
    }
    break;
#else
      case DATAULL: /* unsigned long long */
          ulldata = (CBF_ull_type *)data;
          ullidata = (CBF_ull_type *)idata;
          while (i < nelem) {
#if CBF_ULL_INTS == 2
              if (ulldata[i].el0 != ullidata[i].el0
                  || ulldata[i].el1 != ullidata[i].el1) {
                  fprintf(stderr, "ULL element %d did not match (%x %x != %x %x)\n",
                          i, ulldata[i].el1,ulldata[i].el0,
                             ullidata[i].el1,ullidata[i].el0);
                  return;
              }
#else
              if (ulldata[i].el0 != ullidata[i].el0
                  || ulldata[i].el1 != ullidata[i].el1
                  || ulldata[i].el2 != ullidata[i].el2
                  || ulldata[i].el3 != ullidata[i].el3) {
                  fprintf(stderr, "ULL element %d did not match (%x %x %x %x != %x %x %x %x)\n",
                          i, ulldata[i].el3,ulldata[i].el2,ulldata[i].el1,ulldata[i].el0,
                          ullidata[i].el3,ullidata[i].el2,ullidata[i].el1,ullidata[i].el0);
                  return;
              }
#endif
              i++;
          }
          break;
      case DATASLL: /* signed long long */
          slldata = (CBF_sll_type *)data;
          sllidata = (CBF_sll_type *)idata;
          while (i < nelem) {
#if CBF_SLL_INTS == 2
              if (slldata[i].el0 != sllidata[i].el0
                  || slldata[i].el1 != sllidata[i].el1) {
                  fprintf(stderr, "SLL element %d did not match (%x %x != %x %x)\n",
                          i, slldata[i].el1,slldata[i].el0,
                          sllidata[i].el1,sllidata[i].el0);
                  return;
              }
#else
              if (slldata[i].el0 != sllidata[i].el0
                  || slldata[i].el1 != sllidata[i].el1
                  || slldata[i].el2 != sllidata[i].el2
                  || slldata[i].el3 != sllidata[i].el3) {
                  fprintf(stderr, "ULL element %d did not match (%x %x %x %x != %x %x %x %x)\n",
                          i, ulldata[i].el3,ulldata[i].el2,ulldata[i].el1,ulldata[i].el0,
                          ullidata[i].el3,ullidata[i].el2,ullidata[i].el1,ullidata[i].el0);
                  return;
              }
#endif
              i++;
          }
          break;          
#endif
  case DATAF: /* float */
    fdata = (float *)data;
    fidata = (float *)idata;
    while (i < nelem) {
      if (fabs(fdata[i] - fidata[i]) > FLT_MIN) {
        fprintf(stderr, "F element %d did not match (%g != %g)\n", i, fdata[i], fidata[i]);
        return;
      }
      i++;
    }
    break;
  case DATAD: /* double */
    ddata = (double *)data;
    didata = (double *)idata;
    while (i < nelem) {
      if (fabs(ddata[i] - didata[i]) > DBL_MIN) {
        fprintf(stderr, "D element %d did not match (%g != %g)\n", i, ddata[i], didata[i]);
        return;
      }
      i++;
    }
    break;
  }
}

void testinteger(const char *fn, int rows, int cols, int type, int comp)
{
  void *data, *idata;
  size_t elsize;
  size_t nelem = rows*cols;
  size_t isize;
  int isign;

  elsize = createtestimage(&data, type, nelem);
  if (elsize == 0) {
    fprintf(stderr, "Could not create test image\n");
    return;
  }

  if (createtestfile(fn, 0, rows, cols, comp, data, elsize, type & 1) != 0) {
    fprintf(stderr, "Could not create test file\n");
    return;
  }
    
  if (readtestimage(fn, &idata, &isize, &isign) != nelem) {
    fprintf(stderr, "Did not read %ld elements\n", (long) nelem);
  }
  if (isize != elsize)
    fprintf(stderr, "Size of elements does not match (%ld != %ld)\n", (long)elsize, (long)isize);

  if (isign != (type & 1))
    fprintf(stderr, "Sign of elements does not match (%d != %d)\n", type & 1, isign);

  checkdata(type, nelem, data, idata);
  free(data);
  free(idata);
}

void testreal(const char *fn, int rows, int cols, int type, int comp)
{
  void *data, *idata;
  size_t elsize;
  size_t nelem = rows*cols;
  size_t isize;
  int isign;

  elsize = createtestimage(&data, type, nelem);
  if (elsize == 0) {
    fprintf(stderr, "Could not create test image\n");
    return;
  }

  if (createtestfile(fn, 1, rows, cols, comp, data, elsize, 1) != 0) {
    fprintf(stderr, "Could not create test file\n");
    return;
  }
  if (readtestimage(fn, &idata, &isize, &isign) != nelem) {
    fprintf(stderr, "Did not read %ld elements\n", (long) nelem);
  }
  if (isize != elsize)
    fprintf(stderr, "Size of elements does not match (%ld != %ld)\n", (long)elsize, (long)isize);

  checkdata(type, nelem, data, idata);
  free(data);
  free(idata);
}

void testall(const char *fn)
{
  int rows = 512;
  int cols = 10;
  int c, t;
  int comp[] = { CBF_NONE,
      /* CBF_PREDICTOR, not implemented! */
      CBF_BYTE_OFFSET,
      CBF_PACKED_V2,
      CBF_CANONICAL
  };

    char * compstr[] = { "CBF_NONE", 
        /* "CBF_PREDICTOR", */
        "CBF_BYTE_OFFSET",
        "CBF_PACKED_V2",
        "CBF_CANONICAL"
    };
    
    char * datastr[] = { "unsigned char",
    "signed char", "unsigned short", "signed short",
    "unsigned int", "signed int", "unsigned long",
    "signed long", 
#ifdef CBF_USE_LONG_LONG
    "unsigned long long", "signed long long",
#else
    "CBF_ull_type", "CBF_sll_type",
#endif
    "float", "double" 
    };
    

  for (c = 0; c < 4; c++) {
    printf("Testing compression scheme %d, %d %s\n", c, comp[c], compstr[c]);
    for (t = 0; t < DATAF; t++) {
      printf(" with data type %d, %s\n", t, datastr[t]);
      testinteger(fn, rows, cols, t, comp[c]);
    }
    printf(" with data type %d, %s\n", DATAF, datastr[DATAF]);
    testreal(fn, rows, cols, DATAF, comp[c]);
    printf(" with data type %d, %s\n", DATAD, datastr[DATAD]);
    testreal(fn, rows, cols, DATAD, comp[c]);
  }
}

int main(int argc, char **argv)
{
  char *fn;

  if (argc < 2)
    fn = "CTC.cbf";
  else
    fn = argv[1];

  printf("Saving to %s\n", fn);
  testall(fn);
  return 0;
}
