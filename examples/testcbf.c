
#include <stdio.h>
#include <sys/time.h>

#include "cbf.h"
#include "cbf_tree.h"

void readheader(cbf_handle ch, int *dims)
{
    int status;
    const char *arrayid;
    const char *s;
    int i, d;

    status = cbf_find_column(ch, "array_id");
    printf("find_col (%d)\n", status);

    status = cbf_get_value(ch, &arrayid);
    printf("get_val (%d) = %s\n", status, arrayid);

    status = cbf_find_category(ch, "array_structure_list");
    printf("find_cat (%d)\n", status);

    status = cbf_rewind_row(ch);
    printf("rewind_row (%d)\n", status);

    status = cbf_find_column(ch, "array_id");
    printf("find_col (%d)\n", status);

    /* Attempt to find rows that matches above array_id */
    while ((status = cbf_find_nextrow(ch, arrayid)) == 0) {
        printf("find_nrow (%d)\n", status);

        status = cbf_find_column(ch, "index");
        printf("find_col (%d)\n", status);
        status = cbf_get_integervalue(ch, &i);
        printf("get_int (%d) = %d\n", status, i);
        d = i - 1;

        status = cbf_find_column(ch, "dimension");
        printf("find_col (%d)\n", status);
        status = cbf_get_integervalue(ch, &i);
        printf("get_int (%d) = %d\n", status, i);
        dims[d] = i;

        status = cbf_find_column(ch, "precedence");
        printf("find_col (%d)\n", status);
        status = cbf_get_integervalue(ch, &i);
        printf("get_int (%d) = %d\n", status, i);

        status = cbf_find_column(ch, "direction");
        printf("find_col (%d)\n", status);
        status = cbf_get_value(ch, &s);
        printf("get_val (%d) = %s\n", status, s);

        status = cbf_find_column(ch, "axis_set_id");
        printf("find_col (%d)\n", status);
        status = cbf_get_value(ch, &s);
        printf("get_val (%d) = %s\n", status, s);

        status = cbf_find_column(ch, "array_id");
        printf("find_col (%d)\n", status);
    }
    printf("find_nrow (%d)\n", status);

    printf("Dimensions: %dx%d\n", dims[0], dims[1]);

    /* Attempt to load data */
    status = cbf_find_category(ch, "array_data");
    printf("find_cat (%d)\n", status);
    status = cbf_find_column(ch, "array_id");
    printf("find_col (%d)\n", status);
    status = cbf_find_row(ch, arrayid);
    printf("find_row (%d)\n", status);
    status = cbf_find_column(ch, "data");
    printf("find_col (%d)\n", status);
}

/*
 * return time taken in milliseconds
 */
int loadfile(const char* name)
{
    FILE *f;
    cbf_handle ch;
    int status;
    unsigned int m;
    const char *dbname;
    int i;
    CBF_NODETYPE ntype;
    struct timeval stv, dtv, ftv;
    int dims[2];
    int fullheader = 0;

    size_t dsize;
    size_t rsize;

    unsigned int cifcomp = 0;
    int bid = 0, els = 0, elu = 0;
    int minel = 0, maxel = 0, isre = 0;
    size_t elsize = 0, elnum = 0;
    size_t dim1 = 0, dim2 = 0, dim3 = 0, pad = 0;
    const char *byteorder = NULL;

    int isreal;
    long dtime;
    long ttime;


    gettimeofday(&stv, NULL);
    f = fopen(name, "rb");

    cbf_make_handle(&ch);

#if 0
    {
      long flen;
      char *buffer;

      fseek(f, 0, SEEK_END);
      flen = ftell(f);
      rewind(f);

      buffer = malloc(flen);
      fread(buffer, 1, flen, f);
      fclose(f);
      status = cbf_read_buffered_file(ch, NULL, MSG_DIGEST|CBF_PARSE_WIDE, buffer, flen);
/*      printf("read_buffered_file(%d)\n", status); */
      free(buffer);
    }
#else
    rewind(f);

    status = cbf_read_widefile(ch, f, MSG_DIGEST);
/*    printf("read_widefile (%d)\n", status); */
#endif

    status = cbf_rewind_datablock(ch);
    printf("rewind_db (%d)\n", status);

    status = cbf_count_datablocks(ch, &m);
    printf("count_dbs (%d) = %d\n", status, m);

    status = cbf_datablock_name(ch, &dbname);
    printf("db_name (%d) = %s\n", status, dbname);

    status = cbf_rewind_blockitem(ch, &ntype);
    printf("rewind_bi (%d) = %d\n", status, ntype);

    status = cbf_find_category(ch, "diffrn_data_frame");
    printf("find_cat (%d)\n", status);
    if (status != 0) {
        status = cbf_find_category(ch, "diffrn_frame_data");
        printf("find_cat (%d)\n", status);
    }

    if (status == 0) {
        readheader(ch, dims);
        fullheader = 1;
    } else {
        printf("Mini CBF!\n");
        status = cbf_find_category(ch, "array_data");
        printf("find_cat (%d)\n", status);
        status = cbf_find_column(ch, "data");
        printf("find_col (%d)\n", status);
    }

    dsize = dims[0]*dims[1];

    status = cbf_get_arrayparameters_wdims(ch, &cifcomp, &bid, &elsize,
					       &els, &elu, &elnum,
					       &minel, &maxel, &isre,
					       &byteorder, &dim1, &dim2, &dim3, &pad);
    printf("get_aparams (%d) = %d, %d, %ld, %d, %d,\n", status, cifcomp, bid, (long) elsize, els, elu);
    printf(" %ld, %d, %d, %d, %s,\n", (long) elnum, minel, maxel, isre, (byteorder == NULL) ? "null" : byteorder);
    printf(" %ld, %ld, %ld, %ld\n", (long) dim1, (long) dim2, (long) dim3, (long) pad);
    if (fullheader && dsize != elnum) {
        printf("Mismatch of CBF binary data size: %ld != %ld\n", (long) dsize, (long) elnum);
        return 0;
    }
    dsize = elnum;

    isreal = (isre == 1);

    gettimeofday(&dtv, NULL);
    if (isreal) {
        double *ddata;

        ddata = (double *) malloc(sizeof(double)*dsize);
        status = cbf_get_realarray(ch, &i, (void *)ddata,
                   sizeof(double), dsize, &rsize);
        if (status == 0) {
            printf("get_dblarray (%d) %ld/%ld\n", status, (long) rsize, (long) dsize);
            double *darray = (double *) malloc(sizeof(double)*dsize);
            for (i=0; i<dsize; i++)
                darray[i] = ddata[i];
            printf("Sample is %g\n", darray[42]);
            free(darray);
        }
        free(ddata);
    } else {
        int *idata;

        idata = (int *) malloc(sizeof(int)*dsize);
        status = cbf_get_integerarray(ch, &i, (void *)idata,
                      sizeof(int), els, dsize, &rsize);
        if (status == 0) {
            printf("get_intarray (%d) %ld/%ld\n", status, (long) rsize, (long) dsize);
            int *iarray = (int *) malloc(sizeof(int)*dsize);
            for (i=0; i<dsize; i++)
                iarray[i] = idata[i];
            printf("Sample is %d\n", iarray[42]);
            free(iarray);
        }
        free(idata);
    }

    cbf_free_handle(ch); /* let cbflib handle the closing of a file */

    gettimeofday(&ftv, NULL);
    dtime = (ftv.tv_sec - dtv.tv_sec)*1000 + (ftv.tv_usec - dtv.tv_usec)/1000;
    ttime = (ftv.tv_sec - stv.tv_sec)*1000 + (ftv.tv_usec - stv.tv_usec)/1000;
    fprintf(stderr, "Time: %dms (%dms)\n", (int) ttime, (int) dtime);
    return (int) ttime;
}

int main(int argc, char *argv [])
{
    char *name;
    int n;
    int t = 0;

    if (argc < 2)
        name = "examples/template_pilatus6m_2463x2527.cbf";
    else
        name = argv[1];

    loadfile(name);

    for (n = 0; n < 10; n++)
        t += loadfile(name);
    fprintf(stderr, "Ave: %dms\n", t/10);
    return 0;
}
