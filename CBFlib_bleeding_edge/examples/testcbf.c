#include "cbf.h"
#include "cbf_tree.h"

int main(int argc, char *argv [])
{
    FILE *f;
    cbf_handle ch;
    int status;
    unsigned int m;
    const char *dbname;
    const char *arrayid;
    const char *s;
    int i;
    CBF_NODETYPE ntype;

    if (argc < 2)
      f = fopen("examples/template_pilatus6m_2463x2527.cbf", "rb");
    else
      f = fopen(argv[1], "rb");

    cbf_make_handle(&ch);

    status = cbf_read_widefile(ch, f, MSG_DIGEST);
    printf("read_widefile (%d)\n", status);

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

    int d;
    int dims[2];

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

    size_t dsize = dims[0]*dims[1];
    size_t rsize;

    /* Attempt to load data */
    status = cbf_find_category(ch, "array_data");
    printf("find_cat (%d)\n", status);
    status = cbf_find_column(ch, "array_id");
    printf("find_col (%d)\n", status);
    status = cbf_find_row(ch, arrayid);
    printf("find_row (%d)\n", status);
    status = cbf_find_column(ch, "data");
    printf("find_col (%d)\n", status);

    unsigned int cifcomp = 0;
    int bid = 0, els = 0, elu = 0;
    int minel = 0, maxel = 0, isre = 0;
    size_t elsize = 0, elnum = 0;
    size_t dim1 = 0, dim2 = 0, dim3 = 0, pad = 0;
    const char *byteorder = NULL;

    status = cbf_get_arrayparameters_wdims(ch, &cifcomp, &bid, &elsize,
					       &els, &elu, &elnum,
					       &minel, &maxel, &isre,
					       &byteorder, &dim1, &dim2, &dim3, &pad);
    printf("get_aparams (%d) = %d, %d, %d, %d, %d,\n", status, cifcomp, bid, elsize, els, elu);
    printf(" %d, %d, %d, %d, %s,\n", elnum, minel, maxel, isre, (byteorder == NULL) ? "null" : byteorder);
    printf(" %d, %d, %d, %d\n", dim1, dim2, dim3, pad);

    int isreal = (isre == 1);

    if (isreal) {
        double *ddata;

        ddata = (double *) malloc(sizeof(double)*dsize);
        status = cbf_get_realarray(ch, &i, (void *)ddata,
				   sizeof(double), dsize, &rsize);
        if (status == 0)
	    printf("get_dblarray (%d) %d/%d\n", status, rsize, dsize);
    } else {
        int *idata;

        idata = (int *) malloc(sizeof(int)*dsize);
        status = cbf_get_integerarray(ch, &i, (void *)idata,
				      sizeof(int), els, dsize, &rsize);
        if (status == 0)
	    printf("get_intarray (%d) %d/%d\n", status, rsize, dsize);
    }

    /*  fclose(f); *//* let cbflib handle the closing of a file */
    return 0;
}
