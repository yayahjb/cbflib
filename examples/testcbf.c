#include "cbf.h"

int main()
{
    FILE *f;
    cbf_handle ch;
    int status;
    unsigned int m;
    const char *dbname;
    const char *arrayid;
    const char *s;
    int i;

    f = fopen("examples/template_pilatus6m_2463x2527.cbf", "rb");

    cbf_make_handle(&ch);

    status = cbf_read_widefile(ch, f, MSG_DIGEST);
    printf("read_widefile (%d)\n", status);

    status = cbf_rewind_datablock(ch);
    printf("rewind_db (%d)\n", status);

    status = cbf_count_datablocks(ch, &m);
    printf("count_dbs (%d) = %d\n", status, m);

    status = cbf_datablock_name(ch, &dbname);
    printf("db_name (%d) = %s\n", status, dbname);

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

    /* Attempt to find rows that matches above array_id */
    while ((status = cbf_find_nextrow(ch, arrayid)) == 0) {
	printf("find_nrow (%d)\n", status);

	status = cbf_find_column(ch, "index");
	printf("find_col (%d)\n", status);
	status = cbf_get_integervalue(ch, &i);
	printf("get_int (%d) = %d\n", status, i);

	status = cbf_find_column(ch, "dimension");
	printf("find_col (%d)\n", status);
	status = cbf_get_integervalue(ch, &i);
	printf("get_int (%d) = %d\n", status, i);

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

    /*  fclose(f); *//* let cbflib handle the closing of a file */
    return 0;
}
