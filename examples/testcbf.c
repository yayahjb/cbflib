#include "cbf.h"

int main() {
	FILE *f;
	cbf_handle ch;
	int status;
	unsigned int m;

	f = fopen("examples/template_pilatus6m_2463x2527.cbf", "rb");

	cbf_make_handle(&ch);

	status = cbf_read_widefile(ch, f, MSG_DIGEST);
	printf("read_widefile (%d)\n", status);

	status = cbf_count_datablocks(ch, &m);
	printf("count_dbs (%d) = %d\n", status, m);

	/*  fclose(f);*/ /* let cbflib handle the closing of a file */
	return 0;
}
