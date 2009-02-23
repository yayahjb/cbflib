import org.iucr.cbflib.*;

// javac -cp cbflib-0.8.0.jar testcbf.java
// LD_LIBRARY_PATH=solib java -cp cbflib-0.8.0.jar:. testcbf
public class testcbf {

    static {
	System.loadLibrary("cbf_wrap");
    }

    public static void main(String[]args) {
	SWIGTYPE_p_FILE f = null;

	if (args.length < 1) {
	    f = cbf.fopen("examples/template_pilatus6m_2463x2527.cbf", "rb");
	} else {
	    f = cbf.fopen(args[0], "rb");
	}

	cbf_handle_struct chs = new cbf_handle_struct();
	int status = 0;
	status = cbf.cbf_read_widefile(chs, f, cbfConstants.MSG_DIGEST);
	System.out.println("read_widefile (" + status + ")");

	status = cbf.cbf_rewind_datablock(chs);
	System.out.println("rewind_db (" + status + ")");

	uintP mp = new uintP();

	status = cbf.cbf_count_datablocks(chs, mp.cast());
	System.out.println("count_dbs (" + status + ") = " + mp.value());

	SWIGTYPE_p_p_char dbnamep = cbf.new_charPP();
	status = cbf.cbf_datablock_name(chs, dbnamep);
	System.out.println("db_name (" + status + ") = " + cbf.charPP_value(dbnamep));

	status = cbf.cbf_find_category(chs, "diffrn_data_frame");
	System.out.println("find_cat (" + status + ")");

	SWIGTYPE_p_p_char s = cbf.new_charPP();
	intP ip = new intP();

	status = cbf.cbf_find_column(chs, "array_id");
	System.out.println("find_col (" + status + ")");
	status = cbf.cbf_get_value(chs, s);
	String arrayid = cbf.charPP_value(s);
	System.out.println("get_val (" + status + ") = " + arrayid);

	status = cbf.cbf_find_category(chs, "array_structure_list");
	System.out.println("find_cat (" + status + ")");
	status = cbf.cbf_rewind_row(chs);
	System.out.println("rewind_row (" + status + ")");

	status = cbf.cbf_find_column(chs, "array_id");
	System.out.println("find_col (" + status + ")");

	int d;
	int[] dims = new int[2];

	// Attempt to find rows that matches above array_id
	while ((status = cbf.cbf_find_nextrow(chs, arrayid)) == 0) {
	    System.out.println("find_nrow (" + status + ")");

	    status = cbf.cbf_find_column(chs, "index");
	    System.out.println("find_col (" + status + ")");
	    status = cbf.cbf_get_integervalue(chs, ip.cast());
	    System.out.println("get_int (" + status + ") = " + ip.value());
	    d = ip.value() - 1;

	    status = cbf.cbf_find_column(chs, "dimension");
	    System.out.println("find_col (" + status + ")");
	    status = cbf.cbf_get_integervalue(chs, ip.cast());
	    System.out.println("get_int (" + status + ") = " + ip.value());
	    dims[d] = ip.value();

	    status = cbf.cbf_find_column(chs, "precedence");
	    System.out.println("find_col (" + status + ")");
	    status = cbf.cbf_get_integervalue(chs, ip.cast());
	    System.out.println("get_int (" + status + ") = " + ip.value());

	    status = cbf.cbf_find_column(chs, "direction");
	    System.out.println("find_col (" + status + ")");
	    status = cbf.cbf_get_value(chs, s);
	    System.out.println("get_val (" + status + ") = " + cbf.charPP_value(s));

	    status = cbf.cbf_find_column(chs, "axis_set_id");
	    System.out.println("find_col (" + status + ")");
	    status = cbf.cbf_get_value(chs, s);
	    System.out.println("get_val (" + status + ") = " + cbf.charPP_value(s));

	    status = cbf.cbf_find_column(chs, "array_id");
	    System.out.println("find_col (" + status + ")");
	}
	System.out.println("find_nrow (" + status + ")");

	System.out.println("Dimensions: " + dims[0] + "x" + dims[1]);

	int dsize = dims[0]*dims[1];
	sizetP rsize = new sizetP();
	intArray idata = new intArray(dsize);

	// Attempt to load data
	status = cbf.cbf_find_category(chs, "array_data");
	System.out.println("find_cat (" + status + ")");
	status = cbf.cbf_find_column(chs, "array_id");
	System.out.println("find_col (" + status + ")");
	status = cbf.cbf_find_row(chs, arrayid);
	System.out.println("find_row (" + status + ")");
	status = cbf.cbf_find_column(chs, "data");
	System.out.println("find_col (" + status + ")");
	status = cbf.cbf_get_integerarray (chs, ip.cast(), cbf.int_void(idata.cast()), 4, 1, dsize, rsize.cast());
	if (status == 0)
	    System.out.println("get_intarray (" + status + ") " + rsize.value() + "/" + dsize);

    }
}
