

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import org.iucr.cbflib.*;

// javac -cp cbflib-0.8.0.jar testcbf.java
// LD_LIBRARY_PATH=solib java -cp cbflib-0.8.0.jar:. testcbf
/**
 *
 */
public class testcbf {

	static {
		try {
			System.loadLibrary("cbf");
		} catch (UnsatisfiedLinkError e) {
			System.err.println("Could not load CBF library");
		}
		try {
			System.loadLibrary("cbf_wrap");
		} catch (UnsatisfiedLinkError e) {
			System.err.println("Could not load CBF wrapper");
		}
	}

	/**
	 * @param chs
	 * @param dims
	 */
	private static void readheader(cbf_handle_struct chs, int[] dims) {
		int status = 0;
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

		// Attempt to load data
		status = cbf.cbf_find_category(chs, "array_data");
		System.out.println("find_cat (" + status + ")");
		status = cbf.cbf_find_column(chs, "array_id");
		System.out.println("find_col (" + status + ")");
		status = cbf.cbf_find_row(chs, arrayid);
		System.out.println("find_row (" + status + ")");
		status = cbf.cbf_find_column(chs, "data");
		System.out.println("find_col (" + status + ")");
	}

	/**
	 * @param name
	 * @return time in milliseconds
	 */
	public static int loadFile(String name) {
		long start = -System.currentTimeMillis();
		cbf_handle_struct chs = new cbf_handle_struct(name);
		if (chs == null)
			System.err.println("Could not open file or make handle structure");

		int status = 0;

		status = cbf.cbf_rewind_datablock(chs);
		System.out.println("rewind_db (" + status + ")");

		uintP mp = new uintP();

		status = cbf.cbf_count_datablocks(chs, mp.cast());
		System.out.println("count_dbs (" + status + ") = " + mp.value());

		SWIGTYPE_p_p_char dbnamep = cbf.new_charPP();
		status = cbf.cbf_datablock_name(chs, dbnamep);
		System.out.println("db_name (" + status + ") = " + cbf.charPP_value(dbnamep));

		int[] ntype = {0};
		status = cbf.cbf_rewind_blockitem(chs, ntype);
		System.out.println("rewind_bi (" + status + ") = " + ntype[0]);

		status = cbf.cbf_find_category(chs, "diffrn_data_frame");
		System.out.println("find_cat (" + status + ")");
		if (status != 0) {
			status = cbf.cbf_find_category(chs, "diffrn_frame_data");
			System.out.println("find_cat (" + status + ")");
		}

		boolean fullheader = false;
		int[] dims = new int[2];

		if (status == 0) {
			readheader(chs, dims);
			fullheader = true;
		} else {
			System.out.println("Mini CBF!");
			status = cbf.cbf_find_category(chs, "array_data");
			System.out.println("find_cat (" + status + ")");
			status = cbf.cbf_find_column(chs, "data");
			System.out.println("find_col (" + status + ")");
		}

		int dsize = dims[0]*dims[1];

		uintP cifcomp = new uintP();
		intP bid = new intP(), els = new intP(), elu = new intP();
		intP minel = new intP(), maxel = new intP(), isre = new intP();
		sizetP elsize = new sizetP(), elnum = new sizetP();
		sizetP dim1 = new sizetP(), dim2 = new sizetP(), dim3 = new sizetP(), pad = new sizetP();
		SWIGTYPE_p_p_char byteorder = cbf.new_charPP();

		status = cbf.cbf_get_arrayparameters_wdims(chs, cifcomp.cast(), bid.cast(), elsize.cast(),
							els.cast(), elu.cast(), elnum.cast(),
							minel.cast(), maxel.cast(), isre.cast(),
							byteorder, dim1.cast(), dim2.cast(), dim3.cast(),
							pad.cast());
		System.out.println("get_aparams (" + status + ") = " + cifcomp.value() + ", "  + bid.value()
					+ ", "  + elsize.value() + ", "  + els.value() + ", "  + elu.value() + ",");
		System.out.println(" "  + elnum.value() + ", "  + minel.value() + ", "  + maxel.value()
					+ ", "  + isre.value() + ", "  + cbf.charPP_value(byteorder) + ",");
		System.out.println(" " + dim1.value() + ", "  + dim2.value() + ", "  + dim3.value()
					+ ", "  + pad.value());
		if (fullheader && dsize != elnum.value()) {
			System.out.println("Mismatch of CBF binary data size: " + dsize + " != " + elnum.value());
			return 0;
		}
		dsize = (int) elnum.value();
		int elbytes = (int) elsize.value();

		boolean isreal = (isre.value() == 1);
		cifcomp.delete();
		elu.delete(); minel.delete(); maxel.delete(); isre.delete();
		elsize.delete(); elnum.delete();
		dim1.delete(); dim2.delete(); dim3.delete(); pad.delete();

		sizetP rsize = new sizetP();

		long dstart = -System.currentTimeMillis();

		if (isreal) {

			if (elbytes != Float.SIZE/8 && elbytes != Double.SIZE/8) {
				System.err.println("Element size, " + elbytes + ", not supported!\n");
			} else {
				ByteBuffer bdata = ByteBuffer.allocateDirect(elbytes*dsize).order(ByteOrder.nativeOrder());
				status = cbf.cbf_get_realarray(chs, bid.cast(), bdata, elbytes, dsize, rsize.cast());
				if (status == 0) {
					System.out.println("get_realarray (" + status + ") " + rsize.value() + "/" + dsize);
				
					switch (elbytes) {
					case Float.SIZE/8:
						float[] farray = new float[dsize];
						bdata.asFloatBuffer().get(farray);
						System.out.println("Sample is " + farray[42]);
						farray = null;
						break;
					case Double.SIZE/8:
						double[] darray = new double[dsize];
						bdata.asDoubleBuffer().get(darray);
						System.out.println("Sample is " + darray[42]);
						darray = null;
						break;
					}
				}

				bdata = null;
			}

		} else {
			if (elbytes != Byte.SIZE/8 && elbytes != Short.SIZE/8 && elbytes != Integer.SIZE/8 && elbytes != Long.SIZE/8) {
				System.err.println("Element size, " + elbytes + ", not supported!\n");
			} else {
				ByteBuffer bdata = ByteBuffer.allocateDirect(elbytes*dsize).order(ByteOrder.nativeOrder());
				status = cbf.cbf_get_integerarray(chs, bid.cast(), bdata, elbytes, els.value(), dsize, rsize.cast());
				if (status == 0) {
					System.out.println("get_intarray (" + status + ") " + rsize.value() + "/" + dsize);

					switch (elbytes) {
					case Byte.SIZE/8:
						byte[] barray = new byte[dsize];
						bdata.get(barray);
						System.out.println("Sample is " + barray[42]);
						barray = null;
						break;
					case Short.SIZE/8:
						short[] sarray = new short[dsize];
						bdata.asShortBuffer().get(sarray);
						System.out.println("Sample is " + sarray[42]);
						sarray = null;
						break;
					case Integer.SIZE/8:
						int[] iarray = new int[dsize];
						bdata.asIntBuffer().get(iarray);
						System.out.println("Sample is " + iarray[42]);
						iarray = null;
						break;
					case Long.SIZE/8:
						long[] larray = new long[dsize];
						bdata.asLongBuffer().get(larray);
						System.out.println("Sample is " + larray[42]);
						larray = null;
						break;
					}

					bdata = null;
				}
			}
		}

		rsize.delete();
		bid.delete(); els.delete();
		chs.delete();
		long now = System.currentTimeMillis();
		dstart += now;
		start += now;
		System.err.printf("Time: %dms (%dms)\n", start, dstart);
		return (int) start;
	}

	/**
	 * @param args
	 */
	public static void main(String[]args) {
		String name;
		if (args.length < 1) {
			name = "examples/template_pilatus6m_2463x2527.cbf";
		} else {
			name = args[0];
		}
		loadFile(name);

		int n;
		int t = 0;
		for (n = 0; n < 10; n++)
			t += loadFile(name);
		System.err.printf("Ave: %dms\n", t/10);
	}

}
