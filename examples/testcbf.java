import org.iucr.cbflib.*;

// javac -cp cbflib-0.8.0.jar testcbf.java
// LD_LIBRARY_PATH=solib java -cp cbflib-0.8.0.jar:. testcbf
public class testcbf {

	static {
		System.loadLibrary("cbf_wrap");
	}

	public static void main(String[] args) {
		SWIGTYPE_p_FILE f = cbf.fopen("examples/template_pilatus6m_2463x2527.cbf", "rb");
		cbf_handle_struct chs = new cbf_handle_struct();
		int status = 0;
		status = cbf.cbf_read_widefile(chs, f, cbfConstants.MSG_DIGEST);
		System.out.println("read_widefile (" + status + ")");

		uintp mp = new uintp();
		status = cbf.cbf_count_datablocks(chs, mp.cast());
		System.out.println("count_dbs (" + status + ") = " + mp.value());
	}
}
