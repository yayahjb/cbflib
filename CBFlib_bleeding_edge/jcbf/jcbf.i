// swig -Iinclude -java -package org.iucr.cbflib -outdir java jcbf.i
// javac -d . java/*.java
// jar cf cbflib-0.8.0.jar org
// gcc -fPIC -fpic -c jcbf_wrap.c -I/dls_sw/dasc/jdk/jdk1.6.0_11/include -I/dls_sw/dasc/jdk/jdk1.6.0_11/include/linux -Iinclude
// gcc -shared jcbf_wrap.o -o solib/libjcbf_wrap.so -Lsolib -lcbf
%module cbf


%{
/* Includes the header in the wrapper code */
#include "cbf_tree.h"
#include "cbf.h"
%}

// Include support for C pointers
%include "cpointer.i"

// Wrap some C pointers in classes
%pointer_class(size_t, sizetP);
%pointer_class(int, intP);
%pointer_class(unsigned int, uintP);
%pointer_class(double, doubleP);

// Wrap char** in functions
%pointer_functions(const char *, charPP);

// Cast to void *
%pointer_cast(int *, void *, int_void);
%pointer_cast(double *, void *, double_void);

// Don't expose the memory allocation/de-allocation functions or any files
%ignore cbf_read_handle(cbf_handle *ppchs);
%ignore cbf_make_handle(cbf_handle *ppchs);
%ignore cbf_free_handle(cbf_handle pchs);
%ignore cbf_read_file (cbf_handle handle, FILE *stream, int flags);
%ignore cbf_read_widefile (cbf_handle handle, FILE *stream, int flags);
%ignore cbf_read_buffered_file (cbf_handle handle, FILE *stream, int flags,
                            const char * buffer, size_t buffer_len);
%ignore cbf_write_file (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);
%ignore cbf_write_local_file (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);
%ignore cbf_write_widefile (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);

// Include support for C arrays
// %include "carrays.i"

// Wrap some arrays in classes
// %array_class(int, intArray);
// %array_class(double, doubleArray);


%include "buffers.i"
%apply void* BUFF {void *value}
int cbf_get_integerarray (cbf_handle  handle,
                          int        *id,
                          void       *value,
                          size_t      elsize,
                          int         elsign,
                          size_t      nelem,
                          size_t     *nelem_read);

%apply void* BUFF {void *value}
int cbf_get_realarray (cbf_handle  handle,
                          int        *id,
                          void       *value,
                          size_t      elsize,
                          size_t      nelem,
                          size_t     *nelem_read);

/* Parse the header file to generate wrappers */
%include "cbf_tree.h"

%include "typemaps.i"
%apply int *OUTPUT { CBF_NODETYPE *type };

%include "cbf.h"

// Add in a custom proxy constructor and destructor
%extend cbf_handle_struct {
	cbf_handle_struct(const char *name) {
		FILE *f;
		f = fopen(name, "rb");
//		fseek(f, 0, SEEK_END);
//		long flen = ftell(f);
//		rewind(f);
//		char *buffer = malloc(flen+1);
//		fread(buffer, 1, flen, f);
//		fclose(f);

		cbf_handle_struct *pchs = 0;
		cbf_make_handle(&pchs);

		if (cbf_read_widefile(pchs, f, MSG_DIGEST))
			pchs = 0;
//		if (cbf_read_buffered_file(pchs, NULL, MSG_DIGEST|CBF_PARSE_WIDE, buffer, flen))
//			pchs = 0;

//		free(buffer);
		return pchs;
	}
	~cbf_handle_struct() {
		cbf_free_handle(self);
	}
}

