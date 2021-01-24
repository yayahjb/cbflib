// swig -Iinclude -java -package org.iucr.cbflib -outdir java cbf.i
// javac -d . java/*.java
// jar cf cbflib-0.9.6.jar org
// gcc -fPIC -fpic -c cbf_wrap.c -I$JDKDIR/include -I$JDKDIR/include/linux -Iinclude
// gcc -shared cbf_wrap.o -o solib/libcbf_wrap.so -Lsolib -lcbf
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
%ignore cbf_read_file(cbf_handle handle, FILE *stream, int flags);
%ignore cbf_read_widefile(cbf_handle handle, FILE *stream, int flags);
%ignore cbf_read_buffered_file(cbf_handle handle, FILE *stream, int flags,
                            const char * buffer, size_t buffer_len);
%ignore cbf_write_file(cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);
%ignore cbf_write_local_file(cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);
%ignore cbf_write_widefile(cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);

// Include support for C arrays
// %include "carrays.i"
//
// Wrap some arrays in classes
// %array_class(int, intArray);
// %array_class(double, doubleArray);

// Use NIO buffers
%include "buffers.i"
%apply void* BUFF {void *value}
int cbf_get_integerarray(cbf_handle  handle,
                          int        *id,
                          void       *value,
                          size_t      elsize,
                          int         elsign,
                          size_t      nelem,
                          size_t     *nelem_read);

%apply void* BUFF {void *value}
int cbf_get_realarray(cbf_handle  handle,
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
%extend _cbf_handle_struct {
	_cbf_handle_struct(const char *name) {
		FILE *f;
		cbf_handle_struct *pchs;

		f = fopen(name, "rb");

		pchs = 0;
		cbf_make_handle(&pchs);

		if (cbf_read_widefile(pchs, f, MSG_DIGEST))
			pchs = 0;

		return pchs;
	}

	void delete_cbf_handle_struct() {
		cbf_free_handle(self);
	}
}

