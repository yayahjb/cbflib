// swig -Iinclude -java -package org.iucr.cbflib -outdir java cbf.i
// javac -d . java/*.java
// jar cf cbflib-0.8.0.jar org
// gcc4 -fPIC -fpic -c cbf_wrap.c -I/dls_sw/dasc/jdk/jdk1.6.0_11/include -I/dls_sw/dasc/jdk/jdk1.6.0_11/include/linux -Iinclude
// gcc4 -shared cbf_wrap.o -o solib/libcbf_wrap.so -Lsolib -lcbf
%module cbf


%{
/* Includes the header in the wrapper code */
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

// Don't expose the memory allocation/de-allocation functions
%ignore cbf_make_handle(cbf_handle *ppchs);
%ignore cbf_free_handle(cbf_handle pchs);

// Include support for C arrays
%include "carrays.i"

// Wrap some arrays in classes
%array_class(int, intArray);
%array_class(double, doubleArray);

/* Parse the header file to generate wrappers */
%include "cbf.h"

// Add in a custom proxy constructor and destructor
%extend cbf_handle_struct {
	cbf_handle_struct() {
		cbf_handle_struct *pchs = 0;
		cbf_make_handle(&pchs);
		return pchs;
	}
	~cbf_handle_struct() {
		cbf_free_handle(self);
	}
}

// File I/O functions
FILE *fopen(char *name, char *mode);
void  fclose(FILE *);
