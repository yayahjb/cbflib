% pycbf_i.w
% nuweb source file used to create
% pycbf.i and to document it in pycbf.w
%
% pycbf - python binding to the CBFlib library
%
% Copyright (C) 2005  Jonathan Wright
%     ESRF, Grenoble, France
%     email: wright@@esrf.fr
%
% Revised for CBFlib 0.9 releases, Herbert J. Bernstein, 23 Aug 2010
%
%######################################################################
%#                                                                    #
%# YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE INCLUDING PYCBF UNDER THE  #
%# TERMS OF THE GPL                                                   #
%#                                                                    #
%# ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API INCLUDING PYCBF  #
%# UNDER THE TERMS OF THE LGPL                                        #
%#                                                                    #
%######################################################################
%
%########################### GPL NOTICES ##############################
%#                                                                    #
%# This program is free software; you can redistribute it and/or      #
%# modify it under the terms of the GNU General Public License as     #
%# published by the Free Software Foundation; either version 2 of     #
%# (the License, or (at your option) any later version.               #
%#                                                                    #
%# This program is distributed in the hope that it will be useful,    #
%# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
%# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      #
%# GNU General Public License for more details.                       #
%#                                                                    #
%# You should have received a copy of the GNU General Public License  #
%# along with this program; if not, write to the Free Software        #
%# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           #
%# 02111-1307  USA                                                    #
%#                                                                    #
%######################################################################
%
%######################### LGPL NOTICES ###############################
%#                                                                    #
%# This library is free software; you can redistribute it and/or      #
%# modify it under the terms of the GNU Lesser General Public         #
%# License as published by the Free Software Foundation; either       #
%# version 2.1 of the License, or (at your option) any later version. #
%#                                                                    #
%# This library is distributed in the hope that it will be useful,    #
%# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
%# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  #
%# Lesser General Public License for more details.                    #
%#                                                                    #
%# You should have received a copy of the GNU Lesser General Public   #
%# License along with this library; if not, write to the Free         #
%# Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    #
%# MA  02110-1301  USA                                                #
%#                                                                    #
%######################################################################
%
\section{Generating the c interface - the SWIG file}

Essentially the swig file starts by saying what to include to build the wrappers,
and then goes on to define the python interface for each function we want to call.

The library appears to define at least three ``objects''; a CBF handle, a cbf\_goniometer and
a cbf\_detector. We will attempt to map these onto python classes. 

FIXME - decide whether introduce a "binary array" class with converters to 
more common representations?

All of the functions in the library appear to return 0 on success and a 
meaningful error code on failure.
We try to propagate that error code across the language barrier via exceptions.

So the SWIG file will start off by including the header files needed
for compilation.  Note the defintion of constants to be passed as
arguments in calls in the form pycbf.CONSTANTNAME

@D Constants used for compression
@{
// The actual wrappers 

// Constants needed from header files

  /* Constants used for compression */

#define CBF_INTEGER     0x0010  /* Uncompressed integer               */
#define CBF_FLOAT       0x0020  /* Uncompressed IEEE floating-point   */
#define CBF_CANONICAL   0x0050  /* Canonical compression              */
#define CBF_PACKED      0x0060  /* Packed compression                 */
#define CBF_PACKED_V2   0x0090  /* CCP4 Packed (JPA) compression V2   */
#define CBF_BYTE_OFFSET 0x0070  /* Byte Offset Compression            */
#define CBF_PREDICTOR   0x0080  /* Predictor_Huffman Compression      */
#define CBF_NONE        0x0040  /* No compression flag                */
#define CBF_COMPRESSION_MASK  \
                        0x00FF  /* Mask to separate compression
                                   type from flags              */
#define CBF_FLAG_MASK   0x0F00  /* Mask to separate flags from
                                   compression type             */
#define CBF_UNCORRELATED_SECTIONS \
                        0x0100  /* Flag for uncorrelated sections     */
#define CBF_FLAT_IMAGE  0x0200  /* Flag for flat (linear) images      */
#define CBF_NO_EXPAND   0x0400  /* Flag to try not to expand          */
@}


@D Constants used for headers
@{
  /* Constants used for headers */

#define PLAIN_HEADERS   0x0001  /* Use plain ASCII headers            */
#define MIME_HEADERS    0x0002  /* Use MIME headers                   */
#define MSG_NODIGEST    0x0004  /* Do not check message digests       */
#define MSG_DIGEST      0x0008  /* Check message digests              */
#define MSG_DIGESTNOW   0x0010  /* Check message digests immediately  */
#define MSG_DIGESTWARN  0x0020  /* Warn on message digests immediately*/
#define PAD_1K          0x0020  /* Pad binaries with 1023 0's         */
#define PAD_2K          0x0040  /* Pad binaries with 2047 0's         */
#define PAD_4K          0x0080  /* Pad binaries with 4095 0's         */
@}

@D Constants used to control CIF parsing
@{
  /* Constants used to control CIF parsing */
  
#define CBF_PARSE_BRC   0x0100  /* PARSE DDLm/CIF2 brace {,...}             */
#define CBF_PARSE_PRN   0x0200  /* PARSE DDLm parens     (,...)             */
#define CBF_PARSE_BKT   0x0400  /* PARSE DDLm brackets   [,...]             */
#define CBF_PARSE_BRACKETS \
                        0x0700  /* PARSE ALL brackets                       */
#define CBF_PARSE_TQ    0x0800  /* PARSE treble quotes """...""" and '''...'''       */
#define CBF_PARSE_CIF2_DELIMS  \
                        0x1000  /* Do not scan past an unescaped close quote
                                   do not accept {} , : " ' in non-delimited
                                   strings'{ */                          
#define CBF_PARSE_DDLm  0x0700  /* For DDLm parse (), [], {}                */
#define CBF_PARSE_CIF2  0x1F00  /* For CIF2 parse {}, treble quotes,
                                   stop on unescaped close quotes           */
#define CBF_PARSE_DEFINES      \
                        0x2000  /* Recognize DEFINE_name            */      
                        
  
#define CBF_PARSE_WIDE      0x4000  /* PARSE wide files                         */

#define CBF_PARSE_UTF8      0x10000 /* PARSE UTF-8                              */

#define HDR_DEFAULT (MIME_HEADERS | MSG_NODIGEST)

#define MIME_NOHEADERS  PLAIN_HEADERS

  /* CBF vs CIF */

#define CBF             0x0000  /* Use simple binary sections         */
#define CIF             0x0001  /* Use MIME-encoded binary sections   */
@}

@D Constants used for encoding
@{
  /* Constants used for encoding */

#define ENC_NONE        0x0001  /* Use BINARY encoding                 */
#define ENC_BASE64      0x0002  /* Use BASE64 encoding                 */
#define ENC_BASE32K     0x0004  /* Use X-BASE32K encoding              */
#define ENC_QP          0x0008  /* Use QUOTED-PRINTABLE encoding       */
#define ENC_BASE10      0x0010  /* Use BASE10 encoding                 */
#define ENC_BASE16      0x0020  /* Use BASE16 encoding                 */
#define ENC_BASE8       0x0040  /* Use BASE8  encoding                 */
#define ENC_FORWARD     0x0080  /* Map bytes to words forward (1234)   */
#define ENC_BACKWARD    0x0100  /* Map bytes to words backward (4321)  */
#define ENC_CRTERM      0x0200  /* Terminate lines with CR             */
#define ENC_LFTERM      0x0400  /* Terminate lines with LF             */

#define ENC_DEFAULT (ENC_BASE64 | ENC_LFTERM | ENC_FORWARD)
@}

\subsection{Exceptions}

We attempt to catch the errors and pass them back to python as 
exceptions. This could still do with a little work to propagage
back the calls causing the errors.

Currently there are two global  constants defined, called error\_message
and error\_status. 
These are filled out when an error occurred, converting the numerical
error value into something the author can read.

There is an implicit assumption that if the library is used 
correctly you will not normally get exceptions. 
This should be addressed further in areas like file opening,
proper python exceptions should be returned.

See the section on exception handling in pycbf.i, above.

Currently you get a meaningful string back. Should perhaps look into
defining these as python exception classes? 
In any case - the SWIG exception handling is defined via the following.
It could have retained the old style if(status = action) but then
harder to see what to return...

@D Exception handling
@{
// Exception handling

  /* Convenience definitions for functions returning error codes */
%exception {
   error_status=0;
   $action
   if (error_status){
     get_error_message();
     PyErr_SetString(PyExc_Exception,error_message);
     return NULL;
   }
}

/* Retain notation from cbf lib but pass on as python exception */

#define cbf_failnez(x) {(error_status = x);} 

/* printf("Called \"x\", status %d\n",error_status);} */

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d in \"x\"\n", err); \
                         { c; } return err; }}
@}

@O pycbf.i
@{
/* File: pycbf.i */

// Indicate that we want to generate a module call pycbf
%module pycbf

%pythoncode %{
__author__ = "Jon Wright <wright@@esrf.fr>"
__date__ = "14 Dec 2005"
__version__ = "CBFlib 0.9"
__credits__ = """Paul Ellis and Herbert Bernstein for the excellent CBFlib!"""
__doc__=""" pycbf - python bindings to the CBFlib library

 A library for reading and writing ImageCIF and CBF files 
 which store area detector images for crystallography.

 This work is a derivative of the CBFlib version 0.7.7 library
 by  Paul J. Ellis of Stanford Synchrotron Radiation Laboratory
 and Herbert J. Bernstein of Bernstein + Sons
 See:
   http://www.bernstein-plus-sons.com/software/CBF/

 Licensing is GPL based, see:
   http://www.bernstein-plus-sons.com/software/CBF/doc/CBFlib_NOTICES.html

 These bindings were automatically generated by SWIG, and the
 input to SWIG was automatically generated by a python script.
 We very strongly recommend you do not attempt to edit them 
 by hand!



 Copyright (C) 2007    Jonathan Wright
                       ESRF, Grenoble, France
                email: wright@@esrf.fr
    
  Revised, August 2010  Herbert J. Bernstein
    Add defines from CBFlib 0.9.1
    
"""
%}


// Used later to pass back binary data
%include "cstring.i"

// Attempt to autogenerate what SWIG thinks the call looks like

// Typemaps are a SWIG mechanism for many things, not least multiple 
// return values
%include "typemaps.i"

// Arrays are needed
%include "carrays.i"
%array_class(double, doubleArray)
%array_class(int, intArray)
%array_class(short, shortArray)
%array_class(long, longArray)

// Following the SWIG 1.3 documentation at
// http://www.swig.org/Doc1.3/Python.html
// section 31.9.5, we map sequences of
// PyFloat, PyLong and PyInt to
// C arrays of double, long and int
//
// But with the strict checking of being a float
// commented out to allow automatic conversions
%{
static int convert_darray(PyObject *input, double *ptr, int size) {
  int i;
  if (!PySequence_Check(input)) {
      PyErr_SetString(PyExc_TypeError,"Expecting a sequence");
      return 0;
  }
  if (PyObject_Length(input) != size) {
      PyErr_SetString(PyExc_ValueError,"Sequence size mismatch");
      return 0;
  }
  for (i =0; i < size; i++) {
      PyObject *o = PySequence_GetItem(input,i);
     /*if (!PyFloat_Check(o)) {
        
         Py_XDECREF(o);
         PyErr_SetString(PyExc_ValueError,"Expecting a sequence of floats");
         return 0;
      }*/
      ptr[i] = PyFloat_AsDouble(o);
      Py_DECREF(o);
  }
  return 1;
}
%}

%typemap(in) double [ANY](double temp[$1_dim0]) {
    if ($input == Py_None) $1 = NULL;
    else 
    if (!convert_darray($input,temp,$1_dim0)) {
      return NULL;
    }
    $1 = &temp[0];
}

%{
    static long convert_larray(PyObject *input, long *ptr, int size) {
        int i;
        if (!PySequence_Check(input)) {
            PyErr_SetString(PyExc_TypeError,"Expecting a sequence");
            return 0;
        }
        if (PyObject_Length(input) != size) {
            PyErr_SetString(PyExc_ValueError,"Sequence size mismatch");
            return 0;
        }
        for (i =0; i < size; i++) {
            PyObject *o = PySequence_GetItem(input,i);
            /*if (!PyLong_Check(o)) {
                Py_XDECREF(o);
                PyErr_SetString(PyExc_ValueError,"Expecting a sequence of long integers");
                return 0;
            }*/
            ptr[i] = PyLong_AsLong(o);
            Py_DECREF(o);
        }
        return 1;
    }
%}

%typemap(in) long [ANY](long temp[$1_dim0]) {
    if (!convert_larray($input,temp,$1_dim0)) {
        return NULL;
    }
    $1 = &temp[0];
}

%{
    static int convert_iarray(PyObject *input, int *ptr, int size) {
        int i;
        if (!PySequence_Check(input)) {
            PyErr_SetString(PyExc_TypeError,"Expecting a sequence");
            return 0;
        }
        if (PyObject_Length(input) != size) {
            PyErr_SetString(PyExc_ValueError,"Sequence size mismatch");
            return 0;
        }
        for (i =0; i < size; i++) {
            PyObject *o = PySequence_GetItem(input,i);
            /*if (!PyInt_Check(o)) {
                Py_XDECREF(o);
                PyErr_SetString(PyExc_ValueError,"Expecting a sequence of long integers");
                return 0;
            }*/
            ptr[i] = (int)PyInt_AsLong(o);
            Py_DECREF(o);
        }
        return 1;
    }
%}

%typemap(in) int [ANY](int temp[$1_dim0]) {
    if (!convert_iarray($input,temp,$1_dim0)) {
        return NULL;
    }
    $1 = &temp[0];
}


%{  // Here is the c code needed to compile the wrappers, but not 
    // to be wrapped 

#include "../include/cbf.h"          
#include "../include/cbf_simple.h"

// Helper functions to generate error message
 

static int error_status = 0;
static char error_message[1024] ; // hope that is long enough

/* prototype */
void get_error_message(void);

void get_error_message(){
  sprintf(error_message,"%s","CBFlib Error(s):");
  if (error_status & CBF_FORMAT        )
    sprintf(error_message,"%s %s",error_message,"CBF_FORMAT       "); 
  if (error_status & CBF_ALLOC         )
    sprintf(error_message,"%s %s",error_message,"CBF_ALLOC        ");
  if (error_status & CBF_ARGUMENT      )
    sprintf(error_message,"%s %s",error_message,"CBF_ARGUMENT     ");
  if (error_status & CBF_ASCII         )
    sprintf(error_message,"%s %s",error_message,"CBF_ASCII        ");
  if (error_status & CBF_BINARY        )
    sprintf(error_message,"%s %s",error_message,"CBF_BINARY       ");
  if (error_status & CBF_BITCOUNT      )
    sprintf(error_message,"%s %s",error_message,"CBF_BITCOUNT     ");
  if (error_status & CBF_ENDOFDATA     )
    sprintf(error_message,"%s %s",error_message,"CBF_ENDOFDATA    ");
  if (error_status & CBF_FILECLOSE     )
    sprintf(error_message,"%s %s",error_message,"CBF_FILECLOSE    ");
  if (error_status & CBF_FILEOPEN      )
    sprintf(error_message,"%s %s",error_message,"CBF_FILEOPEN     ");
  if (error_status & CBF_FILEREAD      )
    sprintf(error_message,"%s %s",error_message,"CBF_FILEREAD     ");
  if (error_status & CBF_FILESEEK      )
    sprintf(error_message,"%s %s",error_message,"CBF_FILESEEK     ");
  if (error_status & CBF_FILETELL      )
    sprintf(error_message,"%s %s",error_message,"CBF_FILETELL     ");
  if (error_status & CBF_FILEWRITE     )
    sprintf(error_message,"%s %s",error_message,"CBF_FILEWRITE    ");
  if (error_status & CBF_IDENTICAL     )
    sprintf(error_message,"%s %s",error_message,"CBF_IDENTICAL    ");
  if (error_status & CBF_NOTFOUND      )
    sprintf(error_message,"%s %s",error_message,"CBF_NOTFOUND     ");
  if (error_status & CBF_OVERFLOW      )
    sprintf(error_message,"%s %s",error_message,"CBF_OVERFLOW     ");
  if (error_status & CBF_UNDEFINED     )
    sprintf(error_message,"%s %s",error_message,"CBF_UNDEFINED    ");
  if (error_status & CBF_NOTIMPLEMENTED)
    sprintf(error_message,"%s %s",error_message,"CBF_NOTIMPLEMENTED");
  if (error_status & CBF_NOCOMPRESSION)
    sprintf(error_message,"%s %s",error_message,"CBF_NOCOMPRESSION");
}


%} // End of code which is not wrapped but needed to compile
@}


@O pycbf.i
@{
@< Constants used for compression @>

@< Constants used for headers @>

@< Constants used to control CIF parsing @>

@< Constants used for encoding @>

@< Exception handling @>

%include "cbfgenericwrappers.i"

// cbf_goniometer object

%include "cbfgoniometerwrappers.i"

%include "cbfdetectorwrappers.i"

// cbfhandle object
%include "cbfhandlewrappers.i"

@}
