% pycbf.w
% nuweb source file used to create pycbf documentation
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
% Nuweb formatted latex file 
% Most of this is standard latex with code rolled in
% Anything to do with @@ characters is probably specific to nuweb
%
%
% The word FIXME anywhere in this document indicates 
% an area where more attention is still needed.
%
% Note that this file (pycbf.w) does not copy and paste from CBFlib 
% (or anywhere) except in the hand wrapped function prototypes. 
% 
%
%

\documentclass[10pt,a4paper,twoside,notitlepage]{article}

\usepackage{graphics} % For the pictures
\usepackage{anysize}  % Try to circumvent Latex default margins
\usepackage{fancyhdr}
\usepackage[dvipdfm,bookmarks=true,backref,bookmarksnumbered=true,
            bookmarkstype=toc]{hyperref}
\newcommand{\var}[1]{\textbf{\textsf{#1}}} % highlight variables in text
\newcommand{\code}[1]{\textbf{\textsf{#1}}} % highlight code in text
\newcommand{\param}[1]{\textbf{\textsf{#1}}} % ... parameters ...
\newcommand{\mb}  [1] {\mathbf{#1}}


\begin{document}

\marginsize{1.5cm}{1.5cm}{1.5cm}{1.5cm} % Needs anysize
%\pagestyle{headings}            % These are ugly - fix them somehow?

\pagestyle{fancy}
%$\renewcommand{\chaptermark}[1]{
%$      \markboth{\chaptername
%$      \ \thechapter.\ #1} {} }

\renewcommand{\sectionmark}[1]{
      \markright {   
      \ \thesection.\ #1} {} }

\fancyhead[LE,RO]{\rightmark}
\fancyhead[LO,RE]{\leftmark}
\fancyfoot[C]{\today}
\fancyfoot[LE,RO]{\thepage}
\fancyfoot[LO,RE]{J. P. Wright}
\renewcommand{\footrulewidth}{0.4pt}

\pagenumbering{arabic}          % Page numbers



\title{\textbf{\textsf{PyCBF}} \\ A python binding to the CBFlib library}
\author{Jon P. Wright \\ Anyone who wishes to contribute, please do!}
\date{Started Dec 12, 2005, already it is \today}

\maketitle

\abstract{
Area detectors at synchrotron facilities can result in huge amounts of data 
being generated very rapidly. 
The IUCr (International Union of Crystallography) has devised a standard file
format for storing and annotating such data, in order that it might be more
easily interchanged and exploited. 
A c library which gives access to this file format has been developed 
by Paul Ellis and Herbert Bernstein (Version 0.7.4, 
http://www.bernstein-plus-sons.com/software/CBF/).
In this document a python interface is developed using the SWIG
 (http://www.swig.org)
package in order to give the author easy access to binary cif files.
}

\tableofcontents
\markboth{}{}

\section*{Index of file names}
@f


\section*{Index of macro names}
@m

\section*{Things to do}

\begin{itemize}
\item Write test code to test each and every function for good and bad args etc
\end{itemize}
\section{Introduction}

The CBFlib library (version 0.7.4) is written in the C language, offering C
 (and C++)
programmers a convenient interface to such files.
The current author uses a different language (python) from day to day and 
so a python interface was desired. 
After a short attempt to make a quick and dirty SWIG interface it was decided
that in the long run it would be better to write a proper interface for python.

All of the functions in the library return an integer reflecting error status.
Usually these integers seem to be zero, and a non-zero return value appears
to mean an error occurred.
Actual return values are returned via pointers in argument lists.
In order to simplify the authors life (as a user) all of those integers have
been made to disappear if they are zero, and cause an ``exception'' to 
be generated if they are not zero. 
This solution might not be the best thing to do, and it can always be changed 
where the return value is intended to normally be used.

Actual return values which were passed back via pointer arguments are now
just passed back as (perhaps multiple) return values.
We must look out for INOUT arguments, none seem to have been found yet, but there 
might be exceptions.
The author has a vague suspicion that python functions generally do not modify their
arguments, but this might be wrong.

The library appears to define (at least) three objects. The one we started on
was the cbf\_handle\_struct defined in cbf.h. 
Many of the functions have their first argument as a pointer to one
of these structures. Therefore we make this structure an object and then 
everything which uses it as first argument is a member function for that
object.

In order to pass image data back and forth there is a difficulty that python
seems to lack a good way to represent large arrays.
The standard library offers an "array" object which claims to efficiently
hold homogenous numerical data. 
Sadly this seems to be limited to one-dimensional arrays.
The builtin string object can hold binary data and this was chosen as 
the way to pass the actual binary back and forth between python and CBFlib.
Unfortunately this means the binary data are pretty useless when they arrive
on the python side, so helper functions are provided to convert the data
to a python (standard library) 1D array and also to a "Numeric" array or a
"Numarray" array. 
The latter two are popular extension modules for manipulating large arrays.

\section{Installation prerequisites}

The document you are reading was generated from a nuweb source file. This
is something very similar to latex with a few extensions for writing out
source code files. As such it keeps together the whole package in a single file
and makes it easier to write documentation. You will need a to obtain the 
preprocessing tool nuweb (perhaps from http://nuweb.sourceforge.net) in
order to build from scratch with the file pycbf.w. Preproccessed output
is hopefully also available to you. 
We do not recommend editing the SWIG generated wrappers!!

Only python version 2.4 has been targetted originally (other versions?) so
that you will probably want to have that version of python installed.

We are building binary extensions, so you also need a working c compiler. 
The compiler used by the author was gcc (for both windows and unix) with
the mingw version under windows.

Finally, you need a copy of swig (from www.swig.org) in order to (re)generate
the c wrappers. 

In case all that sounds scary, then fear not, it is likely that a single download
for windows will just work with the right version of python. Unix systems
come with many of those things available anyway.

@i pycbf_i.w

Despite the temptation to just throw everything from the c header files
into the interface, a short experience suggested we are better off to pull 
out only the parts we want and make the calls more pythonic

The input files "CBFhandlewrappers.i", etc. are created by the make\_pycbf.py
script.

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


\section{Docstrings}

The file doc/CBFlib.html is converted to a file CBFlib.txt to generate the
docstrings and many of the wrappers.  The conversion was done by the
text-based browser, links.

This text document is then parsed by a python script called make\_pycbf.py 
to generate the .i files which are included by the swig wrapper generator.
Unfortunately this more complicated for non-python users but seemed less
error prone and involved less typing for the author.

@i make_pycbf.w

\section{Building python extensions - the setup file}


Based on the contents of the makefile for CBFlib we will just 
pull in all of the library for now. We use the distutils approach.



@O setup.py
@{

# Import the things to build python binary extensions

from distutils.core import setup, Extension

# Make our extension module

e = Extension('_pycbf',
              sources = ["pycbf_wrap.c","../src/cbf_simple.c"],
         extra_compile_args=["-g"],
         library_dirs=["../lib/"],
         libraries=["cbf"],
         include_dirs = ["../include"] )
            
# Build it
setup(name="_pycbf",ext_modules=[e],)
@}


\section{Building and testing the resulting package}

Aim to build and test in one go (so that the source and the binary match!!)

@o win32.bat
@{
nuweb pycbf
latex pycbf
nuweb pycbf
latex pycbf
dvipdfm pycbf
nuweb pycbf
C:\python24\python make_pycbf.py > TODO.txt
"C:\program files\swigwin-1.3.31\swig.exe" -python pycbf.i
C:\python24\python setup.py build --compiler=mingw32
copy build\lib.win32-2.4\_pycbf.pyd .
REM C:\python24\python pycbf_test1.py
C:\python24\python pycbf_test2.py
C:\python24\python pycbf_test3.py
C:\python24\lib\pydoc.py -w pycbf
C:\python24\python makeflatascii.py pycbf_ascii_help.txt
@}

@o linux.sh
@{
nuweb pycbf
latex pycbf
nuweb pycbf
latex pycbf
dvipdfm pycbf
nuweb pycbf
lynx -dump CBFlib.html > CBFlib.txt
python make_pycbf.py 
swig -python pycbf.i
python setup.py build 
rm _pycbf.so
cp build/lib.linux-i686-2.4/_pycbf.so .
python pycbf_test1.py
python pycbf_test2.py
pydoc -w pycbf
python makeflatascii.py pycbf_ascii_help.txt
@}




This still gives bold in the ascii (=sucks)

@O makeflatascii.py
@{
import pydoc, pycbf, sys
f = open(sys.argv[1],"w")
pydoc.pager=lambda text: f.write(text)
pydoc.TextDoc.bold = lambda self,text : text
pydoc.help(pycbf)
@}

\section{Debugging compiled extensions}

Since it can be a bit of a pain to see where things go wrong here is a
quick recipe for poking around with a debugger:

\begin{verbatim}
amber $> gdb /bliss/users//blissadm/python/bliss_python/suse82/bin/python
GNU gdb 5.3
Copyright 2002 Free Software Foundation, Inc.
GDB is free software, covered by the GNU General Public License, and you are
welcome to change it and/or distribute copies of it under certain conditions.
Type "show copying" to see the conditions.
There is absolutely no warranty for GDB.  Type "show warranty" for details.
This GDB was configured as "i586-suse-linux"...
(gdb) br _PyImport_LoadDynamicModule
Breakpoint 1 at 0x80e4199: file Python/importdl.c, line 28.
\end{verbatim}

This is how to get a breakpoint when loading the module
\begin{verbatim}
(gdb) run
Starting program: /mntdirect/_bliss/users/blissadm/python/bliss_python/suse82/bin/python
[New Thread 16384 (LWP 18191)]
Python 2.4.2 (#3, Feb 17 2006, 09:12:13)
[GCC 3.3 20030226 (prerelease) (SuSE Linux)] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> import pycbf
[Switching to Thread 16384 (LWP 18191)]

Breakpoint 1, _PyImport_LoadDynamicModule (name=0xbfffd280 "_pycbf.so",
    pathname=0xbfffd280 "_pycbf.so", fp=0x819e208) at Python/importdl.c:28
28              if ((m = _PyImport_FindExtension(name, pathname)) != NULL) {
(gdb) finish
Run till exit from #0  _PyImport_LoadDynamicModule (
    name=0xbfffd280 "_pycbf.so", pathname=0xbfffd280 "_pycbf.so", fp=0x819e208)
    at Python/importdl.c:28
load_module (name=0xbfffd710 "_pycbf", fp=0x819e208,
    buf=0xbfffd280 "_pycbf.so", type=3, loader=0x405b44f4)
    at Python/import.c:1678
1678                    break;
Value returned is $1 = (PyObject *) 0x405662fc
(gdb) break cbf_read_file
Breakpoint 2 at 0x407f0508: file ../src/cbf.c, line 221.
(gdb) cont
Continuing.
\end{verbatim}

We now have a breakpoint where we wanted inside the dynamically loaded file. 
\begin{verbatim}
>>> o=pycbf.cbf_handle_struct()
>>> o.read_file("../img2cif_packed.cif",pycbf.MSG_DIGEST)

Breakpoint 2, cbf_read_file (handle=0x81f7c08, stream=0x8174f58,
    headers=136281096) at ../src/cbf.c:221
221       if (!handle)
(gdb)
\end{verbatim}

Now you can step through the c...

\section{Things which are currently missing}

This is the to do list. Obviously we could benefit a lot from more
extensive testing and checking of the docstrings etc.

\input "TODO.txt"

\section{Testing}

Some test programs to see if anything appears to work. Eventually
it would be good to write a proper unit test suite.

\subsection{Read a file based on cif2cbf.c}

This is a pretty ugly translation of the program cif2cbf.c skipping
all of the writing parts. 
It appeared to work with the file img2cif\_packed.cif which is built
when you build CBFlib, hence that file is hardwired in.

@O pycbf_test1.py
@{
import pycbf
object = pycbf.cbf_handle_struct() # FIXME
object.read_file("../img2cif_packed.cif",pycbf.MSG_DIGEST)
object.rewind_datablock()
print "Found",object.count_datablocks(),"blocks"
object.select_datablock(0)
print "Zeroth is named",object.datablock_name()
object.rewind_category()
categories = object.count_categories()
for i in range(categories):
    print "Category:",i,
    object.select_category(i)
    category_name = object.category_name()
    print "Name:",category_name,
    rows=object.count_rows()
    print "Rows:",rows,
    cols = object.count_columns()
    print "Cols:",cols
    loop=1
    object.rewind_column()
    while loop is not 0:
        column_name = object.column_name()
        print "column name \"",column_name,"\"",
        try:
           object.next_column()
        except:
           break
    print
    for j in range(rows):
        object.select_row(j)
        object.rewind_column()
        print "row:",j
        for k in range(cols):
            name=object.column_name()
            print "col:",name,
            object.select_column(k)
            typeofvalue=object.get_typeofvalue()
            print "type:",typeofvalue
            if typeofvalue.find("bnry") > -1:
                print "Found the binary!!",
                s=object.get_integerarray_as_string()
                print type(s)
                print dir(s)
                print len(s)
                try:
                   import Numeric
                   d = Numeric.fromstring(s,Numeric.UInt32) 
                   # Hard wired Unsigned Int32
                   print d.shape
                   print d[0:10],d[d.shape[0]/2],d[-1]
                   d=Numeric.reshape(d,(2300,2300))
#                   from matplotlib import pylab
#                   pylab.imshow(d,vmin=0,vmax=1000)
#                   pylab.show()
                except ImportError:
                   print "You need to get Numeric and matplotlib to see the data"
            else:
                value=object.get_value()
                print "Val:",value,i
    print
del(object)
#
print dir()
#object.free_handle(handle) 
@}


\subsection{Try to test the goniometer and detector}

Had some initial difficulties but then downloaded an input cbf file which defines 
a goniometer and detector. 
The file was found in the example data which comes with CBFlib.

This test is clearly minimalistic for now - it only checks the objects 
for apparent existence of
a single member function.

@O pycbf_test2.py
@{
import pycbf
obj = pycbf.cbf_handle_struct()
obj.read_file("../adscconverted.cbf",0)
obj.select_datablock(0)
g = obj.construct_goniometer()
print "Rotation axis is",g.get_rotation_axis()
d = obj.construct_detector(0)
print "Beam center is",d.get_beam_center()
@}


It appears to work - eventually. Surprising

\subsection{Test cases for the generics}

@O pycbf_test3.py
@{
import pycbf, unittest
class GenericTests(unittest.TestCase):

    def test_get_local_integer_byte_order(self):
        self.assertEqual( pycbf.get_local_integer_byte_order(),
                          'little_endian')

    def test_get_local_real_byte_order(self):
        self.assertEqual( pycbf.get_local_real_byte_order() ,
                          'little_endian')

    def test_get_local_real_format(self):
        self.assertEqual( pycbf.get_local_real_format(), 
                          'ieee 754-1985')

    def test_compute_cell_volume(self):
        self.assertEqual( pycbf.compute_cell_volume((2.,3.,4.,90.,90.,90.)),
                           24.0)
if __name__=="__main__":
    unittest.main()

@}

\section{Worked example 1 : xmas beamline + mar ccd detector at the ESRF}

Now for the interesting part. We will attempt to actually use pycbf for a real
dataprocessing task. Crazy you might think.

The idea is the following - we want to take the header information from some 
mar ccd files (and eventually also the user or the spec control system) and
pass this information into cif headers which can be read by fit2d (etc).

\subsection{Reading marccd headers}

Some relatively ugly code which parses a c header and then tries to interpret
the mar ccd header format. 

FIXME : byteswapping and ends???

@O xmas/readmarheader.py
@{#!/usr/bin/env python
import struct

# Convert mar c header file types to python struct module types
mar_c_to_python_struct = {
    "INT32"  : "i",
    "UINT32" : "I",
    "char"   : "c",
    "UINT16" : "H"
    }

# Sizes (bytes) of mar c header objects
mar_c_sizes = {
    "INT32"  : 4,
    "UINT32" : 4,
    "char"   : 1,
    "UINT16" : 2
    }

# This was worked out by trial and error from a trial image I think
MAXIMAGES=9



def make_format(cdefinition):
    """
    Reads the header definition in c and makes the format 
    string to pass to struct.unpack
    """
    lines = cdefinition.split("\n")
    fmt = ""
    names = []
    expected = 0
    for line in lines:
        if line.find(";")==-1:
            continue
        decl  = line.split(";")[0].lstrip().rstrip()
        try:
            [type, name] = decl.split()
        except:
            #print "skipping:",line
            continue
        #        print "type:",type,"  name:",name

        if name.find("[")>-1:
            # repeated ... times
            try:
                num = name.split("[")[1].split("]")[0]
                num = num.replace("MAXIMAGES",str(MAXIMAGES))
                num = num.replace("sizeof(INT32)","4")
                times = eval(num)
            except:
                print "Please decode",decl
                raise
        else:
            times=1
        try:
            fmt   += mar_c_to_python_struct[type]*times
            names += [name]*times
            expected += mar_c_sizes[type]*times
        except:
            #print "skipping",line
            continue
        #print "%4d %4d"%(mar_c_sizes[type]*times,expected),name,":",times,line
    #print struct.calcsize(fmt),expected
    return names, fmt

def read_mar_header(filename):
    """
    Get the header from a binary file
    """
    f = open(filename,"rb")
    f.seek(1024)
    header=f.read(3072)
    f.close()
    return header


def interpret_header(header, fmt, names):
    """
    given a format and header interpret it
    """
    values = struct.unpack(fmt,header)
    dict = {}
    i=0
    for name in names:
        if dict.has_key(name):
            if type(values[i]) == type("string"): 
                 dict[name] = dict[name]+values[i]
            else:
                 try:
                     dict[name].append(values[i])
                 except:
                     dict[name] = [dict[name],values[i]]
        else:
            dict[name] = values[i]
        i=i+1

    return dict


# Now for the c definition (found on mar webpage)
# The following string is therefore copyrighted by Mar I guess
        
cdefinition = """
typedef struct frame_header_type {
         /* File/header format parameters (256 bytes) */
         UINT32        header_type;      /* flag for header type  
                                           (can be  used as magic number) */
         char header_name[16];           /* header name (MMX) */
         UINT32        header_major_version;     /* header_major_version  (n.) */
         UINT32        header_minor_version;     /* header_minor_version  (.n) */
         UINT32        header_byte_order;/* BIG_ENDIAN (Motorola,MIPS);  
                                            LITTLE_ENDIAN (DEC, Intel) */
         UINT32        data_byte_order;  /* BIG_ENDIAN (Motorola,MIPS);  
                                            LITTLE_ENDIAN (DEC, Intel) */
         UINT32        header_size;      /* in bytes                     */
         UINT32        frame_type;       /* flag for frame type */
         UINT32        magic_number;     /* to be used as a flag - 
                                            usually  to indicate new file */
         UINT32        compression_type; /* type of image compression    */
         UINT32        compression1;     /* compression parameter 1 */
         UINT32        compression2;     /* compression parameter 2 */
         UINT32        compression3;     /* compression parameter 3 */
         UINT32        compression4;     /* compression parameter 4 */
         UINT32        compression5;     /* compression parameter 4 */
         UINT32        compression6;     /* compression parameter 4 */
         UINT32        nheaders;         /* total number of headers      */
         UINT32        nfast;            /* number of pixels in one line */
         UINT32        nslow;            /* number of lines in image     */
         UINT32        depth;            /* number of bytes per pixel    */
         UINT32        record_length;    /* number of pixels between 
                                            succesive rows */
         UINT32        signif_bits;      /* true depth of data, in bits  */
         UINT32        data_type;        /* (signed,unsigned,float...) */
         UINT32        saturated_value;  /* value marks pixel as saturated */
         UINT32        sequence;         /* TRUE or FALSE */
         UINT32        nimages;          /* total number of images - size of 
                                            each is nfast*(nslow/nimages) */
         UINT32        origin;           /* corner of origin             */
         UINT32        orientation;      /* direction of fast axis       */
         UINT32        view_direction;   /* direction to view frame      */
         UINT32        overflow_location;/* FOLLOWING_HEADER,  FOLLOWING_DATA */
         UINT32        over_8_bits;      /* # of pixels with counts  255 */
         UINT32        over_16_bits;     /* # of pixels with count  65535 */
         UINT32        multiplexed;      /* multiplex flag */
         UINT32        nfastimages;      /* # of images in fast direction */
         UINT32        nslowimages;      /* # of images in slow direction */
         UINT32        background_applied; /* flags correction has been applied - 
                                              hold magic number ? */
         UINT32        bias_applied;       /* flags correction has been applied - 
                                              hold magic number ? */
         UINT32        flatfield_applied;  /* flags correction has been applied - 
                                              hold magic number ? */
         UINT32        distortion_applied; /* flags correction has been applied - 
                                              hold magic number ? */
         UINT32        original_header_type;     /* Header/frame type from  file 
                                                    that frame is read from */
         UINT32        file_saved;         /* Flag that file has been  saved, 
                                              should be zeroed if modified */
         char reserve1[(64-40)*sizeof(INT32)-16];

         /* Data statistics (128) */
         UINT32        total_counts[2];  /* 64 bit integer range = 1.85E19*/
         UINT32        special_counts1[2];
         UINT32        special_counts2[2];
         UINT32        min;
         UINT32        max;
         UINT32        mean;
         UINT32        rms;
         UINT32        p10;
         UINT32        p90;
         UINT32        stats_uptodate;
         UINT32        pixel_noise[MAXIMAGES]; /* 1000*base noise value (ADUs) */
         char reserve2[(32-13-MAXIMAGES)*sizeof(INT32)];

         /* More statistics (256) */
         UINT16 percentile[128];


         /* Goniostat parameters (128 bytes) */
         INT32 xtal_to_detector;  /* 1000*distance in millimeters */
         INT32 beam_x;            /* 1000*x beam position (pixels) */
         INT32 beam_y;            /* 1000*y beam position (pixels) */
         INT32 integration_time;  /* integration time in  milliseconds */
         INT32 exposure_time;     /* exposure time in milliseconds */
         INT32 readout_time;      /* readout time in milliseconds */
         INT32 nreads;            /* number of readouts to get this  image */
         INT32 start_twotheta;    /* 1000*two_theta angle */
         INT32 start_omega;       /* 1000*omega angle */
         INT32 start_chi;         /* 1000*chi angle */
         INT32 start_kappa;       /* 1000*kappa angle */
         INT32 start_phi;         /* 1000*phi angle */
         INT32 start_delta;       /* 1000*delta angle */
         INT32 start_gamma;       /* 1000*gamma angle */
         INT32 start_xtal_to_detector; /* 1000*distance in mm (dist in um)*/
         INT32 end_twotheta;           /* 1000*two_theta angle */
         INT32 end_omega;              /* 1000*omega angle */
         INT32 end_chi;                /* 1000*chi angle */
         INT32 end_kappa;              /* 1000*kappa angle */
         INT32 end_phi;                /* 1000*phi angle */
         INT32 end_delta;              /* 1000*delta angle */
         INT32 end_gamma;              /* 1000*gamma angle */
         INT32 end_xtal_to_detector;   /* 1000*distance in mm (dist in um)*/
         INT32 rotation_axis;          /* active rotation axis */
         INT32 rotation_range;         /* 1000*rotation angle */
         INT32 detector_rotx;          /* 1000*rotation of detector  around X */
         INT32 detector_roty;          /* 1000*rotation of detector  around Y */
         INT32 detector_rotz;          /* 1000*rotation of detector  around Z */
         char reserve3[(32-28)*sizeof(INT32)];

         /* Detector parameters (128 bytes) */
         INT32 detector_type;            /* detector type */
         INT32 pixelsize_x;              /* pixel size (nanometers) */
         INT32 pixelsize_y;              /* pixel size (nanometers) */
         INT32 mean_bias;                        /* 1000*mean bias value */
         INT32 photons_per_100adu;       /* photons / 100 ADUs */
         INT32 measured_bias[MAXIMAGES]; /* 1000*mean bias value for each image*/
         INT32 measured_temperature[MAXIMAGES];  /* Temperature of each  
                                                    detector in milliKelvins */
         INT32 measured_pressure[MAXIMAGES]; /* Pressure of each  chamber 
                                               in microTorr */
         /* Retired reserve4 when MAXIMAGES set to 9 from 16 and 
            two fields removed, and temp and pressure added
          char reserve4[(32-(5+3*MAXIMAGES))*sizeof(INT32)]
         */

         /* X-ray source and optics parameters (128 bytes) */
         /* X-ray source parameters (8*4 bytes) */
         INT32 source_type;              /* (code) - target, synch. etc */
         INT32 source_dx;                /* Optics param. - (size  microns) */
         INT32 source_dy;                /* Optics param. - (size  microns) */
         INT32 source_wavelength;        /* wavelength  (femtoMeters) */
         INT32 source_power;             /* (Watts) */
         INT32 source_voltage;           /* (Volts) */
         INT32 source_current;           /* (microAmps) */
         INT32 source_bias;              /* (Volts) */
         INT32 source_polarization_x;    /* () */
         INT32 source_polarization_y;    /* () */
         char reserve_source[4*sizeof(INT32)];

         /* X-ray optics_parameters (8*4 bytes) */
         INT32 optics_type;              /* Optics type (code)*/
         INT32 optics_dx;                /* Optics param. - (size  microns) */
         INT32 optics_dy;                /* Optics param. - (size  microns) */
         INT32 optics_wavelength;        /* Optics param. - (size  microns) */
         INT32 optics_dispersion;        /* Optics param. - (*10E6) */
         INT32 optics_crossfire_x;       /* Optics param. - (microRadians) */
         INT32 optics_crossfire_y;       /* Optics param. - (microRadians) */
         INT32 optics_angle;             /* Optics param. - (monoch.  
                                                    2theta - microradians) */
         INT32 optics_polarization_x;    /* () */
         INT32 optics_polarization_y;    /* () */
         char reserve_optics[4*sizeof(INT32)];

         char reserve5[((32-28)*sizeof(INT32))];

         /* File parameters (1024 bytes) */
         char filetitle[128];            /*  Title                  */
         char filepath[128];             /* path name for data  file  */
         char filename[64];              /* name of data  file  */
         char acquire_timestamp[32];     /* date and time of  acquisition */
         char header_timestamp[32];      /* date and time of header  update  */
         char save_timestamp[32];        /* date and time file  saved */
         char file_comments[512];        /* comments, use as desired   */
         char reserve6[1024-(128+128+64+(3*32)+512)];

         /* Dataset parameters (512 bytes) */
         char dataset_comments[512];     /* comments, used as desired   */
         /* pad out to  3072 bytes */
         char pad[3072-(256+128+256+(3*128)+1024+512)];     

         } frame_header;
"""



class marheaderreader:
    """
    Class to sit and read a series of images (makes format etc only once)
    """
    def __init__(self):
        """
        Initialise internal stuff
        """
        self.names , self.fmt = make_format(cdefinition)
    def get_header(self,filename):
        """
        Reads a header from file filename
        """
        h=read_mar_header(filename)
        dict = interpret_header(h,self.fmt,self.names)
        # Append ESRF formatted stuff
        items = self.readesrfstring(dict["dataset_comments[512]"])
        for pair in items:
            dict[pair[0]]=pair[1]
        items = self.readesrfstring(dict["file_comments[512]"])
        for pair in items:
            dict[pair[0]]=pair[1]
        dict["pixelsize_x_mm"]= str(float(dict["pixelsize_x"])/1e6)
        dict["pixelsize_y_mm"]= str(float(dict["pixelsize_y"])/1e6)
        dict["integration_time_sec"]= str(float(dict["integration_time"])/1e3)
        dict["beam_y_mm"]= str(float(dict["pixelsize_y_mm"])*
                                         float(dict["beam_y"])/1000.)
        dict["beam_x_mm"]= str(float(dict["pixelsize_x_mm"])*
                                         float(dict["beam_x"])/1000.)
        
        return dict
    
    def readesrfstring(self,s):
        """
        Interpret the so called "esrf format" header lines 
        which are in comment sections
        """
        s=s.replace("\000","")
        items = filter(None, [len(x)>1 and x or None for x in [
            item.split("=") for item in s.split(";")]])
        return items


if __name__=="__main__":
    """
    Make a little program to process files
    """
    import sys
    print "Starting"
    names,fmt = make_format(cdefinition)
    print "Names and format made"
    h = read_mar_header(sys.argv[1])
    print "Read header, interpreting"
    d = interpret_header(h,fmt,names)
    printed = {}
    for name in names:
        if printed.has_key(name):
            continue
        print name,":",d[name]
        printed[name]=1

@}

\subsection{Writing out cif files for fit2d/xmas}

A script which is supposed to pick up some header information from the mar images, 
some more infomation from the user and the create cif files.

This relies on a "template" cif file to get it started (avoids me programming everything).

@O xmas/xmasheaders.py
@{#!/usr/bin/env python


import pycbf

# Some cbf helper functions - obj would be a cbf_handle_struct object

def writewavelength(obj,wavelength):
    obj.set_wavelength(float(wavelength))

def writecellpar(obj,cifname,value):
    obj.find_category("cell")
    obj.find_column(cifname)
    obj.set_value(value)

def writecell(obj,cell):
    """
    call with cell = (a,b,c,alpha,beta,gamma)
    """
    obj.find_category("cell")
    obj.find_column("length_a")
    obj.set_value(str(cell[0]))
    obj.find_column("length_b")
    obj.set_value(str(cell[1]))
    obj.find_column("length_c")
    obj.set_value(str(cell[2]))
    obj.find_column("angle_alpha")
    obj.set_value(str(cell[3]))
    obj.find_column("angle_beta")
    obj.set_value(str(cell[4]))
    obj.find_column("angle_gamma")
    obj.set_value(str(cell[5]))

def writeUB(obj,ub):
    """
    call with ub that can be indexed ub[i][j]
    """
    obj.find_category("diffrn_orient_matrix")
    for i in (1,2,3):
        for j in (1,2,3):
            obj.find_column("UB[%d][%d]"%(i,j))
            obj.set_value(str(ub[i-1][j-1]))
            
def writedistance(obj,distance):
    obj.set_axis_setting("DETECTOR_Z",float(distance),0.)
        

def writebeam_x_mm(obj,cen):
    obj.set_axis_setting("DETECTOR_X",float(cen),0.)

def writebeam_y_mm(obj,cen):
    obj.set_axis_setting("DETECTOR_Y",float(cen),0.)

def writeSPECcmd(obj,s):
    obj.find_category("diffrn_measurement")
    obj.find_column("details")
    obj.set_value(s)

def writeSPECscan(obj,s):
    obj.find_category("diffrn_scan")
    obj.find_column("id")
    obj.set_value("SCAN%s"%(s))
    obj.find_category("diffrn_scan_axis")
    obj.find_column("scan_id")
    obj.rewind_row()
    for i in range(obj.count_rows()):
        obj.select_row(i)
        obj.set_value("SCAN%s"%(s))
    obj.find_category("diffrn_scan_frame")
    obj.find_column("scan_id")
    obj.rewind_row()
    obj.set_value("SCAN%s"%(s))


def writepixelsize_y_mm(obj,s):
    """
    Units are mm for cif
    """
    # element number  = assume this is first and only detector
    element_number = 0
    # axis number = faster or slower... ? Need to check precedence ideally...
    obj.find_category("array_structure_list")
    obj.find_column("axis_set_id")
    obj.find_row("ELEMENT_Y")
    obj.find_column("precedence")
    axis_number = obj.get_integervalue()
    
    obj.set_pixel_size(element_number, axis_number, float(s) )
    
    obj.find_category("array_structure_list_axis")
    obj.find_column("axis_id")
    obj.find_row("ELEMENT_Y")
    obj.find_column("displacement")
    obj.set_doublevalue("%.6g",float(s)/2.0)
    obj.find_column("displacement_increment")
    obj.set_doublevalue("%.6g",float(s))

def writepixelsize_x_mm(obj,s):
    # element number  = assume this is first and only detector
    element_number = 0
    # axis number = faster or slower... ? Need to check precedence ideally...
    obj.find_category("array_structure_list")
    obj.find_column("axis_set_id")
    obj.find_row("ELEMENT_X")
    obj.find_column("precedence")
    axis_number = obj.get_integervalue()
    
    obj.set_pixel_size(element_number, axis_number, float(s) )
    
    obj.find_category("array_structure_list_axis")
    obj.find_column("axis_id")
    obj.find_row("ELEMENT_X")
    obj.find_column("displacement")
    obj.set_doublevalue("%.6g",float(s)/2.0)
    obj.find_column("displacement_increment")
    obj.set_doublevalue("%.6g",float(s))

def writeintegrationtime(obj,s):
    obj.find_category("diffrn_scan_frame")
    obj.find_column("integration_time")
    obj.set_value(str(s).replace("\000",""))

def writenfast(obj,s):
    obj.find_category("array_structure_list")
    obj.find_column("index")
    obj.find_row("1")
    obj.find_column("dimension")
    obj.set_value(str(s))

def writenslow(obj,s):
    obj.find_category("array_structure_list")
    obj.find_column("index")
    obj.find_row("2")
    obj.find_column("dimension")
    obj.set_value(str(s))


functiondict = {
    "lambda"   : writewavelength,
    "beam_x_mm"   : writebeam_x_mm,
    "beam_y_mm"   : writebeam_y_mm,
    "distance" : writedistance,
    "UB"       : writeUB,
    "cell"     : writecell,
    "cmd"      : writeSPECcmd,
    "scan"     : writeSPECscan,
    "nfast"    : writenfast,
    "nslow"    : writenslow,
    "pixelsize_y_mm" : writepixelsize_y_mm,
    "pixelsize_x_mm" : writepixelsize_x_mm,
    "integration_time_sec" : writeintegrationtime,
    "tth"      : lambda obj,value : obj.set_axis_setting(
                                "DETECTOR_TWO_THETA_VERTICAL",float(value),0.),
    "chi"      : lambda obj,value : obj.set_axis_setting(
                                     "GONIOMETER_CHI",float(value),0.),
    "th"       : lambda obj,value : obj.set_axis_setting(
                                     "GONIOMETER_THETA",float(value),0.),
    "phi"      : lambda obj,value : obj.set_axis_setting(
                                     "GONIOMETER_PHI",float(value),0.),
    "lc_a"     : lambda obj,value : writecellpar(obj,"length_a",value),
    "lc_b"     : lambda obj,value : writecellpar(obj,"length_b",value),
    "lc_c"     : lambda obj,value : writecellpar(obj,"length_c",value),
    "lc_al"    : lambda obj,value : writecellpar(obj,"angle_alpha",value),
    "lc_be"    : lambda obj,value : writecellpar(obj,"angle_beta",value),
    "lc_ga"    : lambda obj,value : writecellpar(obj,"angle_gamma",value)
    }

"""
    #
    # Not implementing these for now
    lc_ra
    lc_rc 0.4742
    lc_rb 1.16
    energy 13
    cp_phi -180
    alpha 7.3716
    lc_ral 90
    cp_tth -180
    lc_rga 90
    beta 17.572
    omega -2.185
    h 0.21539
    k 0.01957
    l 5.9763
    cp_chi -180
    lc_rbe 90
    cp_th -180
    azimuth 0
"""

# Finally a class for creating header files.
# It reads a template and then offers a processfile command 
# for running over a file series

class cifheader:
    
    def __init__(self,templatefile):
        self.cbf=pycbf.cbf_handle_struct()
        self.cbf.read_template(templatefile)
        from readmarheader import marheaderreader
        self.marheaderreader = marheaderreader()

        
    def processfile(self,filename, outfile=None,
                    format="mccd",
                    **kwds):
        outfile=outfile.replace(format,"cif")
        
        if format == "mccd":
            items = self.marheaderreader.get_header(filename)

        if format == "bruker":
            pass
        if format == "edf":
            pass
        
        self.items=items
        
        # Take the image header items as default
        self.updateitems(items)

        # Allow them to be overridden
        self.updateitems(kwds)

        # Write the file
        self.writefile(outfile)


        
    def writefile(self,filename):
        self.cbf.write_file(filename,pycbf.CIF,pycbf.MIME_HEADERS,
                            pycbf.ENC_BASE64)
        

    def updateitems(self,dict):
        names = dict.keys()
        for name in names:
            value = dict[name]
            # use a dictionary of functions
            if functiondict.has_key(name):
                # print "calling",functiondict[name],value
                apply(functiondict[name],(self.cbf,value))
            else:
                #print "ignoring",name,value
                pass

        
if __name__=="__main__":
    import sys
    
    obj=cifheader("xmas_cif_template.cif")

    ub = [[0.11, 0.12, 0.13] , [0.21, 0.22, 0.23], [0.31, 0.32, 0.33]]

    for filename in sys.argv[1:]:
        fileout = filename.split("/")[-1]
        obj.processfile(filename, outfile=fileout, UB=ub, distance=123.456)
@}


\subsection{A template cif file for the xmas beamline}

This was sort of copied and modified from an example file. It has NOT been checked.
Hopefully the four circle geometry at least vaguely matches what is at the beamline.

@O xmas/xmas_cif_template.cif
@{
###CBF: VERSION 0.6
# CBF file written by cbflib v0.6



data_image_1



loop_
_diffrn.id
_diffrn.crystal_id
 DS1 DIFFRN_CRYSTAL_ID

loop_
_cell.length_a                     5.959(1)
_cell.length_b                     14.956(1)
_cell.length_c                     19.737(3)
_cell.angle_alpha                  90
_cell.angle_beta                   90
_cell.angle_gamma                  90


loop_
_diffrn_orient_matrix.id 'DS1'
_diffrn_orient_matrix.type 
; reciprocal axis matrix, multiplies hkl vector to generate
  diffractometer xyz vector and diffractometer angles
;
_diffrn_orient_matrix.UB[1][1]            0.11
_diffrn_orient_matrix.UB[1][2]            0.12
_diffrn_orient_matrix.UB[1][3]            0.13
_diffrn_orient_matrix.UB[2][1]            0.21
_diffrn_orient_matrix.UB[2][2]            0.22
_diffrn_orient_matrix.UB[2][3]            0.23
_diffrn_orient_matrix.UB[3][1]            0.31
_diffrn_orient_matrix.UB[3][2]            0.32
_diffrn_orient_matrix.UB[3][3]            0.33




loop_
_diffrn_source.diffrn_id
_diffrn_source.source
_diffrn_source.current
_diffrn_source.type
 DS1 synchrotron 200.0 'XMAS beamline bm28 ESRF'

loop_
_diffrn_radiation.diffrn_id
_diffrn_radiation.wavelength_id
_diffrn_radiation.probe
_diffrn_radiation.monochromator
_diffrn_radiation.polarizn_source_ratio
_diffrn_radiation.polarizn_source_norm
_diffrn_radiation.div_x_source
_diffrn_radiation.div_y_source
_diffrn_radiation.div_x_y_source
_diffrn_radiation.collimation
 DS1 WAVELENGTH1 x-ray 'Si 111' 0.8 0.0 0.08 0.01 0.00 '0.20 mm x 0.20 mm'

loop_
_diffrn_radiation_wavelength.id
_diffrn_radiation_wavelength.wavelength
_diffrn_radiation_wavelength.wt
 WAVELENGTH1 1.73862 1.0

loop_
_diffrn_detector.diffrn_id
_diffrn_detector.id
_diffrn_detector.type
_diffrn_detector.details
_diffrn_detector.number_of_axes
 DS1 MAR 'MAR XMAS' 'slow mode' 5

loop_
_diffrn_detector_axis.detector_id
_diffrn_detector_axis.axis_id
 MAR DETECTOR_TWO_THETA_VERTICAL
 MAR DETECTOR_X
 MAR DETECTOR_Y
 MAR DETECTOR_Z
 MAR DETECTOR_PITCH

loop_
_diffrn_detector_element.id
_diffrn_detector_element.detector_id
 ELEMENT1 MAR

loop_
_diffrn_data_frame.id
_diffrn_data_frame.detector_element_id
_diffrn_data_frame.array_id
_diffrn_data_frame.binary_id
 FRAME1 ELEMENT1 ARRAY1 1

loop_
_diffrn_measurement.diffrn_id
_diffrn_measurement.id
_diffrn_measurement.number_of_axes
_diffrn_measurement.method
_diffrn_measurement.details
 DS1 GONIOMETER 3 rotation
 'i0=1.000 i1=1.000 i2=1.000 ib=1.000 beamstop=20 mm 0% attenuation'

loop_
_diffrn_measurement_axis.measurement_id
_diffrn_measurement_axis.axis_id
 GONIOMETER GONIOMETER_PHI
 GONIOMETER GONIOMETER_CHI
 GONIOMETER GONIOMETER_THETA


loop_
_diffrn_scan.id
_diffrn_scan.frame_id_start
_diffrn_scan.frame_id_end
_diffrn_scan.frames
 SCAN1 FRAME1 FRAME1 1

loop_
_diffrn_scan_axis.scan_id
_diffrn_scan_axis.axis_id
_diffrn_scan_axis.angle_start
_diffrn_scan_axis.angle_range
_diffrn_scan_axis.angle_increment
_diffrn_scan_axis.displacement_start
_diffrn_scan_axis.displacement_range
_diffrn_scan_axis.displacement_increment
 SCAN1 GONIOMETER_THETA 0.0 0.0 0.0 0.0 0.0 0.0
 SCAN1 GONIOMETER_CHI 0.0 0.0 0.0 0.0 0.0 0.0
 SCAN1 GONIOMETER_PHI 185 1 1 0.0 0.0 0.0
 SCAN1 DETECTOR_TWO_THETA_VERTICAL 0.0 0.0 0.0 0.0 0.0 0.0
 SCAN1 DETECTOR_Z 0.0 0.0 0.0 103.750 0 0
 SCAN1 DETECTOR_Y 0.0 0.0 0.0 0.0 0.0 0.0
 SCAN1 DETECTOR_X 0.0 0.0 0.0 0.0 0.0 0.0
 SCAN1 DETECTOR_PITCH 0.0 0.0 0.0 0.0 0.0 0.0

loop_
_diffrn_scan_frame.frame_id
_diffrn_scan_frame.frame_number
_diffrn_scan_frame.integration_time
_diffrn_scan_frame.scan_id
_diffrn_scan_frame.date
 FRAME1 1 360 SCAN1 1997-12-04T10:23:48

loop_
_diffrn_scan_frame_axis.frame_id
_diffrn_scan_frame_axis.axis_id
_diffrn_scan_frame_axis.angle
_diffrn_scan_frame_axis.displacement
 FRAME1 GONIOMETER_THETA 0.0 0.0
 FRAME1 GONIOMETER_CHI 0.0 0.0
 FRAME1 GONIOMETER_PHI 185 0.0
 FRAME1 DETECTOR_TWO_THETA_VERTICAL 185 0.0
 FRAME1 DETECTOR_Z 0.0 103.750
 FRAME1 DETECTOR_Y 0.0 0.0
 FRAME1 DETECTOR_X 0.0 0.0
 FRAME1 DETECTOR_PITCH 0.0 0.0

loop_
_axis.id
_axis.type
_axis.equipment
_axis.depends_on
_axis.vector[1]
_axis.vector[2]
_axis.vector[3]
_axis.offset[1]
_axis.offset[2]
_axis.offset[3]
 GONIOMETER_THETA rotation goniometer . 1 0 0 . . .
 GONIOMETER_CHI rotation goniometer GONIOMETER_THETA 0 0 1 . . .
 GONIOMETER_PHI rotation goniometer GONIOMETER_PHI 1 0 0 . . .
 SOURCE general source . 0 0 1 . . .
 GRAVITY general gravity . 0 -1 0 . . .
 DETECTOR_TWO_THETA_VERTICAL rotation goniometer . 1 0 0 . . .
 DETECTOR_Z translation detector DETECTOR_TWO_THETA_VERTICAL 0 0 -1 0 0 0
 DETECTOR_Y translation detector DETECTOR_Z 0 1 0 0 0 0
 DETECTOR_X translation detector DETECTOR_Y 1 0 0 0 0 0
 DETECTOR_PITCH rotation detector DETECTOR_X 0 1 0 0 0 0
 ELEMENT_X translation detector DETECTOR_PITCH 1 0 0 -94.0032 94.0032 0
 ELEMENT_Y translation detector ELEMENT_X 0 1 0 0 0 0

loop_
_array_structure_list.array_id
_array_structure_list.index
_array_structure_list.dimension
_array_structure_list.precedence
_array_structure_list.direction
_array_structure_list.axis_set_id
 ARRAY1 1 2049 1 increasing ELEMENT_X
 ARRAY1 2 2049 2 increasing ELEMENT_Y

loop_
_array_structure_list_axis.axis_set_id
_array_structure_list_axis.axis_id
_array_structure_list_axis.displacement
_array_structure_list_axis.displacement_increment
 ELEMENT_X ELEMENT_X 0.0408 0.0816
 ELEMENT_Y ELEMENT_Y -0.0408 -0.0816

loop_
_array_intensities.array_id
_array_intensities.binary_id
_array_intensities.linearity
_array_intensities.gain
_array_intensities.gain_esd
_array_intensities.overload
_array_intensities.undefined_value
 ARRAY1 1 linear 0.30 0.03 65000 0

loop_
_array_structure.id
_array_structure.encoding_type
_array_structure.compression_type
_array_structure.byte_order
 ARRAY1 "signed 32-bit integer" packed little_endian
@}



\end{document}
