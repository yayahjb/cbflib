######################################################################
#  Makefile - command file for make to create CBFlib                 #
#                                                                    #
# Version 0.7.5 12 April 2006                                        #
#                                                                    #
#                          Paul Ellis and                            #
#         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        #
#                                                                    #
# (C) Copyright 2006 Herbert J. Bernstein                            #
#                                                                    #
######################################################################

######################################################################
#                                                                    #
# YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL #
#                                                                    #
# ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  #
# OF THE LGPL                                                        #
#                                                                    #
######################################################################

########################### GPL NOTICES ##############################
#                                                                    #
# This program is free software; you can redistribute it and/or      #
# modify it under the terms of the GNU General Public License as     #
# published by the Free Software Foundation; either version 2 of     #
# (the License, or (at your option) any later version.               #
#                                                                    #
# This program is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      #
# GNU General Public License for more details.                       #
#                                                                    #
# You should have received a copy of the GNU General Public License  #
# along with this program; if not, write to the Free Software        #
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           #
# 02111-1307  USA                                                    #
#                                                                    #
######################################################################

######################### LGPL NOTICES ###############################
#                                                                    #
# This library is free software; you can redistribute it and/or      #
# modify it under the terms of the GNU Lesser General Public         #
# License as published by the Free Software Foundation; either       #
# version 2.1 of the License, or (at your option) any later version. #
#                                                                    #
# This library is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  #
# Lesser General Public License for more details.                    #
#                                                                    #
# You should have received a copy of the GNU Lesser General Public   #
# License along with this library; if not, write to the Free         #
# Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    #
# MA  02110-1301  USA                                                #
#                                                                    #
######################################################################

######################################################################
#                                                                    #
#                    Stanford University Notices                     #
#  for the CBFlib software package that incorporates SLAC software   #
#                 on which copyright is disclaimed                   #
#                                                                    #
# This software                                                      #
# -------------                                                      #
# The term "this software", as used in these Notices, refers to      #
# those portions of the software package CBFlib that were created by #
# employees of the Stanford Linear Accelerator Center, Stanford      #
# University.                                                        #
#                                                                    #
# Stanford disclaimer of copyright                                   #
# --------------------------------                                   #
# Stanford University, owner of the copyright, hereby disclaims its  #
# copyright and all other rights in this software.  Hence, anyone    #
# may freely use it for any purpose without restriction.             #
#                                                                    #
# Acknowledgement of sponsorship                                     #
# ------------------------------                                     #
# This software was produced by the Stanford Linear Accelerator      #
# Center, Stanford University, under Contract DE-AC03-76SFO0515 with #
# the Department of Energy.                                          #
#                                                                    #
# Government disclaimer of liability                                 #
# ----------------------------------                                 #
# Neither the United States nor the United States Department of      #
# Energy, nor any of their employees, makes any warranty, express or #
# implied, or assumes any legal liability or responsibility for the  #
# accuracy, completeness, or usefulness of any data, apparatus,      #
# product, or process disclosed, or represents that its use would    #
# not infringe privately owned rights.                               #
#                                                                    #
# Stanford disclaimer of liability                                   #
# --------------------------------                                   #
# Stanford University makes no representations or warranties,        #
# express or implied, nor assumes any liability for the use of this  #
# software.                                                          #
#                                                                    #
# Maintenance of notices                                             #
# ----------------------                                             #
# In the interest of clarity regarding the origin and status of this #
# software, this and all the preceding Stanford University notices   #
# are to remain affixed to any copy or derivative of this software   #
# made or distributed by the recipient and are to be affixed to any  #
# copy of software made or distributed by the recipient that         #
# contains a copy or derivative of this software.                    #
#                                                                    #
# Based on SLAC Software Notices, Set 4                              #
# OTT.002a, 2004 FEB 03                                              #
######################################################################



######################################################################
#                               NOTICE                               #
# Creative endeavors depend on the lively exchange of ideas. There   #
# are laws and customs which establish rights and responsibilities   #
# for authors and the users of what authors create.  This notice     #
# is not intended to prevent you from using the software and         #
# documents in this package, but to ensure that there are no         #
# misunderstandings about terms and conditions of such use.          #
#                                                                    #
# Please read the following notice carefully.  If you do not         #
# understand any portion of this notice, please seek appropriate     #
# professional legal advice before making use of the software and    #
# documents included in this software package.  In addition to       #
# whatever other steps you may be obliged to take to respect the     #
# intellectual property rights of the various parties involved, if   #
# you do make use of the software and documents in this package,     #
# please give credit where credit is due by citing this package,     #
# its authors and the URL or other source from which you obtained    #
# it, or equivalent primary references in the literature with the    #
# same authors.                                                      #
#                                                                    #
# Some of the software and documents included within this software   #
# package are the intellectual property of various parties, and      #
# placement in this package does not in any way imply that any       #
# such rights have in any way been waived or diminished.             #
#                                                                    #
# With respect to any software or documents for which a copyright    #
# exists, ALL RIGHTS ARE RESERVED TO THE OWNERS OF SUCH COPYRIGHT.   #
#                                                                    #
# Even though the authors of the various documents and software      #
# found here have made a good faith effort to ensure that the        #
# documents are correct and that the software performs according     #
# to its documentation, and we would greatly appreciate hearing of   #
# any problems you may encounter, the programs and documents any     #
# files created by the programs are provided **AS IS** without any   *
# warranty as to correctness, merchantability or fitness for any     #
# particular or general use.                                         #
#                                                                    #
# THE RESPONSIBILITY FOR ANY ADVERSE CONSEQUENCES FROM THE USE OF    #
# PROGRAMS OR DOCUMENTS OR ANY FILE OR FILES CREATED BY USE OF THE   #
# PROGRAMS OR DOCUMENTS LIES SOLELY WITH THE USERS OF THE PROGRAMS   #
# OR DOCUMENTS OR FILE OR FILES AND NOT WITH AUTHORS OF THE          #
# PROGRAMS OR DOCUMENTS.                                             #
######################################################################

######################################################################
#                                                                    #
#                           The IUCr Policy                          #
#      for the Protection and the Promotion of the STAR File and     #
#     CIF Standards for Exchanging and Archiving Electronic Data     #
#                                                                    #
# Overview                                                           #
#                                                                    #
# The Crystallographic Information File (CIF)[1] is a standard for   #
# information interchange promulgated by the International Union of  #
# Crystallography (IUCr). CIF (Hall, Allen & Brown, 1991) is the     #
# recommended method for submitting publications to Acta             #
# Crystallographica Section C and reports of crystal structure       #
# determinations to other sections of Acta Crystallographica         #
# and many other journals. The syntax of a CIF is a subset of the    #
# more general STAR File[2] format. The CIF and STAR File approaches #
# are used increasingly in the structural sciences for data exchange #
# and archiving, and are having a significant influence on these     #
# activities in other fields.                                        #
#                                                                    #
# Statement of intent                                                #
#                                                                    #
# The IUCr's interest in the STAR File is as a general data          #
# interchange standard for science, and its interest in the CIF,     #
# a conformant derivative of the STAR File, is as a concise data     #
# exchange and archival standard for crystallography and structural  #
# science.                                                           #
#                                                                    #
# Protection of the standards                                        #
#                                                                    #
# To protect the STAR File and the CIF as standards for              #
# interchanging and archiving electronic data, the IUCr, on behalf   #
# of the scientific community,                                       #
#                                                                    #
# # holds the copyrights on the standards themselves,                *
#                                                                    #
# # owns the associated trademarks and service marks, and            *
#                                                                    #
# # holds a patent on the STAR File.                                 *
#                                                                    #
# These intellectual property rights relate solely to the            #
# interchange formats, not to the data contained therein, nor to     #
# the software used in the generation, access or manipulation of     #
# the data.                                                          #
#                                                                    #
# Promotion of the standards                                         #
#                                                                    #
# The sole requirement that the IUCr, in its protective role,        #
# imposes on software purporting to process STAR File or CIF data    #
# is that the following conditions be met prior to sale or           #
# distribution.                                                      #
#                                                                    #
# # Software claiming to read files written to either the STAR       *
# File or the CIF standard must be able to extract the pertinent     #
# data from a file conformant to the STAR File syntax, or the CIF    #
# syntax, respectively.                                              #
#                                                                    #
# # Software claiming to write files in either the STAR File, or     *
# the CIF, standard must produce files that are conformant to the    #
# STAR File syntax, or the CIF syntax, respectively.                 #
#                                                                    #
# # Software claiming to read definitions from a specific data       *
# dictionary approved by the IUCr must be able to extract any        #
# pertinent definition which is conformant to the dictionary         #
# definition language (DDL)[3] associated with that dictionary.      #
#                                                                    #
# The IUCr, through its Committee on CIF Standards, will assist      #
# any developer to verify that software meets these conformance      #
# conditions.                                                        #
#                                                                    #
# Glossary of terms                                                  #
#                                                                    #
# [1] CIF:  is a data file conformant to the file syntax defined     #
# at http://www.iucr.org/iucr-top/cif/spec/index.html                #
#                                                                    #
# [2] STAR File:  is a data file conformant to the file syntax       #
# defined at http://www.iucr.org/iucr-top/cif/spec/star/index.html   #
#                                                                    #
# [3] DDL:  is a language used in a data dictionary to define data   #
# items in terms of "attributes". Dictionaries currently approved    #
# by the IUCr, and the DDL versions used to construct these          #
# dictionaries, are listed at                                        #
# http://www.iucr.org/iucr-top/cif/spec/ddl/index.html               #
#                                                                    #
# Last modified: 30 September 2000                                   #
#                                                                    #
# IUCr Policy Copyright (C) 2000 International Union of              #
# Crystallography                                                    #
######################################################################


#
# Set the compiler and flags
#
#CC	= cc
CC	= gcc
C++	= g++
#CFLAGS	= -O
#CFLAGS	= -g3 -O2
CFLAGS  = -g  -Wall

#
# Program to use to pack shars
#
SHAR	= /usr/bin/shar
#SHAR	= /usr/local/bin/gshar

#
# Program to use to create archives
#
AR	= /usr/bin/ar

#
# Program to use to add an index to an archive
#
RANLIB  = /usr/bin/ranlib

#
# Directories
#
ROOT     = .
LIB      = $(ROOT)/lib
BIN      = $(ROOT)/bin
SRC      = $(ROOT)/src
INCLUDE  = $(ROOT)/include
EXAMPLES = $(ROOT)/examples
DOC      = $(ROOT)/doc
GRAPHICS = $(ROOT)/html_graphics

#
# Include directories
#
INCLUDES = -I$(INCLUDE) -I$(SRC)

######################################################################
#  You should not need to make modifications below this line         #
######################################################################

#
# Suffixes of files to be used or built
#
.SUFFIXES:	.c .o

#
# Common dependencies
#
COMMONDEP = Makefile

#
# Source files
#
SOURCE   =  $(SRC)/cbf.c               \
            $(SRC)/cbf_alloc.c         \
            $(SRC)/cbf_ascii.c         \
            $(SRC)/cbf_binary.c        \
            $(SRC)/cbf_byte_offset.c   \
            $(SRC)/cbf_canonical.c     \
            $(SRC)/cbf_codes.c         \
            $(SRC)/cbf_compress.c      \
            $(SRC)/cbf_context.c       \
            $(SRC)/cbf_file.c          \
            $(SRC)/cbf_lex.c           \
            $(SRC)/cbf_packed.c        \
            $(SRC)/cbf_predictor.c     \
            $(SRC)/cbf_read_binary.c   \
            $(SRC)/cbf_read_mime.c     \
            $(SRC)/cbf_simple.c        \
            $(SRC)/cbf_string.c        \
            $(SRC)/cbf_stx.c           \
            $(SRC)/cbf_tree.c          \
            $(SRC)/cbf_uncompressed.c  \
            $(SRC)/cbf_write.c         \
            $(SRC)/cbf_write_binary.c  \
            $(SRC)/md5c.c
            
#
# Header files
#
HEADERS   =  $(INCLUDE)/cbf.h                  \
             $(INCLUDE)/cbf_alloc.h            \
             $(INCLUDE)/cbf_ascii.h            \
             $(INCLUDE)/cbf_binary.h           \
             $(INCLUDE)/cbf_byte_offset.h      \
             $(INCLUDE)/cbf_canonical.h        \
             $(INCLUDE)/cbf_codes.h            \
             $(INCLUDE)/cbf_compress.h         \
             $(INCLUDE)/cbf_context.h          \
             $(INCLUDE)/cbf_file.h             \
             $(INCLUDE)/cbf_lex.h              \
             $(INCLUDE)/cbf_packed.h           \
             $(INCLUDE)/cbf_predictor.h        \
             $(INCLUDE)/cbf_read_binary.h      \
             $(INCLUDE)/cbf_read_mime.h        \
             $(INCLUDE)/cbf_simple.h           \
             $(INCLUDE)/cbf_string.h           \
             $(INCLUDE)/cbf_stx.h              \
             $(INCLUDE)/cbf_tree.h             \
             $(INCLUDE)/cbf_uncompressed.h     \
             $(INCLUDE)/cbf_write.h            \
             $(INCLUDE)/cbf_write_binary.h     \
             $(INCLUDE)/global.h                   \
             $(INCLUDE)/md5.h


#
# Documentation files
#
DOCUMENTS = $(DOC)/CBFlib.pdf                   \
            $(DOC)/CBFlib.rtf                   \
            $(DOC)/CBFlib_NOTICES.html          \
            $(DOC)/CBFlib_NOTICES.txt           \
            $(DOC)/ChangeLog                    \
            $(DOC)/ChangeLog.html               \
            $(DOC)/MANIFEST                     \
            $(DOC)/example.html

#
# HTML Graphics files
#
JPEGS     = $(GRAPHICS)/CBFbackground.jpg      \
            $(GRAPHICS)/CBFbig.jpg             \
            $(GRAPHICS)/CBFbutton.jpg          \
            $(GRAPHICS)/cbflibbackground.jpg   \
            $(GRAPHICS)/cbflibbig.jpg          \
            $(GRAPHICS)/cbflibbutton.jpg       \
            $(GRAPHICS)/cifhome.jpg            \
            $(GRAPHICS)/iucrhome.jpg           \
            $(GRAPHICS)/noticeButton.jpg


#
# Default: instructions
#
default:
	@echo ' '
	@echo '***************************************************************'
	@echo ' '
	@echo ' PLEASE READ README and doc/CBFlib_NOTICES.txt'
	@echo ' '
	@echo ' Before making the CBF library and example programs, check'
	@echo ' that the C compiler name and flags are correct:'
	@echo ' '
	@echo ' The current values are:'
	@echo ' '
	@echo '   $(CC) $(CFLAGS)'
	@echo ' '
	@echo ' To compile the CBF library and example programs type:'
	@echo ' '
	@echo '   make all'
	@echo ' '
	@echo ' To run a set of tests type:'
	@echo ' '
	@echo '   make tests'
	@echo ' '
	@echo ' The tests assume that "example.mar2300" is in this directory'
	@echo ' This file can be obtained from'
	@echo ' '
	@echo '   http://smb.slac.stanford.edu/~ellis/'
	@echo ' '
	@echo ' To clean up the directories type:'
	@echo ' '
	@echo '   make clean'
	@echo ' '
	@echo '***************************************************************'
	@echo ' '

#
# Compile the library and examples
#
all:	$(LIB) $(BIN)            \
	$(LIB)/libcbf.a          \
        $(BIN)/convert_image     \
        $(BIN)/makecbf           \
        $(BIN)/img2cif           \
        $(BIN)/cif2cbf           \
	$(BIN)/testcell          \
	$(BIN)/cif2c             \
        clean

#
# Directories
#
$(LIB):
	mkdir $(LIB)

$(BIN):
	mkdir $(BIN)

#
# Parser
#
$(SRC)/cbf_stx.c: $(SRC)/cbf.stx.y
	bison $(SRC)/cbf.stx.y -o $(SRC)/cbf.stx.tab.c -d
	mv $(SRC)/cbf.stx.tab.c $(SRC)/cbf_stx.c
	mv $(SRC)/cbf.stx.tab.h $(INCLUDE)/cbf_stx.h

#
# CBF library
#
$(LIB)/libcbf.a: $(SOURCE) $(HEADERS) $(COMMONDEP)
	-rm -f *.o
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) -c $(SOURCE)
	$(AR) cr $@ *.o
	$(RANLIB) $@

#
# convert_image example program
#
$(BIN)/convert_image: $(LIB)/libcbf.a $(EXAMPLES)/convert_image.c $(EXAMPLES)/img.c
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) \
              $(EXAMPLES)/convert_image.c $(EXAMPLES)/img.c -L$(LIB) \
	      -lcbf -lm -o $@

#
# makecbf example program
#
$(BIN)/makecbf: $(LIB)/libcbf.a $(EXAMPLES)/makecbf.c $(EXAMPLES)/img.c
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) \
              $(EXAMPLES)/makecbf.c $(EXAMPLES)/img.c -L$(LIB) \
	      -lcbf -lm -o $@

#
# img2cif example program
#
$(BIN)/img2cif: $(LIB)/libcbf.a $(EXAMPLES)/img2cif.c $(EXAMPLES)/img.c
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) \
              $(EXAMPLES)/img2cif.c $(EXAMPLES)/img.c -L$(LIB) \
	      -lcbf -lm -o $@

#
# cif2cbf example program
#
$(BIN)/cif2cbf: $(LIB)/libcbf.a $(EXAMPLES)/cif2cbf.c $(EXAMPLES)/img.c
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) \
              $(EXAMPLES)/cif2cbf.c $(EXAMPLES)/img.c -L$(LIB) \
	      -lcbf -lm -o $@
#
# testcell example program
#
$(BIN)/testcell: $(LIB)/libcbf.a $(EXAMPLES)/testcell.C
	$(C++) $(CFLAGS) $(INCLUDES) $(WARNINGS) \
              $(EXAMPLES)/testcell.C -L$(LIB) \
	      -lcbf -lm -o $@

#
# cif2c example program
#
$(BIN)/cif2c: $(LIB)/libcbf.a $(EXAMPLES)/cif2c.c
	$(C++) $(CFLAGS) $(INCLUDES) $(WARNINGS) \
              $(EXAMPLES)/cif2c.c -L$(LIB) \
	      -lcbf -lm -o $@

#
# Data files for tests
#

example.mar2300:
	@echo '***************************************************************'
	@echo ' '
	@echo ' Please retrieve example.mar2300 from
	@echo '   http://smb.slac.stanford.edu/~ellis/'
	@echo ' '
	@echo '***************************************************************'

9ins.cif:	9ins.cif.gz
	gunzip < 9ins.cif.gz > 9ins.cif

#
# Tests
#
tests:	$(LIB) $(BIN) basic extra

#
# Basic Tests
#

basic:	$(BIN)/makecbf $(BIN)/img2cif $(BIN)/cif2cbf example.mar2300
	$(BIN)/makecbf example.mar2300 makecbf.cbf
	$(BIN)/img2cif -c packed -m headers -d digest \
		-e base64 < example.mar2300 > img2cif_packed.cif
	$(BIN)/img2cif -c canonical -m headers -d digest \
		-e base64 < example.mar2300 > img2cif_canonical.cif
	$(BIN)/img2cif -c packed -m headers -d digest \
		-e none < example.mar2300 > img2cif_packed.cbf
	$(BIN)/img2cif -c canonical -m headers -d digest \
		-e none < example.mar2300 > img2cif_canonical.cbf
	$(BIN)/cif2cbf -e none -c packed \
		img2cif_canonical.cif cif2cbf_packed.cbf
	$(BIN)/cif2cbf -e none -c canonical \
		img2cif_packed.cif cif2cbf_canonical.cbf
	-cmp cif2cbf_packed.cbf    makecbf.cbf
	-cmp cif2cbf_packed.cbf    img2cif_packed.cbf
	-cmp cif2cbf_canonical.cbf img2cif_canonical.cbf

#
# Extra Tests
#
extra:	$(BIN)/convert_image $(BIN)/cif2cbf $(BIN)/testcell\
	makecbf.cbf 9ins.cif example.mar2300 converted_orig.cbf adscconverted_original.cbf
	$(BIN)/cif2cbf -e hex -c none \
		makecbf.cbf cif2cbf_ehcn.cif
	$(BIN)/cif2cbf -e none -c packed \
		cif2cbf_ehcn.cif cif2cbf_encp.cbf
	-cmp makecbf.cbf cif2cbf_encp.cbf
	$(BIN)/cif2cbf -i 9ins.cif -o 9ins.cbf
	-cmp 9ins.cif 9ins.cbf
	$(BIN)/convert_image example.mar2300 converted.cbf
	-cmp converted.cbf converted_orig.cbf
	$(BIN)/testcell < testcell.dat > testcell.prt
	-cmp testcell.prt testcell_orig.prt
	$(BIN)/convert_image -m x=y -r 2 -d adscquantum315 mb_LP_1_001.img adscconverted.cbf
	-cmp adscconverted.cbf adscconverted_original.cbf


#
# Remove all non-source files
#
empty:
	@-rm -f  $(LIB)/libcbf.a
	@-rm -f  $(BIN)/makecbf
	@-rm -f  $(BIN)/img2cif
	@-rm -f  $(BIN)/cif2cbf
	@-rm -f  $(BIN)/convert_image
	@-rm -f  $(BIN)/testcell
	@-rm -f  $(BIN)/cif2c
	@-rm -f  makecbf.cbf
	@-rm -f  img2cif_packed.cif
	@-rm -f  img2cif_canonical.cif
	@-rm -f  img2cif_packed.cbf
	@-rm -f  img2cif_canonical.cbf
	@-rm -f  img2cif_raw.cbf
	@-rm -f  cif2cbf_packed.cbf
	@-rm -f  cif2cbf_canonical.cbf
	@-rm -f  converted.cbf
	@-rm -f  adscconverted.cbf
	@-rm -f  cif2cbf_ehcn.cif
	@-rm -f  cif2cbf_ehcn.cbf
	@-rm -f  9ins.cbf
	@-rm -f  9ins.cif
	@-rm -f  testcell.prt

#
# Remove temporary files
#
clean:
	@-rm -f core 
	@-rm -f *.o
	@-rm -f *.u
#
# Restore to distribution state
#
distclean:	clean empty

#
# Create a Shell Archive for distribution
#

shar:   $(DOCUMENTS) $(SOURCE) $(SRC)/cbf.stx $(HEADERS) \
         $(EXAMPLES)/img.c \
	 $(EXAMPLES)/img.h \
	 $(EXAMPLES)/makecbf.c $(EXAMPLES)/img2cif.c $(EXAMPLES)/cif2cbf.c \
	 $(EXAMPLES)/convert_image.c $(EXAMPLES)/testcell.C\
	 $(EXAMPLES)/template_adscquantum4_2304x2304.cbf \
	 $(EXAMPLES)/template_mar345_2300x2300.cbf \
	 README.html README Makefile \
	 $(JPEGS) 9ins.cif.gz
	-/bin/rm -f CBFlib.shar*
	$(SHAR) -p -o CBFlib.shar -n CBFlib.shar -M \
	 $(DOCUMENTS) $(SOURCE) $(SRC)/cbf.stx $(HEADERS) \
         $(EXAMPLES)/img.c \
	 $(EXAMPLES)/img.h \
	 $(EXAMPLES)/makecbf.c $(EXAMPLES)/img2cif.c $(EXAMPLES)/cif2cbf.c \
	 $(EXAMPLES)/convert_image.c \
	 $(EXAMPLES)/template_adscquantum4_2304x2304.cbf \
	 $(EXAMPLES)/template_mar345_2300x2300.cbf \
	 README.html README Makefile \
	 $(JPEGS) 9ins.cif.gz
	mv CBFlib.shar.01 CBFlib.shar
	compress CBFlib.shar

#
# Create a Tape Archive for distribution
#

tar:   $(DOCUMENTS) $(SOURCE) $(SRC)/cbf.stx $(HEADERS) \
         $(EXAMPLES)/img.c \
	 $(EXAMPLES)/img.h \
	 $(EXAMPLES)/makecbf.c $(EXAMPLES)/img2cif.c $(EXAMPLES)/cif2cbf.c \
	 $(EXAMPLES)/convert_image.c $(EXAMPLES)/testcell.C\
	 $(EXAMPLES)/template_adscquantum4_2304x2304.cbf \
	 $(EXAMPLES)/template_mar345_2300x2300.cbf \
	 $(EXAMPLES)/template_adscquantum315_3072x3072.cbf \
	 README.html README Makefile \
	 $(JPEGS) 9ins.cif.gz testcell.dat testcell_orig.prt \
	 converted_orig.cbf adscconverted_original.cbf
	-/bin/rm -f CBFlib.tar*
	tar cvBf CBFlib.tar \
	 $(DOCUMENTS) $(SOURCE) $(SRC)/cbf.stx $(HEADERS) \
         $(EXAMPLES)/img.c \
	 $(EXAMPLES)/img.h \
	 $(EXAMPLES)/makecbf.c $(EXAMPLES)/img2cif.c $(EXAMPLES)/cif2cbf.c \
	 $(EXAMPLES)/convert_image.c $(EXAMPLES)/testcell.C\
	 $(EXAMPLES)/template_adscquantum4_2304x2304.cbf \
	 $(EXAMPLES)/template_mar345_2300x2300.cbf \
	 README.html README Makefile \
	 $(JPEGS) 9ins.cif.gz testcell.dat testcell_orig.prt \
	 converted_orig.cbf \
	 adscconverted_original.cbf
	 gzip --best CBFlib.tar
