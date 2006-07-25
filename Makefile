######################################################################
#  Makefile - command file for make to create CBFlib                 #
#                                                                    #
#  Version 0.7.2 22 April 2001                                       #
#                                                                    #
#             Paul Ellis (ellis@ssrl.slac.stanford.edu) and          #
#          Herbert J. Bernstein (yaya@bernstein-plus-sons.com)       #
#                                                                    #
#                 PLEASE READ doc/CBFlib_NOTICES.txt                 #
######################################################################
#


#
# Set the compiler and flags
#
#CC	= cc
CC	= gcc
#CFLAGS	= -O
#CFLAGS	= -g3 -O2
CFLAGS  = -g -O2

#
# Program to use to pack shars
#
SHAR	= /usr/bin/shar
#SHAR	= /usr/local/bin/gshar


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
$(SRC)/cbf_stx.c: $(SRC)/cbf.stx
	bison -d $(SRC)/cbf.stx
	mv $(SRC)/cbf.stx.tab.c $(SRC)/cbf_stx.c
	mv $(SRC)/cbf.stx.tab.h $(INCLUDE)/cbf_stx.h

#
# CBF library
#
$(LIB)/libcbf.a: $(SOURCE) $(HEADERS) $(COMMONDEP)
	-rm -f *.o
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) -c $(SOURCE)
	$(AR) cr $@ *.o

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
# Tests
#
tests:	$(BIN)/makecbf $(BIN)/img2cif $(BIN)/cif2cbf example.mar2300
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
extra:	$(BIN)/cif2cbf makecbf.cbf
	$(BIN)/cif2cbf -e hex -c none \
		makecbf.cbf cif2cbf_ehcn.cif
	$(BIN)/cif2cbf -e none -c packed \
		cif2cbf_ehcn.cif cif2cbf_encp.cbf
	-cmp makecbf.cbf cif2cbf_encp.cbf


#
# Remove all non-source files
#
empty:  clean
	@-rm -f  $(LIB)/libcbf.a
	@-rm -f  $(BIN)/makecbf
	@-rm -f  $(BIN)/img2cif
	@-rm -f  $(BIN)/cif2cbf
	@-rm -f  makecbf.cbf
	@-rm -f  img2cif_packed.cif
	@-rm -f  img2cif_canonical.cif
	@-rm -f  img2cif_packed.cbf
	@-rm -f  img2cif_canonical.cbf
	@-rm -f  img2cif_raw.cbf
	@-rm -f  cif2cbf_packed.cbf
	@-rm -f  cif2cbf_canonical.cbf

#
# Remove temporary files
#
clean:
	@-rm -f core 
	@-rm -f *.o
	@-rm -f *.u

#
# Create a Shell Archive for distribution
#

shar:   $(DOCUMENTS) $(SOURCE) $(SRC)/cbf.stx $(HEADERS) \
         $(EXAMPLES)/img.c \
	 $(EXAMPLES)/img.h \
	 $(EXAMPLES)/makecbf.c $(EXAMPLES)/img2cif.c $(EXAMPLES)/cif2cbf.c \
	 $(EXAMPLES)/convert_image.c \
	 $(EXAMPLES)/template_adscquantum4_2304x2304.cbf \
	 $(EXAMPLES)/template_mar345_2300x2300.cbf \
	 README.html README Makefile \
	 $(JPEGS)
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
	 $(JPEGS)
	mv CBFlib.shar.01 CBFlib.shar
	compress CBFlib.shar

#
# Create a Tape Archive for distribution
#

tar:   $(DOCUMENTS) $(SOURCE) $(SRC)/cbf.stx $(HEADERS) \
         $(EXAMPLES)/img.c \
	 $(EXAMPLES)/img.h \
	 $(EXAMPLES)/makecbf.c $(EXAMPLES)/img2cif.c $(EXAMPLES)/cif2cbf.c \
	 $(EXAMPLES)/convert_image.c \
	 $(EXAMPLES)/template_adscquantum4_2304x2304.cbf \
	 $(EXAMPLES)/template_mar345_2300x2300.cbf \
	 README.html README Makefile \
	 $(JPEGS)
	-/bin/rm -f CBFlib.tar*
	tar cvBf CBFlib.tar \
	 $(DOCUMENTS) $(SOURCE) $(SRC)/cbf.stx $(HEADERS) \
         $(EXAMPLES)/img.c \
	 $(EXAMPLES)/img.h \
	 $(EXAMPLES)/makecbf.c $(EXAMPLES)/img2cif.c $(EXAMPLES)/cif2cbf.c \
	 $(EXAMPLES)/convert_image.c \
	 $(EXAMPLES)/template_adscquantum4_2304x2304.cbf \
	 $(EXAMPLES)/template_mar345_2300x2300.cbf \
	 README.html README Makefile \
	 $(JPEGS)
	gzip --best CBFlib.tar

