######################################################################
#  Makefile - command file for make to create CBFlib                 #
#                                                                    #
# Version 0.4 15 November 1998                                       #
#                                                                    #
#             Paul Ellis (ellis@ssrl.slac.stanford.edu) and          #
#          Herbert J. Bernstein (yaya@bernstein-plus-sons.com)       #
#                                                                    #
#                 PLEASE READ doc/CBFlib_NOTICES.txt                 #
######################################################################


#
# Set the compiler and flags
#
CC	= cc
#CC	= gcc
CFLAGS	= -g

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
            $(SRC)/cbf_string.c        \
            $(SRC)/cbf_stx.c           \
            $(SRC)/cbf_tree.c          \
            $(SRC)/cbf_uncompressed.c  \
            $(SRC)/cbf_write.c         \
            $(SRC)/cbf_write_binary.c  \
            $(SRC)/md5c.c
            
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
	@echo '   http://biosg1.slac.stanford.edu/biosg1-users/ellis/Public/'
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
# CBF library
#
$(LIB)/libcbf.a: $(SOURCE) $(COMMONDEP)
	-rm -f *.o
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) -c $(SOURCE)
	$(AR) cr $@ *.o

#
# makecbf example program
#
$(BIN)/makecbf: $(LIB)/libcbf.a $(EXAMPLES)/makecbf.c $(EXAMPLES)/img.c
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) \
              $(EXAMPLES)/makecbf.c $(EXAMPLES)/img.c -L$(LIB) \
	      -lcbf -lcbf -lm -o $@

#
# img2cif example program
#
$(BIN)/img2cif: $(LIB)/libcbf.a $(EXAMPLES)/img2cif.c $(EXAMPLES)/img.c
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) \
              $(EXAMPLES)/img2cif.c $(EXAMPLES)/img.c -L$(LIB) \
	      -lcbf -lcbf -lm -o $@

#
# cif2cbf example program
#
$(BIN)/cif2cbf: $(LIB)/libcbf.a $(EXAMPLES)/cif2cbf.c $(EXAMPLES)/img.c
	$(CC) $(CFLAGS) $(INCLUDES) $(WARNINGS) \
              $(EXAMPLES)/cif2cbf.c $(EXAMPLES)/img.c -L$(LIB) \
	      -lcbf -lcbf -lm -o $@

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
	$(BIN)/img2cif -c canonical -m noheaders -d digest \
		-e none < example.mar2300 > img2cif_raw.cbf
	$(BIN)/cif2cbf -e none -c packed \
		img2cif_canonical.cif cif2cbf_packed.cbf
	$(BIN)/cif2cbf -e none -c canonical \
		img2cif_packed.cif cif2cbf_canonical.cbf
	-cmp cif2cbf_packed.cbf makecbf.cbf
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

shar:	doc/CBFlib_NOTICES.txt doc/CBFlib.ps doc/CBFlib.pdf doc/CBFlib.html \
	 doc/MANIFEST Makefile README doc/cbf_definition_rev.txt \
	 doc/cbf_definition_rev.html doc/cbfext98.dic doc/cbfext98.html \
	 src/cbf.c include/cbf.h src/cbf_alloc.c include/cbf_alloc.h \
	 src/cbf_ascii.c include/cbf_ascii.h src/cbf_binary.c \
	 include/cbf_binary.h src/cbf_byte_offset.c include/cbf_byte_offset.h \
	 src/cbf_canonical.c include/cbf_canonical.h src/cbf_codes.c \
	 include/cbf_codes.h src/cbf_compress.c include/cbf_compress.h \
	 src/cbf_context.c include/cbf_context.h src/cbf_file.c \
	 include/cbf_file.h src/cbf_lex.c include/cbf_lex.h src/cbf_packed.c \
	 include/cbf_packed.h src/cbf_predictor.c include/cbf_predictor.h \
	 src/cbf_read_binary.c include/cbf_read_binary.h src/cbf_read_mime.c \
	 include/cbf_read_mime.h src/cbf_string.c include/cbf_string.h \
	 src/cbf_stx.c include/cbf_stx.h src/cbf_tree.c include/cbf_tree.h \
	 src/cbf_uncompressed.c include/cbf_uncompressed.h src/cbf_write.c \
	 include/cbf_write.h src/cbf_write_binary.c \
	 include/cbf_write_binary.h src/cbf.stx examples/img.c \
	 examples/img.h src/md5c.c src/global.h src/md5.h \
	 examples/makecbf.c examples/img2cif.c examples/cif2cbf.c \
	 README.html doc/CBFlib_NOTICES.html doc/example.html \
	 html_graphics/CBFbackground.jpg \
	 html_graphics/CBFbig.jpg \
	 html_graphics/CBFbutton.jpg \
	 html_graphics/cbflibbackground.jpg \
	 html_graphics/cbflibbig.jpg \
	 html_graphics/cbflibbutton.jpg \
	 html_graphics/cifhome.jpg \
	 html_graphics/iucrhome.jpg \
	 html_graphics/noticeButton.jpg
	-/bin/rm -f CBFlib.shar*
	$(SHAR) -p -o CBFlib.shar -n CBFlib.shar -M -T \
	 doc/CBFlib_NOTICES.txt doc/CBFlib.ps \
	 -B doc/CBFlib.pdf -T doc/CBFlib.html \
	 doc/MANIFEST Makefile README doc/cbf_definition_rev.txt \
	 doc/cbf_definition_rev.html doc/cbfext98.dic doc/cbfext98.html \
	 src/cbf.c include/cbf.h src/cbf_alloc.c include/cbf_alloc.h \
	 src/cbf_ascii.c include/cbf_ascii.h src/cbf_binary.c \
	 include/cbf_binary.h src/cbf_byte_offset.c include/cbf_byte_offset.h \
	 src/cbf_canonical.c include/cbf_canonical.h src/cbf_codes.c \
	 include/cbf_codes.h src/cbf_compress.c include/cbf_compress.h \
	 src/cbf_context.c include/cbf_context.h src/cbf_file.c \
	 include/cbf_file.h src/cbf_lex.c include/cbf_lex.h src/cbf_packed.c \
	 include/cbf_packed.h src/cbf_predictor.c include/cbf_predictor.h \
	 src/cbf_read_binary.c include/cbf_read_binary.h src/cbf_read_mime.c \
	 include/cbf_read_mime.h src/cbf_string.c include/cbf_string.h \
	 src/cbf_stx.c include/cbf_stx.h src/cbf_tree.c include/cbf_tree.h \
	 src/cbf_uncompressed.c include/cbf_uncompressed.h src/cbf_write.c \
	 include/cbf_write.h src/cbf_write_binary.c \
	 include/cbf_write_binary.h src/cbf.stx examples/img.c \
	 examples/img.h src/md5c.c src/global.h src/md5.h \
	 examples/makecbf.c examples/img2cif.c examples/cif2cbf.c \
	 README.html doc/CBFlib_NOTICES.html doc/example.html -B \
	 html_graphics/CBFbackground.jpg \
	 html_graphics/CBFbig.jpg \
	 html_graphics/CBFbutton.jpg \
	 html_graphics/cbflibbackground.jpg \
	 html_graphics/cbflibbig.jpg \
	 html_graphics/cbflibbutton.jpg \
	 html_graphics/cifhome.jpg \
	 html_graphics/iucrhome.jpg \
	 html_graphics/noticeButton.jpg
	mv CBFlib.shar.01 CBFlib.shar
	compress CBFlib.shar

#
# Create a Tape Archive for distribution
#

tar:	doc/CBFlib_NOTICES.txt doc/CBFlib.ps doc/CBFlib.pdf doc/CBFlib.html \
	 doc/MANIFEST Makefile README doc/cbf_definition_rev.txt \
	 doc/cbf_definition_rev.html doc/cbfext98.dic doc/cbfext98.html \
	 src/cbf.c include/cbf.h src/cbf_alloc.c include/cbf_alloc.h \
	 src/cbf_ascii.c include/cbf_ascii.h src/cbf_binary.c \
	 include/cbf_binary.h src/cbf_byte_offset.c include/cbf_byte_offset.h \
	 src/cbf_canonical.c include/cbf_canonical.h src/cbf_codes.c \
	 include/cbf_codes.h src/cbf_compress.c include/cbf_compress.h \
	 src/cbf_context.c include/cbf_context.h src/cbf_file.c \
	 include/cbf_file.h src/cbf_lex.c include/cbf_lex.h src/cbf_packed.c \
	 include/cbf_packed.h src/cbf_predictor.c include/cbf_predictor.h \
	 src/cbf_read_binary.c include/cbf_read_binary.h src/cbf_read_mime.c \
	 include/cbf_read_mime.h src/cbf_string.c include/cbf_string.h \
	 src/cbf_stx.c include/cbf_stx.h src/cbf_tree.c include/cbf_tree.h \
	 src/cbf_uncompressed.c include/cbf_uncompressed.h src/cbf_write.c \
	 include/cbf_write.h src/cbf_write_binary.c \
	 include/cbf_write_binary.h src/cbf.stx examples/img.c \
	 examples/img.h src/md5c.c src/global.h src/md5.h \
	 examples/makecbf.c examples/img2cif.c examples/cif2cbf.c \
	 README.html doc/CBFlib_NOTICES.html doc/example.html \
	 html_graphics/CBFbackground.jpg \
	 html_graphics/CBFbig.jpg \
	 html_graphics/CBFbutton.jpg \
	 html_graphics/cbflibbackground.jpg \
	 html_graphics/cbflibbig.jpg \
	 html_graphics/cbflibbutton.jpg \
	 html_graphics/cifhome.jpg \
	 html_graphics/iucrhome.jpg \
	 html_graphics/noticeButton.jpg
	-/bin/rm -f CBFlib.tar*
	tar cvBf CBFlib.tar \
	 doc/CBFlib_NOTICES.txt doc/CBFlib.ps \
	 doc/CBFlib.pdf doc/CBFlib.html \
	 doc/MANIFEST Makefile README doc/cbf_definition_rev.txt \
	 doc/cbf_definition_rev.html doc/cbfext98.dic doc/cbfext98.html \
	 src/cbf.c include/cbf.h src/cbf_alloc.c include/cbf_alloc.h \
	 src/cbf_ascii.c include/cbf_ascii.h src/cbf_binary.c \
	 include/cbf_binary.h src/cbf_byte_offset.c include/cbf_byte_offset.h \
	 src/cbf_canonical.c include/cbf_canonical.h src/cbf_codes.c \
	 include/cbf_codes.h src/cbf_compress.c include/cbf_compress.h \
	 src/cbf_context.c include/cbf_context.h src/cbf_file.c \
	 include/cbf_file.h src/cbf_lex.c include/cbf_lex.h src/cbf_packed.c \
	 include/cbf_packed.h src/cbf_predictor.c include/cbf_predictor.h \
	 src/cbf_read_binary.c include/cbf_read_binary.h src/cbf_read_mime.c \
	 include/cbf_read_mime.h src/cbf_string.c include/cbf_string.h \
	 src/cbf_stx.c include/cbf_stx.h src/cbf_tree.c include/cbf_tree.h \
	 src/cbf_uncompressed.c include/cbf_uncompressed.h src/cbf_write.c \
	 include/cbf_write.h src/cbf_write_binary.c \
	 include/cbf_write_binary.h src/cbf.stx examples/img.c \
	 examples/img.h src/md5c.c src/global.h src/md5.h \
	 examples/makecbf.c examples/img2cif.c examples/cif2cbf.c \
	 README.html doc/CBFlib_NOTICES.html doc/example.html \
	 html_graphics/CBFbackground.jpg \
	 html_graphics/CBFbig.jpg \
	 html_graphics/CBFbutton.jpg \
	 html_graphics/cbflibbackground.jpg \
	 html_graphics/cbflibbig.jpg \
         html_graphics/cbflibbutton.jpg \
	 html_graphics/cifhome.jpg \
	 html_graphics/iucrhome.jpg \
	 html_graphics/noticeButton.jpg
	compress CBFlib.tar

