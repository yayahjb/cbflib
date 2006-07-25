#  Makefile for CBFlib
#

#
#  suffixes of files to be used or built
#
.SUFFIXES:	.c .o

#
#  select the C compiler and parser generator
#
CC	=	gcc
YACC	=	bison -y
CFLAGS	=	-g

#
#  The source codes for the C compiler
#
CSRCS	=	cbf.c cbf_alloc.c cbf_ascii.c cbf_binary.c \
		cbf_canonical.c cbf_compress.c cbf_context.c \
		cbf_file.c cbf_lex.c cbf_packed.c cbf_stx.c cbf_tree.c \
		cbf_write.c cbf_mime.c img.c cbf_codes.c md5c.c xmalloc.c \
	 	cbf_decode.c cbf_part.c cbf_unixos.c string.c uudecode.c

#
#  The header files used
#
CHDRS	=	cbf.h cbf_alloc.h cbf_ascii.h cbf_binary.h cbf_canonical.h \
		cbf_compress.h cbf_context.h cbf_file.h cbf_lex.h \
		cbf_packed.h cbf_stx.h cbf_tree.h cbf_write.h img.h \
		md5.h xmalloc.h cbf_part.h common.h

#
#  object files
#
OBJS	=	cbf.o cbf_alloc.o cbf_ascii.o cbf_binary.o \
		cbf_canonical.o cbf_compress.o cbf_context.o \
		cbf_file.o cbf_lex.o cbf_packed.o cbf_stx.o cbf_tree.o \
		cbf_write.o cbf_mime.o img.o cbf_codes.o md5c.o xmalloc.o \
		cbf_decode.o cbf_part.o cbf_unixos.o string.o uudecode.o
.c.o:
		$(CC) $(CFLAGS) -c $<

$(OBJS):	$(CHDRS)

cbf_stx.c \
cbf_stx.h :	cbf.stx.y
		$(YACC) -l -d cbf.stx.y
		-mv -f cbf_stx.c cbf_stx.c.BAK
		mv y.tab.c cbf_stx.c
		-mv -f cbf_stx.h cbf_stx.h.BAK
		mv y.tab.h cbf_stx.h

makecbf.o:	makecbf.c

img2cif.o:	img2cif.c

cif2cbf.o:	cif2cbf.c

makecbf:	$(OBJS) makecbf.o
		$(CC) $(CFLAGS) -o makecbf makecbf.o $(OBJS) -lm


img2cif:	$(OBJS) img2cif.o
		$(CC) $(CFLAGS) -o img2cif img2cif.o $(OBJS) -lm

cif2cbf:	$(OBJS) cif2cbf.o
		$(CC) $(CFLAGS) -o cif2cbf cif2cbf.o $(OBJS) -lm

tests:		makecbf img2cif cif2cbf ../example.mar2300
		./makecbf ../example.mar2300 makecbf.cbf
		./img2cif -c packed -m headers -d digest \
			-e base64 < ../example.mar2300 > img2cif_packed.cif
		./img2cif -c canonical -m headers -d digest \
			-e base64 < ../example.mar2300 > img2cif_canonical.cif
		./img2cif -c packed -m headers -d digest \
			-e none < ../example.mar2300 > img2cif_packed.cbf
		./img2cif -c canonical -m headers -d digest \
			-e none < ../example.mar2300 > img2cif_canonical.cbf
		./img2cif -c canonical -m noheaders -d digest \
			-e none < ../example.mar2300 > img2cif_raw.cbf
		./cif2cbf -e none -c packed \
			img2cif_canonical.cif cif2cbf_packed.cbf
		./cif2cbf -e none -c canonical \
			img2cif_packed.cif cif2cbf_canonical.cbf
		diff -a cif2cbf_packed.cbf makecbf.cbf
		diff -a cif2cbf_canonical.cbf img2cif_canonical.cbf

shars:		MANIFEST Makefile $(CSRCS) $(CHDRS) cbf.stx.y \
		makecbf.c img2cif.c cif2cbf.c ../CBFlib_NOTICES.txt \
		cbf_PARSER.simple COPYING.PARSER \
		../CBFlib_NOTICES.html ../cbf_definition_rev.html \
		../CBFlib.html ../cbfext98.html ../cbfext98.dic \
		../cbf_definition_rev.txt ../CBFlib.txt
		ln -s -f ../CBFlib_NOTICES.txt .
		ln -s -f ../CBFlib_NOTICES.html .
		ln -s -f ../cbf_definition_rev.html .
		ln -s -f ../CBFlib.html .
		ln -s -f ../cbfext98.html .
		ln -s -f ../cbf_definition_rev.txt .
		ln -s -f ../CBFlib.txt .
		ln -s -f ../cbfext98.dic .
		-/bin/rm -f Part*
		-/bin/rm -f CBFlib.shar
		-/bin/rm -f CBFlib.shar.Z
		makekit  -iMANIFEST -oMANIFEST -h2 -p -s1000k
		mv Part01 CBFlib.shar
		compress CBFlib.shar
		mv CBFlib.shar.Z ..

postshar:
		touch cbf_stx.c
		touch cbf_stx.h
		/bin/mv -f CBFlib_NOTICES.txt ..
		/bin/mv -f CBFlib_NOTICES.html ..
		/bin/mv -f cbf_definition_rev.html ..
		/bin/mv -f CBFlib.html ..
		/bin/mv -f cbfext98.html ..
		/bin/mv -f cbf_definition_rev.txt ..
		/bin/mv -f CBFlib.txt ..
		/bin/mv -f cbfext98.dic ..
		ln -s -f ../CBFlib_NOTICES.txt .
		ln -s -f ../CBFlib_NOTICES.html .
		ln -s -f ../cbf_definition_rev.html .
		ln -s -f ../CBFlib.html .
		ln -s -f ../cbfext98.html .
		ln -s -f ../cbf_definition_rev.txt .
		ln -s -f ../CBFlib.txt .
		ln -s -f ../cbfext98.dic .
		-/bin/rm -f ../index.html
		ln -s ../cbf_definition_rev.html ../index.html
		touch postshar

clean:
		-/bin/rm $(OBJS)

distclean:	clean
		-/bin/rm cbf_stx.c
		-/bin/rm cbf_stx.h
		-/bin/rm *.cif
		-/bin/rm *.cbf
		-/bin/rm *.BAK

