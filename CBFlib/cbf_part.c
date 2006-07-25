/*
 * Read MIME body-part, stopping on boundaries.
 * THIS VERSION FOR USE WITH CIF TEXT FIELDS
 * DO NOT USE FOR EMAIL MESSAGES
 */
/* (C) Copyright 1994 by Carnegie Mellon University
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of Carnegie
 * Mellon University not be used in advertising or publicity
 * pertaining to distribution of the software without specific,
 * written prior permission.  Carnegie Mellon University makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * CARNEGIE MELLON UNIVERSITY DISCLAIMS ALL WARRANTIES WITH REGARD TO
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY BE LIABLE
 * FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
 * OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */


/**********************************************************************
 *  Hooks to restrict reads to CIF semi-colon delimited text sections *
 *  and to terminate on non-ascii characters                          *
 *                                                                    *
 *  H. J. Bernstein, 14 May 1998                                      *
 *  yaya@bernstein-plus-sons.com                                      *
 *  The following notice applies only to those modifications          *
 **********************************************************************/
/**********************************************************************
 *                                 NOTICE                             *
 * Creative endeavors depend on the lively exchange of ideas. There   *
 * are laws and customs which establish rights and responsibilities   *
 * for authors and the users of what authors create.  This notice     *
 * is not intended to prevent you from using the software and         *
 * documents in this package, but to ensure that there are no         *
 * misunderstandings about terms and conditions of such use.          *
 *                                                                    *
 * Please read the following notice carefully.  If you do not         *
 * understand any portion of this notice, please seek appropriate     *
 * professional legal advice before making use of the software and    *
 * documents included in this software package.  In addition to       *
 * whatever other steps you may be obliged to take to respect the     *
 * intellectual property rights of the various parties involved, if   *
 * you do make use of the software and documents in this package,     *
 * please give credit where credit is due by citing this package,     *
 * its authors and the URL or other source from which you obtained    *
 * it, or equivalent primary references in the literature with the    *
 * same authors.                                                      *
 *                                                                    *
 * Some of the software and documents included within this software   *
 * package are the intellectual property of various parties, and      *
 * placement in this package does not in any way imply that any       *
 * such rights have in any way been waived or diminished.             *
 *                                                                    *
 * With respect to any software or documents for which a copyright    *
 * exists, ALL RIGHTS ARE RESERVED TO THE OWNERS OF SUCH COPYRIGHT.   *
 *                                                                    *
 * Even though the authors of the various documents and software      *
 * found here have made a good faith effort to ensure that the        *
 * documents are correct and that the software performs according     *
 * to its documentation, and we would greatly appreciate hearing of   *
 * any problems you may encounter, the programs and documents any     *
 * files created by the programs are provided **AS IS** without any   *
 * warranty as to correctness, merchantability or fitness for any     *
 * particular or general use.                                         *
 *                                                                    *
 * THE RESPONSIBILITY FOR ANY ADVERSE CONSEQUENCES FROM THE USE OF    *
 * PROGRAMS OR DOCUMENTS OR ANY FILE OR FILES CREATED BY USE OF THE   *
 * PROGRAMS OR DOCUMENTS LIES SOLELY WITH THE USERS OF THE PROGRAMS   *
 * OR DOCUMENTS OR FILE OR FILES AND NOT WITH AUTHORS OF THE          *
 * PROGRAMS OR DOCUMENTS.                                             *
 **********************************************************************/

#include <stdio.h>
#include <string.h>

#include "cbf_part.h"
#include "xmalloc.h"

#define BUFSIZE 1024 /* must be > PART_MAX_BOUNDARY_LEN */
#define GROWBOUNDARY 20

static int pendingboundary();

/*
 * Create, initialize, and return a new struct part pointer
 * for the input file 'infile'.
 */
struct part *part_init(infile)
FILE *infile;
{
    static struct part zeropart;
    struct part *newpart;

    newpart = (struct part *)xmalloc(sizeof(struct part));
    *newpart = zeropart;
    newpart->infile = infile;
    newpart->buf = (unsigned char *)xmalloc(BUFSIZE);
    newpart->buf_alloc = BUFSIZE;
    newpart->ciftext = 0;
    newpart->size = 0;
    newpart->fpos = 0;
    newpart->id = 0;
    newpart->binflag = 0;
    newpart->gotfile = 0;
    newpart->xferfile = 0;
    newpart->conversions = 0;

    return newpart;
}

/*
 * Close and free 'part'.
 */
int part_close(part)
struct part *part;
{
    fclose(part->infile);
    if (part->buf) free(part->buf);
    if (part->boundary) free(part->boundary);
    return 0;         /* avoid warning messages -- HJB, 28 Jul 98 */
}

/*
 * Return the multipart depth of 'part'.  Top-level is '0'.
 */
int
part_depth(part)
struct part *part;
{
    return part->boundary_num;
}

/*
 * Add to 'part' the multipart boundary 'boundary'.
 */
int
part_addboundary(part, boundary)
struct part *part;
char *boundary;
{
    /* Grow boundary array if necessary */
    if (part->boundary_num == part->boundary_alloc) {
	part->boundary_alloc += GROWBOUNDARY;
	part->boundary = (char (*)[PART_MAX_BOUNDARY_LEN+1])
	    xrealloc((char *)part->boundary,
		     part->boundary_alloc * (PART_MAX_BOUNDARY_LEN+1));
	part->boundary_length = (int *)
	    xrealloc((char *)part->boundary_length,
		     part->boundary_alloc * sizeof(int));
    }

    strncpy(part->boundary[part->boundary_num], boundary,
	    PART_MAX_BOUNDARY_LEN);
    part->boundary[part->boundary_num][PART_MAX_BOUNDARY_LEN] = '\0';
    part->boundary_length[part->boundary_num] =
	strlen(part->boundary[part->boundary_num]);
    part->boundary_num++;
    if (part->boundary_seen+1 == part->boundary_num) {
	part->boundary_seen++;
    }
    return 0;         /* avoid warning messages -- HJB, 28 Jul 98 */
}

/*
 * Private function that is only called from the part_getc() macro.
 *
 * Fills the input buffer for 'part' if necessary.  Returns the next
 * input character or EOF if at a boundary or end of file.
 */
int
part_fill(part)
struct part *part;
{
    /* part_getc() decremented this before calling us, put it back */
    part->cnt++;

    /* Return EOF if we saw a boundary */

    if (part->boundary_seen < part->boundary_num) return EOF;

    /* for a CIF, abort on binary */
      if (part->ciftext && part->binflag) {
          part->ptr = part->buf;
          if (!part->cnt) {
             part->fpos -= part->cnt;
             fseek(part->infile,-part->cnt,SEEK_CUR);
          }
          part->cnt = 0;
          part->boundary_seen = 0;
          return EOF;
      }

    /* Fill buffer if it is empty */
    if (part->cnt == 0) {
	part->ptr = part->buf;
	part->cnt = fread(part->buf, 1, part->buf_alloc, part->infile);
        part->fpos += part->cnt;
	if (part->cnt == 0) {
	    part->boundary_seen = 0;
	    return EOF;
	}
    }

    /* If there is a newline, see if it is followed by a boundary */
    if (part->ptr[0] == '\n' && pendingboundary(part)) {
	return EOF;
    }

    part->cnt--;
    return *part->ptr++;
}

/*
 * Read a line into the array 's', of size 'n', from 'part'.
 * Reads until 'n'-1 characters are read, a newline is read, or
 * an EOF is encountered.  The array is then nul-terminated and returned.
 * If the first character read is an EOF, then a null pointer is instead
 * returned.
 */
char *
part_gets(s, n, part)
char *s;
int n;
struct part *part;
{
    int c;
    char *p = s;
    
    if (n == 0) return 0;
    n--;
    while (n-- && (c = part_getc(part)) != EOF) {
	*p++ = c;
	if (c == '\n') break;
    }
    if (p == s) return 0;
    *p++ = '\0';
    return s;
}

/*
 * Push back the string 's' into the input buffer of 'part'.
 * Leaves room in the input buffer to push back an additional single
 * character using the prot_ungetc() macro.
 */
int
part_ungets(s, part)
char *s;
struct part *part;
{
    int len = strlen(s);
    int i;

    /* Grow buffer if necessary */
    if (part->cnt + len + 1 > part->buf_alloc) {
	i = part->ptr - part->buf;
	part->buf_alloc = part->cnt + len + 1;
	part->buf = (unsigned char *)
	    xrealloc((char *)part->buf, part->buf_alloc);
	part->ptr = part->buf + i;
    }

    /* Move current data down to make room for new data if necessary */
    if (len + 1 > part->ptr - part->buf) {
	for (i = part->cnt-1; i >= 0; i--) {
	    part->buf[len+1+i] = part->ptr[i];
	}
	part->ptr = part->buf + len + 1;
    }

    /* Copy in the new data */
    part->ptr -= len;
    part->cnt += len;
    for (i = 0; i < len; i++) {
	part->ptr[i] = s[i];
    }
    return 0;         /* avoid warning messages -- HJB, 28 Jul 98 */
}

/*
 * Reset the saw-boundary state of 'part' and set up to read next
 * body-part Returns nonzero iff the pending boundary was a final
 * boundary of the current multipart.
 */
int
part_readboundary(part)
struct part *part;
{
    int c;
    int sawfinal = 0;

    if (part->boundary_seen < part->boundary_num-1) {
	/* We saw an enclosing boundary.  Signal end of multipart, but
	 * don't skip over the boundary.
	 */
	part->boundary_num--;
	return 1;
    }

    /* Deal with EOF on input stream */
    if (part->cnt == 0) return 1;

    /* If CIF text and if ;, treat this as EOF  */
    if (part->ciftext && part->cnt >= 2 && part->ptr[1] == ';' ) {
    part->boundary_seen = 0;
    return 1;
    }

    /* Skip over delimiter, reset the "saw boundary" state */
    part->ptr += part->boundary_length[part->boundary_seen] + 3;
    part->cnt -= part->boundary_length[part->boundary_seen] + 3;
    part->boundary_seen = part->boundary_num;
    
    /* Check for two dashes, which indicate a final delimiter */
    c = part_getc(part);
    if (c == '-') {
	c = part_getc(part);
	if (c == '-') {
	    sawfinal = 1;
	    part->boundary_num--;
	}
    }

    /* Eat rest of the boundary line */
    while (c != '\n' && c != EOF) {
	c = part_getc(part);
    }
	
    return sawfinal;
}    

	    
/*
 * Return nonzero and set the saw-boundary state iff 'part'
 * is positioned at a boundary.
 *
 * if we encounter a "\n;", treat it as an EOF
 *

 */
static int
pendingboundary(part)
struct part *part;
{
    int bufleft;
    int i;
    int xfill;

    /* Fill buffer if we don't have enough to do our look ahead */
    if (part->cnt < 3 ||
	(part->cnt < PART_MAX_BOUNDARY_LEN+3 && part->ptr[1] == '-' &&
	 part->ptr[2] == '-')) {
    	
	bufleft = part->buf_alloc - part->cnt - (part->ptr - part->buf);

	/* If not enough room, move everything to beginning of buffer */
	if (part->ptr!=part->buf && bufleft + part->cnt < PART_MAX_BOUNDARY_LEN+3) {
	    for (i = 0; i < part->cnt; i++) {
		part->buf[i] = part->ptr[i];
	    }
	    part->ptr = part->buf;
	    bufleft = part->buf_alloc - part->cnt;
	}

	/* Read in more data */
        xfill = fread(part->ptr+part->cnt, 1, bufleft, part->infile);
	part->cnt += xfill;
        part->fpos += xfill;
    }
    
    /* If CIF text and if ;, treat this as a boundary  */
    if (part->ciftext && part->cnt >= 2 && part->ptr[1] == ';' ) {
    part->boundary_seen = 0;
    return 1;
    }
    
    /* If no "--", it's not a boundary */
    if (part->cnt < 3 || part->ptr[1] != '-' || part->ptr[2] != '-') {
	return 0;
    }

    for (i = 0; i < part->boundary_num; i++) {
	if (part->cnt - 3 >= part->boundary_length[i] &&
	    !strncmp((char *)part->ptr+3, part->boundary[i],
		     part->boundary_length[i])) {
	    break;
	}
    }

    if (i == part->boundary_num) return 0;

    /* Saw boundary, index 'i' */ 
    part->boundary_seen = i;
    return 1;
}
