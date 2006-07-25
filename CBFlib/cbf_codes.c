/* (C) Copyright 1993,1994 by Carnegie Mellon University
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
/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.  */

/**********************************************************************
 *  Hooks for size-limited file reads                                 *
 *                                                                    *
 *  H. J. Bernstein, 27 April 1998                                    *         
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
#include <ctype.h>
#include "xmalloc.h"
#include "md5.h"


/*  The following flags control line termination  */

int MPACK_CR_term = 0;
int MPACK_NL_term = 1;

void set_MP_terms(crterm,nlterm)
int crterm, nlterm;
{
    MPACK_CR_term = 0;
    if (crterm) MPACK_CR_term = 1;
    MPACK_NL_term = 0;
    if (nlterm) MPACK_NL_term = 1;
    return;
}

#define SHORT 1
#define LONG  3

static char basis_16[] =
   "0123456789ABCDEF";

int toqp_sized(infile, outfile, limit, size) 
FILE *infile, *outfile;
long limit;
long size;
{
    int c1, c1mode, ct=0, written=0;
    int cnext, cnextmode;
    long toread;

    toread = size;
    if (limit && limit < 76) return 1;
    cnext = 0; cnextmode = SHORT;

    while (toread && (c1 = getc(infile)) != EOF) {
    
        toread--;
        if ( ((c1 >= 32 && c1 <= 38) ||
              (c1 == 42) || (c1 >= 48 && c1 <= 57) ||
              (c1 >= 59 && c1 <= 60) || (c1 == 62) ||
              (c1 >= 64 && c1 <= 126)) && 
          !(c1 == ';' && ct == 0)) {
          c1mode = SHORT;
        } else {
          c1mode = LONG;
        }
        
        if ( (ct+c1mode) > 75 ) {
          putc('=',outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
          limit -= ct + 1 + MPACK_CR_term + MPACK_NL_term;
          }
          written += ct + 1 + MPACK_CR_term + MPACK_NL_term;
          ct = 0;
        }
        if ( c1mode == SHORT) {
          putc(c1, outfile);
          ct++;
        } else {
          putc('=',outfile);
          putc(basis_16[( c1 >> 4) & 0xF], outfile);
          putc(basis_16[( c1 & 0xF ) ],outfile);
          ct += LONG;
        }
        if ( limit && (limit-ct) < 76) {
            putc('=',outfile);
            if (MPACK_CR_term) putc('\r',outfile);
            if (MPACK_NL_term) putc('\n',outfile);
            limit -= ct + 1 + MPACK_CR_term + MPACK_NL_term;
            written += ct + 1 + MPACK_CR_term + MPACK_NL_term;
            return 1;
        }
    }
    if (ct) {
    putc('=', outfile);
    if (MPACK_CR_term) putc('\r',outfile);
    if (MPACK_NL_term) putc('\n',outfile);
    ct += 1 + MPACK_CR_term + MPACK_NL_term;
    }
    return written + ct;
}


int toqp(infile, outfile, limit) 
FILE *infile, *outfile;
long limit;
{
    int c1, c1mode, ct=0, written=0;
    int cnext, cnextmode;

    if (limit && limit < 76) return 1;
    cnext = 0; cnextmode = SHORT;

    while ((c1 = getc(infile)) != EOF) {
    
        if ( ((c1 >= 32 && c1 <= 38) ||
              (c1 == 42) || (c1 >= 48 && c1 <= 57) ||
              (c1 >= 59 && c1 <= 60) || (c1 == 62) ||
              (c1 >= 64 && c1 <= 126)) && 
          !(c1 == ';' && ct == 0)) {
          c1mode = SHORT;
        } else {
          c1mode = LONG;
        }
        
        if ( (ct+c1mode) > 75 ) {
          putc('=',outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= ct + 1 + MPACK_CR_term + MPACK_NL_term;
          }
          written += ct + 1 + MPACK_CR_term + MPACK_NL_term;
          ct = 0;
        }
        if ( c1mode == SHORT) {
          putc(c1, outfile);
          ct++;
        } else {
          putc('=',outfile);
          putc(basis_16[( c1 >> 4) & 0xF], outfile);
          putc(basis_16[( c1 & 0xF ) ],outfile);
          ct += 3;
        }
        if ( limit && (limit-ct) < 76) {
            putc('=',outfile);
            if (MPACK_CR_term) putc('\r',outfile);
            if (MPACK_NL_term) putc('\n',outfile);
            limit -= ct + 1 + MPACK_CR_term + MPACK_NL_term;
            written += ct + 1 + MPACK_CR_term + MPACK_NL_term;
            return 1;
        }
    }
    if (ct) {
    putc('=', outfile);
    if (MPACK_CR_term) putc('\r',outfile);
    if (MPACK_NL_term) putc('\n',outfile);
    ct += 1 + MPACK_CR_term + MPACK_NL_term;
    }
    return written + ct;
}




static char basis_64[] =
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

int to64_sized(infile, outfile, limit, size) 
FILE *infile, *outfile;
long limit;
long size;
{
    int c1, c2, c3, ct=0, written=0;
    long toread;

    toread = size;
    if (limit && limit < 72 + MPACK_CR_term + MPACK_NL_term) return 1;

    while ( toread && ((c1 = getc(infile)) != EOF)) {
        toread--;
        c2 = EOF;
        c3 = EOF;
        if (toread) {
          c2 = getc(infile);
          toread--;
        }
        if (c2 == EOF) {
            output64chunk(c1, 0, 0, 2, outfile);
        } else {
            if (toread) {
              c3 = getc(infile);
              toread --;
            }
            if (c3 == EOF) {
                output64chunk(c1, c2, 0, 1, outfile);
            } else {
                output64chunk(c1, c2, c3, 0, outfile);
            }
        }
        ct += 4;
        if (ct > 71) {
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= ct + MPACK_CR_term + MPACK_NL_term;
            if (limit < (72 + MPACK_CR_term + MPACK_NL_term)) return 1;
          }
          written += 72 + MPACK_CR_term + MPACK_NL_term;
          ct = 0;
        }
    }
    if (ct) {
       if (MPACK_CR_term) putc('\r',outfile);
       if (MPACK_NL_term) putc('\n',outfile);
       ct += MPACK_CR_term + MPACK_NL_term;
    }
    return written + ct;
}

int to64(infile, outfile, limit) 
FILE *infile, *outfile;
long limit;
{
    int c1, c2, c3, ct=0, written=0;

    if (limit && limit < 72 + MPACK_CR_term + MPACK_NL_term) return 1;

    while ((c1 = getc(infile)) != EOF) {
        c2 = getc(infile);
        if (c2 == EOF) {
            output64chunk(c1, 0, 0, 2, outfile);
        } else {
            c3 = getc(infile);
            if (c3 == EOF) {
                output64chunk(c1, c2, 0, 1, outfile);
            } else {
                output64chunk(c1, c2, c3, 0, outfile);
            }
        }
        ct += 4;
        if (ct > 71) {
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
          limit -= ct + MPACK_CR_term + MPACK_NL_term;
          if (limit < 72 + MPACK_CR_term + MPACK_NL_term) return 1;
        }
        written += 72 + MPACK_CR_term + MPACK_NL_term;
            ct = 0;
        }
    }
    if (ct) {
      if (MPACK_CR_term) putc('\r',outfile);
      if (MPACK_NL_term) putc('\n',outfile);
      ct += MPACK_CR_term + MPACK_NL_term;
    }
    return written + ct;
}

output64chunk(c1, c2, c3, pads, outfile)
FILE *outfile;
{
    putc(basis_64[c1>>2], outfile);
    putc(basis_64[((c1 & 0x3)<< 4) | ((c2 & 0xF0) >> 4)], outfile);
    if (pads == 2) {
        putc('=', outfile);
        putc('=', outfile);
    } else if (pads) {
        putc(basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
        putc('=', outfile);
    } else {
        putc(basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
        putc(basis_64[c3 & 0x3F], outfile);
    }
    return 0;    /* avoid warning messages -- HJB 29 Jul 98 */
}

char *md5contextTo64(context)
MD5_CTX *context;
{
    unsigned char digest[18];
    char encodedDigest[25];
    int i;
    char *p;

    MD5Final(digest, context);
    digest[sizeof(digest)-1] = digest[sizeof(digest)-2] = 0;

    p = encodedDigest;
    for (i=0; i < sizeof(digest); i+=3) {
    *p++ = basis_64[digest[i]>>2];
    *p++ = basis_64[((digest[i] & 0x3)<<4) | ((digest[i+1] & 0xF0)>>4)];
    *p++ = basis_64[((digest[i+1] & 0xF)<<2) | ((digest[i+2] & 0xC0)>>6)];
    *p++ = basis_64[digest[i+2] & 0x3F];
    }
    *p-- = '\0';
    *p-- = '=';
    *p-- = '=';
    return strsave(encodedDigest);
}    

char *md5digest_sized(infile, len, size)
FILE *infile;
long *len;
long size;
{
    MD5_CTX context;
    char buf[1000];
    long length = 0;
    int nbytes;
    int sob;
    long toread;

    toread = size;
    sob = sizeof(buf);
    if ( sob > toread) sob = toread;
    MD5Init(&context);
    while (nbytes = fread(buf, 1, sob, infile)) {
      toread -= nbytes;
      if ( sob > toread) sob = toread;
      length += nbytes;
      MD5Update(&context, buf, nbytes);
    }
    rewind(infile);
    if (len) *len = length;
    return md5contextTo64(&context);
}

char *md5digest(infile, len)
FILE *infile;
long *len;
{
    MD5_CTX context;
    char buf[1000];
    long length = 0;
    int nbytes;
    
    MD5Init(&context);
    while (nbytes = fread(buf, 1, sizeof(buf), infile)) {
    length += nbytes;
    MD5Update(&context, buf, nbytes);
    }
    rewind(infile);
    if (len) *len = length;
    return md5contextTo64(&context);
}

int todec_sized(infile, outfile, limit, size, elsize, bytedir) 
FILE *infile, *outfile;
long limit;
long size;
size_t elsize;
int bytedir;
{
    int ct=0, written=0;
    int igot, iput;
    int c[8];
    long toread;
    long wasread;
    int binblk;
    char buffer[50], cuffer[22];
    long long  word;

    toread = size;
    wasread = 0;
    if (limit && limit < 122 + MPACK_CR_term + MPACK_NL_term) return 1;
    if (bytedir > 0) {
      sprintf(buffer,
      "# Decimal encoding, byte %lu, byte order 12...", wasread);
    } else {
      sprintf(buffer,
      "# Decimal encoding, byte %lu, byte order ...21", wasread);
    }
    fputs(buffer,outfile);
    if (MPACK_CR_term) putc('\r',outfile);
    if (MPACK_NL_term) putc('\n',outfile);
    if (limit) {
      limit -= strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
    }
    written += strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
    putc('#',outfile);
    if (MPACK_CR_term) putc('\r',outfile);
    if (MPACK_NL_term) putc('\n',outfile);
    if (limit) {
      limit -= 1 + MPACK_CR_term + MPACK_NL_term;
    }
    written += 1 + MPACK_CR_term + MPACK_NL_term;

    binblk = 0;
    while ( toread && ((c[0] = getc(infile)) != EOF)) {

        toread--;
        igot = 1;
        wasread++;
        binblk++;
        while ( toread && igot < elsize && ((c[igot] = getc(infile)) !=EOF )) {
          toread --;
          igot++;
          wasread++;
          binblk++;
        }

        if ( (ct+elsize*3) > 74 ) {
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= ct  + MPACK_CR_term + MPACK_NL_term;
          }
          written += ct  + MPACK_CR_term + MPACK_NL_term;
          ct = 0;
        }
        if (ct) {
          putc(' ',outfile);
          ct++;
        } else {
          putc('D',outfile);
          putc('0'+elsize,outfile);
          if (bytedir > 0) {
            putc ('>',outfile);
	  } else {
            putc ('<',outfile);
	  }
          putc(' ',outfile);
          ct += 4;
        }
        word = 0;
        if (bytedir > 0) {
        word = c[0];
        for (iput = 1; iput < igot; iput++) {
          word = (word << 8)|c[iput];
        }
        } else {
        word = c[igot-1];
        for (iput = igot-2; iput >= 0; iput--) {
          word = (word << 8)|c[iput];
        }
        }
        sprintf(buffer,"%lld",word);
        if ( bytedir <= 0 && igot < elsize ) {
          for ( iput = igot; iput < elsize; iput++ ) {
            putc('=',outfile);
            putc('=',outfile);
          }
          ct += (elsize-igot)*2;
        }
        fputs(buffer,outfile);
        ct += strlen(buffer);
        if ( bytedir >  0 && igot < elsize ) {
          for ( iput = igot; iput < elsize; iput++ ) {
            putc('=',outfile);
            putc('=',outfile);
          }
          ct += (elsize-igot)*2;
        }

        if ( limit && (limit-ct) < 76) {
            if (MPACK_CR_term) putc('\r',outfile);
            if (MPACK_NL_term) putc('\n',outfile);
            limit -= ct  + MPACK_CR_term + MPACK_NL_term;
            written += ct  + MPACK_CR_term + MPACK_NL_term;
            return 1;
        }
        if (binblk >= 512*elsize) {
          if(ct) {
            if (MPACK_CR_term) putc('\r',outfile);
            if (MPACK_NL_term) putc('\n',outfile);
            if (limit) {
              limit -= ct  + MPACK_CR_term + MPACK_NL_term;
            }
            written += ct  + MPACK_CR_term + MPACK_NL_term;
            ct = 0;
          }

          putc('#',outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= 1 + MPACK_CR_term + MPACK_NL_term;
          }
          written += 1 + MPACK_CR_term + MPACK_NL_term;

          if (bytedir > 0) {
            sprintf(buffer,
            "# Decimal encoding, byte %lu, byte order 12...", wasread);
          } else {
            sprintf(buffer,
            "# Decimal encoding, byte %lu, byte order ...21", wasread);
          }
          fputs(buffer,outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
          }
          written += strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
          putc('#',outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= 1 + MPACK_CR_term + MPACK_NL_term;
          }
          written += 1 + MPACK_CR_term + MPACK_NL_term;
          binblk = 0;
        }
    }
    if (ct) {
       if (MPACK_CR_term) putc('\r',outfile);
       if (MPACK_NL_term) putc('\n',outfile);
       ct += MPACK_CR_term + MPACK_NL_term;
    }
    return written + ct;
}


int tohex_sized(infile, outfile, limit, size, elsize, bytedir) 
FILE *infile, *outfile;
long limit;
long size;
size_t elsize;
int bytedir;
{
    int ct=0, written=0;
    int igot, iput;
    int c[8];
    long toread;
    long wasread;
    int binblk;
    char buffer[50];

    toread = size;
    wasread = 0;
    if (limit && limit < 122 + MPACK_CR_term + MPACK_NL_term) return 1;
    if (bytedir > 0) {
      sprintf(buffer,
      "# Hexadecimal encoding, byte %lu, byte order 12...", wasread);
    } else {
      sprintf(buffer,
      "# Hexadecimal encoding, byte %lu, byte order ...21", wasread);
    }
    fputs(buffer,outfile);
    if (MPACK_CR_term) putc('\r',outfile);
    if (MPACK_NL_term) putc('\n',outfile);
    if (limit) {
      limit -= strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
    }
    written += strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
    putc('#',outfile);
    if (MPACK_CR_term) putc('\r',outfile);
    if (MPACK_NL_term) putc('\n',outfile);
    if (limit) {
      limit -= 1 + MPACK_CR_term + MPACK_NL_term;
    }
    written += 1 + MPACK_CR_term + MPACK_NL_term;

    binblk = 0;
    while ( toread && ((c[0] = getc(infile)) != EOF)) {

        toread--;
        igot = 1;
        wasread++;
        binblk++;
        while ( toread && igot < elsize && ((c[igot] = getc(infile)) !=EOF )) {
          toread --;
          igot++;
          wasread++;
          binblk++;
        }

        if ( (ct+elsize*2) > 74 ) {
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= ct  + MPACK_CR_term + MPACK_NL_term;
          }
          written += ct  + MPACK_CR_term + MPACK_NL_term;
          ct = 0;
        }
        if (ct) {
          putc(' ',outfile);
          ct++;
        } else {
          putc('H',outfile);
          putc('0'+elsize,outfile);
          if (bytedir > 0) {
            putc ('>',outfile);
	  } else {
            putc ('<',outfile);
	  }
          putc(' ',outfile);
          ct += 4;
        }
        if (bytedir > 0) {
        for (iput = 0; iput < igot; iput++) {
          putc(basis_16[( c[iput] >> 4) & 0xF], outfile);
          putc(basis_16[( c[iput] & 0xF ) ],outfile);
        }
        for ( iput = igot; iput < elsize; iput++ ) {
          putc('=',outfile);
          putc('=',outfile);
        }
        ct += (elsize-igot)*2;
        } else {
        for ( iput = igot; iput < elsize; iput++ ) {
          putc('=',outfile);
          putc('=',outfile);
        }
        ct += (elsize-igot)*2;
        for (iput = igot-1; iput >= 0; iput--) {
          putc(basis_16[( c[iput] >> 4) & 0xF], outfile);
          putc(basis_16[( c[iput] & 0xF ) ],outfile);
        }
        }
        ct += igot*2;
        if ( limit && (limit-ct) < 76) {
            if (MPACK_CR_term) putc('\r',outfile);
            if (MPACK_NL_term) putc('\n',outfile);
            limit -= ct  + MPACK_CR_term + MPACK_NL_term;
            written += ct  + MPACK_CR_term + MPACK_NL_term;
            return 1;
        }
        if (binblk >= 512*elsize) {
          if(ct) {
            if (MPACK_CR_term) putc('\r',outfile);
            if (MPACK_NL_term) putc('\n',outfile);
            if (limit) {
              limit -= ct  + MPACK_CR_term + MPACK_NL_term;
            }
            written += ct  + MPACK_CR_term + MPACK_NL_term;
            ct = 0;
          }

          putc('#',outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= 1 + MPACK_CR_term + MPACK_NL_term;
          }
          written += 1 + MPACK_CR_term + MPACK_NL_term;

          if (bytedir > 0) {
            sprintf(buffer,
            "# Hexadecimal encoding, byte %lu, byte order 12...", wasread);
          } else {
            sprintf(buffer,
            "# Hexadecimal encoding, byte %lu, byte order ...21", wasread);
          }
          fputs(buffer,outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
          }
          written += strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
          putc('#',outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= 1 + MPACK_CR_term + MPACK_NL_term;
          }
          written += 1 + MPACK_CR_term + MPACK_NL_term;
          binblk = 0;
        }
    }
    if (ct) {
       if (MPACK_CR_term) putc('\r',outfile);
       if (MPACK_NL_term) putc('\n',outfile);
       ct += MPACK_CR_term + MPACK_NL_term;
    }
    return written + ct;
}


int tooct_sized(infile, outfile, limit, size, elsize, bytedir) 
FILE *infile, *outfile;
long limit;
long size;
size_t elsize;
int bytedir;
{
    int ct=0, written=0;
    int igot, iput;
    int c[8];
    long toread;
    long wasread;
    int binblk;
    char buffer[50], cuffer[22];
    unsigned long long word;

    toread = size;
    wasread = 0;
    if (limit && limit < 122 + MPACK_CR_term + MPACK_NL_term) return 1;
    if (bytedir > 0) {
      sprintf(buffer,
      "# Octal encoding, byte %lu, byte order 12...", wasread);
    } else {
      sprintf(buffer,
      "# Octal encoding, byte %lu, byte order ...21", wasread);
    }
    fputs(buffer,outfile);
    if (MPACK_CR_term) putc('\r',outfile);
    if (MPACK_NL_term) putc('\n',outfile);
    if (limit) {
      limit -= strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
    }
    written += strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
    putc('#',outfile);
    if (MPACK_CR_term) putc('\r',outfile);
    if (MPACK_NL_term) putc('\n',outfile);
    if (limit) {
      limit -= 1 + MPACK_CR_term + MPACK_NL_term;
    }
    written += 1 + MPACK_CR_term + MPACK_NL_term;

    binblk = 0;
    while ( toread && ((c[0] = getc(infile)) != EOF)) {

        toread--;
        igot = 1;
        wasread++;
        binblk++;
        while ( toread && igot < elsize && ((c[igot] = getc(infile)) !=EOF )) {
          toread --;
          igot++;
          wasread++;
          binblk++;
        }

        if ( (ct+elsize*3) > 74 ) {
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= ct  + MPACK_CR_term + MPACK_NL_term;
          }
          written += ct  + MPACK_CR_term + MPACK_NL_term;
          ct = 0;
        }
        if (ct) {
          putc(' ',outfile);
          ct++;
        } else {
          putc('O',outfile);
          putc('0'+elsize,outfile);
          if (bytedir > 0) {
            putc ('>',outfile);
	  } else {
            putc ('<',outfile);
	  }
          putc(' ',outfile);
          ct += 4;
        }
        word = 0;
        if (bytedir > 0) {
        word = c[0];
        for (iput = 1; iput < igot; iput++) {
          word = (word << 8)|c[iput];
        }
        for ( iput = igot; iput < elsize; iput++ ) {
          putc('=',outfile);
          putc('=',outfile);
        }
        ct += (elsize-igot)*2;
        } else {
        for ( iput = igot; iput < elsize; iput++ ) {
          putc('=',outfile);
          putc('=',outfile);
        }
        ct += (elsize-igot)*2;
        word = c[igot-1];
        for (iput = igot-2; iput >= 0; iput--) {
          word = (word << 8)|c[iput];
        }
        }
        sprintf(buffer,"%llo",word);
        fputs(buffer,outfile);
        ct += strlen(buffer);

        if ( limit && (limit-ct) < 76) {
            if (MPACK_CR_term) putc('\r',outfile);
            if (MPACK_NL_term) putc('\n',outfile);
            limit -= ct  + MPACK_CR_term + MPACK_NL_term;
            written += ct  + MPACK_CR_term + MPACK_NL_term;
            return 1;
        }
        if (binblk >= 512*elsize) {
          if(ct) {
            if (MPACK_CR_term) putc('\r',outfile);
            if (MPACK_NL_term) putc('\n',outfile);
            if (limit) {
              limit -= ct  + MPACK_CR_term + MPACK_NL_term;
            }
            written += ct  + MPACK_CR_term + MPACK_NL_term;
            ct = 0;
          }

          putc('#',outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= 1 + MPACK_CR_term + MPACK_NL_term;
          }
          written += 1 + MPACK_CR_term + MPACK_NL_term;

          if (bytedir > 0) {
            sprintf(buffer,
            "# Octal encoding, byte %lu, byte order 12...", wasread);
          } else {
            sprintf(buffer,
            "# Octal encoding, byte %lu, byte order ...21", wasread);
          }
          fputs(buffer,outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
          }
          written += strlen(buffer) + MPACK_CR_term + MPACK_NL_term;
          putc('#',outfile);
          if (MPACK_CR_term) putc('\r',outfile);
          if (MPACK_NL_term) putc('\n',outfile);
          if (limit) {
            limit -= 1 + MPACK_CR_term + MPACK_NL_term;
          }
          written += 1 + MPACK_CR_term + MPACK_NL_term;
          binblk = 0;
        }
    }
    if (ct) {
       if (MPACK_CR_term) putc('\r',outfile);
       if (MPACK_NL_term) putc('\n',outfile);
       ct += MPACK_CR_term + MPACK_NL_term;
    }
    return written + ct;
}
