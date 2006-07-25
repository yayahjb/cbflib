/*
 * Decode MIME parts.
 */
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
 * SOFTWARE.  */

/**********************************************************************
 *  Hooks to restrict reads to CIF semi-colon delimited text sections *
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
#include <ctype.h>
#include "xmalloc.h"
#include "common.h"
#include "cbf_part.h"
#include "md5.h"
#include "cbf.h"

extern char *os_idtodir();
extern FILE *os_newtypedfile();
extern char *md5contextTo64();

/* The possible content transfer encodings */
enum encoding { enc_none, 
                enc_qp, 
                enc_base64, 
                enc_base8, 
                enc_base10, 
                enc_base16 };

char *ParseHeaders();
enum encoding parseEncoding();
params ParseContent();
char *getParam();
char *getDispositionFilename();
void from64(struct part *inpart, 
              FILE *outfile, char **digestp, int suppressCR);
void fromqp(struct part *inpart, FILE *outfile, char **digestp); 
void fromhod(struct part *inpart, FILE *outfile, char **digestp); 
void fromnone(struct part *inpart, FILE *outfile, char **digestp);
void SkipWhitespace(char **s);
 


/*
 * Read and handle an RFC 822 message from the body-part 'inpart'.
 */
handleMessage(inpart, defaultContentType, inAppleDouble, extractText)
struct part *inpart;
char *defaultContentType;
int inAppleDouble;
int extractText;
{
    char *headers, *subject, *contentType, *contentDisposition, *contentMD5;
    enum encoding contentEncoding;
    params contentParams;

    /* Parse the headers, getting the ones we're interested in */
    headers = ParseHeaders(inpart, &subject, &contentType, &contentEncoding,
			   &contentDisposition, &contentMD5);
    if (!headers) return 1;

    /* If no content type, or a non-MIME content type, use the default */
    if (!contentType || !strchr(contentType, '/')) {
	contentType = defaultContentType;
    }
    contentParams = ParseContent(&contentType);

    if (!cistrcmp(contentType, "message/rfc822")) {
	if (contentEncoding != enc_none) {
	    warn("ignoring invalid content encoding on message/rfc822");
	}

	/* Simple recursion */
	return handleMessage(inpart, "text/plain", 0, extractText);
    }
    else if (!cistrcmp(contentType, "message/partial")) {
	if (contentEncoding != enc_none) {
	    warn("ignoring invalid content encoding on message/partial");
	}
	return handlePartial(inpart, headers, contentParams, extractText);
    }
    else if (!cistrncmp(contentType, "message/", 8)) {
	/* Probably message/external.  We don't care--toss it */
	return ignoreMessage(inpart);
    }
    else if (!cistrncmp(contentType, "multipart/", 10)) {
	if (contentEncoding != enc_none) {
	    warn("ignoring invalid content encoding on multipart");
	}
	return handleMultipart(inpart, contentType, contentParams,
			       extractText);
    }
    else if (part_depth(inpart) == 0 &&
	     !cistrncmp(contentType, "text/", 5) &&
	     contentEncoding == enc_none &&
	     !getDispositionFilename(contentDisposition) &&
	     !getParam(contentParams, "name")) {
	/* top-level text message, handle as possible uuencoded file */
	return handleUuencode(inpart, subject, extractText);
    }
    else if (!extractText && !inAppleDouble &&
	     !cistrncmp(contentType, "text/", 5) &&
	     !getDispositionFilename(contentDisposition) &&
	     !getParam(contentParams, "name")) {
	return handleText(inpart, contentEncoding);
    }
    else {
	/* Some sort of attachment, extract it */
	return saveToFile(inpart, inAppleDouble, contentType, contentParams,
			  contentEncoding, contentDisposition, contentMD5);
    }
}

/*
 * Skip whitespace and RFC-822 comments.
 */
void       /* avoid warnings -- HJB, 28 Jul 98 */
SkipWhitespace(s)
char **s;
{
    char *p = *s;
    int commentlevel = 0;

    while (*p && (isspace(*p) || *p == '(')) {
	if (*p == '\n') {
	    p++;
	    if (*p != ' ' && *p != '\t') {
		*s = 0;
		return;
	    }
	}
	else if (*p == '(') {
	    p++;
	    commentlevel++;
	    while (commentlevel) {
		switch (*p) {
		case '\n':
		    p++;
		    if (*p == ' ' || *p == '\t') break;
		    /* FALL THROUGH */
		case '\0':
		    *s = 0;
		    return;
		    
		case '\\':
		    p++;
		    break;

		case '(':
		    commentlevel++;
		    break;

		case ')':
		    commentlevel--;
		    break;
		}
		p++;
	    }
	}
	else p++;
    }
    if (*p == 0) {
	*s = 0;
    }
    else {
	*s = p;
    }
}

/*
 * Read and parse the headers of an RFC 822 message, returning them in
 * a pointer to a static buffer.  The headers are read from 'inpart'.
 * A pointer to the value of any Subject:, Content-Type:,
 * Content-Disposition:, or Content-MD5: header is stored in the space
 * pointed to by 'subjectp', 'contentTypep', contentDispositionp, and
 * contentMD5p, respectively.  The Content-Transfer-Encoding is stored
 * in the enum pointed to by 'contentEncodingp'.
 */
#define HEADGROWSIZE 1000
char *ParseHeaders(inpart, subjectp, contentTypep, contentEncodingp,
		   contentDispositionp, contentMD5p)
struct part *inpart;
char **subjectp, **contentTypep;
enum encoding *contentEncodingp;
char **contentDispositionp, **contentMD5p;
{
    static int alloced = 0;
    static char *headers;
    int left, len, i;
    char *next, *val;
    char numbuf[50];

    /* Read headers into buffer pointed to by "headers" */
    if (!alloced) {
	headers = xmalloc(alloced = HEADGROWSIZE);
    }
    next = headers;
    *next++ = '\n';		/* Leading newline to make matching header names easier */
    left = alloced - 2;		/* Allow room for terminating null */

    while (part_gets(next, left, inpart) && (*next != '\n' || next[-1] != '\n')) {
	len = strlen(next);

	if (next[-1] == '\n') {
	    /* Check for valid header-ness of "next" */
	    for (i = 0; i < len; i++) {
		if (next[i] == ':' ||
		    next[i] <= ' ' || next[i] >= '\177') break;
	    }
	    if (i == 0 || next[i] != ':') {
		/* Check for header continuation line */
		if (next == headers+1 || (next[0] != ' ' && next[0] != '\t')) {
		    /*
		     * Not a valid header, push back on input stream
		     * and stop reading input.
		     */
		    part_ungets(next, inpart);
		    break;
		}
	    }
	}

	left -= len;
	next += len;

	if (left < 100) {
	    len = next - headers;
	    alloced += HEADGROWSIZE;
	    left += HEADGROWSIZE;
	    headers = xrealloc(headers, alloced);
	    next = headers + len;
	}
    }

    *next = '\0';

    /* Look for the headers we find particularly interesting */
    *subjectp = *contentTypep = *contentDispositionp = *contentMD5p = 0;
    *contentEncodingp = enc_none;
    for (next = headers; *next; next++) {
	if (*next == '\n') {
	    switch(next[1]) {
	    case 's':
	    case 'S':
		if (!cistrncmp(next+2, "ubject:", 7)) {
		    val = next+9;
		    SkipWhitespace(&val);
		    if (val) *subjectp = val;
		}
		break;

	    case 'c':
	    case 'C':
		if (!cistrncmp(next+2, "ontent-type:", 12)) {
		    val = next+14;
		    SkipWhitespace(&val);
		    if (val) *contentTypep = val;
		}
		else if (!cistrncmp(next+2, "ontent-transfer-encoding:", 25)) {
		    *contentEncodingp = parseEncoding(next+27);
		}
		else if (!cistrncmp(next+2, "ontent-disposition:", 19)) {
		    val = next+21;
		    SkipWhitespace(&val);
		    if (val) *contentDispositionp = val;
		}
		else if (!cistrncmp(next+2, "ontent-md5:", 11)) {
		    val = next+13;
		    SkipWhitespace(&val);
		    if (val) *contentMD5p = val;
		}
		break;

	    case 'x':
	    case 'X':
	        if (!cistrncmp(next+2, "-binary-size:", 13 )) {
	            val = next+15;
	            SkipWhitespace(&val);	       
	              if (val) {
	                inpart->size=0;
	                while (*val && isdigit(*val)) {
			  inpart->size *= 10;
	                  inpart->size += (*val-'0');
	                  val++;
	                }
	              }
	       } else {
	         if (!cistrncmp(next+2, "-binary-id:", 11 )) {
	           val = next+13;
	           SkipWhitespace(&val);	       
	           if (val) {
	              inpart->id=0;
	              while (*val && isdigit(*val)) {
			inpart->id *= 10;
	                inpart->id += (*val-'0');
	              val++;
	              }	
	           }
	         }
               }        
	    }  /* end of switch */
	}
    }
    return headers;
}

/*
 * Parse the Content-Transfer-Encoding: value pointed to by 's'.
 * Returns the appropriate encoding enum.
 */
enum encoding parseEncoding(s)
char *s;
{
    SkipWhitespace(&s);
    if (s) {
	switch (*s) {
	case 'q':
	case 'Q':
	    if (!cistrncmp(s+1, "uoted-printable", 15) &&
		(isspace(s[16]) || s[16] == '(')) {
		return enc_qp;
	    }
	    break;

	case '7':
	case '8':
	    if (!cistrncmp(s+1, "bit", 3) &&
		(isspace(s[4]) || s[4] == '(')) {
		return enc_none;
	    }
	    break;

	case 'b':
	case 'B':
	    if (!cistrncmp(s+1, "ase64", 5) &&
		(isspace(s[6]) || s[6] == '(')) {
		return enc_base64;
	    }
	    if (!cistrncmp(s+1, "inary", 5) &&
		(isspace(s[6]) || s[6] == '(')) {
		return enc_none;
	    }
            break;
        case 'x':
        case 'X':
            if (!cistrncmp(s+1, "-base8", 6) &&
                (isspace(s[7]) || s[7] == '(')) {
                return enc_base8;
            }
            if (!cistrncmp(s+1, "-base10", 7) &&
                (isspace(s[8]) || s[8] == '(')) {
                return enc_base10;
            }
            if (!cistrncmp(s+1, "-base16", 7) &&
                (isspace(s[8]) || s[8] == '(')) {
                return enc_base16;
            }
	}
	warn("ignoring unknown content transfer encoding\n");	
    }
    return enc_none;
}

/*
 * Parse the value of a Content-Type: header.
 * 'headerp' points to a pointer to the input string.
 * The pointer pointed to by 'headerp' is changed to point to
 * a static buffer containing the content type stripped of whitespace
 * and parameters.  The parameters are converted to a type suitable for
 * getParm() and returned.
 */
#define PARAMGROWSIZE 10
params ParseContent(headerp)
char **headerp;
{
    char *header;
    static int palloced = 0;
    static char **param;
    static int calloced = 0;
    static char *cbuf;
    char *p;
    int nparam;

    p = header = *headerp;

    /* Find end of header, including continuation lines */
    do {
	p = strchr(p+1, '\n');
    } while (p && isspace(p[1]));
    if (!p) {
	p = header + strlen(header);
    }

    /* If necessary, allocate/grow cbuf to hold header. */
    if (p - header >= calloced) {
	calloced = p - header + 1;
	if (calloced < 200) calloced = 200;
	cbuf = xrealloc(cbuf, calloced);
    }

    /* Copy header to cbuf */
    strncpy(cbuf, header, p - header);
    cbuf[p - header] = 0;
    header = *headerp = cbuf;
    
    nparam = 0;

    /* Strip whitespace from content type */
    /* ParseHeaders() stripped leading whitespace */
    p = header;
    while (header && *header && *header != ';') {
	while (*header && !isspace(*header) && *header != '(' &&
	       *header != ';') {
	    *p++ = *header++;
	}
	SkipWhitespace(&header);
    }
    if (!header || !*header) return 0;
    header++;
    *p = '\0';
    
    /* Parse the parameters */
    while (*header) {
	SkipWhitespace(&header);
	if (!header) break;

	if (nparam+1 >= palloced) {
	    palloced += PARAMGROWSIZE;
	    param = (char **) xrealloc((char *)param, palloced * sizeof(char *));
	}
	param[nparam++] = header;

	/* Find any separating semicolon.  Pay attention to quoted-strings */
	while (*header && *header != ';') {
	    if (*header == '\"') {
		++header;
		while (*header && *header != '\"') {
		    if (*header == '\\') {
			++header;
			if (!*header) break;
		    }
		    ++header;
		}
		if (!*header) break;
	    }
	    else if (*header == '(') {
		/* Convert comments to spaces */
		p = header;
		SkipWhitespace(&p);
		if (!p) {
		    break;
		}
		while (header < p) *header++ = ' ';
		header--;
	    }
	    header++;
	}
	if (*header) *header++ = '\0';
    }
    if (param) param[nparam] = 0;
    return param;
}

/*
 * Get the value of the parameter named 'key' from the content-type
 * parameters 'cParams'.  Returns a pointer to a static bufer which
 * contains the value, or null if no such parameter was found.
 */
#define VALUEGROWSIZE 100
char *getParam(cParams, key)
params cParams;
char *key;
{
    static char *value;
    static int alloced = 0;
    int left;
    int keylen = strlen(key);
    char *from, *to;

    if (!cParams) return 0;

    if (!alloced) {
	value = xmalloc(alloced = VALUEGROWSIZE);
    }

    /* Find the named parameter */
    while (*cParams) {
	if (!cistrncmp(key, *cParams, keylen) &&
	    ((*cParams)[keylen] == '=' || isspace((*cParams)[keylen]))) break;
	cParams++;
    }
    if (!*cParams) return 0;

    /* Skip over the "=" and any surrounding whitespace */
    from = *cParams + keylen;
    while (*from && isspace(*from)) from++;
    if (*from++ != '=') return 0;
    while (*from && isspace(*from)) from++;
    if (!*from) return 0;

    /* Copy value into buffer */
    to = value;
    left = alloced - 1;
    if (*from == '\"') {
	/* Quoted-string */
	from++;
	while (*from && *from != '\"') {
	    if (!--left) {
		alloced += VALUEGROWSIZE;
		value = xrealloc(value, alloced);
		to = value + alloced - left - 2;
	    }
	    if (*from == '\\') {
		from++;
		if (!*from) return 0;
	    }
	    *to++ = *from++;
	}
	if (!*from) return 0;
    }
    else {
	/* Just a token */
	while (*from && !isspace(*from)) {
	    if (!--left) {
		alloced += VALUEGROWSIZE;
		value = xrealloc(value, alloced);
		to = value + alloced - left - 2;
	    }
	    *to++ = *from++;
	}
    }
    *to = '\0';
    return value;
}

/*
 * Get the value of the "filename" parameter in a Content-Disposition:
 * header.  Returns a pointer to a static buffer containing the value, or
 * a null pointer if there was no such parameter.
 */
char *
getDispositionFilename(disposition)
char *disposition;
{
    static char *value;
    static int alloced = 0;
    int left;
    char *to;

    if (!disposition) return 0;

    /* Skip until we find ";" "filename" "=" tokens. */
    for (;;) {
	/* Skip until we find ";" */
	while (*disposition != ';') {
	    if (!*disposition) return 0;
	    else if (*disposition == '\"') {
		++disposition;
		while (*disposition && *disposition != '\"') {
		    if (*disposition == '\\') {
			++disposition;
			if (!*disposition) return 0;
		    }
		    ++disposition;
		}
		if (!*disposition) return 0;
	    }
	    else if (*disposition == '(') {
		SkipWhitespace(&disposition);
		if (!disposition) return 0;
		disposition--;
	    }
	    disposition++;
	}

	/* Skip over ";" and trailing whitespace */
	disposition++;
	SkipWhitespace(&disposition);
	if (!disposition) return 0;

	/*
	 * If we're not looking at a "filename" token, go back
	 * and look for another ";".  Otherwise skip it and
	 * trailing whitespace.
	 */
	if (cistrncmp(disposition, "filename", 8) != 0) continue;
	disposition += 8;
	if (!isspace(*disposition) && *disposition != '=' &&
	    *disposition != '(') {
	    continue;
	}
	SkipWhitespace(&disposition);
	if (!disposition) return 0;

	/* If we're looking at a ";", we found what we're looking for */
	if (*disposition++ == '=') break;
    }

    SkipWhitespace(&disposition);
    if (!disposition) return 0;
      
    if (!alloced) {
	value = xmalloc(alloced = VALUEGROWSIZE);
    }

    /* Copy value into buffer */
    to = value;
    left = alloced - 1;
    if (*disposition == '\"') {
	/* Quoted-string */
	disposition++;
	while (*disposition && *disposition != '\"') {
	    if (!--left) {
		alloced += VALUEGROWSIZE;
		value = xrealloc(value, alloced);
		to = value + alloced - left - 2;
	    }
	    if (*disposition == '\\') {
		disposition++;
		if (!*disposition) return 0;
	    }
	    *to++ = *disposition++;
	}
	if (!*disposition) return 0;
    }
    else {
	/* Just a token */
	while (*disposition && !isspace(*disposition) &&
	       *disposition != '(') {
	    if (!--left) {
		alloced += VALUEGROWSIZE;
		value = xrealloc(value, alloced);
		to = value + alloced - left - 2;
	    }
	    *to++ = *disposition++;
	}
    }
    *to = '\0';
    return value;
}    

/*
 * Read and handle a message/partial object from the file 'inpart'.
 */
handlePartial(inpart, headers, contentParams, extractText)
struct part *inpart;
char *headers;
params contentParams;
int extractText;
{
    char *id, *dir, *p;
    int thispart;
    int nparts = 0;
    char buf[1024];
    FILE *partfile, *outfile;
    struct part *outpart;
    int i, docopy;

    id = getParam(contentParams, "id");
    if (!id) {
	warn("partial message has no id parameter");
	goto ignore;
    }

    /* Get directory to store the parts being reassembled */
    dir = os_idtodir(id);
    if (!dir) goto ignore;

    p = getParam(contentParams, "number");
    if (!p) {
	warn("partial message doesn't have number parameter");
	goto ignore;
    }
    thispart = atoi(p);

    if (p = getParam(contentParams, "total")) {
	nparts = atoi(p);
	if (nparts <= 0) {
	    warn("partial message has invalid number of parts");
	    goto ignore;
	}
	/* Store number of parts in reassembly directory */
	sprintf(buf, "%sCT", dir);
	partfile = fopen(buf, "w");
	if (!partfile) {
	    os_perror(buf);
	    goto ignore;
	}
	fprintf(partfile, "%d\n", nparts);
	fclose(partfile);
    }
    else {
	/* Try to retrieve number of parts from reassembly directory */
	sprintf(buf, "%sCT", dir);
	if (partfile = fopen(buf, "r")) {
	    if (fgets(buf, sizeof(buf), partfile)) {
		nparts = atoi(buf);
		if (nparts < 0) nparts = 0;
	    }
	    fclose(partfile);
	}
    }

    /* Sanity check */
    if (thispart <= 0 || (nparts && thispart > nparts)) {
	warn("partial message has invalid number");
	goto ignore;
    }

    sprintf(buf, "Saving part %d ", thispart);
    if (nparts) sprintf(buf+strlen(buf), "of %d ", nparts);
    strcat(buf, getParam(contentParams, "id"));
    chat(buf);

    /* Create file to store this part */
    sprintf(buf, "%s%d", dir, thispart);
    partfile = fopen(buf, "w");
    if (!partfile) {
	os_perror(buf);
	goto ignore;
    }

    /* Do special-case header handling for first part */
    if (thispart == 1) {
	int skippedfirstbyte = 0;

	while (*headers) {
	    if (*headers == '\n' &&
		(!cistrncmp(headers, "\ncontent-", 9) ||
		 !cistrncmp(headers, "\nmessage-id:", 12))) {
		/* Special case, skip header */
		headers++;
		while (*headers && (*headers != '\n' || isspace(headers[1]))) {
		    headers++;
		}
	    }
	    else {
		/* First byte of headers is extra newline, don't write it to file */
		if (skippedfirstbyte++)	putc(*headers, partfile);
		headers++;
	    }
	}
	docopy = 0;
	/* Handle headers in the multipart/partial body */
	while (part_gets(buf, sizeof(buf), inpart)) {
	    if (*buf == '\n') {
		putc('\n', partfile);
		break;
	    }
	    if (!cistrncmp(buf, "content-", 8) || !cistrncmp(buf, "message-id:", 11)) {
		docopy = 1;
	    }
	    else if (!isspace(*buf)) {
		docopy = 0;
	    }

	    if (docopy) fputs(buf, partfile);
	    while(buf[strlen(buf)-1] != '\n' && part_gets(buf, sizeof(buf), inpart)) {
		if (docopy) fputs(buf, partfile);
	    }
	}
    }

    /* Copy the contents to the file */
    while (part_gets(buf, sizeof(buf), inpart)) {
	fputs(buf, partfile);
    }
    fclose(partfile);

    /* Check to see if we have all parts.  Start from the highest numbers
     * as we are more likely not to have them.
     */
    for (i = nparts; i; i--) {
	sprintf(buf, "%s%d", dir, i);
	partfile = fopen(buf, "r");
	if (partfile) {
	    fclose(partfile);
	}
	else {
	    break;
	}
    }

    if (i || !nparts) {
	/* We don't have all the parts yet */
	return 0;
    }

    /* We have everything, concatenate all the parts into a single file */
    sprintf(buf, "%sFULL", dir);
    outfile = fopen(buf, "w");
    if (!outfile) {
	os_perror(buf);
	return 1;
    }
    for (i=1; i<=nparts; i++) {
	sprintf(buf, "%s%d", dir, i);
	partfile = fopen(buf, "r");
	if (!partfile) {
	    os_perror(buf);
	    return 1;
	}
	while (fgets(buf, sizeof(buf), partfile)) {
	    fputs(buf, outfile);
	}
	fclose(partfile);

	/* Done with message part file, delete it */
	sprintf(buf, "%s%d", dir, i);
	remove(buf);
    }

    /* Open the concatenated file for reading and handle it */
    fclose(outfile);
    sprintf(buf, "%sFULL", dir);
    outfile = fopen(buf, "r");
    if (!outfile) {
	os_perror(buf);
	return 1;
    }
    outpart = part_init(outfile);
    handleMessage(outpart, "text/plain", 0, extractText);
    part_close(outpart);

    /* Clean up the rest of the reassembly directory */
    sprintf(buf, "%sFULL", dir);
    remove(buf);
    sprintf(buf, "%sCT", dir);
    remove(buf);
    os_donewithdir(dir);

    return 0;

 ignore:
    ignoreMessage(inpart);
    return 1;
}

/*
 * Skip over a message object from the file 'inpart'.
 */
ignoreMessage(inpart)
struct part *inpart;
{
    while (part_getc(inpart) != EOF);
    return 0;
}

/*
 * Read and handle a multipart object from 'inpart'.
 */
handleMultipart(inpart, contentType, contentParams, extractText)
struct part *inpart;
char *contentType;
params contentParams;
int extractText;
{
    char *id;
    char *defaultContentType = "text/plain";
    int isAppleDouble = 0;

    /* Components of multipart/digest have a different default content-type */
    if (!cistrcmp(contentType, "multipart/digest")) {
	defaultContentType = "message/rfc822";
    }
    if (!cistrcmp(contentType, "multipart/appledouble")) {
	isAppleDouble++;
    }

    if (!(id = getParam(contentParams, "boundary"))) {
	warn("multipart message has no boundary parameter");
	id="";
    }

    /* Add the new boundary id */
    part_addboundary(inpart, id);

#ifdef __riscos
    /*
     * "Marcel" encodes RISCOS directory structure in the multipart
     * structure.  That is the Wrong Way to do it, but we hold our
     * nose and pass the information to the OS layer.
     */
    os_boundaryhookopen(part_depth(inpart));
#endif

    /*
     * Skip over preamble.
     * HACK: The initial boundary doesn't have to start with a newline,
     * so we deal with this by stuffing an initial newline into the input
     * stream
     */
    part_ungetc('\n', inpart);
    ignoreMessage(inpart);

    /* Handle the component messages */
    while (!part_readboundary(inpart)) {
	handleMessage(inpart, defaultContentType, isAppleDouble, extractText);
    }

#ifdef __riscos
    os_boundaryhookclose(part_depth(inpart));
#endif

    /* Skip over postamble */
    ignoreMessage(inpart);

    /* Remove any lingering unused description file */
    (void) remove(TEMPFILENAME);

    return 0;
}

/*
 * Handle a text message object from 'inpart' by saving it to
 * the temporary description file.
 */
int handleText(inpart, contentEncoding)
struct part *inpart;
enum encoding contentEncoding;
{
    FILE *descfile;

    descfile = fopen(TEMPFILENAME, "w");
    if (!descfile) {
	os_perror(TEMPFILENAME);
	ignoreMessage(inpart);
	return 1;
    }

    /* Write the file, handling the appropriate encoding */
    switch (contentEncoding) {
    case enc_none:
	fromnone(inpart, descfile, (char **)0);
	break;

    case enc_qp:
	fromqp(inpart, descfile, (char **)0);
	break;

    case enc_base64:
	from64(inpart, descfile, (char **)0, 1);
	break;

    case enc_base8:
    case enc_base10:
    case enc_base16:
        fromhod(inpart, descfile, (char **)0);
        break;
    }

    fclose(descfile);
    return 0;
}

/*
 * Read a message object from 'inpart' and save it to a file.
 */
saveToFile(inpart, inAppleDouble, contentType, contentParams,
	   contentEncoding, contentDisposition, contentMD5)
struct part *inpart;
int inAppleDouble;
char *contentType;
params contentParams;
enum encoding contentEncoding;
char *contentDisposition, *contentMD5;
{
    FILE *outfile = 0;
    int flags = 0;
    int suppressCR = 0;
    char *outputmd5;
    char *fname;
    unsigned int convtype;
    int resultcode, maxbits, bits0, bits1;

    if (!cistrncmp(contentType, "text/", 5)) {
	suppressCR = 1;
    }
    else if (contentEncoding == enc_base64
             || contentEncoding == enc_base8
             || contentEncoding == enc_base10
             || contentEncoding == enc_base16 ) {
	/*
	 * HEURISTIC: It is not in general possible to determine whether
	 * any non-text content type is line-oriented.  We guess
	 * the "binary" status of a part from the composer's choice
	 * of content transfer encoding.
	 *
	 * If the content transfer encoding is "binary" and the input is
	 * not line-oriented, we're screwed anyway--the input file has
	 * been opened in text mode.  So the "binary output file" heuristic
	 * is not applied in this case.
	 */
	flags |= FILE_BINARY;
    }

    if (inAppleDouble) flags |= FILE_INAPPLEDOUBLE;

    if (inpart->ciftext){
      inpart->conversions = getParam(contentParams, "conversions");
      inpart->conversions = strsave(inpart->conversions); 
    }
    
    /* Find an appropriate filename and create the output file */
    fname = getDispositionFilename(contentDisposition);
    if (!fname) fname = getParam(contentParams, "name");
    if (fname) fname = strsave(fname);
    if (!inpart->xferfile) {
      outfile = os_newtypedfile(fname, contentType, flags, contentParams);
    } else {
      outfile = inpart->xferfile;
    }
    if (fname) free(fname);
    if (!outfile) {
	ignoreMessage(inpart);
	return 1;
    }

    if (inpart->ciftext){
      convtype = CBF_NONE;
      if(!cistrncmp(inpart->conversions, "x-cbf_packed", 12))
        convtype = CBF_PACKED;
      if(!cistrncmp(inpart->conversions, "x-cbf_canonical", 15))
        convtype = CBF_CANONICAL;
      if(!cistrncmp(inpart->conversions, "x-cbf_byte_offset", 17))
        convtype = CBF_BYTE_OFFSET;
      if(!cistrncmp(inpart->conversions, "x-cbf_predictor", 15))
        convtype = CBF_PREDICTOR;
      maxbits = sizeof(int) * CHAR_BIT;
      if (maxbits > 64) maxbits = 64;
      bits0 = maxbits;
      bits1 = convtype & 0x0ff;
      putc (bits1 & 0xff, outfile);
      bits0 -=8;
      bits1 = convtype >> (maxbits - bits0);
      while (bits0 >= 8) { 
        putc (bits1 & 0x0ff, outfile);
        bits1 >>= 8;
        bits0 -=8;
      }
      bits0 = 64-maxbits;
      for (bits0 = 64-maxbits; bits0 > 0; bits0 -=8) {
        putc (0,outfile);
      }
    }

    /* Write the file, handling the appropriate encoding */
    switch (contentEncoding) {
    case enc_none:
	fromnone(inpart, outfile, &outputmd5);
	break;

    case enc_qp:
	fromqp(inpart, outfile, &outputmd5);
	break;

    case enc_base64:
	from64(inpart, outfile, &outputmd5, suppressCR);
	break;
    case enc_base8:
    case enc_base10:
    case enc_base16:
        fromhod(inpart, outfile, &outputmd5);
        break;
    }
    rewind(outfile);
    if (! inpart->binflag ) {
    /* Check the MD5 digest if it was supplied */
    if (contentMD5) {
	if (strncmp(outputmd5, contentMD5, strlen(outputmd5)) != 0) {
	    os_warnMD5mismatch();
	}
    }
    free(outputmd5);
    }

    if (!inpart->ciftext)os_closetypedfile(outfile);

    return inpart->binflag;
}

#define XX 127
/*
 * Table for decoding hexadecimal in quoted-printable
 */
static char index_hex[256] = {
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
     0, 1, 2, 3,  4, 5, 6, 7,  8, 9,XX,XX, XX,XX,XX,XX,
    XX,10,11,12, 13,14,15,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,10,11,12, 13,14,15,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
};
#define HEXCHAR(c)  (index_hex[(unsigned char)(c)])

/*
 * Table for decoding base64
 */
static char index_64[256] = {
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,62, XX,XX,XX,63,
    52,53,54,55, 56,57,58,59, 60,61,XX,XX, XX,XX,XX,XX,
    XX, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14,
    15,16,17,18, 19,20,21,22, 23,24,25,XX, XX,XX,XX,XX,
    XX,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40,
    41,42,43,44, 45,46,47,48, 49,50,51,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
};
#define CHAR64(c)  (index_64[(unsigned char)(c)])

void           /* avoid warning messages -- HJB, 28 Jul 98 */
from64(inpart, outfile, digestp, suppressCR)
struct part *inpart;
FILE *outfile;
char **digestp;
int suppressCR;
{
    int c1, c2, c3, c4;
    int DataDone = 0;
    char buf[3];
    MD5_CTX context;
    long size;

    if (digestp) MD5Init(&context);
    size = 0;
    while ((c1 = part_getc(inpart)) != EOF) {
        if (c1 != '=' && CHAR64(c1) == XX) {
            continue;
        }
        if (DataDone) continue;
        do {
            c2 = part_getc(inpart);
        } while (c2 != EOF && c2 != '=' && CHAR64(c2) == XX);
        do {
            c3 = part_getc(inpart);
        } while (c3 != EOF && c3 != '=' && CHAR64(c3) == XX);
        do {
            c4 = part_getc(inpart);
        } while (c4 != EOF && c4 != '=' && CHAR64(c4) == XX);
        if (c2 == EOF || c3 == EOF || c4 == EOF) {
            warn("Premature EOF");
            break;
        }
        if (c1 == '=' || c2 == '=') {
            DataDone=1;
            continue;
        }
        c1 = CHAR64(c1);
        c2 = CHAR64(c2);
	buf[0] = ((c1<<2) | ((c2&0x30)>>4));
        if (!suppressCR || buf[0] != '\r'){
          putc(buf[0], outfile);
          size++;
        }
        if (c3 == '=') {
	    if (digestp) MD5Update(&context, buf, 1);
            DataDone = 1;
        } else {
            c3 = CHAR64(c3);
	    buf[1] = (((c2&0x0F) << 4) | ((c3&0x3C) >> 2));
            if (!suppressCR || buf[1] != '\r') {
              putc(buf[1], outfile);
              size++;
            }
            if (c4 == '=') {
		if (digestp) MD5Update(&context, buf, 2);
                DataDone = 1;
            } else {
                c4 = CHAR64(c4);
		buf[2] = (((c3&0x03) << 6) | c4);
                if (!suppressCR || buf[2] != '\r') {
                  putc(buf[2], outfile);
		  size++;
                }         
		if (digestp) MD5Update(&context, buf, 3);		
            }
        }
    }
    if (digestp) *digestp = md5contextTo64(&context);
    if (inpart->size && inpart->ciftext) {
      if (inpart->size != size) warn("X-Binary-Size does not match Size");
    }
    inpart->size = size;
    inpart->gotfile = 1;
}

void           /* avoid warning messages -- HJB, 28 Jul 98 */
fromqp(inpart, outfile, digestp)
struct part *inpart;
FILE *outfile;
char **digestp;
{
    int c1, c2;
    MD5_CTX context;
    char c;
    long size;

    if (digestp) MD5Init(&context);
    size = 0;

    while ((c1 = part_getc(inpart)) != EOF) {
	if (c1 == '=') {
	    c1 = part_getc(inpart);
	    if (c1 != '\n') {
		c1 = HEXCHAR(c1);
		c2 = part_getc(inpart);
		c2 = HEXCHAR(c2);
		c = c1<<4 | c2;
		if (c != '\r') {
                  putc(c, outfile);
                  size++;
                }
		if (digestp) MD5Update(&context, &c, 1);
	    }
	} else {
	    putc(c1, outfile);
            size++;
	    if (c1 == '\n') {
		if (digestp) MD5Update(&context, "\r", 1);
	    }
	    c = c1;
	    if (digestp) MD5Update(&context, &c, 1);
	}
    }
    if (digestp) *digestp=md5contextTo64(&context);
    if (inpart->size && inpart->ciftext) {
      if (inpart->size != size) warn("X-Binary-Size does not match Size");
    }
    inpart->size = size;
    inpart->gotfile = 1;
}

void           /* avoid warning messages -- HJB, 28 Jul 98 */
fromnone(inpart, outfile, digestp) 
struct part *inpart;
FILE *outfile;
char **digestp;
{
    int c;
    char ch;
    MD5_CTX context;
    long size;

    if (digestp) MD5Init(&context);
    size = 0;

    while ((c = part_getc(inpart)) != EOF) {
        if ( inpart->ciftext && c == 213 ) {
          inpart->binflag = 1;
          ignoreMessage(inpart);
          return;
        }
	putc(c, outfile);
        size++;
	if (c == '\n') {
	    if (digestp) MD5Update(&context, "\r", 1);
	}
	ch = c;
	if (digestp) MD5Update(&context, &ch, 1);
    }
    if (digestp) *digestp=md5contextTo64(&context);
    if (inpart->size && inpart->ciftext) {
      if (inpart->size != size) warn("X-Binary-Size does not match Size");
    }
    inpart->size = size;
    inpart->gotfile = 1;
}

void           /* avoid warning messages -- HJB, 28 Jul 98 */
fromhod(inpart, outfile, digestp)
struct part *inpart;
FILE *outfile;
char **digestp;
{
    int c1, ibyte;
    int column, bcol, comment;
    MD5_CTX context;
    char c;
    int radix;
    char buffer[80];
    long long word;
    int bytedir, elsize, flush, kpad;
    long size;

    if (digestp) MD5Init(&context);
    size = 0;

    column = 0;
    radix = 10;
    bytedir = 1;
    comment = 0;
    elsize = 4;
    bcol = 0;
    flush = 0;
    kpad = 0;
    while ((c1 = part_getc(inpart)) != EOF) {
        if (flush) {
            if (bcol > 0 ) {
              buffer[bcol] = 0;
	      if ( radix == 16 ) {
                sscanf(buffer,"%llx",&word);
	      } else {
                if ( radix ==  8) {
                  sscanf(buffer,"%llo",&word);
	        } else {
		  sscanf(buffer,"%lli",&word);
                }
	      }
              bcol = 0;
              kpad -= (kpad%2);
              for (ibyte = 0; ibyte < elsize-kpad/2; ibyte++ ){
                if (bytedir < 0) {
                  c = (word >> (ibyte*8))&255;
                } else {
	          c = (word >> ((elsize-kpad/2-ibyte-1)*8))&255;
	        }
                putc (c, outfile);
                if (digestp) MD5Update(&context, &c, 1);
              }
              size += elsize-kpad/2;
              kpad = 0;
            }
            flush = 0;
        }
        column++;
        if (c1 == '\n' || c1 == '\r') {
          column = 0;
          comment = 0;
          flush = 1;
        }
        if (!comment) {
	  switch (c1) {
            case '>':
              bytedir = 1;
              break;
            case '<':
              bytedir = -1;
              break; 
            case '#': 
              comment = 1;
	  }
          if (column == 1 ) {
            if ( c1 == 'H' || c1 == 'h' ) radix = 16;
            if ( c1 == 'D' || c1 == 'd' ) radix = 10;
            if ( c1 == 'O' || c1 == 'o' ) radix = 8;
          }
          if (column == 2 ) {
	    if (isdigit(c1) && c1 != '0' ) elsize = c1-'0';
	  }
          if (column > 3 ) {
          if (isspace(c1)) {
            flush = 1;
	  } else {
            if ( c1 == '=' ) {
              kpad++;
            } else {
              buffer[bcol++] = c1;
            }
          }
          }
	}
    }
    if (bcol > 0 ) {
      buffer[bcol] = 0;
      if (radix == 16) {
        sscanf(buffer,"%llx",&word);
      } else {
        if (radix == 8) {
          sscanf(buffer,"%llo",&word);
	} else {
	  sscanf(buffer,"%lli",&word);
        }
      }
      bcol = 0;
      kpad -= (kpad%2);
      for (ibyte = 0; ibyte < elsize-kpad/2; ibyte++ ){
         if (bytedir < 0) {
           c = (word >> (ibyte*8))&255;
         } else {
	   c = (word >> ((elsize-kpad/2-ibyte-1)*8))&255;
	 }
          putc (c, outfile);
          if (digestp) MD5Update(&context, &c, 1);
       }
       size += elsize-kpad/2;
       kpad = 0;
    }
    if (digestp) *digestp=md5contextTo64(&context);
    if (inpart->size && inpart->ciftext) {
      if (inpart->size != size) warn("X-Binary-Size does not match Size");
    }
    inpart->size = size;
    inpart->gotfile = 1;
}
