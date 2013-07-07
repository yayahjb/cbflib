/**********************************************************************
 * cbf_hdf5 -- read and write HDF5/NeXus files                        *
 *                                                                    *
 * Version 0.9.3 21 December 2012                                     *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2009, 2012 Herbert J. Bernstein                      *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
 **********************************************************************/

/*************************** GPL NOTICES ******************************
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * (the License, or (at your option) any later version.               *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           *
 * 02111-1307  USA                                                    *
 *                                                                    *
 **********************************************************************/

/************************* LGPL NOTICES *******************************
 *                                                                    *
 * This library is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU Lesser General Public         *
 * License as published by the Free Software Foundation; either       *
 * version 2.1 of the License, or (at your option) any later version. *
 *                                                                    *
 * This library is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
 * Lesser General Public License for more details.                    *
 *                                                                    *
 * You should have received a copy of the GNU Lesser General Public   *
 * License along with this library; if not, write to the Free         *
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    *
 * MA  02110-1301  USA                                                *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 *                    Stanford University Notices                     *
 *  for the CBFlib software package that incorporates SLAC software   *
 *                 on which copyright is disclaimed                   *
 *                                                                    *
 * This software                                                      *
 * -------------                                                      *
 * The term "this software", as used in these Notices, refers to      *
 * those portions of the software package CBFlib that were created by *
 * employees of the Stanford Linear Accelerator Center, Stanford      *
 * University.                                                        *
 *                                                                    *
 * Stanford disclaimer of copyright                                   *
 * --------------------------------                                   *
 * Stanford University, owner of the copyright, hereby disclaims its  *
 * copyright and all other rights in this software.  Hence, anyone    *
 * may freely use it for any purpose without restriction.             *
 *                                                                    *
 * Acknowledgement of sponsorship                                     *
 * ------------------------------                                     *
 * This software was produced by the Stanford Linear Accelerator      *
 * Center, Stanford University, under Contract DE-AC03-76SFO0515 with *
 * the Department of Energy.                                          *
 *                                                                    *
 * Government disclaimer of liability                                 *
 * ----------------------------------                                 *
 * Neither the United States nor the United States Department of      *
 * Energy, nor any of their employees, makes any warranty, express or *
 * implied, or assumes any legal liability or responsibility for the  *
 * accuracy, completeness, or usefulness of any data, apparatus,      *
 * product, or process disclosed, or represents that its use would    *
 * not infringe privately owned rights.                               *
 *                                                                    *
 * Stanford disclaimer of liability                                   *
 * --------------------------------                                   *
 * Stanford University makes no representations or warranties,        *
 * express or implied, nor assumes any liability for the use of this  *
 * software.                                                          *
 *                                                                    *
 * Maintenance of notices                                             *
 * ----------------------                                             *
 * In the interest of clarity regarding the origin and status of this *
 * software, this and all the preceding Stanford University notices   *
 * are to remain affixed to any copy or derivative of this software   *
 * made or distributed by the recipient and are to be affixed to any  *
 * copy of software made or distributed by the recipient that         *
 * contains a copy or derivative of this software.                    *
 *                                                                    *
 * Based on SLAC Software Notices, Set 4                              *
 * OTT.002a, 2004 FEB 03                                              *
 **********************************************************************/



/**********************************************************************
 *                               NOTICE                               *
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

/**********************************************************************
 *                                                                    *
 *                           The IUCr Policy                          *
 *      for the Protection and the Promotion of the STAR File and     *
 *     CIF Standards for Exchanging and Archiving Electronic Data     *
 *                                                                    *
 * Overview                                                           *
 *                                                                    *
 * The Crystallographic Information File (CIF)[1] is a standard for   *
 * information interchange promulgated by the International Union of  *
 * Crystallography (IUCr). CIF (Hall, Allen & Brown, 1991) is the     *
 * recommended method for submitting publications to Acta             *
 * Crystallographica Section C and reports of crystal structure       *
 * determinations to other sections of Acta Crystallographica         *
 * and many other journals. The syntax of a CIF is a subset of the    *
 * more general STAR File[2] format. The CIF and STAR File approaches *
 * are used increasingly in the structural sciences for data exchange *
 * and archiving, and are having a significant influence on these     *
 * activities in other fields.                                        *
 *                                                                    *
 * Statement of intent                                                *
 *                                                                    *
 * The IUCr's interest in the STAR File is as a general data          *
 * interchange standard for science, and its interest in the CIF,     *
 * a conformant derivative of the STAR File, is as a concise data     *
 * exchange and archival standard for crystallography and structural  *
 * science.                                                           *
 *                                                                    *
 * Protection of the standards                                        *
 *                                                                    *
 * To protect the STAR File and the CIF as standards for              *
 * interchanging and archiving electronic data, the IUCr, on behalf   *
 * of the scientific community,                                       *
 *                                                                    *
 * * holds the copyrights on the standards themselves,                *
 *                                                                    *
 * * owns the associated trademarks and service marks, and            *
 *                                                                    *
 * * holds a patent on the STAR File.                                 *
 *                                                                    *
 * These intellectual property rights relate solely to the            *
 * interchange formats, not to the data contained therein, nor to     *
 * the software used in the generation, access or manipulation of     *
 * the data.                                                          *
 *                                                                    *
 * Promotion of the standards                                         *
 *                                                                    *
 * The sole requirement that the IUCr, in its protective role,        *
 * imposes on software purporting to process STAR File or CIF data    *
 * is that the following conditions be met prior to sale or           *
 * distribution.                                                      *
 *                                                                    *
 * * Software claiming to read files written to either the STAR       *
 * File or the CIF standard must be able to extract the pertinent     *
 * data from a file conformant to the STAR File syntax, or the CIF    *
 * syntax, respectively.                                              *
 *                                                                    *
 * * Software claiming to write files in either the STAR File, or     *
 * the CIF, standard must produce files that are conformant to the    *
 * STAR File syntax, or the CIF syntax, respectively.                 *
 *                                                                    *
 * * Software claiming to read definitions from a specific data       *
 * dictionary approved by the IUCr must be able to extract any        *
 * pertinent definition which is conformant to the dictionary         *
 * definition language (DDL)[3] associated with that dictionary.      *
 *                                                                    *
 * The IUCr, through its Committee on CIF Standards, will assist      *
 * any developer to verify that software meets these conformance      *
 * conditions.                                                        *
 *                                                                    *
 * Glossary of terms                                                  *
 *                                                                    *
 * [1] CIF:  is a data file conformant to the file syntax defined     *
 * at http://www.iucr.org/iucr-top/cif/spec/index.html                *
 *                                                                    *
 * [2] STAR File:  is a data file conformant to the file syntax       *
 * defined at http://www.iucr.org/iucr-top/cif/spec/star/index.html   *
 *                                                                    *
 * [3] DDL:  is a language used in a data dictionary to define data   *
 * items in terms of "attributes". Dictionaries currently approved    *
 * by the IUCr, and the DDL versions used to construct these          *
 * dictionaries, are listed at                                        *
 * http://www.iucr.org/iucr-top/cif/spec/ddl/index.html               *
 *                                                                    *
 * Last modified: 30 September 2000                                   *
 *                                                                    *
 * IUCr Policy Copyright (C) 2000 International Union of              *
 * Crystallography                                                    *
 **********************************************************************/



#ifdef __cplusplus

extern "C" {
    
#endif
    
#include "cbf.h"
#include "cbf_tree.h"
#include "cbf_hdf5.h"
#include "cbf_ascii.h"
#include "cbf_binary.h"
#include "cbf_compress.h"
#include "cbf_file.h"
#include "cbf_write.h"
#include "cbf_write_binary.h"
#include "cbf_read_mime.h"
#include "cbf_string.h"
#include "cbf_codes.h"
#include "cbf_alloc.h"
#include "cbf_simple.h"
#include "cbf_tree.h"
#include "cbf_hdf5_filter.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <errno.h>
    
    
    
    /* Macros to get the current location in a file in form `file:line' */
    
#define __STR2(n) #n
#define __STR(n) __STR2(n)
#define __WHERE__ __FILE__":"__STR(__LINE__)
    
    /* Macro to check the given error code & print some
     useful output if it is set */
    
#define CBF_CHECK_ERROR(cbferror) \
{ \
const int __error = (cbferror); \
if (CBF_SUCCESS != __error) fprintf(stderr,__WHERE__": CBF error: %s\n",cbf_strerror(__error)); \
};
    
    /****************************************************************
     The following section of code is extracted from J. Sloan's
     cbf_hdf5.i
     ****************************************************************/
    
    /*
     Some comparison functions for use in checking the content of
     HDF5 datasets/attributes
     
     Should return 0 on success, non-zero on failure
     */
    
    static int cmp_double(const void * a, const void * b, size_t N)
    {
        /* go through each vector comparing all values
         The allowed delta is 1.e-38+1.e-13*maxmimum of the
         summed pairs of absolute values
         */
        
        double delta = 0.;
        const double * da = a;
        const double * db = b;
        
        size_t i;
        
        for (i=0; i < N; i++) {
            if (fabs(da[i])+fabs(db[i]) > delta) delta = fabs(da[i])+fabs(db[i]);
            if (fabs(da[i]-db[i]) > 1.e-38+1.e-13*delta) return 1;
        }
        
        for (i=0; i < N-1; i++) {
            if (fabs(da[i]-da[i]) > 1.e-38+1.e-13*delta) return 1;
        }
        
        return 0;
    }
    
    static int cmp_int(const void * a, const void * b, size_t N)
    {
        /* go through each vector comparing all values */
        
        while (N && *(const int *)(a)++ == *(const int *)(b)++) --N;
        
        /* if any are not equal the loop will exit early and N is non-zero */
        
        return N;
    }
    
    static int cmp_vlstring(const void * a, const void * b, size_t N)
    {
        /* go through each vector comparing all values */
        while (N && !cbf_cistrcmp(*(const char **)(a)++, *(const char **)(b)++)) --N;
        /* if any are not equal the loop will exit early and N is non-zero */
        return N;
    }
    
	/**
     Put a character into a buffer at a given position.
     
     Ensures the buffer is long enough to hold the new character, realloc'ing if needed, and then inserts it.
     
     \param c The character.
     \param buf A pointer to the realloc'able buffer.
     \param n A pointer to the current length of the buffer.
     \param k The offset to place the character <code>c</code> in.
     
     \return void
	 */
	void cbf_push_buf(const int c, char * * const buf, size_t * const n, size_t *k)
	{
		assert(buf);
		assert(n);
		assert(k);
		if (*k >= *n) {
			*n = 2 * *k;
			*buf = realloc(*buf, *n);
		}
		(*buf)[(*k)++] = c;
	}
    
	/**
     Function to tokenise a pilatus v1.2 minicbf header
     
     Will split the header into null-terminated strings that are passed around via a given realloc'able buffer.
     
     Behviour is determined by <code>newline</code>, if <code>newline</code> is non-zero:
     A token starting with a digit, <code>[0-9]</code>, will cause a string consisting of digits
     and any number or combination of <code>[T:-.]</code> characters to be matched as a token.
     Otherwise, the stream will be tokenised according to the default rules.
     
     The default tokenisation method is to split the stream on groups of characters from the set <code>[#=:,() \t\f\v\r\n]</code>.
     Any number of adjacent <code>'\r'</code> and <code>'\n'</code> characters are compressed into a single
     newline (<code>"\n"</code>) token.
	 */
    
    
	static int _cbf_scan_pilatus_V1_2_miniheader
    (char * * const buf,
     size_t * const n,
     int * const newline,
     const int getRestOfLine,
     const char * * const string)
	{
#define cbf_sgetc(str) (**str ? *(*str)++ : 0)
#define cbf_seof(str) (!**str)
#define cbf_ungetc(str) (--(*str))
		int c = 0; /* current character */
		size_t k = 0; /* current line length */
		const char spaceChars[] = "#=:,() \t\f\v";
		const char newlineChars[] = "\n\r";
        
		/* check that sensible arguments are given */
		assert(buf);
		assert(n);
		assert(newline);
		assert(string);
        
		/*
         Skip space-equivalent characters to find something interesting.
         This ends with one of two states: a non-space character is found, or EOF.
         */
		do {
			c = cbf_sgetc(string);
			if (cbf_seof(string)) break;
			if (strchr(spaceChars,c)) continue;
			else break;
		} while (1);
        
		/* if I am at the end of the stream, free the buffer & return 0 */
		if (cbf_seof(string)) {
			free((void*)(*buf));
			*buf = 0;
			*n = 0;
			return CBF_SUCCESS;
		}
        
		/* I have a token, starting with the character c: read it */
		if (strchr(newlineChars,c)) {
			/* it's a newline token: consume all consecutive newline-equivalent characters */
			do {
				c = cbf_sgetc(string);
			} while (!cbf_seof(string) && strchr(newlineChars,c));
			if (!cbf_seof(string)) cbf_ungetc(string);
			cbf_push_buf('\n', buf, n, &k);
			*newline = 1;
		} else {
			/* It's a string token */
			if (getRestOfLine || (*newline && isdigit(c))) {
				/* scan to the end of the current line */
				do {
					cbf_push_buf(c, buf, n, &k);
					c = cbf_sgetc(string);
				} while (!cbf_seof(string) && !strchr(newlineChars,c));
				if (!cbf_seof(string)) cbf_ungetc(string);
			} else {
				/* scan it as an arbitrary token */
				do {
					cbf_push_buf(c, buf, n, &k);
					c = cbf_sgetc(string);
				} while (!cbf_seof(string) && !strchr(spaceChars,c) && !strchr(newlineChars,c));
				if (!cbf_seof(string)) cbf_ungetc(string);
			}
			*newline = 0;
		}
		/* null-terminate the token */
		cbf_push_buf('\0', buf, n, &k);
        
		return CBF_SUCCESS;
	}
    
	/**
     Helper function to check if a string is a valid (pilatus format) null-terminated date.
     
     Requires string of format:
     <code>YYYY-MM-DDThh:mm:ss.s+</code>
     <code>Y</code>: year
     <code>M</code>: month
     <code>D</code>: day
     <code>h</code>: hour
     <code>m</code>: minute
     <code>s</code>: second
     <code>+</code>: second may be fractional, any number of digits are allowed
     <code>[-T:]</code>: literal characters
     
     \param str The string to test.
     
     \return non-zero on success, zero otherwise.
	 */
	int cbf_isPilatusDate(const char * str)
	{
		if (!str) return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if ('-'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if ('-'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if ('T'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (':'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (':'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if ('.'==*str) ++str; else return 0;
		while (isdigit(*str)) ++str;
		return '\0'==*str;
	}
    
	/**
     Helper function to take the data associated with a pilatus axis and write the
     axis attributes to a nexus axis.
     
     \return An error code
	 */
	static int _cbf_pilatusAxis2nexusAxisAttrs
    (hid_t h5data,
     const char * const units,
     const cbf_hdf5_configItem * const axisItem,
     int (*cmp)(const void *, const void *, size_t))
    {
        int error = CBF_SUCCESS;
        cbf_reportnez(cbf_H5Arequire_string(h5data,"units",cbf_cistrcmp(units, "deg.")?units:"deg"),error);
        /* transformation type */
        cbf_reportnez(cbf_H5Arequire_string(h5data,"transformation_type","rotation"),error);
        /* dependency */
        cbf_reportnez(cbf_H5Arequire_string(h5data,"depends_on",axisItem->depends_on?axisItem->depends_on:""),error);
        if (!axisItem->depends_on) fprintf(stderr,"Missing dependancy for nexus axis '%s'\n",axisItem->nexus);
        { /* vector */
            const hsize_t vdims[] = {3};
            double buf[3] = {0./0.};
            cbf_reportnez(cbf_H5Arequire_cmp(h5data,"vector",1,vdims,H5T_IEEE_F64LE,
                                             axisItem->vector,buf,cmp),error);
        }
        return error;
    }
    
    
    /****************************************************************
     The following section of code is extracted from J. Sloan's
     cbf_hdf5_common.c
     ****************************************************************/
    
    
	/** \brief Check the validity of an object identifier
     
     Function to check validity of a HDF5 identifier.
     HDF5's predefined types are never counted as valid by this function,
     so it can't be used to test the validity of a type constant.
     Types obtained by using H5Tcopy are safe to test.
     
     \param ID An HDF5 object identifier.
     
     \return Non-zero if the type is valid, zero otherwise.
     */
	int cbf_H5Ivalid(const hid_t ID)
	{
		const htri_t v = H5Iis_valid(ID);
		if (v < 0) fprintf(stderr, "%s:%d: H5Iis_valid call failed.\n", __FILE__, __LINE__);
		return v > 0;
	}
    
	/** \brief Try to free an object identifier
     
     Function to close any handle without tracking its type.
     Don't use this if a more specific function can be used instead, ie if the
     type is known, as this function will be less efficient.
     
     \param ID An HDF5 object identifier to be closed.
     
     \return An error code.
     */
	int cbf_H5Ifree(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) {
			switch (H5Iget_type(ID)) {
				case H5I_FILE : return H5Fclose(ID) >= 0 ? CBF_SUCCESS : CBF_H5ERROR;
				case H5I_GROUP : return H5Gclose(ID) >= 0 ? CBF_SUCCESS : CBF_H5ERROR;
				case H5I_DATATYPE : return H5Tclose(ID) >= 0 ? CBF_SUCCESS : CBF_H5ERROR;
				case H5I_DATASPACE : return H5Sclose(ID) >= 0 ? CBF_SUCCESS : CBF_H5ERROR;
				case H5I_DATASET : return H5Dclose(ID) >= 0 ? CBF_SUCCESS : CBF_H5ERROR;
				case H5I_ATTR : return H5Aclose(ID) >= 0 ? CBF_SUCCESS : CBF_H5ERROR;
				default: return CBF_H5ERROR;
			}
		} else return CBF_SUCCESS;
	}
    
	/* find/create/free a HDF5 group if it's valid & possibly set the ID to an invalid identifier
     can write requireGroup function as {if (!find(group)) create(group); return group;} */
    
	/** \brief Attempt to create a group
     
     Helper function to create a HDF5 group and return a CBFlib error code, to make error handling more consistant.
     
     \param group A pointer to a HDF5 ID type where the group will be stored.
     \param name The name that the group will be given.
     \param parent The group that will contain the newly created group.
     
     \sa cbf_H5Gfree
     \sa cbf_H5Gdestroy
     
     \return An error code.
     */
	int cbf_H5Gcreate(hid_t * const group, const char * const name, const hid_t parent)
	{
		if (!group || !name || !cbf_H5Ivalid(parent)) return CBF_ARGUMENT;
		return cbf_H5Ivalid(*group = H5Gcreate2(parent,name,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT)) ? CBF_SUCCESS : CBF_H5ERROR;
	}
    
	/** \brief Ensure a group exists
     
     Checks for the existance of a group with the given name and parent.
     Will create the group if it cannot be found, or open it if it already exists.
     It is an error if a matching group cannot be found or created.
     
     \param group A pointer to a HDF5 ID type where the group will be stored.
     \param name The name that the group will be given.
     \param parent The group that will contain the newly created group.
     
     \sa cbf_H5Gcreate
     \sa cbf_H5Gfree
     \sa cbf_H5Gdestroy
     
     \return An error code.
     */
	int cbf_H5Grequire(hid_t * const group, const char * const name, const hid_t parent)
	{
		/* check the arguments */
		if (!group || !name || !cbf_H5Ivalid(parent)) return CBF_ARGUMENT;
        
		/* check if the link exists */
		const htri_t l = H5Lexists(parent, name, H5P_DEFAULT);
		if (l < 0) return CBF_H5ERROR;
		else if (!l) return cbf_H5Gcreate(group, name, parent);
		else {
			/* check if the group exists */
			const htri_t e = H5Oexists_by_name(parent, name, H5P_DEFAULT);
			if (e < 0) return CBF_H5ERROR;
			else if (!e) {
				/* The link exists but the object doesn't - remove the link & create the object */
				if (H5Ldelete(parent, name, H5P_DEFAULT) < 0) return CBF_H5ERROR;
				else return cbf_H5Gcreate(group, name, parent);
			} else {
				/* my object exists - check its type */
				hid_t g = H5Oopen(parent, name, H5P_DEFAULT);
				if (H5I_GROUP == H5Iget_type(g)) {
					/* it's a group - return it */
					*group = g;
					return CBF_SUCCESS;
				} else {
					/* not a group - close the object & fail */
					H5Oclose(g);
					return CBF_H5DIFFERENT;
				}
			}
		}
	}
    
    /** \brief Close a HDF5 group
     
     Attempt to close a group, but don't modify the identifier that described it.
     
     \param ID The HDF5 group to be closed.
     
     \return An error code.
     */
	int cbf_H5Gfree(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Gclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		return CBF_SUCCESS;
	}
    
	/** \brief Close a HDF5 group
     
     Attempt to close a group, clobbering the identifier that described it.
     
     \param ID A pointer to the HDF5 group to be closed.
     
     \return An error code.
     */
	int cbf_H5Gdestroy(hid_t * const ID)
	{
		const int err = cbf_H5Gfree(*ID);
		*ID = CBF_H5FAIL;
		return err;
	}
    
	/* Open/close a HDF5 file if it's valid & possibly set the ID to an invalid identifier - deliberately avoid find/create/free or
     get/set/clear naming convensions */
    
	/** \brief Attempt to open an HDF5 file by file name
     
     Will try to open a file of the given name with suitable values for some of it's properties to make memory leaks less likely.
     
     \param file A pointer to an HDF5 ID where the newly opened file should be stored.
     \param name The name of the file to attempt to open.
     
     \sa cbf_H5Fclose
     \sa cbf_H5Fdestroy
     
     \return An error code.
     */
	int cbf_H5Fopen(hid_t * const file, const char * const name)
	{
		/* define variables & check args */
		int error = (!file || !name) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);
        
		/* check variables */
		reportFail(cbf_H5Ivalid(fapl), CBF_H5ERROR, error);
        
		/* do some work */
		reportFail(H5Pset_fclose_degree(fapl,H5F_CLOSE_STRONG)>=0, CBF_H5ERROR, error);
		reportFail(cbf_H5Ivalid(*file = H5Fcreate(name,H5F_ACC_TRUNC,H5P_DEFAULT,fapl)), CBF_H5ERROR, error);
        
		/* ensure variables are properly closed */
		if (cbf_H5Ivalid(fapl)) H5Pclose(fapl);
        
		/* done */
		return error;
	}
    
	/** \brief Close a HDF5 file
     
     Attempt to close a file, but don't modify the identifier that described it.
     
     \param ID The HDF5 file to be closed.
     
     \return An error code.
     */
	int cbf_H5Fclose(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Fclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		return CBF_SUCCESS;
	}
    
	/** \brief Close a HDF5 file
     
     Attempt to close a file, clobbering the identifier that described it.
     
     \param ID A pointer to the HDF5 file to be closed.
     
     \return An error code.
     */
	int cbf_H5Fdestroy(hid_t * const ID)
	{
		const int err = cbf_H5Fclose(*ID);
		*ID = CBF_H5FAIL;
		return err;
	}
    
	/* Attributes */
    
	/** \brief Check for an attribute with the given space/type/value, or set one if it doesn't exist.
     
     Checks the existance of an attribute of the given name, creating it if it doesn't exist.
     
     Checks the size, type and value of an existing attribute to find out if it is the same as
     what was requested as determined by a custom comparison function which may use some extra data
     for more sophisticated tests.
     
     \param ID The HDF5 object that the attribute will be applied to.
     \param name The name of the attribute.
     \param rank The number of dimensions of the attribute data, 0 for scalar data.
     \param dim The length of each dimension, not used for scalar data.
     \param type The HDF5 type of the attribute data.
     \param value The data to be written to the attribute.
     \param buf A buffer to be used when reading an existing attribute of the same size.
     \param cmp A comparison function to test if a previously set value is equal to the value I asked for.
     
     \sa cbf_H5Arequire_string
     
     \return An error code.
     */
	int cbf_H5Arequire_cmp
    (const hid_t ID,
     const char * const name,
     const int rank,
     const hsize_t * const dim,
     const hid_t type,
     const void * const value,
     void * const buf,
     int (*cmp)(const void * a, const void * b, size_t N))
	{
		/* define variables & check args */
		int error = (!cbf_H5Ivalid(ID) || !name || (!!rank && !dim) || rank<0) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t attrSpace = CBF_H5FAIL;
		hid_t attrType = type;
		hid_t attr = CBF_H5FAIL;
		cbf_reportFail(cbf_H5Screate(&attrSpace, rank, dim, 0), error);
        
		/* do some work */
		if (H5Aexists(ID,name)) {
			hid_t attr = H5Aopen(ID,name,H5P_DEFAULT);
			hid_t currSpace = H5Aget_space(attr);
			hid_t currType = H5Aget_type(attr);
			/* check everything was opened properly */
			if (!cbf_H5Ivalid(attr)) error |= CBF_H5ERROR;
			if (!cbf_H5Ivalid(currSpace)) error |= CBF_H5ERROR;
			if (!cbf_H5Ivalid(currType)) error |= CBF_H5ERROR;
			if (CBF_SUCCESS==error) {
                /* check that the dataspace is correct */
				const htri_t eq = H5Sextent_equal(currSpace,attrSpace);
				if (eq<0) error |= CBF_H5ERROR;
				else if (!eq) error |= CBF_H5DIFFERENT;
				else /* success */;
			}
			if (CBF_SUCCESS==error) {
                /* check the datatype is correct */
				const htri_t eq = H5Tequal(currType,attrType);
				if (eq<0) error |= CBF_H5ERROR;
				else if (!eq) error |= CBF_H5DIFFERENT;
				else /* success */;
			}
			/* check that the data is correct */
			if (CBF_SUCCESS==error) {
				const size_t N = H5Sget_simple_extent_npoints(currSpace);
				const size_t vlStr = H5Tis_variable_str(currType);
				H5Aread(attr,attrType,buf);
				if (!!cmp(value,buf,N)) {
                    fprintf(stderr,__WHERE__": Incorrect attribute value\n");
                    error |= CBF_H5DIFFERENT;
                }
				if (vlStr < 0) error |= CBF_H5ERROR;
				if (vlStr > 0) H5Dvlen_reclaim(currType, currSpace, H5P_DEFAULT, buf);
            }
			/* check local variables are properly closed */
			if (cbf_H5Ivalid(currSpace))  H5Sclose(currSpace);
			if (cbf_H5Ivalid(currType))  H5Tclose(currType);
		} else {
			reportFail(cbf_H5Ivalid(attr = H5Acreate2(ID,name,attrType,attrSpace,H5P_DEFAULT,H5P_DEFAULT)), CBF_H5ERROR, error);
			reportFail(H5Awrite(attr,attrType,value)>=0, CBF_H5ERROR, error);
		}
        
		/* check local variables are properly closed */
		if (cbf_H5Ivalid(attrSpace))  H5Sclose(attrSpace);
		if (cbf_H5Ivalid(attr))  H5Aclose(attr);
        
		/* done */
		return error;
	}
    
    static int cmp_string(const void * a, const void * b, size_t N) {return N==1 ? strcmp(a, b) : 1;}
    
	/** \brief Check for a scalar string attribute with a given value, or set one if it doesn't exist.
     
     Forwarding function that calls \c cbf_H5Arequire_cmp with the appropriate arguments to compare two strings.
     
     \param location HDF5 object to which the string attribute should/will belong.
     \param name The name of the attribute.
     \param value The value which the attribute should/will have.
     
     \sa cbf_H5Arequire_cmp
     
     \return An error code.
     */
	int cbf_H5Arequire_string
    (const hid_t location,
     const char * const name,
     const char * const value)
	{
		int error = CBF_SUCCESS;
		hid_t h5atype = CBF_H5FAIL;
		const size_t len = strlen(value)+1;
		char * const buf = memset(malloc(len),'\0',len);
		error |= cbf_H5Tcreate_string(&h5atype,strlen(value));
		error |= cbf_H5Arequire_cmp(location,name,0,0,h5atype,value,buf,cmp_string);
		cbf_H5Tfree(h5atype);
		free((void*)(buf));
		return error;
	}
    
	/*  find/create/free hdf5 datasets without directly using hdf5 API */
    
	/** \brief Creates a new dataset in the given location.
     
     The \c dataset parameter gives a location to store the dataset for use by the caller, for example to add an attribute to it.
     If non-zero the returned handle MUST be free'd by the caller with \c cbf_H5Dfree.
     
     The \c rank of the data must be equal to the length of the \c dim, \c max & \c chunk parameters, if they are given, and should be:
     \li 0, for scalar data
     \li 1, for vector data
     \li 2, for matrix data
     \li 3, for volume data
     etc...
     The maximum rank is defined by the HDF5 library, a negative rank makes no sense.
     
     \c type should usually be one of:
     \c H5T_STD_I8LE, \c H5T_STD_I16LE, \c H5T_STD_I32LE, \c H5T_STD_I64LE,
     \c H5T_STD_U8LE, \c H5T_STD_U16LE, \c H5T_STD_U32LE, \c H5T_STD_U64LE,
     \c H5T_STD_I8BE, \c H5T_STD_I16BE, \c H5T_STD_I32BE, \c H5T_STD_I64BE,
     \c H5T_STD_U8BE, \c H5T_STD_U16BE, \c H5T_STD_U32BE, \c H5T_STD_U64BE,
     \c H5T_IEEE_F32LE, \c H5T_IEEE_F64LE,
     \c H5T_IEEE_F32BE, \c H5T_IEEE_F64BE or a value returned by \c cbf_H5Tcreate_string.
     It is not limited to the above values, and can take any defined HDF5 datatype.
     
     \param location The hdf5 group/file in which to put the dataset.
     \param dataset An optional pointer to a location where the dataset handle should be stored for further use.
     \param name The name of the new dataset, or NULL for an anonymous dataset, in which case the dataset point is mandatory
     \param rank The rank of the data, must be equal to the length of the \c dim and \c max arrays, if they are given.
     \param dim The dimensions of the data, pointer to an array of length \c rank which should where
     \c dim[i] \> 0 for \c i = [0, \c rank ), unused if \c rank == 0.
     \param max The maximum size of each dimension, pointer or an array of length \c rank where
     \c dim[i] \<= \c max[i] \<= \c H5S_UNLIMITED for \code i = [0, rank) \endcode, unused if \code rank == 0 \endcode.
     \param chunk The chunk size for the dataset, as a pointer to an array of length \c rank (or \c 0 if chunking should not be enabled).
     \param type The type of each data element, can take things like \c H5T_STD_I32LE as predefined constants.
     
     \sa cbf_H5Dfind
     \sa cbf_H5Dset_extent
     \sa cbf_H5Dwrite
     \sa cbf_H5Dread
     \sa cbf_H5Drequire_scalar_F64LE
     \sa cbf_H5Drequire_string
     \sa cbf_H5Dfree
     \sa cbf_H5Ddestroy
     
     \sa cbf_H5Tcreate_string
     
     \return An error code.
     */
	int cbf_H5Dcreate
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const int rank,
     const hsize_t * const dim,
     const hsize_t * const max,
     const hsize_t * const chunk,
     const hid_t type)
	{
		/* define variables & check args */
		int error = (!cbf_H5Ivalid(location) || (!!rank && !dim) || rank<0) ? CBF_ARGUMENT : CBF_SUCCESS;
        if (!name && !dataset) return CBF_ARGUMENT;
		hid_t dataSpace = CBF_H5FAIL;
		hid_t dcpl = H5Pcreate(H5P_DATASET_CREATE);
		hid_t dataset_local = CBF_H5FAIL;
        
		/* check variables are valid */
		cbf_reportFail(cbf_H5Screate(&dataSpace, rank, dim, max), error);
		reportFail(cbf_H5Ivalid(dcpl), CBF_H5ERROR, error);
        
		/* allow dataset to be chunked */
		if (!!rank && NULL!=chunk) reportFail(H5Pset_chunk(dcpl,rank,chunk)>=0, CBF_H5ERROR, error);
        
		/* create the dataset */
        if (name) {
		    dataset_local = H5Dcreate2(location,name,type,dataSpace,H5P_DEFAULT,dcpl,H5P_DEFAULT);
        } else {
            dataset_local = H5Dcreate_anon(location,type,dataSpace,dcpl,H5P_DEFAULT);
        }
		reportFail(cbf_H5Ivalid(dataset_local), CBF_H5ERROR, error);
        
		/* check local variables are properly closed */
		if (cbf_H5Ivalid(dataSpace)) H5Sclose(dataSpace);
		if (cbf_H5Ivalid(dcpl)) H5Pclose(dcpl);
        
		/* if the dataset object is requested then return it, otherwise close it */
		if (dataset) *dataset = dataset_local;
		else if (cbf_H5Ivalid(dataset_local)) H5Dclose(dataset_local);
        
		/* done */
		return error;
	}
    
	/** \brief Look for a dataset with the given properties.
     
     Succeeds without returning a valid dataset ID if no dataset exists and fails if one with different properties exists.
     Finding that the dataset doesn't exist is not a failure - the function worked and returned useful information.
     So, if it returns \c CBF_SUCCESS then the dataset must be free'd at some point, otherwise it doesn't need to be free'd.
     
     Use as:
     \code
     int error = cbf_H5Dfind(., &dataset, ...);
     if (CBF_SUCCESS==error) {
     if (cbf_H5Ivalid(dataset)) {
     use_existing_dataset(dataset);
     } else {
     cbf_H5Dcreate(...);
     use_new_datset(dataset);
     }
     } else {
     handle_error(error);
     }
     \endcode
     
     \param location The hdf5 group/file in which to put the dataset.
     \param dataset A pointer to a HDF5 object identifier that is set to the location of a valid object or an invalid value if the function
     succeeds, otherwise is left in an undefined state.
     \param name The name of the existing/new dataset.
     \param rank See \c cbf_H5Dcreate
     \param dim See \c cbf_H5Dcreate
     \param max See \c cbf_H5Dcreate
     \param chunk See \c cbf_H5Dcreate
     \param type See \c cbf_H5Dcreate
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Dset_extent
     \sa cbf_H5Dwrite
     \sa cbf_H5Dread
     \sa cbf_H5Drequire_scalar_F64LE
     \sa cbf_H5Drequire_string
     \sa cbf_H5Dfree
     \sa cbf_H5Ddestroy
     
     \return An error code indicating whether the function successfully determined the presence (or otherwise) of an appropriate dataset.
     */
	int cbf_H5Dfind
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const int rank,
     const hsize_t * const dim,
     const hsize_t * const max,
     const hsize_t * const chunk,
     const hid_t type)
	{
		/* check the arguments */
		if (!cbf_H5Ivalid(location) || !dataset || !name || (!!rank && !dim) || rank<0) return CBF_ARGUMENT;
        
		/* check if the link exists */
		const htri_t l = H5Lexists(location, name, H5P_DEFAULT);
		if (l < 0) return CBF_H5ERROR;
		else if (!l) {
			*dataset = CBF_H5FAIL;
			return CBF_SUCCESS;
		} else {
			/* check if the group exists */
			const htri_t e = H5Oexists_by_name(location, name, H5P_DEFAULT);
			if (e < 0) return CBF_H5ERROR;
			else if (!e) {
				/* The link exists but the object doesn't - try to remove the link & tell the caller that there is no dataset */
				if (H5Ldelete(location, name, H5P_DEFAULT) < 0) return CBF_H5ERROR;
				else {
					*dataset = CBF_H5FAIL;
					return CBF_SUCCESS;
				}
			} else {
				/* my object exists - check its type */
				hid_t g = H5Oopen(location, name, H5P_DEFAULT);
				if (H5I_DATASET == H5Iget_type(g)) {
					int error = CBF_SUCCESS;
					/* it's a dataset - check its properties */
					hid_t currSpace = H5Dget_space(g);
					hid_t currType = H5Dget_type(g);
					hid_t dataSpace = CBF_H5FAIL;
					cbf_reportFail(cbf_H5Screate(&dataSpace, rank, dim, max), error);
					if (CBF_SUCCESS==error) {
						/* Check space */
						const htri_t eq = H5Sextent_equal(currSpace,dataSpace);
						if (eq < 0) error |= CBF_H5ERROR;
						else if (!eq) error |= CBF_H5DIFFERENT;
						else /* success */;
					}
					/* TODO: Check chunk? */
					if (CBF_SUCCESS==error) {
				 		/* check the datatype is correct */
						const htri_t eq = H5Tequal(currType,type);
						if (eq<0) error |= CBF_H5ERROR;
						else if (!eq) error |= CBF_H5DIFFERENT;
						else /* success */;
					}
					H5Tclose(currType);
					H5Sclose(currSpace);
					H5Sclose(dataSpace);
					*dataset = CBF_SUCCESS==error ? g : CBF_H5FAIL;
					return error;
				} else {
					/* not a dataset - close the object & fail */
					H5Oclose(g);
					return CBF_H5DIFFERENT;
				}
			}
		}
	}
    
	/** \brief Change the extent of a chunked dataset to the values in \c dim.
     
     Forwards to a HDF5 function to change the extent of a dataset.
     Doesn't check that the number of elements in \c dim matches the rank of the dataset.
     
     \param dataset A handle for the dataset whose extent is to be changed.
     \param dim The new extent of the dataset, if the function succeeds. Must be the same length as the rank of the dataset.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Dfind
     \sa cbf_H5Dwrite
     \sa cbf_H5Dread
     \sa cbf_H5Drequire_scalar_F64LE
     \sa cbf_H5Drequire_string
     \sa cbf_H5Dfree
     \sa cbf_H5Ddestroy
     
     \return An error code.
     */
	int cbf_H5Dset_extent(const hid_t dataset, const hsize_t * const dim)
	{
		int error = (!dim) ? CBF_ARGUMENT : CBF_SUCCESS;
		if (H5Dset_extent(dataset,dim) < 0) error |= CBF_H5ERROR;
		return error;
	}
    
	/** \brief Add some data to the specified position in the dataset, without checking what (if anything) was there before.
     
     Assumes the dataset has the appropriate size to contain all the data and overwrites any existing data that may be there.
     The \c rank of the dataset is assumed to be known, and the size of the array parameters is not tested.
     
     \param dataset The dataset to write the data to.
     \param offset Where to start writing the data, as an array of \c rank numbers.
     \param stride The number of elements in the dataset to step for each element to be written, where
     null is equivalent to a stride of [1, 1, 1, ..., 1], as an array of \c rank numbers.
     \param count The number of elements in each dimension to be written, as an array of \c rank numbers.
     \param value The address of the data to be written.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Dfind
     \sa cbf_H5Dset_extent
     \sa cbf_H5Dread
     \sa cbf_H5Drequire_scalar_F64LE
     \sa cbf_H5Drequire_string
     \sa cbf_H5Dfree
     \sa cbf_H5Ddestroy
     
     \return An error code.
     */
	int cbf_H5Dwrite
    (const hid_t dataset,
     const hsize_t * const offset,
     const hsize_t * const stride,
     const hsize_t * const count,
     const void * const value)
	{
		/* define variables & check args */
		int error = (!cbf_H5Ivalid(dataset)) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t datatype = H5Dget_type(dataset);
		hid_t filespace = H5Dget_space(dataset);
		const int rank = H5Sget_simple_extent_ndims(filespace);
		hid_t memspace = !rank ? H5Screate(H5S_SCALAR) : H5Screate_simple(rank,count,0);
		if ((!!rank && (!offset || !count)) || rank<0) error |= CBF_ARGUMENT;
        
		/* check variables are valid */
		reportFail(cbf_H5Ivalid(filespace), CBF_H5ERROR, error);
		reportFail(cbf_H5Ivalid(datatype), CBF_H5ERROR, error);
        
		/* select elements & write the dataset */
		if (!!rank) {
			reportFail(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, 0)>=0, CBF_H5ERROR, error);
		} else {
			reportFail(H5Sselect_all(filespace)>=0, CBF_H5ERROR, error);
		}
		reportFail(H5Dwrite(dataset,datatype,memspace,filespace,H5P_DEFAULT,value)>=0, CBF_H5ERROR, error);
        
		/* check local variables are properly closed */
		if (cbf_H5Ivalid(memspace)) H5Sclose(memspace);
		if (cbf_H5Ivalid(filespace)) H5Sclose(filespace);
		if (cbf_H5Ivalid(datatype)) H5Tclose(datatype);
        
		/* done */
		return error;
	}
    
	/** \brief Extract some existing data from a dataset at a known position.
     
     Read some data from a given location in the dataset to an existing location in memory.
     Does not check the length of the array parameters, which should all have \c rank elements or (in some cases) be null.
     
     \param dataset The dataset to read the data from.
     \param offset Where to start reading the data, as an array of \c rank numbers.
     \param stride The number of elements in the dataset to step for each element to be read, where
     null is equivalent to a stride of [1, 1, 1, ..., 1], as an array of \c rank numbers.
     \param count The number of elements in each dimension to be read, as an array of \c rank numbers.
     \param value The location where the data is to be stored.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Dfind
     \sa cbf_H5Dset_extent
     \sa cbf_H5Dwrite
     \sa cbf_H5Drequire_scalar_F64LE
     \sa cbf_H5Drequire_string
     \sa cbf_H5Dfree
     \sa cbf_H5Ddestroy
     
     \return An error code.
     */
	int cbf_H5Dread
    (const hid_t dataset,
     const hsize_t * const offset,
     const hsize_t * const stride,
     const hsize_t * const count,
     void * const value)
	{
		/* define variables & check args */
		int error = (!cbf_H5Ivalid(dataset)) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t datatype = H5Dget_type(dataset);
		hid_t filespace = H5Dget_space(dataset);
		const int rank = H5Sget_simple_extent_ndims(filespace);
		hid_t memspace = !rank ? H5Screate(H5S_SCALAR) : H5Screate_simple(rank,count,0);
		if ((!!rank && (!offset || !count)) || rank<0) error |= CBF_ARGUMENT;
        
		/* check variables are valid */
		reportFail(cbf_H5Ivalid(filespace), CBF_H5ERROR, error);
		reportFail(cbf_H5Ivalid(datatype), CBF_H5ERROR, error);
        
		/* select elements & read the dataset */
		if (!!rank) {
			reportFail(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, 0)>=0, CBF_H5ERROR, error);
		} else {
			reportFail(H5Sselect_all(filespace)>=0, CBF_H5ERROR, error);
		}
		reportFail(H5Dread(dataset, datatype, memspace, filespace, H5P_DEFAULT, value)>=0, CBF_H5ERROR, error);
        
		/* check local variables are properly closed */
		if (cbf_H5Ivalid(memspace)) H5Sclose(memspace);
		if (cbf_H5Ivalid(filespace)) H5Sclose(filespace);
		if (cbf_H5Ivalid(datatype)) H5Tclose(datatype);
        
		/* done */
		return error;
	}
    
	/** \brief Write a scalar 64-bit floating point number as a dataset.
     
     Convenience function using the HDF5 abstraction layer to avoid the need to consider array-related
     parameters for a scalar dataset and to automatically set the string type to the correct size.
     
     \param location The group containing the new dataset.
     \param dataset An optional pointer to a place to store the new dataset.
     \param name The name of the new dataset.
     \param value The value of the new dataset.
     \param cmp A comparison function to test if a previously set value is equal to the value I asked for.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Dfind
     \sa cbf_H5Dset_extent
     \sa cbf_H5Dwrite
     \sa cbf_H5Dread
     \sa cbf_H5Drequire_string
     \sa cbf_H5Dfree
     \sa cbf_H5Ddestroy
     
     \return An error code.
     */
	int cbf_H5Drequire_scalar_F64LE
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const double value)
	{
		int error = CBF_SUCCESS;
		hid_t _dataset = CBF_H5FAIL;
		error |= cbf_H5Dfind(location,&_dataset,name,0,0,0,0,H5T_IEEE_F64LE);
		if (CBF_SUCCESS==error) {
			if (!cbf_H5Ivalid(_dataset)) {
				error |= cbf_H5Dcreate(location,&_dataset,name,0,0,0,0,H5T_IEEE_F64LE);
				error |= cbf_H5Dwrite(_dataset,0,0,0,&value);
			} else {
				double data = 0./0.;
				error |= cbf_H5Dread(_dataset,0,0,0,&data);
				if (fabs(value - data)> 1.e-38+1.e-13*(fabs(value)+fabs(data))) {
					fprintf(stderr,"Error: data doesn't match (%g vs %g) for nexus field '%s'\n",data,value,name);
					error |= CBF_H5DIFFERENT;
				}
			}
			/* cleanup temporary dataset? */
			if (dataset) *dataset = _dataset;
			else cbf_H5Dfree(_dataset);
		} else {
			fprintf(stderr,"Attempt to determine existence of nexus field '%s' failed\n",name);
		}
		return error;
	}
    
    /** \brief Write a single string as a dataset.
     
     Convenience function using the HDF5 abstraction layer to avoid the need to consider array-related
     parameters for a scalar dataset and to automatically set the string type to the correct size.
     
     \param location The group containing the new dataset.
     \param dataset An optional pointer to a place to store the new dataset.
     \param name The name of the new dataset.
     \param value The value of the new dataset.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Dfind
     \sa cbf_H5Dset_extent
     \sa cbf_H5Dwrite
     \sa cbf_H5Dread
     \sa cbf_H5Drequire_scalar_F64LE
     \sa cbf_H5Dfree
     \sa cbf_H5Ddestroy
     
     \return An error code.
     */
	int cbf_H5Drequire_string
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const char * const value)
	{
		int error = CBF_SUCCESS;
		hid_t _dataset = CBF_H5FAIL;
		hid_t dataType = CBF_H5FAIL;
		error |= cbf_H5Tcreate_string(&dataType, strlen(value));
		error |= cbf_H5Dfind(location,&_dataset,name,0,0,0,0,dataType);
		if (CBF_SUCCESS==error) {
			if (!cbf_H5Ivalid(_dataset)) {
				error |= cbf_H5Dcreate(location,&_dataset,name,0,0,0,0,dataType);
				error |= cbf_H5Dwrite(_dataset,0,0,0,value);
			} else {
				hid_t currType = H5Dget_type(_dataset);
				char * const data = malloc(H5Tget_size(currType));
				H5Tclose(currType);
				error |= cbf_H5Dread(_dataset,0,0,0,data);
				if (0 != strcmp(value, data)) {
					fprintf(stderr,"Error: data doesn't match ('%s' vs '%s') for nexus field '%s'\n",data,value,name);
					error |= CBF_H5DIFFERENT;
				}
				free((void*)data);
			}
			/* cleanup temporary dataset? */
			if (dataset) *dataset = _dataset;
			else cbf_H5Dfree(_dataset);
		} else {
			fprintf(stderr,"Attempt to determine existence of nexus field '%s' failed\n",name);
		}
		H5Tclose(dataType);
		return error;
	}
    
	/** \brief Close a HDF5 dataset
     
     Attempt to close a dataset, but don't modify the identifier that described it.
     
     \param ID The HDF5 dataset to be closed.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Ddestroy
     \sa cbf_H5Ivalid
     
     \return An error code.
     */
	int cbf_H5Dfree(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Dclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		return CBF_SUCCESS;
	}
    
	/** \brief Close a HDF5 dataset
     
     Attempt to close a dataset, clobbering the identifier that described it.
     
     \param ID A pointer to the HDF5 dataset to be closed.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Dfree
     \sa cbf_H5Ivalid
     
     \return An error code.
     */
	int cbf_H5Ddestroy(hid_t * const ID)
	{
		const int err = cbf_H5Dfree(*ID);
		*ID = CBF_H5FAIL;
		return err;
	}
    
	/* Custom HDF5 types - to get the correct string type for datasets in a consistent way */
    
	/** \brief Get a HDF5 string datatype with a specified length
     
     Convenience function to create a datatype suitable for use when storing a string.
     
     \param type A pointer to a the HDF5 handle of the new datatype, which should be free'd with \c cbf_H5Tfree
     \param len The length of the string datatype - should be \c strlen() or \c H5T_VARIABLE
     
     \sa cbf_H5Tfree
     \sa cbf_H5Tdestroy
     
     \return An error code.
     */
	int cbf_H5Tcreate_string(hid_t * type, const size_t len)
	{
		*type = H5Tcopy(H5T_C_S1);
		return H5Tset_size(*type,H5T_VARIABLE==len?len:len+1) < 0 ? CBF_H5ERROR : CBF_SUCCESS;
	}
    
	/** \brief Close a HDF5 datatype identifier
     
     Attempt to close a datatype identifier, but don't modify the identifier that described it.
     
     \param ID The HDF5 datatype to be closed.
     
     \sa cbf_H5Tcreate_string
     \sa cbf_H5Tdestroy
     \sa cbf_H5Ivalid
     
     \return An error code.
     */
	int cbf_H5Tfree(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Tclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		return CBF_SUCCESS;
	}
    
	/** \brief Close a HDF5 datatype identifier
     
     Attempt to close a datatype, clobbering the identifier that described it.
     
     \param ID A pointer to the HDF5 datatype to be closed.
     
     \sa cbf_H5Tcreate_string
     \sa cbf_H5Tfree
     \sa cbf_H5Ivalid
     
     \return An error code.
     */
	int cbf_H5Tdestroy(hid_t * const ID)
	{
		const int err = cbf_H5Tfree(*ID);
		*ID = CBF_H5FAIL;
		return err;
	}
    
	/* HDF5 dataspace functions: I need a uniform method of creating data spaces to ensure correct operation of comparison functions */
    
	/** \brief Create a dataspace with some given values
     
     Helper function which creates a HDF5 dataspace.
     
     Maximum dimensions can be set to infinity by passing \c H5S_UNLIMITED in the appropriate slot of the \c max parameter.
     If \c max is a null pointer the maximum length is set to the current length as given by \c dim .
     
     \param ID A pointer to a HDF5 identifier that will contain the new dataspace.
     \param rank The number of dimensions of the new dataspace.
     \param dim The current size of each dimension of the dataspace, should be an array of length \c rank .
     \param max The maximum size of each dimension, should be an array of length \c rank .
     
     \return An error code.
     */
	int cbf_H5Screate
    (hid_t * const ID,
     const int rank,
     const hsize_t * const dim,
     const hsize_t * const max)
	{
		if (!ID || (!!rank && !dim) || rank<0) return CBF_ARGUMENT;
		else {
			hid_t space = !rank ? H5Screate(H5S_SCALAR) : H5Screate_simple(rank, dim, max);
			if (cbf_H5Ivalid(space)) {
				*ID = space;
				return CBF_SUCCESS;
			} else return CBF_H5ERROR;
		}
	}
    
	/** \brief Close a HDF5 dataspace identifier
     
     Attempt to close a dataspace identifier, but don't modify the identifier that described it.
     
     \param ID The HDF5 dataspace to be closed.
     
     \sa cbf_H5Screate
     \sa cbf_H5Sdestroy
     \sa cbf_H5Ivalid
     
     \return An error code.
     */
	int cbf_H5Sfree(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Sclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		return CBF_SUCCESS;
	}
    
	/** \brief Close a HDF5 dataspace identifier
     
     Attempt to close a dataspace, clobbering the identifier that described it.
     
     \param ID A pointer to the HDF5 dataspace to be closed.
     
     \sa cbf_H5Screate
     \sa cbf_H5Sfree
     \sa cbf_H5Ivalid
     
     \return An error code.
     */
    
	int cbf_H5Sdestroy(hid_t * const ID)
	{
		const int err = cbf_H5Sfree(*ID);
		*ID = CBF_H5FAIL;
		return err;
	}
    
    
    /****************************************************************
     End of section of code extracted from J. Sloan's
     cbf_hdf5_common.c
     ****************************************************************/
    
    /****************************************************************
     The following section of code is extracted from J. Sloan's
     config.c
     ****************************************************************/
    
    /* Some error codes for use by the parsing functions - definitions should not be visible */
    const int parseErrorUnexpectedInput = 1;
    const int parseErrorExpectedDelimeter = 2;
    const int parseErrorExpectedNumber = 3;
    const int parseErrorOpeningFile = 4;
    const int parseErrorExpectedString = 5;
    const int parseErrorDuplicateField = 6;
    const int parseErrorUnexpectedEOF = 7;
    const int parseErrorUndefinedValue = 8;
    
    /**
     Tokenise an input stream, returning one token at a time into the given buffer.
     
     \param buf A pointer to a realloc-able buffer for storing the input, free'd when EOF is reached.
     \param n The current size of \c buf.
     \param ln The current line number of the file.
     \param pre The previous character, needed to test for unexpected EOL state.
     \param stream The stream to be tokenised.
     
     \return An error code.
     */
    int cbf_hdf5_parseScan(char * * const buf, size_t * n, size_t * ln, char * const pre, FILE * stream)
    {
        int c = 0; /* current character */
        size_t k = 0; /* current line length */
        
        /* check that sensible arguments are given */
        assert(buf);
        assert(n);
        assert(ln);
        assert(pre);
        assert(stream);
        
        /* skip blanks & comments to get to an interesting character */
        do {
            c = fgetc(stream);
            if (feof(stream)) break;
            *pre = c;
            if ('\n' == c) ++*ln;
            if ('#' == c) {
                do {
                    c = fgetc(stream);
                    if (feof(stream)) break;
                    *pre = c;
                    if ('\n' == c) ++*ln;
                } while (!isspace(c) || isblank(c));
            }
            if (isblank(c)) continue;
            else break;
        } while (1);
        
        /* if I am at the end of the stream, free the buffer & return 0 */
        if (feof(stream)) {
            free((void*)(*buf));
            *buf = 0;
            *n = 0;
            return '\n'==*pre ? CBF_SUCCESS : parseErrorUnexpectedEOF;
        }
        
        /* I have a token: read it */
        if (isspace(c)) {
            /* it's a newline token */
            cbf_push_buf('\n', buf, n, &k);
        } else {
            /* it's a string : leave the terminating character in the stream as it may be a token itself */
            if ('[' == c || ']' == c) {
                /* it's a vector start or end token : match the structure of a vector at a higher level */
                cbf_push_buf(c, buf, n, &k);
            } else while (1) {
                cbf_push_buf(c, buf, n, &k);
                c = fgetc(stream);
                if (feof(stream)) break;
                *pre = c;
                if (isspace(c) || '[' == c || ']' == c) {
                    ungetc(c, stream);
                    break;
                }
            }
        }
        /* null-terminate the token */
        cbf_push_buf('\0', buf, n, &k);
        
        return (feof(stream) && '\n'!=*pre) ? parseErrorUnexpectedEOF : CBF_SUCCESS;
    }
    
    /**
     The returned string is "success" for CBF_SUCCESS, "unknown error" if the given error code is not recognised or a non-empty string briefly describing the error otherwise.
     
     The returned string must not be free'd.
     */
    const char * cbf_hdf5_configParseStrerror(const int error)
    {
        if (error == CBF_SUCCESS) return "success";
        else if (error == parseErrorUnexpectedInput) return "unexpected input";
        else if (error == parseErrorExpectedDelimeter) return "expected a delimiter";
        else if (error == parseErrorExpectedNumber) return "expected a number";
        else if (error == parseErrorOpeningFile) return "could not open file";
        else if (error == parseErrorExpectedString) return "expected a string";
        else if (error == parseErrorDuplicateField) return "duplicate data";
        else return "unknown error";
    }
    
    /**
     Initialises name & depends_on to null, vector to [nan,nan,nan].
     */
    cbf_hdf5_configItem cbf_hdf5_createConfigItem()
    {
        cbf_hdf5_configItem item;
        item.minicbf = 0;
        item.nexus = 0;
        item.depends_on = 0;
        item.vector[0] = 0.;
        item.vector[1] = 0.;
        item.vector[2] = 0.;
        return item;
    }
    
    void cbf_hdf5_destroyConfigItem(const cbf_hdf5_configItem item)
    {
        free((void*)(item.minicbf));
        free((void*)(item.nexus));
        free((void*)(item.depends_on));
    }
    
     
    /**
     Initialises size & capacity to 0, doesn't allocate storage immediately.
     */
    cbf_hdf5_configItemVectorhandle cbf_hdf5_createConfigItemVector()
    {
        cbf_hdf5_configItemVectorhandle vector = (cbf_hdf5_configItemVectorhandle)(malloc(sizeof(cbf_hdf5_configItemVector)));
        vector->item = 0;
        vector->nItems = 0;
        vector->maxItems = 0;
        vector->sample_depends_on = 0;
        return vector;
    }
    
    /**
     Destroys any children of the object, free's the memory for the array of children and free's the memory for the vector itself.
     */
    void cbf_hdf5_destroyConfigItemVector(const cbf_hdf5_configItemVectorhandle vector)
    {
        size_t i = 0;
        for (; i < vector->nItems; ++i) cbf_hdf5_destroyConfigItem(vector->item[i]);
        free(vector->item);
        free((void*)(vector->sample_depends_on));
        free(vector);
    }
    
    /**
     Releases any previously held dependancy and takes ownership of a new one.
     The given string will be free'd by the object when it is no longer needed.
     */
    void cbf_hdf5_configItemVector_setSampleDependsOn(cbf_hdf5_configItemVectorhandle vector, const char * const depends_on)
    {
        free((void*)(vector->sample_depends_on));
        vector->sample_depends_on = depends_on;
    }
    
    /**
     \return The current dependancy setting for the sample group, or zero if not set.
     */
    const char * cbf_hdf5_configItemVector_getSampleDependsOn(cbf_hdf5_configItemVectorhandle vector)
    {
        return vector->sample_depends_on;
    }
    
    /**
     The vector will take ownership of the item's contents. This may invalidate any previously obtained pointers to items in the vector.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_push(cbf_hdf5_configItemVectorhandle vector, cbf_hdf5_configItem item)
    {
        if (!(vector->nItems < vector->maxItems)) {
            /* increase the maximum number of items */
            const size_t k = 4;
            vector->maxItems = (size_t)((float)(vector->nItems)/(float)(k))*k + k;
            vector->item = realloc(vector->item, vector->maxItems*sizeof(cbf_hdf5_configItem));
        }
        /* ensure I have enough items */
        assert(vector->maxItems > vector->nItems);
        
        /* add the item to the end of the vector & set the item count to the correct number. */
        vector->item[vector->nItems++] = item;
        return vector->item+vector->nItems-1;
    }
    
    /**
     \return An iterator to a matching entry, or an iterator to the current end element.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_findMinicbf(const cbf_hdf5_configItemVectorhandle vector, const char * const name)
    {
        cbf_hdf5_configItem * it = cbf_hdf5_configItemVector_begin(vector);
        while (cbf_hdf5_configItemVector_end(vector) != it && (!it->minicbf || strcmp(it->minicbf,name))) ++it;
        return it;
    }
    
    /**
     \return An iterator to a matching entry, or an iterator to the current end element.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_findNexus(const cbf_hdf5_configItemVectorhandle vector, const char * const name)
    {
        cbf_hdf5_configItem * it = cbf_hdf5_configItemVector_begin(vector);
        while (cbf_hdf5_configItemVector_end(vector) != it && (!it->nexus || strcmp(it->nexus,name))) ++it;
        return it;
    }
    
    /**
     Performs bounds-checking, and returns 0 if the given index is out-of-bounds. The index type is unsigned, so only one comparison is needed to do this.
     
     \return A pointer to an item in the vector that may be modified but should not be free'd, subsequent vector operations may invalidate this pointer.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_at(const cbf_hdf5_configItemVectorhandle vector, const size_t n)
    {
        return (n < vector->nItems) ? vector->item+n : 0;
    }
    
    /**
     \return A pointer to an item in the vector that may be modified but should not be free'd, subsequent vector operations may invalidate this pointer.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_begin(const cbf_hdf5_configItemVectorhandle vector)
    {
        return vector->item;
    }
    
    /**
     \return A pointer to an item in the vector that may be modified but should not be free'd, subsequent vector operations may invalidate this pointer.
     */
    const cbf_hdf5_configItem * cbf_hdf5_configItemVector_end(const cbf_hdf5_configItemVectorhandle vector)
    {
        return vector->item+vector->nItems;
    }
    
    int cbf_hdf5_parseExtractVector
    (FILE * const configFile,
     FILE * const logFile,
     cbf_hdf5_configItem * it,
     char * * const buf,
     size_t * n,
     size_t * ln,
     char * const pre)
    {
        char * end = 0;
        
#define GET_TOKEN() \
{ \
const int e = cbf_hdf5_parseScan(buf, n, ln, pre, configFile); \
if (e != CBF_SUCCESS) { \
fprintf(logFile,"\nError: %s\n",cbf_hdf5_configParseStrerror(e)); \
return e; \
} \
};
        
#define REQUIRE_TOKEN(TKN) \
{ \
const char * const _tkn = (TKN); \
if (strcmp(_tkn,*buf)) { \
fprintf(logFile,"Config parsing error on line %lu: expected " #TKN ", got '%s'\n",*ln,*buf); \
return parseErrorUnexpectedInput; \
} \
};
        
#define REQUIRE_NOT_EOL() \
{ \
if (!strcmp("\n",*buf)) { \
fprintf(logFile,"Config parsing error on line %lu: unexpected newline\n",*ln); \
return parseErrorUnexpectedInput; \
} \
};
        
        /* literal '['. */
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        REQUIRE_TOKEN("[");
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        errno = 0;
        it->vector[0] = strtod(*buf, &end);
        if (errno != 0 || end == *buf) {
            fprintf(logFile,"Config parsing error on line %lu: expected a number, got '%s'\n",*ln,*buf);
            return parseErrorExpectedNumber;
        }
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        errno = 0;
        it->vector[1] = strtod(*buf, &end);
        if (errno != 0 || end == *buf) {
            fprintf(logFile,"Config parsing error on line %lu: expected a number, got '%s'\n",*ln,*buf);
            return parseErrorExpectedNumber;
        }
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        errno = 0;
        it->vector[2] = strtod(*buf, &end);
        if (errno != 0 || end == *buf) {
            fprintf(logFile,"Config parsing error on line %lu: expected a number, got '%s'\n",*ln,*buf);
            return parseErrorExpectedNumber;
        }
        /* literal ']'. */
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        REQUIRE_TOKEN("]");
        
#undef GET_TOKEN
#undef REQUIRE_TOKEN
#undef REQUIRE_NOT_EOL
        
        return CBF_SUCCESS;
    }
    
    int cbf_hdf5_parseConfig(FILE * const configFile, FILE * const logFile, cbf_hdf5_configItemVectorhandle vec)
    {
        char * tkn = 0;
        size_t n = 0, ln = 1;
        char pre = '\0';
        
#define GET_TOKEN() \
{ \
const int e = cbf_hdf5_parseScan(&tkn, &n, &ln, &pre, configFile); \
if (e != CBF_SUCCESS) { \
fprintf(logFile,"\nError: %s\n",cbf_hdf5_configParseStrerror(e)); \
return e; \
} \
};
        
#define REQUIRE_TOKEN(TKN) \
{ \
const char * const _tkn = (TKN); \
if (strcmp(_tkn,tkn)) { \
fprintf(logFile,"Config parsing error on line %lu: expected " #TKN ", got '%s'\n",ln,tkn); \
return parseErrorUnexpectedInput; \
} \
};
        
#define REQUIRE_EOL() \
{ \
if (strcmp("\n",tkn)) { \
fprintf(logFile,"Config parsing error on line %lu: expected '\\n', got '%s'\n",ln,tkn); \
return parseErrorUnexpectedInput; \
} \
};
        
#define REQUIRE_NOT_EOL() \
{ \
if (!strcmp("\n",tkn)) { \
fprintf(logFile,"Config parsing error on line %lu: unexpected newline\n",ln); \
return parseErrorUnexpectedInput; \
} \
};
        
#define REQUIRE_NEXUS_AXIS() \
{ \
if (strcmp(".",tkn) && cbf_hdf5_configItemVector_end(vec) == cbf_hdf5_configItemVector_findNexus(vec,tkn)) { \
fprintf(logFile,"Config parsing error on line %lu: Nexus axis '%s' not defined\n",ln,tkn); \
return parseErrorUndefinedValue; \
} \
};
        
#define REQUIRE_VECTOR() \
{ \
const int e = cbf_hdf5_parseExtractVector(configFile, logFile, it, &tkn, &n, &ln, &pre); \
if (CBF_SUCCESS != e) { \
fprintf(logFile,"Error reading a vector: %s\n",cbf_hdf5_configParseStrerror(e)); \
return e; \
} \
};
        
        /* first token of the line */
        GET_TOKEN();
        while (tkn) {
            if (!cbf_cistrcmp("map",tkn)) {
                /* storage that I don't need to free within this function */
                cbf_hdf5_configItem * it;
                /* minicbf axis name */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                it = cbf_hdf5_configItemVector_findMinicbf(vec,tkn);
                if (cbf_hdf5_configItemVector_end(vec) != it) {
                    fprintf(logFile,"Config parsing error on line %lu: Duplicate axis definition for minicbf axis '%s'\n",ln,tkn);
                    return parseErrorDuplicateField;
                }
                it = cbf_hdf5_configItemVector_push(vec,cbf_hdf5_createConfigItem());
                it->minicbf = strdup(tkn);
                /* literal 'to'. */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                REQUIRE_TOKEN("to");
                /* nexus axis name */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                if (cbf_hdf5_configItemVector_end(vec) != cbf_hdf5_configItemVector_findNexus(vec,tkn)) {
                    fprintf(logFile,"Config parsing error on line %lu: Duplicate axis definition for Nexus axis '%s'\n",ln,tkn);
                    return parseErrorDuplicateField;
                }
                it->nexus = strdup(tkn);
                /* newline */
                GET_TOKEN();
                REQUIRE_EOL();
            } else if (!cbf_cistrcmp("Sample",tkn)) {
                /* literal 'depends-on'. */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                REQUIRE_TOKEN("depends-on");
                /* nexus axis name */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                REQUIRE_NEXUS_AXIS();
                cbf_hdf5_configItemVector_setSampleDependsOn(vec,strdup(tkn));
                /* newline */
                GET_TOKEN();
                REQUIRE_EOL();
            } else if (!strcmp("\n",tkn)) {
            } else {
                /* find entry by nexus axis name */
                cbf_hdf5_configItem * const it = cbf_hdf5_configItemVector_findNexus(vec,tkn);
                if (cbf_hdf5_configItemVector_end(vec) == it) {
                    fprintf(logFile,"Config parsing error on line %lu: Nexus axis '%s' not defined\n",ln,tkn);
                    return parseErrorUndefinedValue;
                }
                /* match depends-on -> vector OR vector -> depends-on. */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                if (!cbf_cistrcmp("vector",tkn)) {
                    /* try to get a vector */
                    REQUIRE_VECTOR();
                    GET_TOKEN();
                    /* optional 'depends-on' */
                    if (!cbf_cistrcmp("depends-on",tkn)) {
                        /* nexus axis name */
                        GET_TOKEN();
                        REQUIRE_NOT_EOL();
                        REQUIRE_NEXUS_AXIS();
                        /* this is a potential memory leak */
                        it->depends_on = strdup(tkn);
                        GET_TOKEN();
                    }
                    /* check for newline */
                    REQUIRE_EOL();
                } else if (!cbf_cistrcmp("depends-on",tkn)) {
                    /* nexus axis name */
                    GET_TOKEN();
                    REQUIRE_NOT_EOL();
                    REQUIRE_NEXUS_AXIS();
                    /* this is a potential memory leak */
                    it->depends_on = strdup(tkn);
                    GET_TOKEN();
                    /* optional 'vector' */
                    if (!cbf_cistrcmp("vector",tkn)) {
                        /* try to get a vector */
                        REQUIRE_VECTOR();
                        GET_TOKEN();
                    }
                    /* check for newline */
                    REQUIRE_EOL();
                } else return parseErrorUnexpectedInput;
            }
            GET_TOKEN();
        }
        return CBF_SUCCESS;
    }
    
    /****************************************************************
     End of section of code extracted from J. Sloan's
     config.c
     ****************************************************************/
    
    
	/* Ensure I have a file to do stuff with.
     There are 4 possible cases:
     1: I dont have a file, no name is supplied -> fail
     2: I dont have a file, a name is supplied -> open it
     3: I have a file already, no name supplied -> success
     4: I have a file already, a name is supplied -> success if they match, else fail
     */
	int cbf_h5handle_require_file(const cbf_h5handle handle, const char * name)
	{
		if (!handle) return CBF_ARGUMENT;
		if (!name) {
			if (!cbf_H5Ivalid(handle->hfile)) return CBF_ARGUMENT;
			else return CBF_SUCCESS;
		} else {
			if (!cbf_H5Ivalid(handle->hfile)) {
				/* open a file with the given name */
				return cbf_H5Fopen(&(handle->hfile), name);
			} else {
				/* check the names match eventually - could be awkward */
				return CBF_NOTIMPLEMENTED;
			}
		}
	}
    
	/* Ensure I have a top-level NXentry group in the handle, below the file
     There are 4 possible cases:
     1: I dont have an entry, no name is supplied -> fail
     2: I dont have an entry, a name is supplied -> ensure the file is valid, try to open the group
     3: I have an entry already, no name supplied -> success
     4: I have an entry already, a name is supplied -> success if they match, else fail
     */
	int cbf_h5handle_require_entry(const cbf_h5handle handle, hid_t * group, const char * name)
	{
		/* if the handle isn't valid the function makes no sense */
		if (!handle) return CBF_ARGUMENT;
        
		if (!name) {
			/* if no name is given just check that the group is in the handle */
			if (!cbf_H5Ivalid(handle->nxid)) return CBF_ARGUMENT;
			else {
				if (group) *group = handle->nxid;
				return CBF_SUCCESS;
			}
		} else {
			/* if a name is given either create the group in a known place or ensure some names match */
			if (!cbf_H5Ivalid(handle->nxid)) {
				/* create/open a group with the given name */
				int error = CBF_SUCCESS;
				cbf_reportnez(cbf_h5handle_require_file(handle,0), error);
				/* TODO: find a suitable require_NXgroup function (which doesn't need to know about the handle) to use here */
				if (CBF_SUCCESS == error) {
					cbf_H5Gcreate(&handle->nxid,name,handle->hfile);
					if (!cbf_H5Ivalid(handle->nxid)) error |= CBF_H5ERROR;
				}
				cbf_reportnez(cbf_H5Arequire_string(handle->nxid,"NX_class","NXentry"), error);
				if (CBF_SUCCESS == error && group) *group = handle->nxid;
				return error;
			} else {
				/*
                 Check the names match, eventually.
                 Could be awkward as anonymous groups do not have names.
                 This probably requires a different function.
                 */
				return CBF_NOTIMPLEMENTED;
			}
		}
	}
    
	/* Ensure I have an NXsample group in the handle called 'sample' below the entry
     There are 4 possible cases:
     1: I have a sample already -> success
     2: I dont have a sample -> ensure the entry is valid, try to open the group
     */
	int cbf_h5handle_require_sample(const cbf_h5handle handle, hid_t * group)
	{
		int error = CBF_SUCCESS;
		if (!handle) return CBF_ARGUMENT;
		if (cbf_H5Ivalid(handle->nxsample)) {
			if (group) *group = handle->nxsample;
			return CBF_SUCCESS;
		}
		/* create/open a group with known name */
		cbf_reportnez(cbf_h5handle_require_entry(handle,0,0), error);
		/* TODO: find a suitable require_NXgroup function (which doesn't need to know about the handle) to use here */
		cbf_reportnez(cbf_H5Gcreate(&(handle->nxsample),"sample",handle->nxid), error);
		cbf_reportnez(cbf_H5Arequire_string(handle->nxsample,"NX_class","NXsample"), error);
		if (group) *group = handle->nxsample;
		return error;
	}
    
	/* Ensure I have an NXinstrument group in the handle called 'instrument' below the entry
     There are 4 possible cases:
     1: I have an instrument already -> success
     2: I dont have an instrument -> ensure the entry is valid, try to open the group
     */
	int cbf_h5handle_require_instrument(const cbf_h5handle handle, hid_t * group)
	{
		int error = CBF_SUCCESS;
		if (!handle) return CBF_ARGUMENT;
		if (cbf_H5Ivalid(handle->nxinst)) {
			if (group) *group = handle->nxinst;
			return CBF_SUCCESS;
		}
		/* create/open a group with known name */
		cbf_reportnez(cbf_h5handle_require_entry(handle,0,0), error);
		/* TODO: find a suitable require_NXgroup function (which doesn't need to know about the handle) to use here */
		cbf_reportnez(cbf_H5Gcreate(&(handle->nxinst),"instrument",handle->nxid), error);
		cbf_reportnez(cbf_H5Arequire_string(handle->nxinst,"NX_class","NXinstrument"), error);
		if (group) *group = handle->nxinst;
		return error;
	}
    
	/* Ensure I have a detector with the given name in the hdf5 handle
     If a detector by the same name exists, success
     If no detector by that name is found, add it
     
     Do not destroy the returned group - the handle owns it.
     */
	int cbf_h5handle_require_detector(const cbf_h5handle handle, hid_t * group)
	{
		int error = CBF_SUCCESS;
		if (!handle) return CBF_ARGUMENT;
		if (cbf_H5Ivalid(handle->nxdetector)) {
			if (group) *group = handle->nxdetector;
			return CBF_SUCCESS;
		}
		/* create/open a group with known name */
		cbf_reportnez(cbf_h5handle_require_instrument(handle,0), error);
		/* TODO: find a suitable require_NXgroup function (which doesn't need to know about the handle) to use here */
		cbf_reportnez(cbf_H5Gcreate(&(handle->nxdetector),"detector",handle->nxinst), error);
		cbf_reportnez(cbf_H5Arequire_string(handle->nxdetector,"NX_class","NXdetector"), error);
		if (group) *group = handle->nxdetector;
		return error;
	}
    
	/* Ensure I have a monochromator in the hdf5 handle
     If a monochromator exists, success
     If no monochromator exists, add it
     
     Do not destroy the returned monochromator group - the handle owns it.
     */
	int cbf_h5handle_require_monochromator(const cbf_h5handle handle, hid_t * group)
	{
		if (!handle) return CBF_ARGUMENT;
        
		if (cbf_H5Ivalid(handle->nxmonochromator)) {
			if (0 != group) *group = handle->nxmonochromator;
			return CBF_SUCCESS;
		} else {
			int error = CBF_SUCCESS;
			cbf_reportnez(cbf_h5handle_require_instrument(handle,0), error);
			cbf_reportnez(cbf_H5Gcreate(&(handle->nxmonochromator),"monochromator",handle->nxinst), error);
			cbf_reportnez(cbf_H5Arequire_string(handle->nxmonochromator,"NX_class","NXmonochromator"), error);
			if (CBF_SUCCESS == error && 0 != group) *group = handle->nxmonochromator;
			return error;
		}
	}
    
    /* Create a dotted CBF location string
     returns a newly allocated string that
     must be freed */
    
    int cbf_location_string(const char* datablock,
                            const char* category,
                            const char* column,
                            unsigned int row,
                            char * * stringout) {
        
        size_t dblen, catlen,collen,rowlen;
        
        char rownum[20];
        
        if (!stringout) return CBF_ARGUMENT;
        
        if (!datablock) datablock = "_(NULL)_";
        if (!category) category = "_(NULL)_";
        if (!column) column = "_(NULL)_";
        
        sprintf(rownum,"%u",row);
        
        dblen = strlen(datablock);
        catlen = strlen(category);
        collen = strlen(column);
        rowlen = strlen(rownum);
        
        if (dblen == 0) {
            datablock = "_(NULL)_"; dblen = 8;
        }
        if (catlen == 0) {
            category = "_(NULL)_"; catlen = 8;
        }
        if (collen == 0) {
            column = "_(NULL)_"; collen = 8;
        }
        
        cbf_failnez(cbf_alloc(((void **) stringout),NULL,
                              dblen+catlen+collen+rowlen+4,1));
        
        strcpy(*stringout,datablock);
        
        strcpy((*stringout)+dblen,".");
        
        strcpy((*stringout)+dblen+1,category);
        
        strcpy((*stringout)+dblen+1+catlen,".");
        
        strcpy((*stringout)+dblen+1+catlen+1,column);
        
        strcpy((*stringout)+dblen+1+catlen+1+collen,".");
        
        strcpy((*stringout)+dblen+1+catlen+1+collen+1,rownum);
        
        return CBF_SUCCESS;
        
    }
    
    /* Conatenate two strings, returning a newly allocated string */
    
    int cbf_strcat(const char * string1, const char * string2,
                   char * * stringout) {
        
        int errorcode;
        
        size_t len1, len2;
        
        if (!string1 || !string2 || !stringout) return CBF_ARGUMENT;
        
        errorcode = 0;
        
        len1 = strlen(string1);
        
        len2 = strlen(string2);
        
        cbf_failnez(cbf_alloc(((void **) stringout),NULL,
                              len1+len2+1,1));
        
        strcpy(*stringout,string1);
        
        strcpy((*stringout)+len1,string2);
        
        return CBF_SUCCESS;
        
    }
    
    
    /* Either open or create a NeXus group*/
    
    
    int cbf_require_nxgroup(cbf_h5handle h5handle,
                            const char * nxgroup,
                            const char * nxclass,
                            hid_t parent_id,
                            hid_t * groupid) {
        
        int errorcode;
        
        if (!h5handle || !nxgroup || !nxclass || !groupid ) return CBF_ARGUMENT;
        
        errorcode = 0;
        
        if (parent_id < 0) parent_id = h5handle->curnxid;
        
        if (parent_id < 0) parent_id = h5handle->nxid;
        
        if ((H5Lexists(parent_id,nxgroup,H5P_DEFAULT)) != 1) {
            
            /* ensure it goes right below the parent_id */
            
            if (h5handle->curnxid>=0 && h5handle->curnxid!= parent_id) {
                
                cbf_h5reportneg(H5Gclose(h5handle->curnxid),CBF_ARGUMENT,errorcode);
                
            }
            
            h5handle->curnxid = parent_id;
            
            cbf_reportnez(cbf_H5Gcreate_in_handle(h5handle,nxgroup,groupid),errorcode);
            
            cbf_reportnez(cbf_apply_h5text_attribute(*groupid,
                                                     "NX_class",nxclass,0),errorcode);
            
            h5handle->curnxid = CBF_H5FAIL;
            
        } else {
            
            if ((*groupid = H5Gopenx(parent_id,nxgroup))<0) return CBF_NOTFOUND;
            
        }
        
        return errorcode;
        
    }
    
    
    /* get an axis vector and offset */
    
    int cbf_get_axis_vector_and_offset(cbf_handle handle,
                                       const char *axis_id,
                                       double vector[3],
                                       double offset[3]) {
        
        /***** Allow for missing vector or offset treat as 0 ***/
        
        if (!handle || !axis_id ) return CBF_ARGUMENT;
        
        cbf_failnez (cbf_find_category   (handle, "axis"))
        cbf_failnez (cbf_find_column     (handle, "id"))
        cbf_failnez (cbf_find_row        (handle, axis_id))
        
        if (vector) {
            
            cbf_failnez (cbf_find_column     (handle, "vector[1]"))
            if (cbf_get_doublevalue (handle, vector)) vector[0] = 0;
            cbf_failnez (cbf_find_column     (handle, "vector[2]"))
            if (cbf_get_doublevalue (handle, vector+1)) vector[1] = 0.;
            cbf_failnez (cbf_find_column     (handle, "vector[3]"))
            if (cbf_get_doublevalue (handle, vector+2)) vector[2] = 0.;
            
        }
        
        if (offset) {
            
            cbf_failnez (cbf_find_column     (handle, "offset[1]"))
            if (cbf_get_doublevalue (handle, offset)) offset[0] = 0.;
            cbf_failnez (cbf_find_column     (handle, "offset[2]"))
            if (cbf_get_doublevalue (handle, offset+1)) offset[1] = 0.;
            cbf_failnez (cbf_find_column     (handle, "offset[3]"))
            if (cbf_get_doublevalue (handle, offset+2)) offset[2] = 0.;
            
        }
        
        return CBF_SUCCESS;
        
        
    }
    
    
    /* Compute the cross-product of 2 3-vectors */
    
    int cbf_cross_product(double vecin1[3],
                          double vecin2[3],
                          double vecout[3] ) {
        
        if (!vecin1 || !vecin2 || !vecout) return CBF_ARGUMENT;
        
        vecout[0] = vecin1[1]*vecin2[2] - vecin1[2]*vecin2[1];
        
        vecout[1] = vecin1[2]*vecin2[0] - vecin1[0]*vecin2[2];
        
        vecout[2] = vecin1[0]*vecin2[1] - vecin1[1]*vecin2[0];
        
        return CBF_SUCCESS;
        
    }
    
    /* compute the L2 norm of a 3-vector */
    
    double cbf_norm(double vector[3]) {
        
        if (!vector) return -1;
        
        return sqrt(vector[0]*vector[0]+vector[1]*vector[1]+vector[2]*vector[2]);
        
    }
    
    /* compute the product of a scalar and a vector */
    
    int cbf_scalar_product(double scalar, double vecin[3], double vecout[3]) {
        
        if (!vecin || ! vecout) return CBF_ARGUMENT;
        
        vecout[0] = scalar * vecin[0];
        
        vecout[1] = scalar * vecin[1];
        
        vecout[2] = scalar * vecin[2];
        
        return CBF_SUCCESS;
        
    }
    
    /* Apply a matrix to a vector */
    
    int cbf_apply_matrix(double matrix[3][3], double vecin[3], double vecout[3]) {
        
        int ii;
        
        if (!matrix || !vecin || !vecout ) return CBF_ARGUMENT;
        
        for (ii=0; ii < 3; ii++) {
            
            vecout[ii] = matrix[ii][0]*vecin[0]
            + matrix[ii][1]*vecin[1]
            + matrix[ii][2]*vecin[2];
            
            if (fabs(vecout[ii])<=1.e-15) vecout[ii] = 0.;
            
        }
        
        return CBF_SUCCESS;
        
    }
    
    
    
    /* compute the transform from CBF vectors to NeXus vectors
     Use the transpose to transfrom from NeXus vectors to CBF*/
    
    int cbf_get_NX_axis_transform(cbf_handle handle,
                                  double matrix [3][3]) {
        
        double beam[3];
        
        double gravity[3];
        
        double x_nx[3], y_nx[3];
        
        double normx_nx, normy_nx;
        
        if (!handle || !matrix) return CBF_ARGUMENT;
        
        /* cross multiple datablocks to find the AXIS category */
        
        if (cbf_find_tag(handle,"_axis.id")) return CBF_NOTFOUND;
        
        /* take the beam, if given, -source if source is given, or -Z */
        
        if (cbf_get_axis_vector_and_offset(handle,"BEAM",beam, NULL)) {
            
            if (cbf_get_axis_vector_and_offset(handle,"SOURCE",beam, NULL)) {
                
                beam[0] = 0.; beam[1] = 0.; beam[2] = -1.;
                
            } else {
                
                beam[0] = -beam[0]; beam[1] = -beam[1]; beam[2] = -beam[2];
            }
            
        }
        
        /* take gravity if given, otherwise [0, -1, 0 ],
         -up if up is givem, otherwise -Y */
        
        if (cbf_get_axis_vector_and_offset(handle,"GRAVITY",gravity,NULL)) {
            
            if (cbf_get_axis_vector_and_offset(handle,"GRAVITY",gravity,NULL)) {
                
                gravity[0] = 0; gravity[1] = -1.; gravity[2] = 0.;
                
            }
            
        }
        
        cbf_failnez(cbf_cross_product(beam,gravity,x_nx));
        
        normx_nx = cbf_norm(x_nx);
        
        if (normx_nx <= 1.e-38) return CBF_ARGUMENT;
        
        cbf_failnez(cbf_scalar_product(1./normx_nx,x_nx,x_nx));
        
        
        cbf_failnez(cbf_cross_product(beam,x_nx,y_nx));
        
        normy_nx = cbf_norm(y_nx);
        
        if (normy_nx <= 1.e-38) return CBF_ARGUMENT;
        
        cbf_failnez(cbf_scalar_product(1./normy_nx,y_nx,y_nx));
        
        matrix[0][0] = x_nx[0]; matrix[0][1] = x_nx[1]; matrix[0][2] = x_nx[2];
        
        matrix[1][0] = y_nx[0]; matrix[1][1] = y_nx[1]; matrix[1][2] = y_nx[2];
        
        matrix[2][0] = beam[0]; matrix[2][1] = beam[1]; matrix[2][2] = beam[2];
        
        return CBF_SUCCESS;
        
    }
    
    
    /* Write the HDF5 version of the NeXus axis definitions, if
     the original CBF had axis definitions */
    
    int cbf_write_h5nxaxes(cbf_handle handle, cbf_h5handle h5handle) {
        
        
        int errorcode;
        
        unsigned int rows, row;
        
        double matrix[3][3];
        
        hsize_t one=1;
        
        hsize_t naught=0;
        
        double zero[1];
        
        hid_t instrumentid;
        
        const char* datablock;
        
        errorcode = 0;
        
        zero[0] = 0.;
        
        /* If we get a transform, there are axes to convert */
        
        cbf_reportnez(cbf_get_NX_axis_transform(handle, matrix),errorcode);
        
        if (errorcode) return errorcode;
        
        
        /* We will need use the instrument group or create it*/
        
        cbf_reportnez(cbf_require_nxgroup(h5handle,
                                          "instrument",
                                          "NXinstrument",
                                          h5handle->nxid,
                                          &instrumentid),errorcode);
        
        cbf_reportnez(cbf_find_category(handle, "axis"),errorcode);
        
        cbf_reportnez(cbf_find_column(handle,"id"),errorcode);
        
        cbf_reportnez(cbf_count_rows(handle,&rows),errorcode);
        
        cbf_reportnez(cbf_datablock_name(handle,&datablock),errorcode);
        
        for(row=0; row < rows; row++) {
            
            char * cbfloc;
            
            char nxequipment[2048];
            
            hid_t equipmentid;
            
            hid_t nxaxisid;
            
            hid_t nxaxisoffsetid;
            
            hid_t dtype, dspace, dprop;
            
            const char * equipment;
            
            const char * equipmentclass;
            
            const char * equipmentname;
            
            const char * axis_id;
            
            const char * depends_on;
            
            const char * type;
            
            const char * system;
            
            hsize_t scanpoints;
            
            size_t sscanpoints;
            
            const char * units;
            
            double vector[3], offset[3];
            
            cbf_reportnez(cbf_find_category(handle, "axis"),errorcode);
            
            cbf_reportnez(cbf_find_column(handle,"id"),errorcode);
            
            depends_on = ".";
            
            equipment = "general";
            
            equipmentclass = "NXcoordinate_system";
            
            type = "general";
            
            system = "laboratory";
            
            cbf_reportnez(cbf_select_row(handle,row),errorcode);
            
            cbf_reportnez(cbf_find_column(handle,"id"),errorcode);
            
            cbf_reportnez(cbf_get_value(handle,&axis_id),errorcode);
            
            if (!cbf_find_column(handle,"equipment")) {
                
                cbf_reportnez(cbf_get_value(handle,&equipment),errorcode);
                
                if (!equipment)  equipment = "general";
                
            }
            
            if (!cbf_find_column(handle,"depends_on")) {
                
                cbf_reportnez(cbf_get_value(handle,&depends_on),errorcode);
                
                if (!depends_on) depends_on = ".";
                
            }
            
            if (!cbf_find_column(handle,"type")) {
                
                cbf_reportnez(cbf_get_value(handle,&type),errorcode);
                
                if (!type) type = "general";
                
            }
            
            if (!cbf_find_column(handle,"system")) {
                
                cbf_reportnez(cbf_get_value(handle,&system),errorcode);
                
                if (!system||!system[0]) system = "laboratory";
                
                if (cbf_cistrcmp(system,".")||cbf_cistrcmp(system,"?"))
                    system = "laboratory";
                
            }
            
            if (cbf_cistrcmp(system,"laboratory")) {
                
                
                cbf_reportnez(cbf_get_axis_vector_and_offset(handle,axis_id,
                                                             vector, offset),errorcode);
                
            } else {
                
                double cbfvector[3], cbfoffset[3];
                
                cbf_reportnez(cbf_get_axis_poise(handle, 0.,
                                                 (double *)cbfvector,(double *)cbfvector+1,(double *)cbfvector+2,
                                                 (double *)cbfoffset,(double *)cbfoffset+1,(double *)cbfoffset+2,
                                                 NULL,axis_id,NULL),errorcode);
                
                system = "McStas_absolute";
                
                cbf_reportnez(cbf_apply_matrix(matrix,cbfvector,vector),errorcode);
                
                cbf_reportnez(cbf_apply_matrix(matrix,cbfoffset,offset),errorcode);
                
                
            }
            
            /*  We have the equipment type in equipment and the axis is in axis_id
             If the equipment type is detector, we need to map the axis_id
             to the appropriate detector so we can put this axis in
             /instrument:NXinstrument
             /CBF_diffrn_detector__DETECTORNAME:NXdetector
             /CBF_axis__AXISID=[]
             
             If the equipment type is goniometer, we need to map the axis_id
             to the appropriate goniometer, so we can put this axis in
             /instrument:NXinstrument
             /CBF_diffrn_measurement__GONIOMETERNAME:NXsample
             /CBF__axis__AXISID=[]
             
             For other equipment types, we put this axis in
             /instrument:NXinstrument
             /coordinate_system:NXcoordinate_system
             /CBF__axis__AXISID=[]
             */
            
            cbf_reportnez(cbf_get_axis_equipment_id(handle,&equipmentname,equipment,axis_id),errorcode);
            
            if (!equipment) equipment = "";
            
            if (cbf_cistrcmp(equipment,"detector")==0) {
                
                strcpy(nxequipment,"CBF_diffrn_detector__");
                
                if (equipmentname) {
                    
                    strncat(nxequipment,equipmentname,2020);
                    
                } else {
                    
                    strcpy(nxequipment,"detector");
                }
                
                equipmentclass = "NXdetector";
                
            } else if (cbf_cistrcmp(equipment,"goniometer")==0) {
                
                strcpy(nxequipment,"CBF_diffrn_measurement__");
                
                if (equipmentname) {
                    
                    strncat(nxequipment,equipmentname,2020);
                    
                    nxequipment[2047] = 0;
                    
                } else {
                    
                    strcpy(nxequipment,"sample");
                }
                
                
                equipmentclass = "NXsample";
                
            } else {
                
                strcpy(nxequipment,"coordinate_system");
                
                equipmentclass = "NXcoordinate_system";
                
            }
            
            
            cbf_reportnez(cbf_require_nxgroup(h5handle,
                                              nxequipment, equipmentclass,
                                              instrumentid, &equipmentid),errorcode);
            
            cbf_reportnez(cbf_get_axis_parameters(handle,
                                                  &sscanpoints,
                                                  &units,
                                                  equipment,
                                                  axis_id),errorcode);
            
            scanpoints = (hsize_t)sscanpoints;
            
            /* At this point we are ready to write the field CBF_axis__AXISID[] */
            
            
            
            if (cbf_norm(offset) > 1.e-20) {
                
                char * nxaxis_offset_name;
                
                char * nxaxis_name;
                
                char * nxdepends_on_name;
                
                hid_t mtype;
                
                cbf_reportnez(cbf_strcat("CBF_axis_offset__",
                                         axis_id,&nxaxis_offset_name),
                              errorcode);
                
                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         axis_id,&nxaxis_name),errorcode);
                
                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         depends_on,&nxdepends_on_name),errorcode);
                
                cbf_h5reportneg(dspace = H5Screate_simple(1,&naught,&one),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(mtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(H5Pset_chunk(dprop, 1, &one),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(nxaxisoffsetid = H5Dcreatex(equipmentid,nxaxis_offset_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);
                
                /* cbf_h5reportneg(H5Dwrite(nxaxisoffsetid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)zero),CBF_ALLOC,errorcode); */
                
                
                cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);
                
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                        "transformation_type",
                                                        "translation",
                                                        errorcode);
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                        "system",system,errorcode);
                
                
                errorcode |= cbf_apply_h5vector_attribute(nxaxisoffsetid,
                                                          "vector",(double *)offset,3,errorcode);
                
                if (!cbf_cistrcmp(depends_on,".")) {
                    
                    errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                            "depends_on",depends_on,errorcode);
                    
                } else {
                    
                    errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                            "depends_on",nxdepends_on_name,errorcode);
                    
                }
                
                cbf_reportnez(cbf_location_string(datablock,"axis","offset",row,&cbfloc),errorcode);
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                        "cbf_location",
                                                        cbfloc,
                                                        errorcode);
                
                cbf_h5reportneg(H5Dclose(nxaxisoffsetid),CBF_FORMAT,errorcode);
                
                
                if (scanpoints > 0) {
                    
                    hsize_t scanpointsfound;
                    
                    hid_t mtype;
                    
                    size_t sscanpointsfound;
                    
                    double * scanarray;
                    
                    cbf_reportnez(cbf_alloc(((void **) &scanarray),NULL,
                                            scanpoints*sizeof(double),1),errorcode);
                    
                    cbf_reportnez(cbf_get_axis_scan_points(handle,
                                                           scanarray,
                                                           (size_t)scanpoints,
                                                           &sscanpointsfound,
                                                           units,
                                                           axis_id),errorcode);
                    
                    scanpointsfound = (hsize_t)sscanpointsfound;
                    
                    if (sscanpointsfound==0) scanpointsfound=1;
                    
                    cbf_h5reportneg(dspace = H5Screate_simple(1,&scanpointsfound,&scanpoints),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(mtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(nxaxisid = H5Dcreatex(equipmentid,nxaxis_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Dwrite(nxaxisid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)scanarray),CBF_ALLOC,errorcode);;
                    
                    cbf_reportnez(cbf_free((void **)(&scanarray),NULL),errorcode);
                    
                    cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);
                    
                    if (units) {
                        
                        errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                                "units",units,errorcode);
                    }
                    
                } else {
                    
                    hid_t mtype;
                    
                    cbf_h5reportneg(dspace = H5Screate_simple(1,&naught,&one),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Pset_chunk(dprop, 1, &one),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(nxaxisid = H5Dcreatex(equipmentid,nxaxis_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);
                    
                    /* cbf_h5reportneg(H5Dwrite(nxaxisid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)zero),CBF_ALLOC,errorcode);*/
                    
                    cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);
                    
                }
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "transformation_type",type,errorcode);
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "system",system,errorcode);
                
                errorcode |= cbf_apply_h5vector_attribute(nxaxisid,
                                                          "vector",(double *)vector,3,errorcode);
                
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "depends_on",nxaxis_offset_name,errorcode);
                
                cbf_reportnez(cbf_location_string(datablock,"axis","vector",row,&cbfloc),errorcode);
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                        "cbf_location",
                                                        cbfloc,
                                                        errorcode);
                
                cbf_h5reportneg(H5Dclose(nxaxisoffsetid),CBF_ALLOC,errorcode);
                
                cbf_reportnez(cbf_free((void **)&nxaxis_offset_name,NULL),errorcode);
                
                cbf_reportnez(cbf_free((void **)&nxaxis_name,NULL),errorcode);
                
                cbf_reportnez(cbf_free((void **)&nxdepends_on_name,NULL),errorcode);
                
                cbf_reportnez(cbf_free((void **)&cbfloc,NULL),errorcode);
                
            } else {
                
                char * nxaxis_name;
                
                char * nxdepends_on_name;
                
                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         axis_id,&nxaxis_name),errorcode);
                
                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         depends_on,&nxdepends_on_name),errorcode);
                
                if (scanpoints > 0) {
                    
                    hsize_t scanpointsfound;
                    
                    hid_t mtype;
                    
                    size_t sscanpointsfound;
                    
                    double * scanarray;
                    
                    cbf_reportnez(cbf_alloc(((void **) &scanarray),NULL,
                                            scanpoints*sizeof(double),1),errorcode);
                    
                    cbf_reportnez(cbf_get_axis_scan_points(handle,
                                                           scanarray,
                                                           (size_t)scanpoints,
                                                           &sscanpointsfound,
                                                           units,
                                                           axis_id),errorcode);
                    
                    scanpointsfound = (hsize_t)sscanpointsfound;
                    
                    if (sscanpointsfound == 0) scanpointsfound=1;
                    
                    cbf_h5reportneg(dspace = H5Screate_simple(1,&scanpointsfound,&scanpoints),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(mtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(nxaxisid = H5Dcreatex(equipmentid,nxaxis_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Dwrite(nxaxisid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)scanarray),CBF_ALLOC,errorcode);
                    
                    cbf_reportnez(cbf_free((void **)(&scanarray),NULL),errorcode);
                    
                    cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);
                    
                    if (units) {
                        
                        errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                                "units",units,errorcode);
                    }
                } else {
                    
                    hid_t mtype;
                    
                    cbf_h5reportneg(dspace = H5Screate_simple(1,&naught,&one),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(mtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Pset_chunk(dprop, 1, &one),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(nxaxisid = H5Dcreatex(equipmentid,nxaxis_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);
                    
                    /* cbf_h5reportneg(H5Dwrite(nxaxisid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)zero),CBF_ALLOC,errorcode); */
                    
                    cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);
                    
                    cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);
                    
                    
                }
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "transformation_type",
                                                        type,
                                                        errorcode);
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "system",system,errorcode);
                
                errorcode |= cbf_apply_h5vector_attribute(nxaxisid,
                                                          "vector",(double *)vector,3,errorcode);
                
                if (!cbf_cistrcmp(depends_on,".")) {
                    
                    errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                            "depends_on",depends_on,errorcode);
                    
                } else {
                    
                    errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                            "depends_on",nxdepends_on_name,errorcode);
                    
                }
                
                cbf_reportnez(cbf_location_string(datablock,"axis","vector",row,&cbfloc),errorcode);
                
                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "cbf_location",
                                                        cbfloc,
                                                        errorcode);
                
                cbf_h5reportneg(H5Dclose(nxaxisid),CBF_FORMAT,errorcode);
                
                cbf_reportnez(cbf_free((void **)&nxaxis_name,NULL),errorcode);
                
                cbf_reportnez(cbf_free((void **)&nxdepends_on_name,NULL),errorcode);
                
                cbf_reportnez(cbf_free((void **)&cbfloc,NULL),errorcode);
            }
            
            cbf_h5reportneg(H5Gclose(equipmentid),CBF_FORMAT,errorcode);
            
        }
        
        return errorcode;
        
    }
    
    /* apply a double vector attribute to a group or dataset */
    
    int cbf_apply_h5vector_attribute(hid_t hid,
                                     const char* attribname,
                                     const double* attribvec,
                                     const size_t dimension,
                                     int errorcode)
    {
        
        hid_t attribspace, attribtype, attribid;
        
        hid_t attribmemtype;
        
        hsize_t dims[1];
        
        attribspace = attribtype = attribmemtype = attribid = CBF_H5FAIL;
        
        /* ensure arguments all given */
        
        if (hid < 0 || !attribname|| !attribvec || dimension < 1 ) return CBF_ARGUMENT;
        
        cbf_h5reportneg(attribspace = H5Screate(H5S_SIMPLE),CBF_ALLOC,errorcode);
        
        dims[0] = dimension;
        
        cbf_h5reportneg(H5Sset_extent_simple(attribspace, 1, dims, NULL),CBF_FORMAT,errorcode);
        
        cbf_h5reportneg(attribtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(attribmemtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(attribid = H5Acreatex(hid,attribname,
                                              attribtype,
                                              attribspace,
                                              H5P_DEFAULT),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(H5Awrite(attribid,attribmemtype,
                                 attribvec),CBF_ALLOC,errorcode);
        
        if (attribspace >= 0)  H5Sclose(attribspace);
        
        if (attribtype >= 0)   H5Tclose(attribtype);
        
        if (attribmemtype >= 0)   H5Tclose(attribmemtype);
        
        if (attribid >= 0)     H5Aclose(attribid);
        
        return errorcode;
        
    }
    
    
    /* apply a long attribute to a group or dataset */
    
    int cbf_apply_h5longasstr_attribute(hid_t hid,
                                        const char* attribname,
                                        const long attriblong,
                                        int errorcode)
    {
        char buffer[20];
        
        if (attriblong > -10 && attriblong < 10) {
            
            sprintf(buffer,"%ld",attriblong);
            
        } else {
            
            sprintf(buffer,"0x%lx",attriblong);
            
        }
        
        return cbf_apply_h5text_attribute(hid,attribname,buffer,errorcode);
        
    }
    
    
    /* apply an integer attribute to a group or dataset as a string */
    
    int cbf_apply_h5intasstr_attribute(hid_t hid,
                                       const char* attribname,
                                       const int attribint,
                                       int errorcode)
    {
        char buffer[20];
        
        if (attribint > -10 && attribint < 10) {
            
            sprintf(buffer,"%d",attribint);
            
        } else {
            
            sprintf(buffer,"0x%x",attribint);
            
        }
        
        return cbf_apply_h5text_attribute(hid,attribname,buffer,errorcode);
        
    }
    
    
    
    /* apply a integer attribute to a group or dataset */
    
    int cbf_apply_h5integer_attribute(hid_t hid,
                                      const char* attribname,
                                      const int attribint,
                                      int errorcode)
    {
        
        hid_t attribspace, attribtype, attribid;
        
        attribspace = attribtype = attribid = CBF_H5FAIL;
        
        /* ensure arguments all given */
        
        if (hid < 0 || !attribname ) return CBF_ARGUMENT;
        
        cbf_h5reportneg(attribspace = H5Screate(H5S_SCALAR),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(attribtype = H5Tcopy(H5T_NATIVE_INT),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(H5Tset_size(attribtype,1),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(attribid = H5Acreatex(hid,attribname,
                                              attribtype,
                                              attribspace,
                                              H5P_DEFAULT),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(H5Awrite(attribid,attribtype,
                                 &attribint),CBF_ALLOC,errorcode);
        
        if (attribspace >= 0)  H5Sclose(attribspace);
        
        if (attribtype >= 0)   H5Tclose(attribtype);
        
        if (attribid >= 0)     H5Aclose(attribid);
        
        return errorcode;
        
    }
    
    
    /* apply a text attribute to a group or dataset */
    
    int cbf_apply_h5text_attribute(hid_t hid,
                                   const char* attribname,
                                   const char* attribtext,
                                   int errorcode)
    {
        
        hid_t attribspace, attribtype, attribid;
        
        attribspace = attribtype = attribid = CBF_H5FAIL;
        
        /* ensure arguments all given */
        
        if (hid < 0 || !attribname || !attribtext ) return CBF_ARGUMENT;
        
        cbf_h5reportneg(attribspace = H5Screate(H5S_SCALAR),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(attribtype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(H5Tset_size(attribtype,strlen(attribtext)),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(attribid = H5Acreatex(hid,attribname,
                                              attribtype,
                                              attribspace,
                                              H5P_DEFAULT),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(H5Awrite(attribid,attribtype,
                                 (const void *)attribtext),CBF_ALLOC,errorcode);
        
        if (attribspace >= 0)  H5Sclose(attribspace);
        
        if (attribtype >= 0)   H5Tclose(attribtype);
        
        if (attribid >= 0)     H5Aclose(attribid);
        
        return errorcode;
        
    }
    
    /* apply a text dataset slab to a group
     
     places the specified datasettext in the specified slab of the
     specified datasetname for group hid.  The dataset is created
     if it does not already exist.
     
     The slabs are indexed from 0
     
     */
    
    int cbf_apply_h5text_dataset_slab(hid_t hid,
                                      const char* datasetname,
                                      const char* datasettext,
                                      const hsize_t slab,
                                      int errorcode)
    {
        hid_t datasetspace, datasettype, datasetid;
        
        hid_t memspace, memtype;
        
        hid_t ndatasettype;
        
        hid_t ndatasetspace;
        
        hid_t nmemtype;
        
        int ndims;
        
        hsize_t offset[1] = {0};
        
        hsize_t stride[1] = {1};
        
        hsize_t count[1]  = {1};
        
        hsize_t chunk[1] = {1};
        
        hsize_t curdim[1];
        
        hsize_t memsize[1] = {1};

        htri_t dsexists;
        
        hsize_t dssize[1];
        
        hsize_t maxdssize[1];
        
        hsize_t dsdims[1];
        
        hsize_t dsmaxdims[1];
        
        hsize_t dsslab;
        
        hid_t anondataset;
        
        void * datasettextbuffer;
        
        size_t old_size, new_size;
        
        datasetspace = datasettype = memspace = memtype = CBF_H5FAIL;
        
        ndatasetspace = ndatasettype = nmemtype = CBF_H5FAIL;
        
        datasetid = CBF_H5FAIL;
        
        memsize[0] = 1;
        
        dssize[0] = 1;
        
        maxdssize[0] = H5S_UNLIMITED;
        
        chunk[0] = 1;
        
        
        /* ensure arguments all given */
        
        if (hid < 0 || !datasetname ||
            !datasettext || errorcode) return CBF_ARGUMENT;
        
        dsexists = H5Lexists(hid,datasetname, H5P_DEFAULT);
        
        if (dsexists < 0 ||
            !dsexists
            || (datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT))< 0) {
            
            /* Create the dataset if we were unable to open it */
            
            cbf_h5reportneg(datasettype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);
            
            cbf_h5reportneg(H5Tset_size(datasettype,strlen(datasettext)),CBF_ALLOC,errorcode);

            cbf_reportnez(cbf_H5Dcreate(hid,&datasetid,datasetname,1,dssize,maxdssize,chunk,datasettype),errorcode);
            
        }  else {
            
            if (datasetid <= 0) {
                
                datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT);
                
            }
            
            cbf_h5reportneg(datasettype = H5Dget_type(datasetid),CBF_FORMAT,errorcode);
            
            cbf_h5reportneg(datasetspace = H5Dget_space(datasetid),CBF_FORMAT,errorcode);
            
            cbf_h5reportneg(ndims = H5Sget_simple_extent_ndims(datasetspace),CBF_FORMAT,errorcode);
            
            old_size = H5Tget_size(datasettype);
            
            new_size = strlen(datasettext);
            
            if (old_size < new_size && ndims == 1) {
                                
                ndatasettype = CBF_H5FAIL;
                
                cbf_h5reportneg(ndatasettype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(H5Sget_simple_extent_dims(datasetspace,
                                                          dsdims,dsmaxdims),CBF_FORMAT,errorcode);
                
                /* rebuild the dataset with longer strings */
                
                if (old_size+((old_size+1)>>1)>= new_size) {
                    
                    new_size = old_size+((old_size+1)>>1);
                    
                }
                
                cbf_reportnez(cbf_alloc(&datasettextbuffer,NULL,
                                        new_size+1,1),errorcode);
                
                if (datasettype >= 0) H5Tclose(datasettype);
                
                datasettype = ndatasettype;
                                
                cbf_h5reportneg(H5Tset_size(datasettype,new_size),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(memtype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(nmemtype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(memspace = H5Screate_simple(1,memsize,NULL),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(H5Tset_size(memtype,new_size),CBF_ALLOC,errorcode);
                
                cbf_h5reportneg(H5Tset_size(nmemtype,new_size),CBF_ALLOC,errorcode);
                
                cbf_reportnez(cbf_H5Dcreate(hid,&anondataset,NULL,1,dsdims,maxdssize,chunk,datasettype),errorcode);
                
                for (dsslab=0; dsslab < dsdims[0]; dsslab++) {
                    
                    offset[0] = dsslab;

                    cbf_h5reportneg(H5Sselect_hyperslab(datasetspace,H5S_SELECT_SET,
                                                        offset,stride,count,0), CBF_FORMAT,errorcode);

                    if (H5Dread(datasetid, memtype, memspace, datasetspace,
                                H5P_DEFAULT, (void *)datasettextbuffer)>=0) {
                                                
                        curdim[0] = dsslab+1;
                        
                        cbf_h5reportneg(H5Dset_extent(anondataset,curdim),CBF_FORMAT,errorcode);
                        
                        offset[0] = dsslab;
                        
                        stride[0] = 1;
                        
                        count[0] = 1;
                        
                        cbf_reportnez(cbf_H5Dwrite(anondataset,offset,stride,count,(void *)datasettextbuffer),errorcode);
                        
                    }
                    
                }
                
                if (nmemtype >= 0) H5Tclose(nmemtype);
                
                if (ndatasetspace >= 0) H5Sclose(ndatasetspace);
                
                if (datasetspace >= 0) H5Sclose(datasetspace);
                
                cbf_h5reportneg(H5Ldelete(hid,datasetname,H5P_DEFAULT), CBF_FORMAT, errorcode);
                
                cbf_h5reportneg(H5Olink(anondataset,hid,datasetname,H5P_DEFAULT,H5P_DEFAULT), CBF_FORMAT, errorcode);
                
                if (datasetid >= 0) H5Dclose(datasetid);
                
                datasetid = anondataset;
                
                if (datasettextbuffer) cbf_free(&datasettextbuffer,NULL);
                
                if (memtype >= 0) H5Tclose(memtype);
                
                datasetspace = memtype = CBF_H5FAIL;
                
            }
            
            if (datasetspace >=0 )H5Sclose(datasetspace);
            
            datasetspace = CBF_H5FAIL;

            
        }
        
        curdim[0] = slab+1;
        
        cbf_h5reportneg(H5Dset_extent(datasetid,curdim),CBF_FORMAT,errorcode);
        
        offset[0] = slab;
        
        stride[0] = 1;
        
        count[0] = 1;
        
        cbf_reportnez(cbf_H5Dwrite(datasetid,offset,stride,count,(void *)datasettext),errorcode);
        
        if (datasetspace >= 0)  H5Sclose(datasetspace);
        
        if (datasettype >= 0)   H5Tclose(datasettype);
        
        if (datasetid >= 0)     H5Dclose(datasetid);
        
        if (memtype >= 0)       H5Tclose(memtype);
        
        return errorcode;
        
    }
    
    /* apply a text dataset to a group */
    
    int cbf_apply_h5text_dataset(hid_t hid,
                                 const char* datasetname,
                                 const char* datasettext,
                                 int errorcode)
    {
        
        hid_t datasetspace, datasettype, datasetid, datasetprop;
        
        datasetspace = datasettype = datasetid = CBF_H5FAIL;
        
        /* ensure arguments all given */
        
        if (hid < 0 || !datasetname || !datasettext
            || errorcode) return CBF_ARGUMENT;
        
        cbf_h5reportneg(datasetspace = H5Screate(H5S_SCALAR),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(datasettype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(H5Tset_size(datasettype,strlen(datasettext)),CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(datasetprop = H5Pcreate(H5P_DATASET_CREATE),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(datasetid = H5Dcreatex(hid,datasetname,
                                               datasettype,
                                               datasetspace,
                                               datasetprop),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(H5Dwrite(datasetid,datasettype,
                                 H5S_ALL,H5S_ALL,
                                 H5P_DEFAULT,
                                 (const void *)datasettext),
                        CBF_ALLOC,errorcode);
        
        if (datasetprop >= 0)   H5Pclose(datasetprop);
        
        if (datasetspace >= 0)  H5Sclose(datasetspace);
        
        if (datasettype >= 0)   H5Tclose(datasettype);
        
        if (datasetid >= 0)     H5Dclose(datasetid);
        
        return errorcode;
        
    }
    
    
    
    /* Write a binary value to an HDF5 file */
    
    int cbf_write_h5binary (cbf_handle handle,
                            cbf_node *column,
                            unsigned int row,
                            cbf_h5handle h5handle)
    {
        hid_t valid, valtype, memtype, valprop, valspace;
        
        int errorcode;
        
        char rownum[10];
        
        cbf_file *infile;
        
        char digest [25];
        
        long start;
        
        size_t size;
        
        hsize_t hsize[3];
        
        unsigned int compression;
        
        unsigned char * rawdata;
        
        void * uncompressedarray;
        
        int id, bits, sign, type, checked_digest, realarray;
        
        const char *byteorder;
        
        size_t dimover, dimfast, dimmid, dimslow;
        
        size_t padding;
        
        size_t elsize;
        
        size_t nelems_read;
        
        unsigned int cd_values[CBF_H5Z_FILTER_CBF_NELMTS];
        
        /* Check the arguments */
        
        if (!handle || !h5handle || !h5handle->hfile)
            
            return CBF_ARGUMENT;
        
        if (!cbf_is_binary (column, row))
            
            return CBF_ARGUMENT;
        
        if (cbf_is_mimebinary (column, row))
            
            return CBF_ARGUMENT;
        
        cbf_failnez (cbf_get_bintext (column, row, &type, &id, &infile,
                                      &start, &size, &checked_digest,
                                      digest, &bits, &sign, &realarray,
                                      &byteorder, &dimover, &dimfast, &dimmid, &dimslow,
                                      &padding, &compression))
        
        if (dimslow == 0) dimslow = 1;
        
        if (dimmid == 0) dimmid = 1;
        
        if (dimfast == 0) dimfast = 1;
        
        /* Position the file at the start of the binary section */
        
        cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))
        
        /* Calculate the digest if necessary */
        
        if (!cbf_is_base64digest (digest))
        {
            
            /* Compute the message digest */
            
            cbf_failnez (cbf_md5digest (infile, size, digest))
            
            
            /* Go back to the start of the binary data */
            
            cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))
            
            
            /* Update the entry */
            
            checked_digest = 1;
            
            cbf_failnez (cbf_set_bintext (column, row, type,
                                          id, infile, start, size,
                                          checked_digest, digest, bits,
                                          sign,  realarray,
                                          byteorder, dimover, dimfast, dimmid, dimslow,
                                          padding, compression))
        }
        
        /* Discard any bits in the buffers */
        
        
        infile->bits [0] = 0;
        infile->bits [1] = 0;
        
        valid = valtype = valprop = valspace = CBF_H5FAIL;
        
        sprintf(rownum,"%d", row);
        
        /* prepare the errorcode */
        
        errorcode = 0;
        
        if (h5handle->flags & CBF_H5_OPAQUE) {
            
            /* Treat the image as an opaque stream of size bytes */
            
            hsize[0] = size;
            
            cbf_h5reportneg(valspace = H5Screate_simple(1,hsize,NULL),
                            CBF_ALLOC,errorcode);
            
            cbf_h5reportneg(valtype = H5Tcreate(H5T_OPAQUE,1),
                            CBF_ALLOC,errorcode);
            
            cbf_h5reportneg(H5Tset_tag(valtype,"stream of opaque bytes"),
                            CBF_ALLOC,errorcode);
            
            
            cbf_h5reportneg(valprop = H5Pcreate(H5P_DATASET_CREATE),
                            CBF_ALLOC,errorcode);
            
            cbf_h5reportneg(valid = H5Dcreatex(h5handle->colid,rownum,
                                               valtype,valspace,
                                               valprop),
                            CBF_ALLOC,errorcode);
            
            
            /* get all the data */
            
            /* first ensure enough space at infile->characters
             for size characters, which means we need enough
             at characters->base for size + the old data,
             if any */
            
            cbf_reportnez(cbf_set_io_buffersize(infile, size),errorcode);
            
            /* now we can safely do the read
             
             because of the file positioning done, infile->characters
             is actually the same as infile_characters_base for a
             file stream based file */
            
            rawdata = (unsigned char*) infile->characters;
            
            if (infile->characters_used < size) {
                
                
                /* We cannot get any more characters from
                 a temporary file of file without a stream */
                
                if (infile->temporary || !infile->stream)  {
                    
                    errorcode |= CBF_FILEREAD;
                    
                }
                
                if (!errorcode && fread(rawdata, 1,
                                        size-(infile->characters_used), infile->stream)
                    != size-infile->characters_used)
                    errorcode |= CBF_FILEREAD;
                
                if (!errorcode) infile->characters_used = size;
                
            }
            
            cbf_h5reportneg(H5Dwrite(valid,valtype,
                                     valspace,H5S_ALL,H5P_DEFAULT,rawdata),
                            CBF_ARGUMENT,errorcode);
            
        } else {
            
            /* Treat the data as an array to be compressed
             
             chunking in planes dimfast x dimmid
             
             */
            
            hsize_t chunk[3];
            
            hsize_t maxdim[3];
            
            hsize[0] = dimslow;
            
            chunk[0] = 1;
            
            maxdim[0] = H5S_UNLIMITED;
            
            hsize[1] = chunk[1] = maxdim[1] = dimmid;
            
            hsize[2] = chunk[2] = maxdim[2] = dimfast;
            
            cbf_h5reportneg(valspace = H5Screate_simple(3,hsize,maxdim),
                            CBF_ALLOC,errorcode);
            
            if (realarray) {
                
                if (byteorder[0]=='l' || byteorder[0]=='L') {
                    
                    if (bits <= 32) {
                        
                        cbf_h5reportneg(valtype = H5Tcopy(H5T_IEEE_F32LE),
                                        CBF_ALLOC,errorcode);
                        cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_FLOAT),
                                        CBF_ALLOC,errorcode)
                        
                        
                    } else {
                        
                        cbf_h5reportneg(valtype = H5Tcopy(H5T_IEEE_F64LE),
                                        CBF_ALLOC,errorcode)
                        cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_DOUBLE),
                                        CBF_ALLOC,errorcode)
                        
                        
                    }
                    
                    
                } else {
                    
                    if (bits <= 32) {
                        
                        cbf_h5reportneg(valtype = H5Tcopy(H5T_IEEE_F32BE),
                                        CBF_ALLOC,errorcode);
                        cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_FLOAT),
                                        CBF_ALLOC,errorcode)
                        
                    } else {
                        
                        cbf_h5reportneg(valtype = H5Tcopy(H5T_IEEE_F64BE),
                                        CBF_ALLOC,errorcode)
                        cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_DOUBLE),
                                        CBF_ALLOC,errorcode)
                        
                    }
                    
                }
                
                
            } else {
                
                if (byteorder[0]=='l' || byteorder[0]=='L') {
                    
                    if (bits <= 8) {
                        
                        if (sign) {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I8LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_CHAR),
                                            CBF_ALLOC,errorcode);
                            
                        } else {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U8LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_UCHAR),
                                            CBF_ALLOC,errorcode);
                        }
                        
                    } else if (bits <= 16) {
                        
                        if (sign) {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I16LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_SHORT),
                                            CBF_ALLOC,errorcode);
                            
                        } else {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U16LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_USHORT),
                                            CBF_ALLOC,errorcode);
                        }
                        
                    } else if (bits <= 32) {
                        
                        if (sign) {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I32LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_INT),
                                            CBF_ALLOC,errorcode);
                            
                        } else {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U32LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_UINT),
                                            CBF_ALLOC,errorcode);
                        }
                        
                    } else {
                        
                        if (sign) {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I64LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_LLONG),
                                            CBF_ALLOC,errorcode);
                            
                        } else {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U64LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_ULLONG),
                                            CBF_ALLOC,errorcode);
                        }
                        
                    }
                    
                    
                } else {
                    
                    if (bits <= 8) {
                        
                        if (sign) {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I8BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_CHAR),
                                            CBF_ALLOC,errorcode);
                            
                        } else {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U8BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_UCHAR),
                                            CBF_ALLOC,errorcode);
                        }
                        
                    } else if (bits <= 16) {
                        
                        if (sign) {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I16BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_SHORT),
                                            CBF_ALLOC,errorcode);
                            
                        } else {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U16BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_USHORT),
                                            CBF_ALLOC,errorcode);
                        }
                        
                    } else if (bits <= 32) {
                        
                        if (sign) {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I32BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_INT),
                                            CBF_ALLOC,errorcode);
                            
                        } else {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U32BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_UINT),
                                            CBF_ALLOC,errorcode);
                            
                        }
                        
                    } else {
                        
                        if (sign) {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I64BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_LLONG),
                                            CBF_ALLOC,errorcode);
                            
                        } else {
                            
                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U64BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_ULLONG),
                                            CBF_ALLOC,errorcode);
                            
                        }
                        
                    }
                    
                }
                
            }
            
            
            cbf_h5reportneg(valprop = H5Pcreate(H5P_DATASET_CREATE),
                            CBF_ALLOC,errorcode);
            
            cbf_h5reportneg(H5Pset_chunk(valprop,3,chunk),
                            CBF_ALLOC,errorcode);
            
            cd_values[CBF_H5Z_FILTER_CBF_COMPRESSION] = compression;
            cd_values[CBF_H5Z_FILTER_CBF_RESERVED]    = 0;
            cd_values[CBF_H5Z_FILTER_CBF_BINARY_ID]   = id;
            cd_values[CBF_H5Z_FILTER_CBF_PADDING]     = padding;
            cd_values[CBF_H5Z_FILTER_CBF_ELSIZE]      = (bits+7)/8;
            cd_values[CBF_H5Z_FILTER_CBF_ELSIGN]      = sign;
            cd_values[CBF_H5Z_FILTER_CBF_REAL]        = realarray;
            cd_values[CBF_H5Z_FILTER_CBF_DIMFAST]     = dimfast;
            cd_values[CBF_H5Z_FILTER_CBF_DIMMID]      = dimmid;
            cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW]     = dimslow;
            
            if (h5handle->flags & CBF_H5_REGISTER_COMPRESSIONS) {
                
                if (!H5Zfilter_avail(CBF_H5Z_FILTER_CBF)) {
                    
                    cbf_h5reportneg(H5Zregister(CBF_H5Z_CBF),CBF_ALLOC,errorcode);
                    
                }
            }
            
            cbf_h5reportneg(H5Pset_filter(valprop,CBF_H5Z_FILTER_CBF,
                                          H5Z_FLAG_OPTIONAL,
                                          CBF_H5Z_FILTER_CBF_NELMTS,
                                          cd_values),CBF_ALLOC,errorcode);
            
            /* fprintf(stderr,"errorcode on setting filter CBF_H5Z_FILTER_CBF %d\n",errorcode); */
            
            valid = H5Dcreatex(h5handle->colid,rownum,
                               valtype,valspace,
                               valprop);
            
            
            /* get all the data */
            
            elsize = (bits+7)/8;
            
            cbf_reportnez(cbf_alloc(((void **) &uncompressedarray),NULL,
                                    dimover*elsize,1),errorcode);
            
            nelems_read = 0;
            
            /* Get the data */
            
            {
                int text_realarray, text_id;
                
                size_t text_dimover, text_dimfast, text_dimmid, text_dimslow, text_padding;
                
                const char *text_byteorder;
                
                cbf_reportnez (cbf_get_binary (column, row,
                                               &text_id,
                                               uncompressedarray,
                                               elsize,
                                               sign,
                                               dimover,
                                               &nelems_read,
                                               &text_realarray,
                                               &text_byteorder,
                                               &text_dimover,
                                               &text_dimfast,
                                               &text_dimmid,
                                               &text_dimslow,
                                               &text_padding),errorcode);
            }
            
            if (nelems_read < dimover) {
                
                cbf_failnez(cbf_free(&uncompressedarray,NULL));
                
                return errorcode|CBF_ENDOFDATA;
                
            }
            
            cbf_h5reportneg(H5Dwrite(valid,memtype,H5S_ALL,
                                     H5S_ALL,H5P_DEFAULT,uncompressedarray),
                            CBF_ARGUMENT,errorcode);
            
            if (memtype >= 0) {
                
                cbf_h5failneg(H5Tclose(memtype),CBF_ARGUMENT);
                
            }
            
            cbf_failnez(cbf_free(&uncompressedarray,NULL));
            
            
        }
        
        errorcode |= cbf_apply_h5integer_attribute(valid,"signal",row+1,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"compression",compression,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"binid",id,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"bits",bits,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"sign",sign,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"bintype",type,errorcode);
        errorcode |= cbf_apply_h5text_attribute(valid,"cbftype","bnry",errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"checked_digest",checked_digest,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"size",(long)size,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"real",realarray,errorcode);
        errorcode |= cbf_apply_h5text_attribute(valid,"byteorder",byteorder,errorcode);
        if (cbf_is_base64digest (digest)) {
            errorcode |= cbf_apply_h5text_attribute(valid,"MD5_digest",digest,errorcode);
        }
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"dimover",(long)dimover,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"dimfast",(long)dimfast,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"dimmid",(long)dimmid,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"dimslow",(long)dimslow,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"padding",(long)padding,errorcode);
        
        
        /* now link the data to entry:NXentry/data:NXdata */
        
        if (h5handle->dataid<0){
            
            /* ensure it goes right below NXentry */
            
            if (h5handle->curnxid>=0) {
                
                cbf_h5reportneg(H5Gclose(h5handle->curnxid),CBF_ARGUMENT,errorcode);
                
                h5handle->curnxid = CBF_H5FAIL;
                
            }
            
            cbf_reportnez(cbf_H5Gcreate_in_handle(h5handle,"data",&(h5handle->dataid)),errorcode);
            
            cbf_failnez(cbf_apply_h5text_attribute(h5handle->dataid,
                                                   "NX_class","NXdata",0));
            
        }
        
        {
            char target_path[1024];
            
            char full_name[1024];
            
            size_t len;
            
            const char * pstr;
            
            strcpy(target_path,"/entry/CBF_cbf/");
            
            full_name[0] = '\0';
            
            len = strlen(target_path)+strlen(rownum)+1;
            
            pstr = (h5handle->bookmark).datablock?
            (h5handle->bookmark).datablock:"_(null)_";
            
            if (len+strlen(pstr)>1020) return CBF_FORMAT;
            
            strncat(target_path,pstr,1020-len);
            
            strcat(target_path,"/");
            
            strncat(full_name,pstr,1020-len);
            
            strcat(full_name,".");
            
            len = strlen(target_path)+strlen(rownum)+1;
            
            pstr = (h5handle->bookmark).category?
            (h5handle->bookmark).category:"_(null)_";
            
            if (len+strlen(pstr)>1021) return CBF_FORMAT;
            
            strncat(target_path,pstr,1021-len);
            
            strcat(target_path,"/");
            
            strncat(full_name,pstr,1020-len);
            
            strcat(full_name,".");
            
            len = strlen(target_path)+strlen(rownum)+1;
            
            pstr = (h5handle->bookmark).column?
            (h5handle->bookmark).column:"_(null)_";
            
            if (len+strlen(pstr)>1022) return CBF_FORMAT;
            
            strncat(target_path,pstr,1022-len);
            
            strcat(target_path,"/");
            
            strncat(full_name,pstr,1020-len);
            
            strcat(full_name,".");
            
            len = strlen(target_path);
            
            if (len+strlen(rownum)>1023) return CBF_FORMAT;
            
            strncat(target_path,rownum,1023-len);
            
            strncat(full_name,rownum,1023-len);
            
            errorcode |= cbf_apply_h5text_attribute(valid,"target",
                                                    target_path,errorcode);
            
            
            cbf_h5reportneg(H5Lcreate_hard(h5handle->colid,rownum,
                                           h5handle->dataid,full_name,H5P_DEFAULT,H5P_DEFAULT),
                            CBF_ARGUMENT,
                            errorcode);
            
        }
        
        if (valid >= 0) {
            
            cbf_h5failneg(H5Dclose(valid),CBF_ARGUMENT);
            
        }
        
        if (valspace >= 0) {
            
            cbf_h5failneg(H5Sclose(valspace),CBF_ARGUMENT);
            
        }
        
        if (valtype >= 0) {
            
            cbf_h5failneg(H5Tclose(valtype),CBF_ARGUMENT);
            
        }
        
        if (valprop >= 0) {
            
            cbf_h5failneg(H5Pclose(valprop),CBF_ARGUMENT);
            
        }
        
        return errorcode;
        
        
        
    }
    
    /* Write an ascii value to an HDF5 file */
    
    int cbf_write_h5ascii (cbf_handle handle,
                           unsigned int row,
                           const char *string,
                           cbf_h5handle h5handle)
    {
        static const char missing [] = { CBF_TOKEN_NULL, '?', '\0' };
        
        
        hid_t valid, valtype, valprop, valspace;
        
        char rownum[10];
        
        char* typecode;
        
        int errorcode;
        
        /* Check the arguments */
        
        if (!handle || !h5handle || h5handle->hfile < 0) return CBF_ARGUMENT;
        
        valid = valtype = valprop = valspace = CBF_H5FAIL;
        
        sprintf(rownum,"%d", row);
        
        if (!string)
            
            string = missing;
        
        else
            
            if (*string != CBF_TOKEN_WORD       &&
                *string != CBF_TOKEN_SQSTRING   &&
                *string != CBF_TOKEN_DQSTRING   &&
                *string != CBF_TOKEN_SCSTRING   &&
                *string != CBF_TOKEN_TSQSTRING  &&
                *string != CBF_TOKEN_TDQSTRING  &&
                *string != CBF_TOKEN_BKTSTRING  &&
                *string != CBF_TOKEN_BRCSTRING  &&
                *string != CBF_TOKEN_PRNSTRING  &&
                *string != CBF_TOKEN_NULL)
                
                return CBF_ARGUMENT;
        
        /* prepare the errorcode */
        
        errorcode = 0;
        
        /* Create a scalar dataspace */
        
        cbf_h5reportneg(valspace = H5Screate(H5S_SCALAR),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(valtype = H5Tcopy(H5T_C_S1),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(H5Tset_size(valtype,strlen(string+1)),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(valprop = H5Pcreate(H5P_DATASET_CREATE),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(valid = H5Dcreatex(h5handle->colid,rownum,
                                           valtype,valspace,valprop),
                        CBF_ALLOC,errorcode);
        
        cbf_h5reportneg(H5Dwrite(valid,valtype,
                                 H5S_ALL,H5S_ALL,H5P_DEFAULT,string+1),
                        CBF_ARGUMENT,errorcode);
        
        errorcode |= cbf_get_value_type(string,(const char **)&typecode);
        
        errorcode |= cbf_apply_h5text_attribute(valid,
                                                "cbftype",typecode,errorcode);
        
        if (valid >= 0) {
            
            cbf_h5failneg(H5Dclose(valid),CBF_ARGUMENT);
            
        }
        
        if (valspace >= 0) {
            
            cbf_h5failneg(H5Sclose(valspace),CBF_ARGUMENT);
            
        }
        
        if (valtype >= 0) {
            
            cbf_h5failneg(H5Tclose(valtype),CBF_ARGUMENT);
            
        }
        
        if (valprop >= 0) {
            
            cbf_h5failneg(H5Pclose(valprop),CBF_ARGUMENT);
            
        }
        
        return errorcode;
        
        
    }
    
    /* Write a value to an HDF5 file */
    
    int cbf_write_h5value (cbf_handle handle, cbf_node *column, unsigned int row,
                           cbf_h5handle h5handle)
    {
        const char *text;
        
        /* Check the arguments */
        
        if (!column || !h5handle || h5handle->hfile < 0)
            
            return CBF_ARGUMENT;
        
        if (row >= column->children)
            
            return CBF_NOTFOUND;
        
        
        /* Get the value */
        
        cbf_failnez (cbf_get_columnrow (&text, column, row))
        
        
        /* Missing value? */
        
        if (!text)
            
            return cbf_write_h5ascii (handle, row, text, h5handle);
        
        
        /* Plain ASCII? */
        
        cbf_failnez (cbf_value_type ((char *) text))
        
        if (*text == CBF_TOKEN_WORD     ||
            *text == CBF_TOKEN_SQSTRING ||
            *text == CBF_TOKEN_DQSTRING ||
            *text == CBF_TOKEN_SCSTRING ||
            *text == CBF_TOKEN_TSQSTRING ||
            *text == CBF_TOKEN_TDQSTRING ||
            *text == CBF_TOKEN_PRNSTRING ||
            *text == CBF_TOKEN_BKTSTRING ||
            *text == CBF_TOKEN_BRCSTRING ||
            *text == CBF_TOKEN_NULL)
            
            return cbf_write_h5ascii (handle, row, text, h5handle);
        
        
        /* Plain binary? */
        
        if (*text == CBF_TOKEN_BIN || *text == CBF_TOKEN_TMP_BIN)
            
            return cbf_write_h5binary (handle, column, row, h5handle);
        
        
        /* Undecoded MIME? */
        
        if (*text == CBF_TOKEN_MIME_BIN)
        {
            /* Convert the value to a normal binary section */
            
            cbf_failnez (cbf_mime_temp (column, row))
            
            return cbf_write_h5binary (handle, column, row, h5handle);
        }
        
        
        /* Fail */
        
        return CBF_ARGUMENT;
    }
    
	/* Write a CBF value into a NeXus file
     Will add a piece of data with a given name to /entry/group@groupNXclass/subGroup@subGroupNXclass/name */
    
	int cbf_map_h5value(
                        const char * const name, const char * const value,
                        const char * const group, const char * const groupNXclass,
                        const char * const subGroup, const char * const subGroupNXclass,
                        const size_t attrc, const cbf_name_value_pair * const attrv,
                        cbf_h5handle h5handle)
	{
		/* check args & define local variables */
		int error = (attrc != 0 && !attrv) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t h5group = CBF_H5FAIL, h5subGroup = CBF_H5FAIL, h5dataset = CBF_H5FAIL, h5type = CBF_H5FAIL;
		const cbf_name_value_pair * attrit = attrv;
        
		/* Get the datatype */
		cbf_reportnez(cbf_H5Tcreate_string(&h5type, strlen(value)), error);
        
		/* Assume nxid is a valid handle, need to ensure that the /entry/instrument group exists, then ensure the detector group exists */
		cbf_reportnez(cbf_require_nxgroup(h5handle, group, groupNXclass, h5handle->nxid, &h5group), error);
		cbf_reportnez(cbf_require_nxgroup(h5handle, subGroup, subGroupNXclass, h5group, &h5subGroup), error);
        
		/* write some data */
		cbf_reportnez(cbf_H5Dcreate(h5subGroup,&h5dataset,name,0,0,0,0,h5type),error);
		cbf_reportnez(cbf_H5Dwrite(h5dataset,0,0,0,value),error);
		for (; attrit < attrv+attrc; ++attrit)
			cbf_reportnez(cbf_H5Arequire_string(h5dataset,attrit->name,attrit->value),error);
        
		/*  clean up */
		if (cbf_H5Ivalid(h5type)) cbf_H5Tfree(h5type);
		if (cbf_H5Ivalid(h5dataset)) cbf_H5Dfree(h5dataset);
		if (cbf_H5Ivalid(h5subGroup)) cbf_H5Gfree(h5subGroup);
		if (cbf_H5Ivalid(h5group)) cbf_H5Gfree(h5group);
		return error;
	}
    
    
    /* Write a category to an HDF5 file */
    
    int cbf_write_h5category (cbf_handle handle,
                              const cbf_node *category,
                              cbf_h5handle h5handle)
    {
		unsigned int column, row;
        
		const char instGroup[] = "instrument";
        
		const char instGroupClass[] = "NXinstrument";
        
        unsigned int colrow;
        
        int errorcode;
        
        errorcode = 0;
        
        /* Check the arguments */
        
        if (!category || !h5handle || h5handle->rootid <0 || h5handle->dbid < 0)
            
            return CBF_ARGUMENT;
        
        
        /* If another category is open, close it */
        
        
        if (h5handle->colid >= 0) {
            
            cbf_h5failneg(H5Gclose(h5handle->colid),CBF_FORMAT);
            
            h5handle->colid = CBF_H5FAIL;
            
        }
        
        if (h5handle->catid >= 0) {
            
            cbf_h5failneg(H5Gclose(h5handle->catid),CBF_FORMAT);
            
            h5handle->catid = CBF_H5FAIL;
            
        }
        
        /* save the category name in the read bookmark */
        
        (h5handle->bookmark).category = category->name;
        
        /* Write the name under the open save frame or datablock */
        
        if (h5handle->sfid <0) {
            
            cbf_h5failneg(h5handle->catid=H5Gcreatex(h5handle->dbid,
                                                     (category->name)?(category->name):"_(null)_"),
                          CBF_FORMAT);
            
            
        } else {
            cbf_h5failneg(h5handle->catid=H5Gcreatex(h5handle->dbid,
                                                     (category->name)?(category->name):"_(null)_"),
                          CBF_FORMAT);
            
            
        }
        
        
		/* Some basic code to ensure the utility of the low-level HDF5 abstraction functions.
         This will be striped out & refactored when a suitable higher-level framework is implemented. */
		cbf_debug_print(category->name);
		if (!cbf_cistrcmp(category->name,"diffrn_detector"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) fprintf(stderr,"CBFlib: warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!cbf_cistrcmp(column_node->name,"type")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("type", text+1, instGroup, instGroupClass, "detector", "NXdetector", 0, 0,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector.type could not be written to nexus file");
				}
				if(!cbf_cistrcmp(column_node->name,"details")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("description", text+1, instGroup, instGroupClass, "detector", "NXdetector", 0, 0,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector.details could not be written to nexus file");
				}
				if(!cbf_cistrcmp(column_node->name,"detector")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("local_name", text+1, instGroup, instGroupClass, "detector", "NXdetector", 0, 0,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector.detector could not be written to nexus file");
				}
				if(!cbf_cistrcmp(column_node->name,"dtime")) {
					cbf_name_value_pair attr = {"units","us"};
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("dead_time", text+1, instGroup, instGroupClass, "detector", "NXdetector", 1, &attr,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector.dtime could not be written to nexus file");
				}
			}
		}
		if (!cbf_cistrcmp(category->name,"diffrn_detector_element"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) fprintf(stderr, "CBFlib: warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!cbf_cistrcmp(column_node->name,"center[1]")){
					cbf_name_value_pair attr = {"units","mm"};
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					/* TODO: give the correct value here */
					cbf_reportnez(cbf_map_h5value("beam_center_x", text+1, instGroup, instGroupClass, "detector", "NXdetector", 1, &attr,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector_element.center[1] could not be written to nexus file");
				}
				if(!cbf_cistrcmp(column_node->name,"center[2]")){
					cbf_name_value_pair attr = {"units","mm"};
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					/* TODO: give the correct value here */
					cbf_reportnez(cbf_map_h5value("beam_center_y", text+1, instGroup, instGroupClass, "detector", "NXdetector", 1, &attr,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector_element.center[2] could not be written to nexus file");
				}
			}
		}
		if (!cbf_cistrcmp(category->name,"diffrn_radiation"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) fprintf(stderr, "CBFlib: warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!cbf_cistrcmp(column_node->name,"div_x_source")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("divergence_x", text+1, instGroup, instGroupClass, "collimator", "NXcollimator", 0, 0,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation.div_x_source could not be written to nexus file");
				}
				if(!cbf_cistrcmp(column_node->name,"div_y_source")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("divergence_y", text+1, instGroup, instGroupClass, "collimator", "NXcollimator", 0, 0,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation.div_y_source could not be written to nexus file");
				}
			}
		}
		if (!cbf_cistrcmp(category->name,"diffrn_radiation_wavelength"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) fprintf(stderr, "CBFlib:  warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!cbf_cistrcmp(column_node->name,"wavelength")){
					cbf_name_value_pair attr = {"units","Angstroms"};
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("wavelength", text+1, instGroup, instGroupClass, "monochromator", "NXmonochromator", 1,
                                                  &attr, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation_wavelength.wavelength could not be written to nexus file");
				}
				if(!cbf_cistrcmp(column_node->name,"wt")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("weight", text+1, instGroup, instGroupClass, "monochromator", "NXmonochromator", 0, 0,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation_wavelength.wt could not be written to nexus file");
				}
			}
		}
        
        
        cbf_failnez(cbf_apply_h5text_attribute(h5handle->catid,
                                               "NX_class","CBF_cbfcat",errorcode));
        
        
        /* now, for each column, make it into a group and
         store each row as a dataset */
        
        
        for (column= 0; column < category->children; column++)
        {
            /* save the column name in the read bookmark */
            
            (h5handle->bookmark).column = (category->child[column])->name;
            
            
            cbf_h5failneg(h5handle->colid=H5Gcreatex(h5handle->catid,
                                                     (category->child[column])->name?
                                                     (category->child[column])->name:"_(null)_"),
                          CBF_FORMAT);
            
            cbf_failnez(cbf_apply_h5text_attribute(h5handle->colid,
                                                   "NX_class","CBF_cbfcol",errorcode));
            
            /* For each row, create a dataset */
            
            for (row=0; row < category->child [column]->children; row++)
            {
                
                (h5handle->bookmark).row = row;
                
                cbf_failnez(cbf_write_h5value(handle,
                                              category->child [column],
                                              row,h5handle));
            }
            
            cbf_h5failneg(H5Gclose(h5handle->colid),CBF_ARGUMENT);
            
            h5handle->colid = CBF_H5FAIL;
        }
        
        
        if (!cbf_cistrcmp(category->name,"diffrn_scan")) {
            
            cbf_node * column_node;
            
            const char * text = 0;
            
            for (column= 0; column < category->children; column++) {
                
                column_node = category->child[column];
                
                if(!cbf_cistrcmp(column_node->name,"id")){
                    
                    if (column_node->children > 1 ) {
                        
                        for (colrow=0; colrow < column_node->children; colrow++) {
                            
                            if(cbf_get_columnrow (&text, column_node, colrow)) {
                                
                                text = " .";
                                
                            }
                            
                            if (cbf_apply_h5text_dataset_slab(h5handle->nxid,
                                                              "CBF_scan_id",
                                                              text+1,
                                                              colrow,errorcode)) break;
                            
                        }
                        
                    } else {
                        
                        
                        if(cbf_get_columnrow (&text, column_node, 0)) break;
                        
                        if (!text) break;
                        
                        if(cbf_apply_h5text_dataset(h5handle->nxid,
                                                    "CBF_scan_id",text+1,errorcode)) break;
                        
                        break;
                        
                    }
                    
                }
            }
        }
        
        if (!cbf_cistrcmp(category->name,"entry")) {
            
            cbf_node * column_node;
            
            const char * text = 0;
            
            for (column= 0; column < category->children; column++) {
                
                column_node = category->child[column];
                
                if(!cbf_cistrcmp(column_node->name,"id")){
                    
                    if (column_node->children > 1 ) {
                        
                        
                        for (colrow=0; colrow < column_node->children; colrow++) {
                            
                            if(cbf_get_columnrow (&text, column_node, colrow)) {
                                
                                text = " .";
                                
                            }
                            
                            if (cbf_apply_h5text_dataset_slab(h5handle->nxid,
                                                              "CBF_entry_id",
                                                              text+1,
                                                              colrow,errorcode)) break;
                            
                        }
                        
                    } else {
                        
                        
                        if(cbf_get_columnrow (&text, column_node, 0)) break;
                        
                        if (!text) break;
                        
                        if(cbf_apply_h5text_dataset(h5handle->nxid,
                                                    "CBF_entry_id",text+1,errorcode)) break;
                        
                        break;
                        
                    }
                    
                }
            }
        }
        
        if (!cbf_cistrcmp(category->name,"diffrn")) {
            
            cbf_node * column_node;
            
            const char * text = 0;
            
            for (column= 0; column < category->children; column++) {
                
                column_node = category->child[column];
                
                if(!cbf_cistrcmp(column_node->name,"id")){
                    
                    if (column_node->children > 1 ) {
                        
                        for (colrow=0; colrow < column_node->children; colrow++) {
                            
                            if(cbf_get_columnrow (&text, column_node, colrow)) {
                                
                                text = " .";
                                
                            }
                            
                            if (cbf_apply_h5text_dataset_slab(h5handle->nxid,
                                                              "CBF_diffrn_id",
                                                              text+1,
                                                              colrow, errorcode)) break;
                            
                        }
                        
                    } else {
                        
                        
                        if(cbf_get_columnrow (&text, column_node, 0)) break;
                        
                        if (!text) break;
                        
                        if(cbf_apply_h5text_dataset(h5handle->nxid,
                                                    "CBF_diffrn_id",text+1,errorcode)) break;
                        
                        break;
                        
                    }
                    
                }
            }
        }
        
        
        /* Success */
        
        return CBF_SUCCESS;
        
    }
    
    
    
    
    /*  create top-level NXentry */
    
    int cbf_create_NXentry(cbf_h5handle h5handle)
    {
        
        if (!h5handle ||
            h5handle->nxid >= 0 ||
            h5handle->hfile < 0) return CBF_ARGUMENT;
        
        cbf_h5failneg(h5handle->nxid=H5Gcreatex(h5handle->hfile,
                                                (const char *)"entry"),
                      CBF_ARGUMENT);
        
        
        cbf_failnez(cbf_apply_h5text_attribute(h5handle->nxid,
                                               "NX_class","NXentry",0));
        
        h5handle->curnxid=CBF_H5FAIL;
        
        h5handle->dataid =CBF_H5FAIL;
        
        return CBF_SUCCESS;
        
    }
    
    
    /*  Create an HDF5 Group below NX entry or below curnxid */
    
    int cbf_H5Gcreate_in_handle(cbf_h5handle h5handle,
                                const char * groupname,
                                hid_t * newgroup)
    {
        hid_t parent;
        
        if (!h5handle) return CBF_ARGUMENT;
        
        if (h5handle->nxid < 0) {
            
            cbf_failnez(cbf_create_NXentry(h5handle));
            
        }
        
        parent = (h5handle->curnxid >= 0)? h5handle->curnxid: h5handle->nxid;
        
        cbf_h5failneg(*newgroup=H5Gcreatex(parent,groupname),
                      CBF_FORMAT);
        
        return CBF_SUCCESS;
        
    }
    
    /*  Create an HDF5 NeXus Group below NX entry or below curnxid */
    
    int cbf_H5NXGcreate(cbf_h5handle h5handle,
                        const char * groupname,
                        const char * nxclass,
                        hid_t * newgroup )
    {
        cbf_failnez(cbf_H5Gcreate_in_handle(h5handle, groupname, newgroup));
        
        cbf_failnez(cbf_apply_h5text_attribute(*newgroup,
                                               "NX_class",nxclass,0));
        
        return CBF_SUCCESS;
        
    }
    
    
    /* Free an H5File handle */
    
    int cbf_free_h5handle(cbf_h5handle h5handle)
	{
        
        void * memblock = (void *) h5handle;
        
        if (h5handle->colid >= 0) {
            cbf_h5onfailneg(H5Gclose(h5handle->colid),CBF_UNDEFINED,cbf_free(&memblock,NULL));
        }
        
        if (h5handle->catid >= 0) {
            cbf_h5onfailneg(H5Gclose(h5handle->catid),CBF_UNDEFINED,cbf_free(&memblock,NULL));
        }
        
        if (h5handle->sfid >= 0) {
            cbf_h5onfailneg(H5Gclose(h5handle->sfid),CBF_UNDEFINED,cbf_free(&memblock,NULL));
        }
        
        if (h5handle->dbid >= 0) {
            cbf_h5onfailneg(H5Gclose(h5handle->dbid),CBF_UNDEFINED,cbf_free(&memblock,NULL));
        }
        
        if (h5handle->rootid >= 0) {
            cbf_h5onfailneg(H5Gclose(h5handle->rootid),CBF_UNDEFINED,cbf_free(&memblock,NULL));
        }
        
        if (h5handle->curnxid >= 0) {
            cbf_h5onfailneg(H5Gclose(h5handle->curnxid),CBF_UNDEFINED,cbf_free(&memblock,NULL));
        }
        
        if (h5handle->dataid >= 0) {
            cbf_h5onfailneg(H5Gclose(h5handle->dataid),CBF_UNDEFINED,cbf_free(&memblock,NULL));
        }
        
        if (h5handle->nxid >= 0) {
            cbf_h5onfailneg(H5Gclose(h5handle->nxid),CBF_UNDEFINED,cbf_free(&memblock,NULL));
		}
        
		if (h5handle->nxdata >= 0) {
			cbf_h5onfailneg(H5Gclose(h5handle->nxdata),CBF_UNDEFINED,cbf_free(&memblock,NULL));
		}
        
		if (h5handle->nxinst >= 0) {
			cbf_h5onfailneg(H5Gclose(h5handle->nxinst),CBF_UNDEFINED,cbf_free(&memblock,NULL));
		}
        
		if (h5handle->nxsample >= 0) {
			cbf_h5onfailneg(H5Gclose(h5handle->nxsample),CBF_UNDEFINED,cbf_free(&memblock,NULL));
		}
        
		if (h5handle->nxdetector >= 0) {
			cbf_h5onfailneg(H5Gclose(h5handle->nxdetector),CBF_UNDEFINED,cbf_free(&memblock,NULL));
		}
        
		if (h5handle->nxmonochromator >= 0) {
			cbf_h5onfailneg(H5Gclose(h5handle->nxmonochromator),CBF_UNDEFINED,cbf_free(&memblock,NULL));
		}
        
        if (h5handle->hfile >= 0) {
            cbf_h5onfailneg(H5Fclose(h5handle->hfile),CBF_FILECLOSE,cbf_free(&memblock,NULL));
        }
        
        return cbf_free(&memblock,NULL);
    }
    
    /* Make an (empty) H5File handle */
    
    int cbf_make_h5handle(cbf_h5handle *h5handle) {
        
        cbf_failnez (cbf_alloc ((void **) h5handle, NULL,
                                sizeof(cbf_h5handle_struct), 1));
        
        (*h5handle)->hfile   = (hid_t)CBF_H5FAIL;
        (*h5handle)->rootid  = (hid_t)CBF_H5FAIL;
        (*h5handle)->dbid    = (hid_t)CBF_H5FAIL;
        (*h5handle)->sfid    = (hid_t)CBF_H5FAIL;
        (*h5handle)->catid   = (hid_t)CBF_H5FAIL;
		(*h5handle)->colid   = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxid    = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxdata  = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxinst  = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxsample  = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxdetector  = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxmonochromator = (hid_t)CBF_H5FAIL;
        (*h5handle)->curnxid = (hid_t)CBF_H5FAIL;
        (*h5handle)->dataid  = (hid_t)CBF_H5FAIL;
        (*h5handle)->rwmode  = 0;
        (*h5handle)->flags = 0;
        
        return CBF_SUCCESS;
        
    }
    
    /* Close the current saveframe in an HDF5 file */
    
    int cbf_close_h5saveframe (cbf_h5handle h5handle)
    {
        
        /* Does the node exist? */
        
        if (!h5handle || h5handle->rootid <0 || h5handle->dbid <0)
            
            return CBF_ARGUMENT;
        
        if (h5handle->colid >= 0) {
            
            cbf_h5failneg(H5Gclose(h5handle->colid),CBF_FORMAT);
            
            h5handle->colid = (hid_t)-1;
            
        }
        
        if (h5handle->catid >= 0) {
            
            cbf_h5failneg(H5Gclose(h5handle->catid),CBF_FORMAT);
            
            h5handle->catid = (hid_t)-1;
            
        }
        
        if (h5handle->sfid >= 0) {
            
            cbf_h5failneg(H5Gclose(h5handle->sfid),CBF_FORMAT);
            
            h5handle->sfid = (hid_t)-1;
            
        }
        
        return CBF_SUCCESS;
    }
    
    
    /* Write a saveframe name to an HDF5 file
     Make a new group of NeXus class CBF_cbfsf
     in the CBF_cbf current datablock
     */
    
    int cbf_write_h5saveframename (const cbf_node *saveframe,
                                   cbf_h5handle h5handle)
    {
        
        /* Does the node exist? */
        
        if (!saveframe || !h5handle || h5handle->rootid <0 || h5handle->dbid <0)
            
            return CBF_ARGUMENT;
        
        /* If another saveframe is open, close all its children
         and the saveframe itself */
        
        if (h5handle->sfid >= 0) {
            
            cbf_failnez(cbf_close_h5saveframe(h5handle));
            
        }
        
        
        /* Write the name */
        
        cbf_h5failneg(h5handle->sfid=H5Gcreatex(h5handle->dbid,
                                                saveframe->name),
                      CBF_FORMAT);
        
        cbf_failnez(cbf_apply_h5text_attribute(h5handle->sfid,
                                               "NX_class", "CBF_cbfsf",0));
        
        return CBF_SUCCESS;
    }
    
    
    
    
    /* Write a datablock name to an HDF5 file
     Make a new group of NeXus class CBF_cbfdb in the CBF_cbf class root
     */
    
    int cbf_write_h5datablockname (const cbf_node *datablock, cbf_h5handle h5handle)
    {
        
        /* Does the node exist? */
        
        if (!datablock || !h5handle || h5handle->rootid <0)
            
            return CBF_ARGUMENT;
        
        /* If another datablock is open, close all its children
         and the datablock itself */
        
        if (h5handle->colid >= 0) {
            
            cbf_h5failneg(H5Gclose(h5handle->colid),CBF_FORMAT);
            
            h5handle->colid = (hid_t)-1;
            
        }
        
        if (h5handle->catid >= 0) {
            
            cbf_h5failneg(H5Gclose(h5handle->catid),CBF_FORMAT);
            
            h5handle->catid = (hid_t)-1;
            
        }
        
        if (h5handle->sfid >= 0) {
            
            cbf_h5failneg(H5Gclose(h5handle->sfid),CBF_FORMAT);
            
            h5handle->sfid = (hid_t)-1;
            
        }
        
        if (h5handle->dbid >= 0) {
            
            cbf_h5failneg(H5Gclose(h5handle->dbid),CBF_FORMAT);
            
            h5handle->dbid = (hid_t)-1;
            
        }
        
        /* save the datablock name in the read bookmark */
        
        (h5handle->bookmark).datablock = datablock->name;
        
        
        /* Write the name */
        
        cbf_h5failneg(h5handle->dbid=H5Gcreatex(h5handle->rootid,
                                                datablock->name),
                      CBF_FORMAT);
        
        cbf_failnez(cbf_apply_h5text_attribute(h5handle->dbid,
                                               "NX_class", "CBF_cbfdb",0));
        
        return CBF_SUCCESS;
    }
    
    
    /* Write a node to an HDF5 file */
    
    int cbf_write_h5node (cbf_handle handle, const cbf_node *node,
                          const cbf_h5handle h5handle)
    {
        unsigned int count;
        
        
        /* Follow any links */
        
        node = cbf_get_link (node);
        
        
        /* Does the node exist? */
        
        if (!node)
            
            return CBF_ARGUMENT;
        
        /* Node type */
        
        switch (node->type)
        {
                
                
                /* For the root, start the file with a CBF group */
                
            case CBF_ROOT:
                
                break;
                
            case CBF_DATABLOCK:
                
                if (h5handle->rootid < 0) return CBF_FORMAT;
                
                cbf_failnez (cbf_write_h5datablockname (node, h5handle))
                
                break;
                
            case CBF_CATEGORY:
                
                cbf_failnez (cbf_write_h5category (handle, node, h5handle))
                
                break;
                
            case CBF_SAVEFRAME:
                
                cbf_failnez (cbf_write_h5saveframename (node, h5handle))
                
                break;
                
                
            default:
                
                return CBF_ARGUMENT;
        }
        
        
        /* Write the children */
        
        if (node->type == CBF_ROOT || node->type == CBF_DATABLOCK || node->type == CBF_SAVEFRAME)
            
            for (count = 0; count < node->children; count++)
            {
                
                cbf_failnez (cbf_write_h5node (handle, node->child [count], h5handle))
                
            }
        
        if (node->type == CBF_SAVEFRAME) {
            
            cbf_failnez(cbf_close_h5saveframe(h5handle));
            
        }
        
        
        /* Flush the buffers */
        
        cbf_h5failneg(H5Fflush(h5handle->hfile,H5F_SCOPE_LOCAL),CBF_ARGUMENT);
        
        return CBF_SUCCESS;
        
        
    }
    
    
    
    /* Create an HDF5 File handle */
    
    int cbf_create_h5handle(cbf_h5handle *h5handle,const char * h5filename)
	{
        hid_t fcreate_prop_list;
        
        cbf_failnez(cbf_make_h5handle(h5handle));
        
        cbf_h5onfailneg(fcreate_prop_list = H5Pcreate(H5P_FILE_ACCESS),
                        CBF_ALLOC,cbf_free((void**) h5handle, NULL));
        
        (*h5handle)->rwmode = 1;
        
        cbf_h5onfailneg(H5Pset_fclose_degree(fcreate_prop_list,H5F_CLOSE_STRONG),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
        cbf_h5onfailneg((*h5handle)->hfile = H5Fcreate(h5filename,H5F_ACC_TRUNC,
                                                       H5P_DEFAULT,fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
        cbf_h5onfailneg(H5Pclose(fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
        cbf_onfailnez(cbf_H5Gcreate_in_handle(*h5handle,"CBF_cbf",
                                              &((*h5handle)->rootid)),
                      cbf_free_h5handle(*h5handle));
        
        cbf_failnez(cbf_apply_h5text_attribute((*h5handle)->rootid,"NX_class",
                                               "CBF_cbf",0));
        
        
        return CBF_SUCCESS;
        
	}
    
	/* Create an HDF5 File handle without adding an CBF_cbf group to it */
    
	int cbf_create_h5handle2(cbf_h5handle *h5handle,const char * h5filename)
	{
		hid_t fcreate_prop_list;
        
		cbf_failnez(cbf_make_h5handle(h5handle));
        
		cbf_h5onfailneg(fcreate_prop_list = H5Pcreate(H5P_FILE_ACCESS),
                        CBF_ALLOC,cbf_free((void**) h5handle, NULL));
        
		(*h5handle)->rwmode = 1;
        
		cbf_h5onfailneg(H5Pset_fclose_degree(fcreate_prop_list,H5F_CLOSE_STRONG),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
		cbf_h5onfailneg((*h5handle)->hfile = H5Fcreate(h5filename,H5F_ACC_TRUNC,
                                                       H5P_DEFAULT,fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
		cbf_h5onfailneg(H5Pclose(fcreate_prop_list), CBF_ARGUMENT,
                        cbf_free((void**) h5handle, NULL));
        
		return CBF_SUCCESS;
	}
    
    
    /*  Write cbf to HDF5 file hfile */
    
    int cbf_write_h5file (cbf_handle handle, cbf_h5handle h5handle, int flags)
    {
		cbf_node *node = NULL;
        
        int errorcode = CBF_SUCCESS;
        
        if (!handle || !h5handle)
            
            return CBF_ARGUMENT;
        
        /* Transfer the flags into h5handle */
        
        h5handle->flags = flags;
        
        /* Find the root node */
        
		cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT));
        
        /* Reset the reference counts */
        
		cbf_failnez( cbf_reset_refcounts(handle->dictionary) );
        
		/* ensure the handle contains some basic structure */
        
		cbf_reportnez(cbf_h5handle_require_entry(h5handle,0,"entry"), errorcode);
		cbf_reportnez(cbf_h5handle_require_instrument(h5handle,0), errorcode);
        
		/* Do the mappings from CBF to nexus */
        
		/* Write the CBF data into the file in a special node to keep
         it separate from actual mappings */
        
        errorcode = cbf_write_h5node (handle, node, h5handle);
        
        if (!errorcode) {
            
            cbf_write_h5nxaxes(handle, h5handle);
            
        }
        
        return errorcode;
        
	}
    
    
    /*
     Check if \token\ matches a given string, allowing optional expressions
     \t\ & \f\ to be executed if it does or doesn't match, respecitvely.
     \t\, \f\ may be empty or a semicolon, ie:
     CBF_CHECK_TOKEN(str, ,;);
     If \token\ doesn't match \str\ then set \error\ to \CBF_H5DIFFERENT\ and print a message.
     */
#define CBF_CHECK_TOKEN(str, t, f) \
{ \
if (cbf_cistrcmp(str,token)) { \
error |= CBF_H5DIFFERENT; \
fprintf(stderr,"%s: error: unexpected token '%s'\n",__WHERE__,token); \
{f;} \
} else {t;} \
};
    
    
    
	/*
     Assuming I have a minicbf:
     extract the data from it;
     convert the header information to nexus classes;
     write all in nexus format to given entry group.
     
     TODO:
	 -	rewrite pilatus header parsing function to return null-terminated string tokens,
     instead of an index into a very long string.
	 -	redo conditional metadata extraction to check for NaNs in the data they require,
     simplifying the control structures, or just write NaNs to the file
	 -	write a function to put a slice of data into a dataset, possibly creating the dataset,
     to remove a very common pattern from the code.
     */
    
	int cbf_write_minicbf_h5file (cbf_handle handle, cbf_h5handle h5handle, cbf_hdf5_configItemVectorhandle axisConfig, int flags)
	{
		cbf_node *node = NULL;
		int errorcode = CBF_SUCCESS;
        
		hid_t detector = CBF_H5FAIL, instrument = CBF_H5FAIL; /* do not free */
        
		if (!handle || !h5handle) return CBF_ARGUMENT;
        
		/* Transfer the flags into h5handle */
		h5handle->flags = flags;
        
		/* Find the root node */
		cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT));
        
		/* Reset the reference counts */
		cbf_failnez( cbf_reset_refcounts(handle->dictionary) );
        
		/* ensure the handle contains some basic structure */
		cbf_reportnez(cbf_h5handle_require_instrument(h5handle,&instrument), errorcode);
		cbf_reportnez(cbf_h5handle_require_detector(h5handle,&detector), errorcode);
        
		/* Do the mappings from CBF to nexus */
		{
			/* get some useful parameters out of the metadata as it's converted */
			double pixel_x = 0./0., pixel_y = 0./0.;
            
			/* assume I have only 1 datablock, TODO: fix this */
			cbf_onfailnez(cbf_select_datablock(handle,0), fprintf(stderr,__WHERE__": CBF error: cannot find datablock 0.\n"));
			/* then search for the 'array_data' category */
			cbf_onfailnez(cbf_find_category(handle,"array_data"),
						  fprintf(stderr,__WHERE__": CBF error: cannot find category 'array_data'.\n"));
            
			/* First: extract the metadata from the CBF, put it in nexus */
			cbf_failnez(cbf_find_column(handle,"header_convention"));
			if (1) { /* get the header convention, check it is a value I understand */
				const char * value = NULL;
				const char vendor_pilatus[] = "PILATUS";
				cbf_failnez(cbf_get_value(handle,&value));
				if (0 == strncmp(value,vendor_pilatus,strlen(vendor_pilatus))) {
					const char version_1_2[] = "1.2";
					value += strlen(vendor_pilatus) + 1;
					if (0 == strncmp(value,version_1_2,strlen(version_1_2))) {
						/* define tokenisation variables, with default buffer sized to almost always be big enough */
						size_t n = 128;
						char * token = malloc(n*sizeof(char));
						int newline = 1;
						/* Numerical values for use after main parsing loop */
						double beam_x = 0./0., beam_y = 0./0.;
						double detector_distance = 0./0.;
						/* Flags to determine what information I actually have */
						/* Other useful values */
						hid_t pilatusDiagnostics = CBF_H5FAIL; /* <- non-nexus group to dump some possibly useful information into */
						/* Get the header data */
						
						cbf_onfailnez(cbf_find_column(handle,"header_contents"),
									  fprintf(stderr,__WHERE__": 'header_contents' not found.\n"));
						/* re-use the 'value' variable, I won't need the old value anymore */
						cbf_onfailnez(cbf_get_value(handle,&value), fprintf(stderr,__WHERE__": 'header_contents' inaccessible.\n"));
                        cbf_H5Drequire_string(detector,0,"type","pixel array");
                        
						/*
                         Do the mapping, iterating over each line of the header.
                         The entire header can be parsed using a trivial FSA, so don't bother with anything particularly complex.
                         */
						do {
							int noMatch = 0;
							{ /* Get the first token */
								int error = CBF_SUCCESS;
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (!token) break;
								if (!strcmp("\n",token)) continue;
							}
                            
							/* check for a time */
							if (cbf_isPilatusDate(token)) {
								int error = CBF_SUCCESS;
								/*
                                 Put the time string into the hdf5 file.
                                 TODO: deal with timestamps in a better way,
                                 ie select the earliest to put in the proper nexus field & store all of them in an array
                                 */
								hid_t type = CBF_H5FAIL;
								hid_t dataset = CBF_H5FAIL;
                                cbf_reportnez(cbf_H5Tcreate_string(&type,strlen(token)),error);
                                cbf_reportnez(cbf_H5Dfind(h5handle->nxid,&dataset,"start_time",0,0,0,0,type),error);
                                if (CBF_SUCCESS==error) {
									if (!cbf_H5Ivalid(dataset)) {
										/* create the dataset & write the data */
										cbf_reportnez(cbf_H5Dcreate(h5handle->nxid,&dataset,"start_time",0,0,0,0,type),error);
										cbf_reportnez(cbf_H5Dwrite(dataset,0,0,0,token),error);
									} else {
										/* TODO: comparison with existing data, to extract earliest time */
									}
								}
								cbf_H5Dfree(dataset);
                                cbf_H5Tfree(type);
							} else if (!cbf_cistrcmp("Pixel_size",token)) {
                                int error = CBF_SUCCESS;
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								{ /* Get x pixel size */
									hid_t h5data = CBF_H5FAIL;
									const double num = strtod(token,0);
									cbf_reportnez(cbf_H5Drequire_scalar_F64LE(detector,&h5data,"x_pixel_size",num),error);
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									CBF_CHECK_TOKEN("m",pixel_x = num,;);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
									cbf_H5Dfree(h5data);
                                }
								/* Get next useful token, just skip over useless stuff */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								{ /* Get y pixel size */
									hid_t h5data = CBF_H5FAIL;
                                    const double num = strtod(token,0);
									cbf_reportnez(cbf_H5Drequire_scalar_F64LE(detector,&h5data,"y_pixel_size",num),error);
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									CBF_CHECK_TOKEN("m",pixel_y = num,;);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
									cbf_H5Dfree(h5data);
								}
							} else if (!cbf_cistrcmp("Silicon",token)) {
								int error = CBF_SUCCESS;
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (!cbf_cistrcmp("sensor",token)) {
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									if (!cbf_cistrcmp("thickness",token)) {
										cbf_reportnez(cbf_H5Drequire_string(detector,0,"sensor_material","Silicon"),error);
										cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
                                        {
											hid_t h5data = CBF_H5FAIL;
											double num = strtod(token,0);
											cbf_reportnez(cbf_H5Drequire_scalar_F64LE(detector,&h5data,"sensor_thickness",
                                                                                      num),error);
											cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
											CBF_CHECK_TOKEN("m",;,;);
											cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
											cbf_H5Dfree(h5data);
								        }
                                    } else noMatch = 1;
								} else noMatch = 1;
							} else if (!cbf_cistrcmp("Detector_distance",token)) {
								int error = CBF_SUCCESS;
								double num = 0./0.;
								hid_t h5data = CBF_H5FAIL;
								/* Get value & units */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								num = strtod(token,0);
								cbf_reportnez(cbf_H5Drequire_scalar_F64LE(detector,&h5data,"distance",
                                                                          num),error);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								CBF_CHECK_TOKEN("m",detector_distance=num,;);
								cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
								cbf_H5Dfree(h5data);
							} else if (!cbf_cistrcmp("Detector",token)) {
								int error = CBF_SUCCESS;
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value),error);
								cbf_reportnez(cbf_H5Drequire_string(detector,0,"description",token),error);
							} else if (!cbf_cistrcmp("N_excluded_pixels",token)) {
                                int error = CBF_SUCCESS;
                                hid_t h5location = CBF_H5FAIL;
								cbf_reportnez(cbf_H5Grequire(&h5location,"pilatus_diagnostics",detector),error);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value),error);
								cbf_reportnez(cbf_H5Drequire_string(h5location,0,"N_excluded_pixels",token),error);
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Excluded_pixels",token)) {
                                int error = CBF_SUCCESS;
                                hid_t h5location = CBF_H5FAIL;
                                /* get the dignostics group */
                                cbf_reportnez(cbf_H5Grequire(&h5location,"pilatus_diagnostics",detector),error);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value),error);
								cbf_reportnez(cbf_H5Drequire_string(h5location,0,"Excluded_pixels",token),error);
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Flat_field",token)) {
								int error = CBF_SUCCESS;
								hid_t h5location = CBF_H5FAIL;
								cbf_reportnez(cbf_H5Grequire(&h5location,"pilatus_diagnostics",detector),error);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value),error);
								cbf_reportnez(cbf_H5Drequire_string(h5location,0,"Flat_field",token),error);
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Trim_file",token)) {
								int error = CBF_SUCCESS;
								hid_t h5location = CBF_H5FAIL;
								cbf_reportnez(cbf_H5Grequire(&h5location,"pilatus_diagnostics",detector),error);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value),error);
								cbf_reportnez(cbf_H5Drequire_string(h5location,0,"Trim_file",token),error);
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Exposure_time",token)) {
								int error = CBF_SUCCESS;
								hid_t h5data = CBF_H5FAIL;
								hid_t h5location = detector;
								const hid_t h5type = H5T_IEEE_F64LE;
								const char h5name[] = "count_time";
								const hsize_t dim[] = {h5handle->slice};
								const hsize_t max[] = {H5S_UNLIMITED};
								const hsize_t chunk[] = {1};
								const hsize_t offset[] = {h5handle->slice};
								const hsize_t count[] = {1};
								/* 1: value */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								cbf_reportnez(cbf_H5Dfind(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
								if (CBF_SUCCESS==error) {
									const double num = strtod(token,0);
									const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
									if (!cbf_H5Ivalid(h5data)) {
										cbf_reportnez(cbf_H5Dcreate(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
									}
									cbf_reportnez(cbf_H5Dset_extent(h5data,dim2),error);
									cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									/* 2: units */
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								} else {
									fprintf(stderr,"Attempt to determine existence of nexus dataset '%s' failed\n",h5name);
								}
							} else if (!cbf_cistrcmp("Exposure_period",token)) {
								int error = CBF_SUCCESS;
								hid_t h5data = CBF_H5FAIL;
								hid_t h5location = detector;
								const hid_t h5type = H5T_IEEE_F64LE;
								const char h5name[] = "frame_time";
								const hsize_t dim[] = {h5handle->slice};
								const hsize_t max[] = {H5S_UNLIMITED};
								const hsize_t chunk[] = {1};
								const hsize_t offset[] = {h5handle->slice};
								const hsize_t count[] = {1};
								/* 1: value */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								cbf_reportnez(cbf_H5Dfind(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
								if (CBF_SUCCESS==error) {
									const double num = strtod(token,0);
									const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
									if (!cbf_H5Ivalid(h5data)) {
										cbf_reportnez(cbf_H5Dcreate(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
									}
									cbf_reportnez(cbf_H5Dset_extent(h5data,dim2),error);
									cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									/* 2: units */
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								} else {
									fprintf(stderr,"Attempt to determine existence of nexus dataset '%s' failed\n",h5name);
								}
							} else if (!cbf_cistrcmp("Tau",token)) {
								int error = CBF_SUCCESS;
								hid_t h5data = CBF_H5FAIL;
								hid_t h5location = detector;
								const hid_t h5type = H5T_IEEE_F64LE;
								const char h5name[] = "dead_time";
								const hsize_t dim[] = {h5handle->slice};
								const hsize_t max[] = {H5S_UNLIMITED};
								const hsize_t chunk[] = {1};
								const hsize_t offset[] = {h5handle->slice};
								const hsize_t count[] = {1};
								/* 1: value */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								cbf_reportnez(cbf_H5Dfind(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
								if (CBF_SUCCESS==error) {
									const double num = strtod(token,0);
									const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
									if (!cbf_H5Ivalid(h5data)) {
										cbf_reportnez(cbf_H5Dcreate(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
									}
									cbf_reportnez(cbf_H5Dset_extent(h5data,dim2),error);
									cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									/* 2: units */
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								} else {
									fprintf(stderr,"Attempt to determine existence of nexus dataset '%s' failed\n",h5name);
								}
							} else if (!cbf_cistrcmp("Count_cutoff",token)) {
								int error = CBF_SUCCESS;
								/* Get value & units */
								hid_t h5data = CBF_H5FAIL;
								hid_t h5location = detector;
								const hid_t h5type = H5T_STD_I32LE;
								const char h5name[] = "saturation_value";
								const hsize_t dim[] = {h5handle->slice};
								const hsize_t max[] = {H5S_UNLIMITED};
								const hsize_t chunk[] = {1};
								const hsize_t offset[] = {h5handle->slice};
								const hsize_t count[] = {1};
								/* 1: value */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								cbf_reportnez(cbf_H5Dfind(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
								if (CBF_SUCCESS==error) {
									const int num = strtol(token,0,10);
									const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
									if (!cbf_H5Ivalid(h5data)) {
										cbf_reportnez(cbf_H5Dcreate(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
									}
									cbf_reportnez(cbf_H5Dset_extent(h5data,dim2),error);
									cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									/* 2: units */
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								} else {
									fprintf(stderr,"Attempt to determine existence of nexus dataset '%s' failed\n",h5name);
								}
							} else if (!cbf_cistrcmp("Threshold_setting",token)) {
								int error = CBF_SUCCESS;
								double num = 0./0.;
								hid_t h5data = CBF_H5FAIL;
								/* Get value & units */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								num = strtod(token,0);
								cbf_reportnez(cbf_H5Drequire_scalar_F64LE(detector,&h5data,"threshold_energy",
                                                                          num),error);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
								cbf_H5Dfree(h5data);
							} else if (!cbf_cistrcmp("Gain_setting",token)) {
								int error = CBF_SUCCESS;
								/* [1,end): gain setting string */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value),error);
								cbf_reportnez(cbf_H5Drequire_string(detector,0,"gain_setting",token),error);
							} else if (!cbf_cistrcmp("Wavelength",token)) {
								int error = CBF_SUCCESS;
								/* Get value & units */
								hid_t h5data = CBF_H5FAIL;
								hid_t h5location = CBF_H5FAIL; /* DO NOT FREE THIS! */
								const hid_t h5type = H5T_IEEE_F64LE;
								const char h5name[] = "wavelength";
								const hsize_t dim[] = {h5handle->slice};
								const hsize_t max[] = {H5S_UNLIMITED};
								const hsize_t chunk[] = {1};
								const hsize_t offset[] = {h5handle->slice};
								const hsize_t count[] = {1};
								/* Get an object which is stored in the handle for later use */
								cbf_h5handle_require_monochromator(h5handle, &h5location);
								/* 1: value */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								cbf_reportnez(cbf_H5Dfind(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
								if (CBF_SUCCESS==error) {
									const double num = strtod(token,0);
									const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
									if (!cbf_H5Ivalid(h5data)) {
										cbf_reportnez(cbf_H5Dcreate(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
									}
									cbf_reportnez(cbf_H5Dset_extent(h5data,dim2),error);
									cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									/* 2: units */
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"units",token),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								} else {
									fprintf(stderr,"Attempt to determine existence of nexus dataset '%s' failed\n",h5name);
								}
							} else if (!cbf_cistrcmp("Beam_xy",token)) {
								int error = CBF_SUCCESS;
								double num_x = 0./0., num_y = 0./0.;
								/*
                                 Extract x & y positions from the header, put them into the file later.
                                 I might need to read all the header to know if I can actually convert these values to NeXus data.
                                 */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								num_x = strtod(token,0);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								num_y = strtod(token,0);
								/* Extract the units (should be pixels, but I don't want to lose important information if it isn't) */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								CBF_CHECK_TOKEN("pixels",{beam_x = num_x; beam_y = num_y;},;);
							} else if (!cbf_cistrcmp("Flux",token)) {
                                int error = CBF_SUCCESS;
								/*
                                 Either a number with some units or a random string, only do anything if it's a number.
                                 */
								const char * end = 0;
								double num = 0./0.;
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								num = strtod(token,(char**)(&end));
								if (end != token && 0.0 != num) {
									/* I have a valid & useful flux, map it to /entry/sample/beam/flux */
									hid_t sample = CBF_H5FAIL; /* DO NOT FREE THIS! */
									hid_t h5data = CBF_H5FAIL;
									hid_t h5location = CBF_H5FAIL;
									/* Ensure I have a valid sample group */
									cbf_reportnez(cbf_h5handle_require_sample(h5handle, &sample),error);
									/* Ensure I have a valid beam group */
									cbf_reportnez(cbf_H5Grequire(&h5location,"beam",sample),error);
									cbf_reportnez(cbf_H5Arequire_string(h5location, "NX_class", "NXbeam"),error);
									/* Store value & units */
									cbf_reportnez(cbf_H5Drequire_scalar_F64LE(h5location, &h5data, "flux", num),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data, "units", "s-1"),error);
									/* cleanup temporary dataset */
									cbf_H5Dfree(h5data);
									cbf_H5Gfree(h5location);
								}
							} else if (!cbf_cistrcmp("Filter_transmission",token)) {
								int error = CBF_SUCCESS;
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								/* write it to the file */
								if (cbf_H5Ivalid(instrument)) {
									/* Get value */
									const double num = strtod(token,0);
									hid_t h5location = CBF_H5FAIL;
									cbf_reportnez(cbf_H5Grequire(&h5location,"attenuator",instrument),error);
									cbf_reportnez(cbf_H5Arequire_string(h5location, "NX_class", "NXattenuator"),error);
									/* Get value & units */
									cbf_reportnez(cbf_H5Drequire_scalar_F64LE(h5location, 0, "attenuator_transmission",
                                                                              num),error);
									/* cleanup temporary dataset */
									cbf_H5Gfree(h5location);
								}
							} else if (!cbf_cistrcmp("Polarization",token)) {
								int error = CBF_SUCCESS;
								hid_t sample = CBF_H5FAIL; /* DO NOT FREE THIS! */
								hid_t h5location = CBF_H5FAIL;
								/* Ensure I have a valid sample group */
								cbf_reportnez(cbf_h5handle_require_sample(h5handle, &sample),error);
								/* Ensure I have a valid beam group */
								cbf_reportnez(cbf_H5Grequire(&h5location,"beam",sample),error);
								cbf_reportnez(cbf_H5Arequire_string(h5location, "NX_class", "NXbeam"),error);
								/* Get value & units */
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (CBF_SUCCESS==error) {
									/* extract value from header */
									const double p = strtod(token, 0);
									/* convert to nexus format */
									const double polarisation[] = {1.0-p, p};
									const hsize_t dim[] = {2};
									const hsize_t offset[] = {0};
									const hsize_t count[] = {2};
									hid_t h5data = CBF_H5FAIL;
									hid_t h5type = H5T_IEEE_F64LE;
									const char h5name[] = "incident_polarisation";
									cbf_reportnez(cbf_H5Dfind(h5location,&h5data,h5name,1,dim,0,0,h5type),error);
									if (CBF_SUCCESS==error) {
										if (!cbf_H5Ivalid(h5data)) {
											cbf_reportnez(cbf_H5Dcreate(h5location,&h5data,h5name,1,dim,0,0,h5type),error);
											cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,polarisation),error);
										} else {
											double data[] = {0./0., 0./0.};
											cbf_reportnez(cbf_H5Dread(h5data,offset,0,count,data),error);
											if (cmp_double(polarisation,data,2))
												fprintf(stderr,"Error: data doesn't match for nexus field '%s'\n",h5name);
										}
										/* cleanup temporary datasets */
										cbf_H5Dfree(h5data);
									} else {
										fprintf(stderr,"Attempt to determine existence of nexus field '%s' failed\n",h5name);
									}
								}
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Alpha",token)) {
								const char axisName[] = "Alpha";
								int error = CBF_SUCCESS;
								/* Find the data for the current axis, or fail */
								cbf_hdf5_configItem * const axisItem = cbf_hdf5_configItemVector_findMinicbf(axisConfig, axisName);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (cbf_hdf5_configItemVector_end(axisConfig) == axisItem) {
									error |= CBF_NOTFOUND;
									fprintf(stderr,"Config settings for axis '%s' could not be found: "
											"this will eventually be a fatal error\n", axisName);
								} else {
									hid_t h5data = CBF_H5FAIL;
									hid_t location = CBF_H5FAIL;
									const hsize_t dim[] = {h5handle->slice};
									const hsize_t max[] = {H5S_UNLIMITED};
									const hsize_t chunk[] = {1};
									const hsize_t offset[] = {h5handle->slice};
									const hsize_t count[] = {1};
									const double num = strtod(token,0);
									cbf_reportnez(cbf_h5handle_require_sample(h5handle, &location),error);
									cbf_reportnez(cbf_H5Dfind(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
									if (CBF_SUCCESS==error) {
										const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
										if (!cbf_H5Ivalid(h5data)) {
											cbf_reportnez(cbf_H5Dcreate(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
										}
										cbf_reportnez(cbf_H5Dset_extent(h5data, dim2),error);
										cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									}
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,axisItem,cmp_double),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								}
							} else if (!cbf_cistrcmp("Kappa",token)) {
								const char axisName[] = "Kappa";
								int error = CBF_SUCCESS;
								/* Find the data for the current axis, or fail */
								cbf_hdf5_configItem * const axisItem = cbf_hdf5_configItemVector_findMinicbf(axisConfig, axisName);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (cbf_hdf5_configItemVector_end(axisConfig) == axisItem) {
									error |= CBF_NOTFOUND;
									fprintf(stderr,"Config settings for axis '%s' could not be found: "
											"this will eventually be a fatal error\n", axisName);
								} else {
									hid_t h5data = CBF_H5FAIL;
									hid_t location = CBF_H5FAIL;
									const hsize_t dim[] = {h5handle->slice};
									const hsize_t max[] = {H5S_UNLIMITED};
									const hsize_t chunk[] = {1};
									const hsize_t offset[] = {h5handle->slice};
									const hsize_t count[] = {1};
									const double num = strtod(token,0);
									cbf_reportnez(cbf_h5handle_require_sample(h5handle, &location),error);
									cbf_reportnez(cbf_H5Dfind(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
									if (CBF_SUCCESS==error) {
										const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
										if (!cbf_H5Ivalid(h5data)) {
											cbf_reportnez(cbf_H5Dcreate(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
										}
										cbf_reportnez(cbf_H5Dset_extent(h5data, dim2),error);
										cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									}
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,axisItem,cmp_double),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								}
							} else if (!cbf_cistrcmp("Phi",token)) {
								const char axisName[] = "Phi";
								int error = CBF_SUCCESS;
								/* Find the data for the current axis, or fail */
								cbf_hdf5_configItem * const axisItem = cbf_hdf5_configItemVector_findMinicbf(axisConfig, axisName);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (cbf_hdf5_configItemVector_end(axisConfig) == axisItem) {
									error |= CBF_NOTFOUND;
									fprintf(stderr,"Config settings for axis '%s' could not be found: "
											"this will eventually be a fatal error\n", axisName);
								} else {
									hid_t h5data = CBF_H5FAIL;
									hid_t location = CBF_H5FAIL;
									const hsize_t dim[] = {h5handle->slice};
									const hsize_t max[] = {H5S_UNLIMITED};
									const hsize_t chunk[] = {1};
									const hsize_t offset[] = {h5handle->slice};
									const hsize_t count[] = {1};
									const double num = strtod(token,0);
									cbf_reportnez(cbf_h5handle_require_sample(h5handle, &location),error);
									cbf_reportnez(cbf_H5Dfind(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
									if (CBF_SUCCESS==error) {
										const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
										if (!cbf_H5Ivalid(h5data)) {
											cbf_reportnez(cbf_H5Dcreate(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
										}
										cbf_reportnez(cbf_H5Dset_extent(h5data, dim2),error);
										cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									}
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,axisItem,cmp_double),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								}
							} else if (!cbf_cistrcmp("Chi",token)) {
								const char axisName[] = "Chi";
								int error = CBF_SUCCESS;
								/* Find the data for the current axis, or fail */
								cbf_hdf5_configItem * const axisItem = cbf_hdf5_configItemVector_findMinicbf(axisConfig, axisName);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (cbf_hdf5_configItemVector_end(axisConfig) == axisItem) {
									error |= CBF_NOTFOUND;
									fprintf(stderr,"Config settings for axis '%s' could not be found: "
											"this will eventually be a fatal error\n", axisName);
								} else {
									hid_t h5data = CBF_H5FAIL;
									hid_t location = CBF_H5FAIL;
									const hsize_t dim[] = {h5handle->slice};
									const hsize_t max[] = {H5S_UNLIMITED};
									const hsize_t chunk[] = {1};
									const hsize_t offset[] = {h5handle->slice};
									const hsize_t count[] = {1};
									const double num = strtod(token,0);
									cbf_reportnez(cbf_h5handle_require_sample(h5handle, &location),error);
									cbf_reportnez(cbf_H5Dfind(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
									if (CBF_SUCCESS==error) {
										const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
										if (!cbf_H5Ivalid(h5data)) {
											cbf_reportnez(cbf_H5Dcreate(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
										}
										cbf_reportnez(cbf_H5Dset_extent(h5data, dim2),error);
										cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									}
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,axisItem,cmp_double),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								}
							} else if (!cbf_cistrcmp("Omega",token)) {
								const char axisName[] = "Omega";
								int error = CBF_SUCCESS;
								/* Find the data for the current axis, or fail */
								cbf_hdf5_configItem * const axisItem = cbf_hdf5_configItemVector_findMinicbf(axisConfig, axisName);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (cbf_hdf5_configItemVector_end(axisConfig) == axisItem) {
									error |= CBF_NOTFOUND;
									fprintf(stderr,"Config settings for axis '%s' could not be found: "
											"this will eventually be a fatal error\n", axisName);
								} else {
									hid_t h5data = CBF_H5FAIL;
									hid_t location = CBF_H5FAIL;
									const hsize_t dim[] = {h5handle->slice};
									const hsize_t max[] = {H5S_UNLIMITED};
									const hsize_t chunk[] = {1};
									const hsize_t offset[] = {h5handle->slice};
									const hsize_t count[] = {1};
									const double num = strtod(token,0);
									cbf_reportnez(cbf_h5handle_require_sample(h5handle, &location),error);
									cbf_reportnez(cbf_H5Dfind(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
									if (CBF_SUCCESS==error) {
										const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
										if (!cbf_H5Ivalid(h5data)) {
											cbf_reportnez(cbf_H5Dcreate(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
										}
										cbf_reportnez(cbf_H5Dset_extent(h5data, dim2),error);
										cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									}
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,axisItem,cmp_double),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								}
							} else if (!cbf_cistrcmp("Start_angle",token)) {
								const char axisName[] = "Start_angle";
								int error = CBF_SUCCESS;
								/* Find the data for the current axis, or fail */
								cbf_hdf5_configItem * const axisItem = cbf_hdf5_configItemVector_findMinicbf(axisConfig, axisName);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (cbf_hdf5_configItemVector_end(axisConfig) == axisItem) {
									error |= CBF_NOTFOUND;
									fprintf(stderr,"Config settings for axis '%s' could not be found: "
											"this will eventually be a fatal error\n", axisName);
								} else {
									hid_t h5data = CBF_H5FAIL;
									hid_t location = CBF_H5FAIL;
									const hsize_t dim[] = {h5handle->slice};
									const hsize_t max[] = {H5S_UNLIMITED};
									const hsize_t chunk[] = {1};
									const hsize_t offset[] = {h5handle->slice};
									const hsize_t count[] = {1};
									const double num = strtod(token,0);
									cbf_reportnez(cbf_h5handle_require_sample(h5handle, &location),error);
									cbf_reportnez(cbf_H5Dfind(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
									if (CBF_SUCCESS==error) {
										const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
										if (!cbf_H5Ivalid(h5data)) {
											cbf_reportnez(cbf_H5Dcreate(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
										}
										cbf_reportnez(cbf_H5Dset_extent(h5data, dim2),error);
										cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									}
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,axisItem,cmp_double),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								}
							} else if (!cbf_cistrcmp("Detector_2theta",token)) {
								const char axisName[] = "Detector_2theta";
								int error = CBF_SUCCESS;
								/* Find the data for the current axis, or fail */
								cbf_hdf5_configItem * const axisItem = cbf_hdf5_configItemVector_findMinicbf(axisConfig, axisName);
								cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
								if (cbf_hdf5_configItemVector_end(axisConfig) == axisItem) {
									error |= CBF_NOTFOUND;
									fprintf(stderr,"Config settings for axis '%s' could not be found: "
											"this will eventually be a fatal error\n", axisName);
								} else {
									hid_t h5data = CBF_H5FAIL;
									hid_t location = CBF_H5FAIL;
									const hsize_t dim[] = {h5handle->slice};
									const hsize_t max[] = {H5S_UNLIMITED};
									const hsize_t chunk[] = {1};
									const hsize_t offset[] = {h5handle->slice};
									const hsize_t count[] = {1};
									const double num = strtod(token,0);
									cbf_reportnez(cbf_H5Dfind(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
									if (CBF_SUCCESS==error) {
										const hsize_t dim2[] = {cbf_max(dim[0],offset[0]+count[0])};
										if (!cbf_H5Ivalid(h5data)) {
											cbf_reportnez(cbf_H5Dcreate(location,&h5data,axisItem->nexus,1,dim,max,chunk,H5T_IEEE_F64LE),error);
										}
										cbf_reportnez(cbf_H5Dset_extent(h5data, dim2),error);
										cbf_reportnez(cbf_H5Dwrite(h5data,offset,0,count,&num),error);
									}
									cbf_reportnez(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value),error);
									cbf_reportnez(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,axisItem,cmp_double),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								}
							} else if (!cbf_cistrcmp("Image_path",token)) {
							} else if (!cbf_cistrcmp("Angle_increment",token)) {
							} else noMatch = 1;
							if (noMatch) {
								fprintf(stderr,"%s: error: Could not match entry in pilatus header: '%s'\n",__WHERE__,token);
							}
							/* Done matching the entry from this line, go on to the next one */
							while (token && strcmp("\n",token)) {
								const int error = _cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value);
								if (CBF_SUCCESS != error) fprintf(stderr,"%s: error: %s\n",__WHERE__,cbf_strerror(error));
							}
						} while (1);
						free(token);
                        
						{
							/*
                             Record the axis that the sample depends on
                             */
                            const char * const depends_on = cbf_hdf5_configItemVector_getSampleDependsOn(axisConfig);
							if (depends_on) {
                                hid_t h5location = CBF_H5FAIL; /* DO NOT FREE THIS */
								cbf_h5handle_require_sample(h5handle, &h5location);
								cbf_H5Drequire_string(h5location,0,"depends_on",depends_on);
							} else {
								fprintf(stderr,"Config settings for 'Sample' could not be found: "
										"this will eventually be a fatal error\n");
							}
						}
						{ /* write beam_center_x */
							int error = CBF_SUCCESS;
							hid_t h5data = CBF_H5FAIL;
							cbf_reportnez(cbf_H5Drequire_scalar_F64LE(detector, &h5data, "beam_center_x",
                                                                      beam_x*pixel_x),error);
							cbf_reportnez(cbf_H5Arequire_string(h5data,"units","m"),error);
							cbf_H5Dfree(h5data);
						}
						{ /* write beam_center_y */
							int error = CBF_SUCCESS;
							hid_t h5data = CBF_H5FAIL;
							cbf_reportnez(cbf_H5Drequire_scalar_F64LE(detector, &h5data, "beam_center_y",
                                                                      beam_y*pixel_y),error);
							cbf_reportnez(cbf_H5Arequire_string(h5data,"units","m"),error);
							cbf_H5Dfree(h5data);
						}
						{
							int error = CBF_SUCCESS;
							/* Detector axes
                             Requires:
                             * beam_center_x
                             * beam_center_y
                             * detector_distance
                             Creates dependancy chain:
                             detector -> rotation -> translation -> .
                             */
							/* Common settings */
							const hsize_t vdims[] = {3};
                            double vbuf[3] = {0./0.};
							const hsize_t dim[] = {h5handle->slice};
							const hsize_t max[] = {H5S_UNLIMITED};
							const hsize_t chunk[] = {1};
							const hsize_t h5offset[] = {h5handle->slice};
							const hsize_t h5count[] = {1};
							const hsize_t dim2[] = {cbf_max(dim[0],h5offset[0]+h5count[0])};
							hid_t h5location = detector;
							{ /* Translation-specific */
								hid_t h5data = CBF_H5FAIL;
								hid_t h5type = H5T_IEEE_F64LE;
								const double num = detector_distance;
								const double vector[] = {0.0, 0.0, 1.0};
								const double offset[] = {-beam_x*pixel_x, -beam_y*pixel_y, 0.0};
								const char * h5name = "axis_translation";
								cbf_reportnez(cbf_H5Dfind(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
								if (CBF_SUCCESS==error) {
									if (!cbf_H5Ivalid(h5data))
                                        cbf_reportnez(cbf_H5Dcreate(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
                                    cbf_reportnez(cbf_H5Dset_extent(h5data,dim2),error);
									cbf_reportnez(cbf_H5Dwrite(h5data,h5offset,0,h5count,&num),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"units","m"),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"offset_units","m"),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"transformation_type","translation"),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"depends_on","."),error);
									cbf_reportnez(cbf_H5Arequire_cmp(h5data,"vector",1,vdims,H5T_IEEE_F64LE,vector,
                                                                     vbuf,cmp_double),error);
									cbf_reportnez(cbf_H5Arequire_cmp(h5data,"offset",1,vdims,H5T_IEEE_F64LE,offset,
                                                                     vbuf,cmp_double),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								} else {
									fprintf(stderr,"Attempt to determine existence of nexus field '%s' failed\n",h5name);
								}
								if (0) fprintf(stderr, __WHERE__ ": 'detector/%s' written\n",h5name);
							}
							{ /* Rotation-specific */
								hid_t h5data = CBF_H5FAIL;
								hid_t h5type = H5T_IEEE_F64LE;
								const double num = 180.0;
								const double vector[] = {0.0, 0.0, 1.0};
								const char * h5name = "axis_rotation";
								cbf_reportnez(cbf_H5Dfind(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
								if (CBF_SUCCESS==error) {
									if (!cbf_H5Ivalid(h5data))
										cbf_reportnez(cbf_H5Dcreate(h5location,&h5data,h5name,1,dim,max,chunk,h5type),error);
									cbf_reportnez(cbf_H5Dset_extent(h5data,dim2),error);
									cbf_reportnez(cbf_H5Dwrite(h5data,h5offset,0,h5count,&num),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"units","deg"),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"transformation_type","rotation"),error);
									cbf_reportnez(cbf_H5Arequire_string(h5data,"depends_on","axis_translation"),error);
									cbf_reportnez(cbf_H5Arequire_cmp(h5data,"vector",1,vdims,H5T_IEEE_F64LE,vector,
                                                                     vbuf,cmp_double),error);
									/* cleanup temporary datasets */
									cbf_H5Dfree(h5data);
								} else {
									fprintf(stderr,"Attempt to determine existence of nexus field '%s' failed\n",h5name);
								}
								if (0) fprintf(stderr, __WHERE__ ": 'detector/%s' written\n",h5name);
							}
							/* tie everything to the detector */
							cbf_reportnez(cbf_H5Drequire_string(detector,0,"depends_on","axis_rotation"),error);
						}
						cbf_H5Gfree(pilatusDiagnostics);
					}
				}
			}
			/* Second: extract the raw data from the CBF, put it in nexus */
			cbf_onfailnez(cbf_find_column(handle,"data"), fprintf(stderr,__WHERE__": CBF error: cannot find column `data'.\n"));
			/* get the first row, TODO: convert every row */
			cbf_onfailnez(cbf_select_row(handle,0), fprintf(stderr,__WHERE__": CBF error: cannot find row 0.\n"));
			if(1) {
				int real = 0, bits = 0, sign = 0;
				size_t cbfdim[] = {0, 0, 0};
				hsize_t h5dim[] = {0, 0, 0};
				hsize_t h5max[] = {0, 0, 0};
				hsize_t h5chunk[] = {0, 0, 0};
				const int rank = sizeof(cbfdim)/sizeof(*cbfdim);
				size_t elsize = 0, nelems = 0, elems_read = 0;
				void * array = NULL;
				hid_t h5type = CBF_H5FAIL;
				/* TODO: read endianness, select appropriate type */
				cbf_get_bintext(handle->node, handle->row, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
								&bits, &sign, &real, NULL, NULL, cbfdim+2, cbfdim+1, cbfdim, NULL, NULL);
				{ /* ensure each element of dim is at least 1, so total number of elements is a multiplication */
					int i = 0;
					nelems = 1;
					for (i = 0; i < rank; ++i) {
						h5dim[i] = h5max[i] = h5chunk[i] = (0 == cbfdim[i]) ? 1 : cbfdim[i];
						nelems *= h5dim[i];
					}
					h5max[0] = H5S_UNLIMITED;
					h5chunk[0] = 1;
					h5dim[0] = h5handle->slice;
				}
				/* I'm only going to deal with multiples of 8 bits */
				elsize = (bits+7)/8;
				/* get the correct hdf5 type, or fail */
				if (real) {
					switch (bits) {
						case 32: h5type = H5T_IEEE_F32LE; break;
						case 64: h5type = H5T_IEEE_F64LE; break;
						default: return CBF_FORMAT;
					}
				} else {
					switch (bits) {
#define __case(n) case n: h5type = sign ? H5T_STD_I ## n ## LE : H5T_STD_U ## n ## LE; break
                            __case(8);
                            __case(16);
                            __case(32);
                            __case(64);
#undef __case
						default: return CBF_FORMAT;
					}
				}
				/* ensure I have an axis for each index of the image - mapping pixel indices to spatial coordinates */
				{
                    int error = CBF_SUCCESS;
					hid_t h5axis = CBF_H5FAIL;
					const char h5name[] = "slow_pixel_direction";
					cbf_reportnez(cbf_H5Dfind(detector,&h5axis,h5name,1,h5dim+1,h5max+1,0,H5T_IEEE_F64LE),error);
					if (CBF_SUCCESS==error) {
						const hsize_t offset[] = {0};
						const hsize_t * count = h5dim+1;
						double * const expected_data = malloc((*count) * sizeof(double));
						{
							hsize_t n = 0;
							for (n=0; n!=h5dim[1]; ++n)
								expected_data[n] = (double)(n)*pixel_x;
						}
						if (!cbf_H5Ivalid(h5axis)) {
							cbf_reportnez(cbf_H5Dcreate(detector,&h5axis,h5name,1,h5dim+1,h5max+1,0,H5T_IEEE_F64LE),error);
							cbf_reportnez(cbf_H5Dwrite(h5axis,offset,0,count,expected_data),error);
						} else {
							double * const actual_data = malloc((*count) * sizeof(double));
							cbf_reportnez(cbf_H5Dread(h5axis,offset,0,count,actual_data),error);
							if (cmp_double(expected_data, actual_data, *count)) {
								fprintf(stderr,__WHERE__": error: data doesn't match in %s, x pixel size might not match\n",h5name);
								errorcode |= CBF_H5DIFFERENT;
							}
							free((void*)(actual_data));
						}
						free((void*)(expected_data));
						cbf_reportnez(cbf_H5Arequire_string(h5axis,"units","m"),error);
						{
							const double vector[] = {1.0, 0.0, 0.0};
							const hsize_t vdims[] = {3};
							double vbuf[3] = {0./0.};
							cbf_reportnez(cbf_H5Arequire_cmp(h5axis,"vector",1,vdims,H5T_IEEE_F64LE,vector,vbuf,cmp_double),error);
						}
                        cbf_reportnez(cbf_H5Arequire_string(h5axis,"transformation_type","translation"),error);
						cbf_reportnez(cbf_H5Arequire_string(h5axis,"depends_on","/entry/instrument/detector/axis_rotation"),error);
						cbf_H5Dfree(h5axis);
					} else {
						errorcode |= error;
						fprintf(stderr,__WHERE__": error locating axis: %s\n", cbf_strerror(error));
					}
				}
				{
                    int error = CBF_SUCCESS;
					hid_t h5axis = CBF_H5FAIL;
					const char h5name[] = "fast_pixel_direction";
					cbf_reportnez(cbf_H5Dfind(detector,&h5axis,h5name,1,h5dim+2,h5max+2,0,H5T_IEEE_F64LE),error);
					if (CBF_SUCCESS==error) {
						const hsize_t offset[] = {0};
						const hsize_t * count = h5dim+2;
						double * const expected_data = malloc((*count) * sizeof(double));
						{
							hsize_t n = 0;
							for (n=0; n!=h5dim[2]; ++n)
								expected_data[n] = (double)(n)*pixel_y;
						}
						if (!cbf_H5Ivalid(h5axis)) {
							cbf_reportnez(cbf_H5Dcreate(detector,&h5axis,h5name,1,h5dim+2,h5max+2,0,H5T_IEEE_F64LE),error);
							cbf_reportnez(cbf_H5Dwrite(h5axis,offset,0,count,expected_data),error);
						} else {
							double * const actual_data = malloc((*count) * sizeof(double));
							cbf_reportnez(cbf_H5Dread(h5axis,offset,0,count,actual_data),error);
							if (cmp_double(expected_data, actual_data, *count)) {
								fprintf(stderr,__WHERE__": error: data doesn't match in %s, y pixel size might not match\n",h5name);
								errorcode |= CBF_H5DIFFERENT;
							}
                            free((void*)(actual_data));
						}
						free((void*)(expected_data));
						cbf_reportnez(cbf_H5Arequire_string(h5axis,"units","m"),error);
						{
							const double vector[] = {0.0, 1.0, 0.0};
							const hsize_t vdims[] = {3};
							double vbuf[3] = {0./0.};
							cbf_reportnez(cbf_H5Arequire_cmp(h5axis,"vector",1,vdims,H5T_IEEE_F64LE,vector,vbuf,cmp_double),error);
						}
                        cbf_reportnez(cbf_H5Arequire_string(h5axis,"transformation_type","translation"),error);
                        cbf_reportnez(cbf_H5Arequire_string(h5axis,"depends_on","/entry/instrument/detector/axis_rotation"),error);
						cbf_H5Dfree(h5axis);
					} else {
                        errorcode |= error;
                        fprintf(stderr,__WHERE__": error locating axis: %s\n", cbf_strerror(error));
					}
				}
				/* allocate an array for the raw data */
				array = malloc(elsize*nelems);
				/* get the raw data, ensuring I get all of it */
				if (real) cbf_get_realarray(handle, 0, array, elsize, nelems, &elems_read);
				else cbf_get_integerarray(handle, 0, array, elsize, sign, nelems, &elems_read);
				if (elems_read != nelems) {
					free(array);
					return CBF_ENDOFDATA;
				}
				/* get a hdf5 dataset */
				{
					int error = CBF_SUCCESS;
					hid_t dataset = CBF_H5FAIL;
					/* get the dataset */
                    cbf_reportnez(cbf_H5Dfind(detector,&dataset,"data",rank,h5dim,h5max,h5chunk,h5type),error);
					if (CBF_SUCCESS==error) {
						const hsize_t h5extent[] = {h5dim[0]+1, h5dim[1], h5dim[2]};
						const hsize_t h5offset[] = {h5dim[0], 0, 0};
						const hsize_t h5count[] = {1, h5dim[1], h5dim[2]};
						const int sig[] = {1};
						int buf[] = {0};
						if (!cbf_H5Ivalid(dataset)) {
							/* define variables & check args */
							hid_t dataSpace = CBF_H5FAIL;
							hid_t dcpl = H5Pcreate(H5P_DATASET_CREATE);
                            
							/* check variables are valid */
							cbf_reportnez(cbf_H5Screate(&dataSpace, rank, h5dim, h5max),error);
                            
							/* allow dataset to be chunked */
							H5Pset_chunk(dcpl,rank,h5chunk);
							/* allow compression */
							if (h5handle->flags & CBF_H5_ZLIB) {
								H5Pset_deflate(dcpl, 2);
							}
                            
							/* create the dataset */
							if (CBF_SUCCESS == errorcode)
								dataset = H5Dcreate2(detector,"data",h5type,dataSpace,H5P_DEFAULT,dcpl,H5P_DEFAULT);
                            
							/* check local variables are properly closed */
							if (cbf_H5Ivalid(dataSpace)) H5Sclose(dataSpace);
							if (cbf_H5Ivalid(dcpl)) H5Pclose(dcpl);
						}
						/* write the data to the hdf5 dataset */
                        cbf_reportnez(cbf_H5Dset_extent(dataset, h5extent),error);
						cbf_reportnez(cbf_H5Dwrite(dataset,h5offset,0,h5count,array),error);
						cbf_reportnez(cbf_H5Arequire_cmp(dataset,"signal",0,0,H5T_STD_I32LE,sig,buf,cmp_int),error);
					} else {
						errorcode |= error;
						fprintf(stderr,__WHERE__": error locating primary dataset: %s\n", cbf_strerror(error));
					}
					/* ensure /entry/data@NXdata exists & has a link to the data */
					if (CBF_SUCCESS==errorcode && !cbf_H5Ivalid(h5handle->nxdata)) {
						cbf_reportnez(cbf_H5Gcreate(&h5handle->nxdata,"data",h5handle->nxid),error);
						cbf_reportnez(cbf_H5Arequire_string(h5handle->nxdata,"NX_class","NXdata"),error);
						H5Lcreate_hard(dataset,".",h5handle->nxdata,"data",H5P_DEFAULT,H5P_DEFAULT);
						H5Lcreate_hard(detector,"slow_pixel_direction",h5handle->nxdata,"x",H5P_DEFAULT,H5P_DEFAULT);
						H5Lcreate_hard(detector,"fast_pixel_direction",h5handle->nxdata,"y",H5P_DEFAULT,H5P_DEFAULT);
                        cbf_reportnez(cbf_H5Arequire_string(h5handle->nxdata,"signal","data"),error);
						{ /* axes=[...] */
							hid_t h5atype = CBF_H5FAIL;
							const char axis0[] = "";
							const char axis1[] = "x";
							const char axis2[] = "y";
							const char * axes[] = {axis0,axis1,axis2};
							const hsize_t dim[] = {3};
                            char * buf[3] = {0};
							cbf_reportnez(cbf_H5Tcreate_string(&h5atype,H5T_VARIABLE),error);
							cbf_reportnez(cbf_H5Arequire_cmp(h5handle->nxdata,"axes",1,dim,h5atype,axes,buf,cmp_vlstring),error);
							cbf_H5Tfree(h5atype);
						}
						{ /* x_indices=1 */
							const int idx[] = {1};
							int buf[] = {0};
							cbf_reportnez(cbf_H5Arequire_cmp(h5handle->nxdata,"x_indices",0,0,H5T_STD_I32LE,idx,buf,cmp_int),error);
						}
						{ /* y_indices=2 */
							const int idx[] = {2};
							int buf[] = {0};
							cbf_reportnez(cbf_H5Arequire_cmp(h5handle->nxdata,"y_indices",0,0,H5T_STD_I32LE,idx,buf,cmp_int),error);
						}
					}
					/* clean up */
					cbf_H5Dfree(dataset);
				}
				free(array);
			}
		}
        
		return errorcode;
	}
    
#undef CBF_CHECK_TOKEN
    
    
    /* Open an HDF5 File handle */
    
    int cbf_open_h5handle(cbf_h5handle *h5handle,
                          const char * h5filename) {
        
        hid_t fcreate_prop_list;
        
        /* check that the file name has been specified and
         is an HDF5 file */
        
        if (!h5filename || !H5Fis_hdf5(h5filename)) return CBF_ARGUMENT;
        
        /* ensure the HDF5 library is ready */
        
        cbf_h5failneg(H5open(),CBF_ARGUMENT);
        
        cbf_failnez(cbf_make_h5handle(h5handle));
        
        cbf_h5onfailneg(fcreate_prop_list = H5Pcreate(H5P_FILE_ACCESS),
                        CBF_ALLOC,cbf_free((void**) h5handle, NULL));
        
        (*h5handle)->rwmode = 0;
        
        cbf_h5onfailneg(H5Pset_fclose_degree(fcreate_prop_list,H5F_CLOSE_STRONG),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
        cbf_h5onfailneg((*h5handle)->hfile = H5Fopen(h5filename,
                                                     H5F_ACC_RDONLY,fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
        cbf_h5onfailneg(H5Pclose(fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
        return CBF_SUCCESS;
        
    }
    
    /* Convert an HDF5 typeclass to a string
     and flag for atomic or not
     copies up to n-1 characters of the
     type string to buffer*/
    
    int cbf_h5type_class_string(H5T_class_t type_class,
                                char * buffer,
                                int * atomic, size_t n ) {
        
        int good_type;
        
        good_type = 0;
        
        *atomic = 1;
        
        buffer[n-1] = '\0';
        
        switch(type_class) {
            case H5T_INTEGER:
                strncpy(buffer,"H5T_INTEGER",n-1);
                break;
            case H5T_FLOAT:
                strncpy(buffer,"H5T_FLOAT",n-1);
                break;
            case H5T_STRING:
                strncpy(buffer,"H5T_STRING",n-1);
                break;
            case H5T_BITFIELD:
                strncpy(buffer,"H5T_BITFIELD",n-1);
                break;
            case H5T_OPAQUE:
                strncpy(buffer,"H5T_OPAQUE",n-1);
                break;
            case H5T_COMPOUND:
                strncpy(buffer,"H5T_COMPOUND",n-1);
                *atomic = 0;
                break;
            case H5T_REFERENCE:
                strncpy(buffer,"H5T_REFERENCE",n-1);
                break;
            case H5T_ENUM:
                strncpy(buffer,"H5T_ENUM",n-1);
                *atomic = 0;
                break;
            case H5T_VLEN:
                strncpy(buffer,"H5T_VLEN",n-1);
                *atomic = 0;
                break;
            case H5T_ARRAY:
                strncpy(buffer,"H5T_ARRAY",n-1);
                *atomic = 0;
                break;
            case -1:
                strncpy(buffer,".",n-1);
                good_type = CBF_ARGUMENT;
                *atomic = 0;
                break;
            default:
                strncpy(buffer,"UNKNOWN",n-1);
                good_type = CBF_ARGUMENT;
                *atomic = 0;
                break;
        }
        
        return good_type;
        
    }
    
    /* Store an HDF5 Dataset in CBF handle, using
     category categoryname, ...*/
    
    int cbf_h5ds_store(cbf_handle handle, haddr_t parent,
                       const char * parent_name,
                       const int target_row,
                       const char * categoryname, hid_t obj_id,
                       hid_t space, hid_t type,
                       const char * name,
                       const int readattrib,
                       void ** value) {
        
        char buffer[25];
        
        int errorcode;
        
        unsigned char* data;
        
        char h5t_type_class[14], h5t_base_type_class[14];
        
        hid_t base_type;
        
        hid_t native_type;
        
        int atomic;
        
        int ndims, kdims, ii;
        
        unsigned int rows;
        
        hsize_t dims[H5S_MAX_RANK];
        
        hsize_t maxdims[H5S_MAX_RANK];
        
        char * byte_order;
        
        size_t type_size, total_size, total_dim;
        
        H5T_class_t type_class, base_type_class;
        
        H5T_order_t type_order;
        
        H5T_sign_t type_sign;
        
        errorcode = 0;
        
        cbf_reportnez(cbf_require_category(handle,categoryname),errorcode);
        
        /*  Give the name of this dataset as its own id */
        
        cbf_reportnez(cbf_require_column(handle,"id"),errorcode);
        
        cbf_reportnez(cbf_count_rows(handle,&rows),errorcode);
        
        if (target_row==-1) {
            
            cbf_reportnez(cbf_new_row(handle),errorcode);
            
        } else {
            
            if ((unsigned int)target_row >= rows ) {
                
                for (ii=rows; ii <= target_row; ii++) {
                    
                    cbf_reportnez(cbf_new_row(handle),errorcode);
                    
                }
            }
            
            cbf_reportnez(cbf_select_row(handle,target_row),errorcode);
        }
        
        cbf_reportnez(cbf_set_value(handle,name),errorcode);
        
        /*  Give the parent name and id for this dataset */
        
        cbf_reportnez(cbf_require_column(handle,"parent_name"),errorcode);
        
        cbf_reportnez(cbf_set_value(handle,parent_name),errorcode);
        
        cbf_reportnez(cbf_require_column(handle,"parent_id"),errorcode);
        
        sprintf(buffer,"0x%lx",(unsigned long)parent);
        
        cbf_reportnez(cbf_set_value(handle,buffer),errorcode);
        
        
        /* get the class, and, if not atomic
         try to get the base class for an array
         give up otherwise */
        
        type_class = H5Tget_class(type);
        
        native_type = H5Tget_native_type(type,H5T_DIR_ASCEND);
        
        base_type = CBF_H5FAIL;
        
        cbf_reportnez(cbf_require_column(handle,"type"),errorcode);
        
        type_size = 0;
        
        if (value) *value = 0;
        
        type_order = -1;
        
        kdims= ndims = H5Sget_simple_extent_ndims(space);
        
        if (ndims <= 0) ndims = 1;
        
        H5Sget_simple_extent_dims(space,dims,maxdims);
        
        if (!cbf_h5type_class_string(
                                     type_class,
                                     h5t_type_class,&atomic,14)) {
            
            cbf_reportnez(cbf_set_value(handle, h5t_type_class),errorcode);
            
            if (!atomic && type_class==H5T_ARRAY){
                base_type = H5Tget_super(type);
                
                base_type_class = H5Tget_class(base_type);
                
                if (!cbf_h5type_class_string(
                                             base_type_class,
                                             h5t_base_type_class,&atomic,14)) {
                    if (!atomic) {
                        strncpy (h5t_base_type_class,".",14);
                        cbf_h5failneg(H5Tclose(base_type),CBF_FORMAT);
                        base_type = CBF_H5FAIL;
                    } else {
                        
                        type_size = H5Tget_size(base_type);
                        
                        type_order = H5Tget_order(base_type);
                        
                        type_sign = H5Tget_sign(base_type);
                        
                        cbf_reportnez(cbf_require_column(handle,"base_type"),errorcode);
                        
                        cbf_reportnez(cbf_set_value(handle,h5t_base_type_class),errorcode);
                    }
                } else {
                    strncpy (h5t_base_type_class,".",14);
                    cbf_h5failneg(H5Tclose(base_type),CBF_FORMAT);
                    base_type = CBF_H5FAIL;
                }
                
            } else if (atomic) {
                
                type_size = H5Tget_size(native_type);
                
                type_order = H5Tget_order(type);
                
                type_sign = H5Tget_sign(type);
                
            }
            
        }
        
        total_size = type_size;
        
        total_dim = 1;
        
        for (ii=0; ii < kdims; ii ++) {
            
            total_size *= dims[ii];
            
            total_dim *= dims[ii];
            
        }
        
        
        if (total_size < type_size) total_size = type_size;
        
        if (total_dim < 1 ) total_dim = 1;
        
        cbf_reportnez(cbf_require_column(handle,"value"),errorcode);
        
        if(total_size > 0) {
            
            if(readattrib) {
                
                /* Process an attribute */
                
                cbf_reportnez(cbf_alloc(((void **) value),NULL,
                                        total_size+1,1),errorcode);
                
                cbf_h5failneg(H5Aread(obj_id,native_type,(void *)*value),
                              CBF_ARGUMENT);
                
                (*((char **)value))[total_size]='\0';
                
                if (type_class==H5T_STRING) {
                    
                    cbf_reportnez(cbf_set_value(handle,(const char *)(*value)),errorcode);
                    
                } else if (type_class==H5T_INTEGER){
                    
                    /* Read of a single integer or an integer array of
                     up to 3 dimensions */
                    
                    char * ivalue;
                    
                    long xdata;
                    
                    unsigned long uxdata;
                    
                    int sign;
                    
                    sign = (type_sign==H5T_SGN_2)?1:0;
                    
                    if (total_dim ==1) {
                        
                        cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                type_size*3+1,1),errorcode);
                        
                        if (H5Tequal(native_type,H5T_NATIVE_CHAR)&&sign) xdata = **((signed char **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_CHAR)&&!sign) uxdata = **((unsigned char **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_SCHAR)) xdata = **((signed char **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_UCHAR)) uxdata = **((unsigned char **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_SHORT)) xdata = **((unsigned short **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_USHORT)) uxdata = **((unsigned short **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_INT)) xdata = **((int **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_UINT)) uxdata = **((unsigned int **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_LONG)) xdata = **((long **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_ULONG)) uxdata = **((unsigned long **)value);
                        
                        if (sign) {
                            
                            sprintf(ivalue,"%ld",xdata);
                            
                        } else {
                            
                            sprintf(ivalue,"%lu",uxdata);
                            
                        }
                        
                        cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);
                        
                        cbf_reportnez(cbf_free((void**)value,NULL),errorcode);
                        
                        *value = (unsigned char *)ivalue;
                        
                    } else {
                        
                        
                        
                        /* process arrays of up to 100 as
                         bracketed strings
                         */
                        
                        if (total_dim < 101) {
                            
                            size_t indices[H5S_MAX_RANK];
                            
                            size_t master_index, ival_index;
                            
                            int idim, level;
                            
                            char buffer[40];
                            
                            cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                    total_dim*((type_size*3)+kdims*2)+1,1),errorcode);
                            
                            for (idim = 0; idim < kdims; idim ++) {
                                
                                indices[idim] = 0;
                                
                                ivalue[idim] = '[';
                                
                            }
                            
                            level = kdims-1;
                            
                            ival_index = kdims;
                            
                            master_index = 0;
                            
                            while (master_index < total_dim) {
                                
                                for (indices[level]=0; indices[level] < dims[level];) {
                                    
                                    if (H5Tequal(native_type,H5T_NATIVE_CHAR)&&sign) xdata = ((*(signed char **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_CHAR)&&!sign) uxdata = ((*(unsigned char **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_SCHAR)) xdata = ((*(signed char **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_UCHAR)) uxdata = ((*(unsigned char **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_SHORT)) xdata = ((*(unsigned short **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_USHORT)) uxdata = ((*(unsigned short **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_INT)) xdata = ((*(int **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_UINT)) uxdata = ((*(unsigned int **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_LONG)) xdata = ((*(long **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_ULONG)) uxdata = ((*(unsigned long **)value)[master_index]);
                                    
                                    
                                    if (sign) {
                                        
                                        sprintf(buffer,"%ld",xdata);
                                        
                                    } else {
                                        
                                        sprintf(buffer,"%lu",uxdata);
                                        
                                    }
                                    
                                    strcat(ivalue+ival_index,buffer);
                                    
                                    ival_index+=strlen(buffer);
                                    
                                    ivalue[ival_index++]= (indices[level] < dims[level]-1)?',':']';
                                    
                                    master_index++;
                                    
                                    indices[level]++;
                                    
                                    if (indices[level] == dims[level]) {
                                        
                                        /* We are at the end of a fast-dimension row
                                         and therefore need to update higher level indices
                                         if any.  */
                                        
                                        indices[level] = 0;
                                        
                                        level --;
                                        
                                        while (level >= 0) {
                                            
                                            indices[level]++;
                                            
                                            if (indices[level] < dims[level]) {
                                                
                                                ivalue[ival_index++] = ',';
                                                
                                                ivalue[ival_index++] = '[';
                                                
                                                level++;
                                                
                                                break;
                                                
                                            } else {
                                                
                                                ivalue[ival_index++] = ']';
                                                
                                                indices[level] = 0;
                                                
                                                level --;
                                                
                                            }
                                            
                                        }
                                        
                                        if (level < 0) break;
                                        
                                        while (level > kdims-1) {
                                            
                                            ivalue[ival_index++] = '[';
                                            
                                            level++;
                                            
                                        }
                                        
                                    }
                                    
                                }
                                
                            }
                            
                            ivalue[ival_index++] = '\0';
                            
                            cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);
                            
                            cbf_reportnez(cbf_set_typeofvalue(handle,"bkts"),errorcode);
                            
                            cbf_reportnez(cbf_free((void**)value,NULL),errorcode);
                            
                            *value = (unsigned char *)ivalue;
                            
                        } else {
                            
                            size_t dimfast, dimmid, dimslow;
                            
                            dimmid = dimslow = 1;
                            
                            dimfast = dims[kdims-1];
                            
                            if (kdims > 1) dimmid = dims[kdims-2];
                            
                            if (kdims > 2) dimslow = total_dim/(dimfast*dimmid);
                            
                            cbf_reportnez(cbf_set_integerarray_wdims_fs(handle,
                                                                        CBF_NIBBLE_OFFSET,target_row,*value,
                                                                        type_size,sign,total_dim,"little_endian",
                                                                        dimfast,dimmid,dimslow,0),errorcode);
                            
                        }
                        
                        
                    }
                    
                    
                } else if (type_class==H5T_FLOAT){
                    
                    /* Read of a single float or double or a float or
                     double array of up to 3 dimensions */
                    
                    char * ivalue;
                    
                    double dxdata;
                    
                    float xdata;
                    
                    if (total_dim ==1) {
                        
                        cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                type_size*2+6,1),errorcode);
                        
                        if (H5Tequal(native_type,H5T_NATIVE_FLOAT)) {
                            
                            xdata = **((float **)value);
                            
                            snprintf(ivalue,type_size*2+5,"%.7g",(double) xdata);
                            
                        } else {
                            
                            dxdata = **((double **)value);
                            
                            snprintf(ivalue,type_size*2+5,"%.15g",dxdata);
                            
                        }
                        
                        cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);
                        
                        cbf_reportnez(cbf_free((void**)value,NULL),errorcode);
                        
                        *value = (unsigned char *)ivalue;
                        
                    } else {
                        
                        
                        
                        /* process arrays of up to 100 as
                         bracketed strings
                         */
                        
                        if (total_dim < 101) {
                            
                            size_t indices[H5S_MAX_RANK];
                            
                            size_t master_index, ival_index;
                            
                            int idim, level;
                            
                            char buffer[40];
                            
                            cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                    total_dim*((type_size*2)+5+kdims*2)+1,1),errorcode);
                            
                            for (idim = 0; idim < kdims; idim ++) {
                                
                                indices[idim] = 0;
                                
                                ivalue[idim] = '[';
                                
                            }
                            
                            level = kdims-1;
                            
                            ival_index = kdims;
                            
                            master_index = 0;
                            
                            while (master_index < total_dim) {
                                
                                for (indices[level]=0; indices[level] < dims[level];) {
                                    
                                    if (H5Tequal(native_type,H5T_NATIVE_FLOAT)) {
                                        
                                        xdata = ((*(float **)value)[master_index]);
                                        
                                        snprintf(buffer,type_size*2+5,"%.7g",(double) xdata);
                                        
                                    } else {
                                        
                                        dxdata = ((*(double **)value)[master_index]);
                                        
                                        snprintf(buffer,type_size*2+5,"%.15g",dxdata);
                                        
                                    }
                                    
                                    strcat(ivalue+ival_index,buffer);
                                    
                                    ival_index+=strlen(buffer);
                                    
                                    ivalue[ival_index++]= (indices[level] < dims[level]-1)?',':']';
                                    
                                    master_index++;
                                    
                                    indices[level]++;
                                    
                                    if (indices[level] == dims[level]) {
                                        
                                        /* We are at the end of a fast-dimension row
                                         and therefore need to update higher level indices
                                         if any.  */
                                        
                                        indices[level] = 0;
                                        
                                        level --;
                                        
                                        while (level >= 0) {
                                            
                                            indices[level]++;
                                            
                                            if (indices[level] < dims[level]) {
                                                
                                                ivalue[ival_index++] = ',';
                                                
                                                ivalue[ival_index++] = '[';
                                                
                                                level++;
                                                
                                                break;
                                                
                                            } else {
                                                
                                                ivalue[ival_index++] = ']';
                                                
                                                indices[level] = 0;
                                                
                                                level --;
                                                
                                            }
                                            
                                        }
                                        
                                        if (level < 0) break;
                                        
                                        while (level > kdims-1) {
                                            
                                            ivalue[ival_index++] = '[';
                                            
                                            level++;
                                            
                                        }
                                        
                                    }
                                    
                                }
                                
                            }
                            
                            ivalue[ival_index++] = '\0';
                            
                            cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);
                            
                            cbf_reportnez(cbf_set_typeofvalue(handle,"bkts"),errorcode);
                            
                            cbf_reportnez(cbf_free((void**)value,NULL),errorcode);
                            
                            *value = (unsigned char *)ivalue;
                            
                        } else {
                            
                            size_t dimfast, dimmid, dimslow;
                            
                            dimmid = dimslow = 1;
                            
                            dimfast = dims[kdims-1];
                            
                            if (kdims > 1) dimmid = dims[kdims-2];
                            
                            if (kdims > 2) dimslow = total_dim/(dimfast*dimmid);
                            
                            cbf_reportnez(cbf_set_realarray_wdims_fs(handle,
                                                                     CBF_NIBBLE_OFFSET,target_row,*value,
                                                                     type_size,total_dim,"little_endian",
                                                                     dimfast,dimmid,dimslow,0),errorcode);
                            
                        }
                        
                        
                    }
                    
                    
                } else if (type_class != H5T_OPAQUE) {
                    
                    unsigned char * hexvalue;
                    
                    unsigned char hexdigs[16] = {'0','1','2','3','4','5','6','7','8','9',
                        'a','b','c','d','e','f'};
                    
                    size_t ii;
                    
                    cbf_reportnez(cbf_alloc(((void **) &hexvalue),NULL,
                                            2*total_size+1,1),errorcode);
                    
                    hexvalue[2*total_size+1] = '\0';
                    
                    for (ii=0; ii< total_size; ii++) {
                        
                        hexvalue[(total_size-ii)*2-2] =
                        hexdigs[((int)(*((unsigned char **)value))[ii])&0xF];
                        
                        hexvalue[(total_size-ii)*2-1] =
                        hexdigs[((int)((*((unsigned char **)value))[ii])>>4)&0xF];
                        
                    }
                    
                    cbf_reportnez(cbf_set_value(handle,(const char *)(hexvalue)),errorcode);
                    
                    cbf_reportnez(cbf_free((void**)value,NULL),errorcode);
                    
                    *value = hexvalue;
                    
                }
                
            } else {
                
                /* process a dataset */
                
                hid_t memspace;
                
                memspace=H5Screate_simple(kdims,dims,NULL);
                
                cbf_reportnez(cbf_alloc(((void **) &data),NULL,
                                        total_size+1,1),errorcode);
                
                cbf_h5failneg(H5Dread(obj_id,native_type,
                                      H5S_ALL,memspace,H5P_DEFAULT,data),
                              CBF_ARGUMENT);
                
                data[total_size]='\0';
                
                if (type_class==H5T_STRING) {
                    
                    cbf_reportnez(cbf_set_value(handle,(const char *)data),errorcode)
                    
                } else if (type_class==H5T_INTEGER){
                    
                    /* Read of a single integer or an integer array of
                     up to 3 dimensions */
                    
                    char * ivalue;
                    
                    long xdata;
                    
                    unsigned long uxdata;
                    
                    int sign;
                    
                    sign = (type_sign==H5T_SGN_2)?1:0;
                    
                    if (total_dim ==1) {
                        
                        cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                type_size*3+1,1),errorcode);
                        
                        if (H5Tequal(native_type, H5T_NATIVE_CHAR)&&sign) xdata = *((signed char *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_CHAR)&&!sign) uxdata = *((unsigned char *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_SCHAR)) xdata = *((signed char *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_UCHAR)) uxdata = *((unsigned char *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_SHORT)) xdata = *((unsigned short *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_USHORT)) uxdata = *((unsigned short *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_INT)) xdata = *((int *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_UINT)) uxdata = *((unsigned int *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_LONG)) xdata = *((long *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_ULONG)) uxdata = *((unsigned long *)data);
                        
                        if (sign) {
                            
                            sprintf(ivalue,"%ld",xdata);
                            
                        } else {
                            
                            sprintf(ivalue,"%lu",uxdata);
                            
                        }
                        
                        cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);
                        
                        cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);
                        
                        data = (unsigned char *)ivalue;
                        
                    } else {
                        
                        
                        
                        /* process arrays of up to 100 as
                         bracketed strings
                         */
                        
                        if (total_dim < 101) {
                            
                            size_t indices[H5S_MAX_RANK];
                            
                            size_t master_index, ival_index;
                            
                            int idim, level;
                            
                            char buffer[40];
                            
                            cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                    total_dim*((type_size*3)+kdims*2)+1,1),errorcode);
                            
                            for (idim = 0; idim < kdims; idim ++) {
                                
                                indices[idim] = 0;
                                
                                ivalue[idim] = '[';
                                
                            }
                            
                            level = kdims-1;
                            
                            ival_index = kdims;
                            
                            master_index = 0;
                            
                            while (master_index < total_dim) {
                                
                                for (indices[level]=0; indices[level] < dims[level];) {
                                    
                                    if (H5Tequal(native_type, H5T_NATIVE_CHAR)
                                        &&sign) xdata = (((signed char *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_CHAR)
                                        &&!sign) uxdata = (((unsigned char *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_SCHAR)) xdata = (((signed char *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_UCHAR)) uxdata = (((unsigned char *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_SHORT)) xdata = (((unsigned short *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_USHORT)) uxdata = (((unsigned short *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_INT)) xdata = (((int *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_UINT)) uxdata = (((unsigned int *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_LONG)) xdata = (((long *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_ULONG)) uxdata = (((unsigned long *)data)[master_index]);
                                    
                                    
                                    if (sign) {
                                        
                                        sprintf(buffer,"%ld",xdata);
                                        
                                    } else {
                                        
                                        sprintf(buffer,"%lu",uxdata);
                                        
                                    }
                                    
                                    strcat(ivalue+ival_index,buffer);
                                    
                                    ival_index+=strlen(buffer);
                                    
                                    ivalue[ival_index++]= (indices[level] < dims[level]-1)?',':']';
                                    
                                    master_index++;
                                    
                                    indices[level]++;
                                    
                                    if (indices[level] == dims[level]) {
                                        
                                        /* We are at the end of a fast-dimension row
                                         and therefore need to update higher level indices
                                         if any.  */
                                        
                                        indices[level] = 0;
                                        
                                        level --;
                                        
                                        while (level >= 0) {
                                            
                                            indices[level]++;
                                            
                                            if (indices[level] < dims[level]) {
                                                
                                                ivalue[ival_index++] = ',';
                                                
                                                ivalue[ival_index++] = '[';
                                                
                                                level++;
                                                
                                                break;
                                                
                                            } else {
                                                
                                                ivalue[ival_index++] = ']';
                                                
                                                indices[level] = 0;
                                                
                                                level --;
                                                
                                            }
                                            
                                        }
                                        
                                        if (level < 0) break;
                                        
                                        while (level > kdims-1) {
                                            
                                            ivalue[ival_index++] = '[';
                                            
                                            level++;
                                            
                                        }
                                        
                                    }
                                    
                                }
                                
                            }
                            
                            ivalue[ival_index++] = '\0';
                            
                            cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);
                            
                            cbf_reportnez(cbf_set_typeofvalue(handle,"bkts"),errorcode);
                            
                            cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);
                            
                            data = (unsigned char *)ivalue;
                            
                        } else {
                            
                            size_t dimfast, dimmid, dimslow;
                            
                            dimmid = dimslow = 1;
                            
                            dimfast = dims[kdims-1];
                            
                            if (kdims > 1) dimmid = dims[kdims-2];
                            
                            if (kdims > 2) dimslow = total_dim/(dimfast*dimmid);
                            
                            cbf_reportnez(cbf_set_integerarray_wdims_fs(handle,
                                                                        CBF_NIBBLE_OFFSET,target_row,data,
                                                                        type_size,sign,total_dim,"little_endian",
                                                                        dimfast,dimmid,dimslow,0),errorcode);
                            
                        }
                        
                        
                    }
                    
                    
                } else if (type_class==H5T_FLOAT){
                    
                    /* Read of a single float or double or a float or
                     double array of up to 3 dimensions */
                    
                    char * ivalue;
                    
                    double dxdata;
                    
                    float xdata;
                    
                    if (total_dim ==1) {
                        
                        cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                type_size*2+6,1),errorcode);
                        
                        if (H5Tequal(native_type, H5T_NATIVE_FLOAT)) {
                            
                            xdata = *((float *)data);
                            
                            snprintf(ivalue,type_size*2+5,"%.7g",(double) xdata);
                            
                        } else {
                            
                            dxdata = *((double *)data);
                            
                            snprintf(ivalue,type_size*2+5,"%.15g",dxdata);
                            
                        }
                        
                        cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);
                        
                        cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);
                        
                        data = (unsigned char *)ivalue;
                        
                    } else {
                        
                        
                        
                        /* process arrays of up to 100 as
                         bracketed strings
                         */
                        
                        if (total_dim < 101) {
                            
                            size_t indices[H5S_MAX_RANK];
                            
                            size_t master_index, ival_index;
                            
                            int idim, level;
                            
                            char buffer[40];
                            
                            cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                    total_dim*((type_size*2)+5+kdims*2)+1,1),errorcode);
                            
                            for (idim = 0; idim < kdims; idim ++) {
                                
                                indices[idim] = 0;
                                
                                ivalue[idim] = '[';
                                
                            }
                            
                            level = kdims-1;
                            
                            ival_index = kdims;
                            
                            master_index = 0;
                            
                            while (master_index < total_dim) {
                                
                                for (indices[level]=0; indices[level] < dims[level];) {
                                    
                                    if (H5Tequal(native_type, H5T_NATIVE_FLOAT)) {
                                        
                                        xdata = (((float *)data)[master_index]);
                                        
                                        snprintf(buffer,type_size*2+5,"%.7g",(double) xdata);
                                        
                                    } else {
                                        
                                        dxdata = (((double *)data)[master_index]);
                                        
                                        snprintf(buffer,type_size*2+5,"%.15g",dxdata);
                                        
                                    }
                                    
                                    strcat(ivalue+ival_index,buffer);
                                    
                                    ival_index+=strlen(buffer);
                                    
                                    ivalue[ival_index++]= (indices[level] < dims[level]-1)?',':']';
                                    
                                    master_index++;
                                    
                                    indices[level]++;
                                    
                                    if (indices[level] == dims[level]) {
                                        
                                        /* We are at the end of a fast-dimension row
                                         and therefore need to update higher level indices
                                         if any.  */
                                        
                                        indices[level] = 0;
                                        
                                        level --;
                                        
                                        while (level >= 0) {
                                            
                                            indices[level]++;
                                            
                                            if (indices[level] < dims[level]) {
                                                
                                                ivalue[ival_index++] = ',';
                                                
                                                ivalue[ival_index++] = '[';
                                                
                                                level++;
                                                
                                                break;
                                                
                                            } else {
                                                
                                                ivalue[ival_index++] = ']';
                                                
                                                indices[level] = 0;
                                                
                                                level --;
                                                
                                            }
                                            
                                        }
                                        
                                        if (level < 0) break;
                                        
                                        while (level > kdims-1) {
                                            
                                            ivalue[ival_index++] = '[';
                                            
                                            level++;
                                            
                                        }
                                        
                                    }
                                    
                                }
                                
                            }
                            
                            ivalue[ival_index++] = '\0';
                            
                            cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);
                            
                            cbf_reportnez(cbf_set_typeofvalue(handle,"bkts"),errorcode);
                            
                            cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);
                            
                            data = (unsigned char *)ivalue;
                            
                        } else {
                            
                            size_t dimfast, dimmid, dimslow;
                            
                            dimmid = dimslow = 1;
                            
                            dimfast = dims[kdims-1];
                            
                            if (kdims > 1) dimmid = dims[kdims-2];
                            
                            if (kdims > 2) dimslow = total_dim/(dimfast*dimmid);
                            
                            cbf_reportnez(cbf_set_realarray_wdims_fs(handle,
                                                                     CBF_NIBBLE_OFFSET,target_row,data,
                                                                     type_size,total_dim,"little_endian",
                                                                     dimfast,dimmid,dimslow,0),errorcode);
                            
                        }
                        
                        
                    }
                    
                    
                } else if (type_class!= H5T_OPAQUE){
                    
                    char * hexvalue;
                    
                    char hexdigs[16] = {'0','1','2','3','4','5','6','7','8','9',
                        'a','b','c','d','e','f'};
                    
                    size_t ii;
                    
                    cbf_reportnez(cbf_alloc(((void **) &hexvalue),NULL,
                                            2*total_size+1,1),errorcode);
                    
                    hexvalue[2*total_size+1] = '\0';
                    
                    for (ii=0; ii< total_size; ii++) {
                        
                        hexvalue[(total_size-ii)*2-2] =
                        hexdigs[((int)(((unsigned char *)data))[ii])&0xF];
                        
                        hexvalue[(total_size-ii)*2-1] =
                        hexdigs[((int)((((unsigned char *)data))[ii])>>4)&0xF];
                        
                    }
                    
                    cbf_reportnez(cbf_set_value(handle,(const char *)(hexvalue)),errorcode);
                    
                    cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);
                    
                    data = (unsigned char *)hexvalue;
                    
                }
                
                
                if (value) {
                    
                    *value=data;
                    
                } else {
                    
                    cbf_reportnez(cbf_free((void **)&data,NULL),errorcode);
                }
                
            }
            
        }
        switch(type_order) {
            case H5T_ORDER_LE:
                byte_order = "H5T_ORDER_LE";
                break;
            case H5T_ORDER_BE:
                byte_order = "H5T_ORDER_BE";
                break;
            case H5T_ORDER_VAX:
                byte_order = "H5T_ORDER_VAX";
                break;
            case H5T_ORDER_MIXED:
                byte_order = "H5T_ORDER_MIXED";
                break;
            case H5T_ORDER_NONE:
                byte_order = "H5T_ORDER_LE";
                break;
            case -1:
                byte_order = ".";
                break;
            default: byte_order="UNKNOWN";
                break;
        }
        
        cbf_reportnez(cbf_require_column(handle,"h5_byte_order"),errorcode);
        
        cbf_reportnez(cbf_set_value(handle,byte_order),errorcode);
        
        if (type_size < 10) {
            
            sprintf(buffer,"%ld",(unsigned long)type_size);
            
        } else {
            
            sprintf(buffer,"0x%lx",(unsigned long)type_size);
            
        }
        
        cbf_reportnez(cbf_require_column(handle,"size"),errorcode);
        
        cbf_reportnez(cbf_set_value(handle,buffer),errorcode);
        
        if (base_type >=0) H5Tclose(base_type);
        
        if (native_type>=0) H5Tclose(native_type);
        
        H5garbage_collect();
        
        return CBF_SUCCESS;
    }
    
    
    /* Callback routine for objects in a group */
    
    
    herr_t cbf_object_visit(hid_t loc_id, const char *name,
                            const H5L_info_t *info,
                            void *op_data){
        
        int cbfrow;
        
        int errorcode;
        
        cbf_handle handle;
        
        haddr_t parent_addr;
        
        hid_t parent_id;
        
        unsigned int row;
        
        const char* parent_name;
        
        const char* grand_parent_name;
        
        int innexus;
        
        int incbf, incbfdb, incbfcat, incbfcol;
        
        hid_t group_id, dataset_id;
        
        herr_t retval;
        
        hsize_t i;
        
        char buffer[25];
        
        char digest[25];
        
        char *value;
        
        char cbftype[5];
        
        cbf_bookmark bookmark;
        
        cbf_bookmark saved_bookmark;
        
        H5O_info_t  objinfo;
        
        hid_t attrib_id,attrib_ds,attrib_type;
        
        hid_t dataset_ds, dataset_type;
        
        H5T_class_t dataset_type_class;
        
        ssize_t attrib_name_size;
        
        int attrib_num;
        
        unsigned int compression;
        
        int binary_id, bits, sign, type, checked_digest, realarray;
        
        const char *byteorder;
        
        size_t binsize;
        
        size_t dimover, dimfast, dimmid, dimslow;
        
        size_t padding;
        
        errorcode = 0;
        
        handle = ((cbf_h5Ovisithandle)op_data)->handle;
        
        if (!handle) return -1;
        
        /* skip the root group itself */
        
        if (name[0]== '.') return 0;
        
        cbf_h5failneg(H5Oget_info_by_name(loc_id,
                                          name, &objinfo, H5P_DEFAULT),CBF_FORMAT);
        parent_id = ((cbf_h5Ovisithandle)op_data)->parent_id;
        
        parent_addr = ((cbf_h5Ovisithandle)op_data)->parent_addr;
        
        parent_name = ((cbf_h5Ovisithandle)op_data)->parent_name;
        
        grand_parent_name = ((cbf_h5Ovisithandle)op_data)->grand_parent_name;
        
        innexus = ((cbf_h5Ovisithandle)op_data)->innexus;
        
        incbf = ((cbf_h5Ovisithandle)op_data)->incbf;
        
        incbfdb = ((cbf_h5Ovisithandle)op_data)->incbfdb;
        
        incbfcat = ((cbf_h5Ovisithandle)op_data)->incbfcat;
        
        incbfcol = ((cbf_h5Ovisithandle)op_data)->incbfcol;
        
        memmove(&saved_bookmark,&(((cbf_h5Ovisithandle)op_data)->bookmark),sizeof(cbf_bookmark));
        
        switch (objinfo.type) {
                
            case H5O_TYPE_GROUP:
                
                /* Skip duplicates */
                
                for (i=0; i < ((cbf_h5Ovisithandle)op_data)->path_size; i++) {
                    
                    if (objinfo.addr ==
                        ((cbf_h5Ovisithandle)op_data)->haddr_path[i])
                        return 0;
                    
                }
                
                if (((cbf_h5Ovisithandle)op_data)->path_size >=
                    ((cbf_h5Ovisithandle)op_data)->capacity) {
                    
                    size_t newcap;
                    
                    newcap = 2*((cbf_h5Ovisithandle)op_data)->capacity;
                    
                    cbf_reportnez(
                                  cbf_realloc(
                                              (void **)(&((cbf_h5Ovisithandle)op_data)->hid_path),
                                              NULL,sizeof(hid_t),newcap),errorcode);
                    cbf_reportnez(
                                  cbf_realloc(
                                              (void **)(&((cbf_h5Ovisithandle)op_data)->haddr_path),
                                              NULL,sizeof(haddr_t),newcap),errorcode);
                    
                    ((cbf_h5Ovisithandle)op_data)->capacity=newcap;
                    
                }
                
                (((cbf_h5Ovisithandle)op_data)->
                 haddr_path)[((cbf_h5Ovisithandle)op_data)->path_size] =
                objinfo.addr;
                
                group_id = H5Gopenx(loc_id,name);
                (((cbf_h5Ovisithandle)op_data)->
                 haddr_path)[((cbf_h5Ovisithandle)op_data)->path_size] =
                group_id;
                (((cbf_h5Ovisithandle)op_data)->path_size)++;
                
                /* We have a group
                 We need to add it to the H5_Groups category
                 in the H5 data block.
                 
                 If it has attributes, we need to add them to
                 the H5Attributes category
                 
                 If it has datasets, we will catch them when we
                 iterate again
                 
                 */
                
                cbf_reportnez(cbf_rewind_datablock(handle),errorcode);
                
                if (cbf_find_datablock(handle,"H5")) {
                    
                    cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);
                    
                }
                
                cbf_reportnez(cbf_require_category(handle,"H5_Groups"),errorcode);
                
                cbf_reportnez(cbf_new_row(handle),errorcode);
                
                cbf_reportnez(cbf_row_number(handle,&row),errorcode);
                
                cbf_reportnez(cbf_require_column(handle,"name"),errorcode);
                
                cbf_reportnez(cbf_set_value(handle,name),errorcode);
                
                cbf_reportnez(cbf_require_column(handle,"parent_name"),errorcode);
                
                cbf_reportnez(cbf_set_value(handle,parent_name),errorcode);
                
                cbf_reportnez(cbf_require_column(handle,"parent_id"),errorcode);
                
                if (!parent_addr) {
                    
                    cbf_reportnez(cbf_set_value(handle,"."),errorcode);
                    
                    cbf_reportnez(cbf_set_typeofvalue(handle,"null"),errorcode);
                    
                } else {
                    
                    sprintf(buffer,"0x%lx",(unsigned long)parent_addr);
                    
                    cbf_reportnez(cbf_set_value(handle,buffer),errorcode);
                    
                }
                
                cbf_reportnez(cbf_require_column(handle,"id"),errorcode);
                
                sprintf(buffer,"0x%lx",(unsigned long)objinfo.addr);
                
                cbf_reportnez(cbf_set_value(handle,buffer),errorcode);
                
                attrib_num = objinfo.num_attrs;
                
                cbf_reportnez(cbf_require_column(handle,"no_attributes"),errorcode);
                
                cbf_reportnez(cbf_set_integervalue(handle,attrib_num),errorcode);
                
                for (i=0; i < attrib_num; i++) {
                    
                    char * attrib_name;
                    attrib_id=H5Aopen_by_idx(group_id,".",
                                             H5_INDEX_NAME,
                                             H5_ITER_INC,
                                             i,H5P_DEFAULT,H5P_DEFAULT);
                    attrib_ds = H5Aget_space(attrib_id);
                    attrib_type = H5Aget_type(attrib_id);
                    attrib_name_size = H5Aget_name(attrib_id,0,NULL);
                    cbf_reportnez(cbf_alloc(((void **) &attrib_name),NULL,
                                            attrib_name_size+1,1),errorcode);
                    
                    cbf_h5failneg(H5Aget_name(attrib_id,
                                              attrib_name_size+1,attrib_name),
                                  CBF_ARGUMENT);
                    cbf_h5ds_store(handle,objinfo.addr,
                                   name,-1,
                                   "H5_Group_attribute",
                                   attrib_id,
                                   attrib_ds,
                                   attrib_type,
                                   attrib_name,1, (void **)&value);
                    if (!cbf_cistrcmp(attrib_name,"NX_class")&& value) {
                        
                        cbf_reportnez(cbf_rewind_datablock(handle),errorcode);
                        
                        if (cbf_find_datablock(handle,"H5")) {
                            
                            cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);
                            
                        }
                        
                        
                        cbf_reportnez(cbf_require_category(handle,"H5_Groups"),errorcode);
                        
                        cbf_reportnez(cbf_find_column(handle,"id"),errorcode);
                        
                        cbf_reportnez(cbf_select_row(handle,row),errorcode);
                        
                        cbf_reportnez(cbf_require_column(handle,"NX_class"),errorcode);
                        
                        cbf_reportnez(cbf_set_value(handle,value),errorcode);
                        
                        if (!cbf_cistrcmp(value,"NXentry")) {
                            
                            ((cbf_h5Ovisithandle)op_data)->innexus = 1;
                            
                        }
                        
                        if (!cbf_cistrcmp(value,"CBF_cbf")||!cbf_cistrcmp(value,"NXcbf")) {
                            
                            ((cbf_h5Ovisithandle)op_data)->incbf = 1;
                            
                        }
                        
                        if (!cbf_cistrcmp(value,"CBF_cbfdb") || !cbf_cistrcmp(value,"NXcbfdb")) {
                            
                            ((cbf_h5Ovisithandle)op_data)->incbfdb = 1;
                            
                            cbf_get_bookmark(handle,&bookmark);
                            
                            if (cbf_find_datablock(handle,name)) {
                                
                                cbf_reportnez(cbf_new_datablock(handle,name),errorcode);
                                
                            }
                            
                            cbf_get_bookmark(handle,
                                             &(((cbf_h5Ovisithandle)op_data)->bookmark));
                            
                            cbf_goto_bookmark(handle,bookmark);
                            
                        }
                        
                        if ((!cbf_cistrcmp(value,"CBF_cbfcat")||!cbf_cistrcmp(value,"NXcbfcat"))&& saved_bookmark.datablock) {
                            
                            ((cbf_h5Ovisithandle)op_data)->incbfcat = 1;
                            
                            cbf_get_bookmark(handle,&bookmark);
                            
                            cbf_goto_bookmark(handle,saved_bookmark);
                            
                            if (cbf_find_category(handle,name)) {
                                
                                cbf_reportnez(cbf_new_category(handle,name),errorcode);
                                
                            }
                            
                            cbf_get_bookmark(handle,
                                             &(((cbf_h5Ovisithandle)op_data)->bookmark));
                            
                            cbf_goto_bookmark(handle,bookmark);
                            
                            
                        }
                        
                        if ((!cbf_cistrcmp(value,"CBF_cbfcol")
                             ||!cbf_cistrcmp(value,"NXcbfcol"))
                            && saved_bookmark.category) {
                            
                            ((cbf_h5Ovisithandle)op_data)->incbfcol = 1;
                            
                            cbf_get_bookmark(handle,&bookmark);
                            
                            cbf_goto_bookmark(handle,saved_bookmark);
                            
                            if (cbf_find_column(handle,name)) {
                                
                                cbf_reportnez(cbf_new_column(handle,name),errorcode);
                                
                            }
                            
                            cbf_get_bookmark(handle,
                                             &(((cbf_h5Ovisithandle)op_data)->bookmark));
                            
                            cbf_goto_bookmark(handle,bookmark);
                            
                        }
                        
                    }
                    
                    
                    cbf_reportnez(cbf_free((void **)&attrib_name,NULL),errorcode);
                    if (value) {
                        cbf_reportnez(cbf_free((void **)&value, NULL),errorcode);
                    }
                    H5Tclose(attrib_type);
                    H5Sclose(attrib_ds);
                    H5Aclose(attrib_id);
                }
                
                ((cbf_h5Ovisithandle)op_data)->parent_addr = objinfo.addr;
                
                ((cbf_h5Ovisithandle)op_data)->parent_id = group_id;
                
                cbf_reportnez(cbf_alloc((void **) &(((cbf_h5Ovisithandle)op_data)->parent_name),NULL,
                                        strlen(name)+1,1),errorcode);
                
                ((cbf_h5Ovisithandle)op_data)->grand_parent_name = parent_name;
                
                if (!name) return -1;
                
                strcpy((char *)((cbf_h5Ovisithandle)op_data)->parent_name,name);
                
                retval = H5Literate_by_name(loc_id, name,H5_INDEX_NAME,
                                            H5_ITER_INC,
                                            NULL,
                                            cbf_object_visit,op_data,H5P_DEFAULT);
                
                H5Gclose(group_id);
                
                cbf_reportnez(cbf_free((void **)(&((cbf_h5Ovisithandle)op_data)->parent_name),NULL),errorcode);
                
                (((cbf_h5Ovisithandle)op_data)->path_size)--;
                
                ((cbf_h5Ovisithandle)op_data)->parent_id = parent_id;
                
                ((cbf_h5Ovisithandle)op_data)->parent_addr = parent_addr;
                
                ((cbf_h5Ovisithandle)op_data)->parent_name = parent_name;
                
                ((cbf_h5Ovisithandle)op_data)->grand_parent_name = grand_parent_name;
                
                ((cbf_h5Ovisithandle)op_data)->innexus = innexus;
                
                ((cbf_h5Ovisithandle)op_data)->incbf = incbf;
                
                ((cbf_h5Ovisithandle)op_data)->incbfdb = incbfdb;
                
                ((cbf_h5Ovisithandle)op_data)->incbfcat = incbfcat;
                
                ((cbf_h5Ovisithandle)op_data)->incbfcol = incbfcol;
                
                return retval;
                break;
                
            case H5O_TYPE_DATASET:
                
                dataset_id = H5Dopen2(loc_id,name,H5P_DEFAULT);
                
                /* We have a dataset
                 We need to add it to the H5_Datasets category
                 in the current data block.
                 
                 If it has attributes, we need to add them to
                 the H5_Attributes category
                 
                 */
                
                
                cbf_reportnez(cbf_rewind_datablock(handle),errorcode);
                
                if (cbf_find_datablock(handle,"H5")) {
                    
                    cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);
                    
                }
                
                cbf_reportnez(cbf_require_category(handle,"H5_Datasets"),errorcode);
                
                cbf_reportnez(cbf_new_row(handle),errorcode);
                
                cbf_reportnez(cbf_row_number(handle,&row),errorcode);
                
                cbf_reportnez(cbf_require_column(handle,"name"),errorcode);
                
                cbf_reportnez(cbf_set_value(handle,name),errorcode);
                
                cbf_reportnez(cbf_require_column(handle,"parent_name"),errorcode);
                
                cbf_reportnez(cbf_set_value(handle,parent_name),errorcode);
                
                cbf_reportnez(cbf_require_column(handle,"parent_id"),errorcode);
                
                if (!parent_addr) {
                    
                    cbf_reportnez(cbf_set_value(handle,"."),errorcode);
                    
                    cbf_reportnez(cbf_set_typeofvalue(handle,"null"),errorcode);
                    
                } else {
                    
                    sprintf(buffer,"0x%lx",(unsigned long)parent_addr);
                    
                    cbf_reportnez(cbf_set_value(handle,buffer),errorcode);
                    
                }
                
                cbf_reportnez(cbf_require_column(handle,"id"),errorcode);
                
                sprintf(buffer,"0x%lx",(unsigned long)objinfo.addr);
                
                cbf_reportnez(cbf_set_value(handle,buffer),errorcode);
                
                attrib_num = objinfo.num_attrs;
                
                cbf_reportnez(cbf_require_column(handle,"no_attributes"),errorcode);
                
                cbf_reportnez(cbf_set_integervalue(handle,attrib_num),errorcode);
                
                dimover = 0;
                
                binsize = 0;
                
                compression = 0;
                
                binary_id = 0;
                
                bits = 0;
                
                sign = 0;
                
                type = 0;
                
                byteorder = " ";
                
                dimfast=dimslow=dimmid = 0;
                
                padding = 0;
                
                cbftype[0] = '\0';
                
                for (i=0; i < attrib_num; i++) {
                    
                    char * attrib_name;
                    attrib_id=H5Aopen_by_idx(dataset_id,".",
                                             H5_INDEX_NAME,
                                             H5_ITER_INC,
                                             i,H5P_DEFAULT,H5P_DEFAULT);
                    attrib_ds = H5Aget_space(attrib_id);
                    attrib_type = H5Aget_type(attrib_id);
                    attrib_name_size = H5Aget_name(attrib_id,0,NULL);
                    cbf_reportnez(cbf_alloc(((void **) &attrib_name),NULL,
                                            attrib_name_size+1,1),errorcode);
                    
                    cbf_h5failneg(H5Aget_name(attrib_id,
                                              attrib_name_size+1,attrib_name),
                                  CBF_ARGUMENT);
                    cbf_h5ds_store(handle,objinfo.addr,
                                   name,-1,
                                   "H5_Dataset_attribute",
                                   attrib_id,
                                   attrib_ds,
                                   attrib_type,
                                   attrib_name,1,(void **)&value);
                    if (*value) {
                        
                        
                        cbf_reportnez(cbf_rewind_datablock(handle),errorcode);
                        
                        if (cbf_find_datablock(handle,"H5")) {
                            
                            cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);
                            
                        }
                        
                        cbf_reportnez(cbf_require_category(handle,"H5_Datasets"),errorcode);
                        
                        cbf_reportnez(cbf_find_column(handle,"id"),errorcode);
                        
                        cbf_reportnez(cbf_select_row(handle,row),errorcode);
                        
                        cbf_reportnez(cbf_require_column(handle,attrib_name),errorcode);
                        
                        cbf_reportnez(cbf_set_value(handle,value),errorcode);
                        
                        if (!cbf_cistrcmp(attrib_name,"compression")) {
                            compression=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"binid")) {
                            binary_id=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"bits")) {
                            bits=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"sign")) {
                            sign = (int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"bintype")) {
                            type=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"checked_digest")) {
                            checked_digest=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"real")) {
                            realarray = (int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"byteorder")) {
                            byteorder=
                            (value[0]=='l'||value[0]=='L')?
                            "little_endian":"big_endian";
                        } else if (!cbf_cistrcmp(attrib_name,"size")) {
                            binsize = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"digest")
                                   ||!cbf_cistrcmp(attrib_name,"MD5_digest")) {
                            strncpy(digest,value,24);
                            digest[24] = 0;
                        } else if (!cbf_cistrcmp(attrib_name,"dimover")) {
                            dimover = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"dimfast")) {
                            dimfast = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"dimmid")) {
                            dimmid = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"dimslow")) {
                            dimslow = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"padding")) {
                            padding = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"cbftype")) {
                            strncpy(cbftype,value,4);
                            cbftype[4] = '\0';
                        }
                    }
                    
                    cbf_reportnez(cbf_free((void **)&attrib_name,NULL),errorcode);
                    if (value) cbf_reportnez(cbf_free((void **)&value, NULL),errorcode);
                    H5Tclose(attrib_type);
                    H5Sclose(attrib_ds);
                    H5Aclose(attrib_id);
                }
                
                dataset_ds         = H5Dget_space(dataset_id);
                dataset_type       = H5Dget_type(dataset_id);
                dataset_type_class = H5Tget_class(dataset_type);
                
                cbf_h5ds_store(handle,objinfo.addr,
                               parent_name,row,
                               "H5_Datasets",
                               dataset_id,
                               dataset_ds,
                               dataset_type,
                               name,0,(void **)&value);
                
                if (incbfcol&&value) {
                    
                    cbfrow = (int)strtol(name,NULL,0);
                    
                    cbf_get_bookmark(handle,&bookmark);
                    
                    cbf_goto_bookmark(handle,saved_bookmark);
                    
                }
                
                if (incbfcol&&binsize&&value) {
                    
                    size_t elsize, nelem;
                    
                    cbf_node * column;
                    
                    cbf_file *tempfile;
                    
                    long start;
                    
                    unsigned int localrow, ii;
                    
                    elsize = (bits+CHAR_BIT-1)/CHAR_BIT;
                    
                    nelem = (binsize+elsize-1)/elsize;
                    
                    if (dimover <=0) dimover = nelem;
                    
                    localrow = row;
                    
                    if (nelem > 0 && elsize > 0) {
                        
                        if (incbfcol) {
                            
                            unsigned int rows;
                            
                            cbf_reportnez(cbf_count_rows(handle,&rows),errorcode);
                            
                            if (cbfrow >= rows) {
                                
                                for (ii=rows; ii <= cbfrow; ii++) {
                                    
                                    cbf_reportnez(cbf_new_row(handle),errorcode);
                                }
                                
                            }
                            
                            cbf_select_row(handle,cbfrow);
                            
                            localrow = cbfrow;
                            
                        } else {
                            
                            
                            cbf_reportnez(cbf_rewind_datablock(handle),errorcode);
                            
                            if (cbf_find_datablock(handle,"H5")) {
                                
                                cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);
                                
                            }
                            
                            
                            cbf_reportnez(cbf_require_category(handle,"H5_Datasets"),errorcode);
                            
                            cbf_reportnez(cbf_find_column(handle,"id"),errorcode);
                            
                            cbf_reportnez(cbf_select_row(handle,localrow),errorcode);
                            
                            cbf_reportnez(cbf_require_column(handle,"value"),errorcode);
                            
                        }
                        
                        column = handle->node;
                        
                        if (dataset_type_class == H5T_OPAQUE) {
                            
                            /* If we have stored as an opqaue dataset, keep it that way */
                            
                            
                            /* Remove the old value */
                            
                            cbf_reportnez (cbf_set_columnrow (column, localrow, NULL, 1),errorcode)
                            
                            
                            /* Get the temporary file */
                            
                            cbf_reportnez (cbf_open_temporary (column->context, &tempfile),errorcode)
                            
                            
                            /* Move to the end of the temporary file */
                            
                            if (cbf_set_fileposition (tempfile, 0, SEEK_END))
                                
                                return CBF_FILESEEK | cbf_delete_fileconnection (&tempfile);
                            
                            
                            /* Get the starting location */
                            
                            if (cbf_get_fileposition (tempfile, &start))
                                
                                return CBF_FILETELL | cbf_delete_fileconnection (&tempfile);
                            
                            
                            /* Discard any bits in the buffers */
                            
                            cbf_reportnez (cbf_reset_bits (tempfile),errorcode)
                            
                            /* Add the binary data to the temporary file */
                            
                            if (!cbf_set_output_buffersize(tempfile,binsize))  {
                                
                                memmove((void *)(tempfile->characters+tempfile->characters_used),
                                        (void *)value,binsize);
                                
                                tempfile->characters_used+=binsize;
                                
                            }
                            
                            cbf_onfailnez(cbf_set_bintext(column,localrow,CBF_TOKEN_TMP_BIN,
                                                          binary_id,tempfile,start,binsize,
                                                          1,digest,bits,sign,realarray,byteorder,
                                                          dimover, dimfast, dimmid, dimslow,
                                                          padding,compression),
                                          cbf_delete_fileconnection (&tempfile));
                            
                            cbf_onfailnez(cbf_flush_bits(tempfile),
                                          cbf_delete_fileconnection (&tempfile));
                        } else {
                            
                            /* If this is not an opqaue object, then recompress
                             using the attributes */
                            
                            cbf_reportnez(cbf_set_binary(handle->node,
                                                         handle->row,
                                                         compression,
                                                         binary_id,
                                                         (void *) value,
                                                         (bits+7)/8,
                                                         sign,
                                                         dimover,
                                                         realarray,
                                                         "little_endian",
                                                         dimover,
                                                         dimfast,
                                                         dimmid,
                                                         dimslow,
                                                         padding ), errorcode);
                            
                        }
                        
                    }
                    
                } else {
                    
                    if (incbfcol && value) {
                        
                        unsigned int rows;
                        
                        cbf_reportnez(cbf_count_rows(handle,&rows),errorcode);
                        
                        if (cbfrow >= rows) {
                            
                            cbf_reportnez(cbf_insert_row(handle,cbfrow),errorcode);
                            
                        }
                        
                        cbf_reportnez(cbf_select_row(handle,cbfrow),errorcode);
                        
                        cbf_reportnez(cbf_set_value(handle,value),errorcode);
                        
                        if (cbftype[0] && cbf_cistrcmp(cbftype,"(null)")
                            && strlen(cbftype) == 4) {
                            
                            cbf_reportnez(cbf_set_typeofvalue(handle,cbftype),errorcode);
                            
                        }
                        
                    }
                    
                }
                
                if (incbfcol&&value) {
                    
                    cbf_reportnez(cbf_goto_bookmark(handle,bookmark),errorcode);
                    
                }
                if (value) cbf_reportnez(cbf_free((void **)&value, NULL),errorcode);
                H5Dclose(dataset_id);
                H5Sclose(dataset_ds);
                H5Tclose(dataset_type);
                
                break;
                
            case H5O_TYPE_NAMED_DATATYPE:
                
                break;
                
            default:
                
                return CBF_FORMAT;
                
                
                
        }
        
        return 0;
    }
    /* Read an HDF5 file */
    
    int cbf_read_h5file(cbf_handle handle, cbf_h5handle h5handle, int flags) {
        
        cbf_node *node;
        
        cbf_h5Ovisit_struct h5Ovisit;
        
        if (!handle || !h5handle || !h5handle->hfile ) return CBF_ARGUMENT;
        
        /* Move the flags into the h5handle */
        
        h5handle -> flags = flags;
        
        /* Delete the old datablocks */
        
        if( handle->commentfile) cbf_failnez (cbf_free_file (&(handle->commentfile)));
        
        cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT));
        
        cbf_failnez (cbf_set_children (node, 0))
        
        handle->node = node;
        
        cbf_failnez (cbf_reset_refcounts(handle->dictionary));
        
        /* register the CBF compressions if requested */
        
        if (h5handle->flags & CBF_H5_REGISTER_COMPRESSIONS) {
            
            if (!H5Zfilter_avail(CBF_H5Z_FILTER_CBF)) {
                
                cbf_h5failneg(H5Zregister(CBF_H5Z_CBF),CBF_ALLOC);
                
            }
        }
        
        h5Ovisit.handle = handle;
        
        h5Ovisit.h5handle = h5handle;
        
        h5Ovisit.parent_addr = 0;
        
        h5Ovisit.parent_id = h5handle->hfile;
        
        h5Ovisit.parent_name = "/";
        
        h5Ovisit.grand_parent_name = NULL;
        
        h5Ovisit.incbf = h5Ovisit.incbfdb = h5Ovisit.incbfcat = h5Ovisit.incbfcol = 0;
        
        h5Ovisit.innexus = 0;
        
        h5Ovisit.bookmark.datablock = NULL;
        
        h5Ovisit.bookmark.category = NULL;
        
        h5Ovisit.bookmark.column = NULL;
        
        h5Ovisit.bookmark.haverow = 0;
        
        
        cbf_failnez(cbf_alloc ((void **) (&(h5Ovisit.hid_path)), NULL,
                               sizeof(hid_t), 1));
        
        cbf_failnez(cbf_alloc ((void **) (&(h5Ovisit.haddr_path)), NULL,
                               sizeof(haddr_t), 1));
        
        h5Ovisit.capacity = 1;
        
        h5Ovisit.path_size = 0;
        
        cbf_failnez(cbf_new_datablock(handle,"H5"));
        
        /* visit the groups in the file, starting with the root group */
        
        cbf_h5failneg(H5Literate(h5handle->hfile,
                                 H5_INDEX_NAME,
                                 H5_ITER_INC,
                                 NULL,
                                 cbf_object_visit,(void *)&h5Ovisit),CBF_FORMAT);
        
        return CBF_SUCCESS;
        
    }
    
    /* get a bookmark from the current information in a cbf handle */
    
    int cbf_get_bookmark(cbf_handle handle, cbf_bookmark * bookmark) {
        
        if (cbf_datablock_name(handle,&(bookmark->datablock))) {
            
            bookmark->datablock = NULL;
            
        }
        
        if (cbf_category_name(handle,&(bookmark->category))) {
            
            bookmark->category = NULL;
            
        }
        
        if (cbf_column_name(handle,&(bookmark->column))) {
            
            bookmark->column = NULL;
            
        }
        
        bookmark->haverow = 1;
        
        if (cbf_row_number(handle,&(bookmark->row))) {
            
            bookmark->row = 0;
            
            bookmark->haverow = 0;
            
        }
        
        return CBF_SUCCESS;
        
    }
    
    /* go to a bookmark in the cbf handle */
    
    int cbf_goto_bookmark(cbf_handle handle, cbf_bookmark bookmark) {
        
        unsigned int rows;
        
        if (bookmark.datablock) {
            
            cbf_failnez(cbf_rewind_datablock(handle));
            
            cbf_failnez(cbf_find_datablock(handle,bookmark.datablock));
            
            if (bookmark.category) {
                
                cbf_failnez(cbf_rewind_category(handle));
                
                cbf_failnez(cbf_find_category(handle,bookmark.category));
                
                if (bookmark.column) {
                    
                    cbf_failnez(cbf_rewind_column(handle));
                    
                    cbf_failnez(cbf_find_column(handle,bookmark.column));
                    
                    if (bookmark.haverow) {
                        
                        cbf_failnez(cbf_count_rows(handle,&rows));
                        
                        if (bookmark.row < rows) {
                            
                            cbf_failnez(cbf_select_row(handle,bookmark.row));
                            
                        }
                        
                    }
                    
                }
                
                
            }
            
        }
        
        return CBF_SUCCESS;
	}
    
#ifdef __cplusplus
    
}

#endif
