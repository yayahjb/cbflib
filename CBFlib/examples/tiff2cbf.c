/**********************************************************************
 * tiffcbf -- convert a tiff file to a cbf file                       *
 *                                                                    *
 * Version 0.7.6 28 June 2006                                         *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006 Herbert J. Bernstein                            *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 * WHILE YOU MAY ALTERNATIVE DISTRIBUTE THE API UNDER THE LGPL        *
 * YOU MAY ***NOT*** DISTRBUTE THIS PROGRAM UNDER THE LGPL            *
 *                                                                    *                                                                    *
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


#include "cbf.h"
#include <tiffio.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

int local_exit(int status) {
  exit(status);
  return status;    /* to avoid warning messages */
}


#undef cbf_failnez
#define cbf_failnez(x) \
 {int err; \
  err = (x); \
  if (err) { \
    fprintf(stderr,"\nCBFlib fatal error %x \n",err); \
    local_exit(-1); \
  } \
 }


int main (int argc, char *argv [])
{
    FILE *out;
    TIFF *tif;
    cbf_handle cbf;
    clock_t a,b;
    
    uint32 width;
    uint32 height;
    uint32 npixels;
    unsigned char * raster;
    
    int imageno;
    
     
    /* Usage */ 
    
    if (argc < 3)
    {
        fprintf (stderr, "\n Usage: %s tifffile cbffile\n", argv [0]);
        exit (2);
    }
    
    
    /* Read the tiff image */
        
    a = clock ();
    
    if (!(tif=TIFFOpen(argv[1], "r"))) {
        
        fprintf(stderr," %s unable to open tiff image %s, abort\n", argv[0], argv[1]);
        local_exit(-1);
    }
    
    b = clock ();
    
    fprintf (stderr, " Time to read the image: %.3fs\n", ((b - a) * 1.0) / CLOCKS_PER_SEC);
    
    
    /* Make a cbf version of the image */
    
    a = clock ();
    
    
    /* Create the cbf */
    
    cbf_failnez (cbf_make_handle (&cbf))
    
    
    /* Make a new data block */
    
    cbf_failnez (cbf_new_datablock (cbf, "image_1"))
    
    
    
    imageno = 0;
    cbf_failnez (cbf_require_category     (cbf, "array_data"))
    do {
        char buffer[40];
        char * headstring;
        size_t headsize, nheadsize;
        unsigned int rows;
        tstrip_t nstrips, strip;
        tsize_t stripsize;
        size_t totread;
        int elsize, elsign, real, plex, treturn;
        uint16 sampleformat, samplesperpixel, bitspersample, planarconfig;
        size_t dimslow, dimmid, dimfast;
        
        plex = 1;
        real = 0;
        elsign = 0;
        
        imageno++;      /* bump the image number, starting with 1
         versus the tiff directory number that starts
         with zero */
        
        /* Make or add to the _array_data category */
        
        cbf_failnez (cbf_require_column       (cbf, "header_convention"))
        cbf_failnez (cbf_count_rows           (cbf, &rows))
        while (rows < imageno) {
            cbf_failnez (cbf_new_row          (cbf))
            rows++;
        }
        cbf_failnez (cbf_select_row           (cbf,imageno-1))
        cbf_failnez (cbf_set_value            (cbf, "TIFF"))
        cbf_failnez (cbf_require_column       (cbf, "array_id"))
        sprintf(buffer,"image_%d",imageno);
        cbf_failnez (cbf_set_value            (cbf, buffer))
        cbf_failnez (cbf_require_column       (cbf, "binary_id"))
        cbf_failnez (cbf_set_integervalue     (cbf, imageno))
        cbf_failnez (cbf_require_column       (cbf, "header_contents"))
        headsize = 1+TIFFSNPrintDirectory(tif,buffer,0,TIFFPRINT_COLORMAP|TIFFPRINT_CURVES);
        headstring = (char *) _TIFFmalloc(headsize);
        if (!headstring) {
            cbf_failnez(CBF_ALLOC);
        }
        nheadsize = TIFFSNPrintDirectory(tif,headstring,headsize-1,TIFFPRINT_COLORMAP|TIFFPRINT_CURVES);
        if (nheadsize > headsize-1) {
            _TIFFfree(headstring);
            cbf_failnez(CBF_ALLOC);
        }
        cbf_onfailnez(cbf_set_value          (cbf,headstring),_TIFFfree(headstring));
        _TIFFfree(headstring);

        cbf_failnez (cbf_require_column       (cbf, "data"))
        treturn = TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
        if (treturn != 1) cbf_failnez(CBF_ARGUMENT);
        dimfast = width;
        treturn = TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height);
        if (treturn != 1) cbf_failnez(CBF_ARGUMENT);
        dimmid = height;
        dimslow = 1;
        treturn = TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sampleformat);
        if (treturn != 1) cbf_failnez(CBF_ARGUMENT);
        treturn = TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
        if (treturn != 1) cbf_failnez(CBF_ARGUMENT);
        treturn = TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitspersample);
        if (treturn != 1) cbf_failnez(CBF_ARGUMENT);
        treturn = TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarconfig);
        if (treturn != 1) cbf_failnez(CBF_ARGUMENT);
        switch ( bitspersample ) {
            case 8:                elsize = 1; break;
            case 16:               elsize = 2; break;
            case 32:               elsize = 4; break;
            case 64:               elsize = 8; break;
            default:               cbf_failnez(CBF_FORMAT);
        }
        switch ( sampleformat ) {
            case SAMPLEFORMAT_UINT:         elsign = 0; real = 0; plex = 1; break;    /* !unsigned integer data */
            case SAMPLEFORMAT_INT:	        elsign = 1; real = 0; plex = 1; break;    /* !signed integer data */
            case SAMPLEFORMAT_IEEEFP:       elsign = 1; real = 1; plex = 1; break;    /* !IEEE floating point data */
            case SAMPLEFORMAT_VOID:         cbf_failnez(CBF_FORMAT);                  /* !untyped data */
            case SAMPLEFORMAT_COMPLEXINT:   elsign = 1; real = 0; plex = 2; 
                                                               elsize /=2; break;     /* !complex signed int */
            case SAMPLEFORMAT_COMPLEXIEEEFP:elsign = 1; real = 1; plex = 2; 
                                                               elsize /=2; break;     /* !complex ieee floating */
            default:                       cbf_failnez(CBF_FORMAT);
        }
        switch ( planarconfig ) {
            case PLANARCONFIG_CONTIG:
                plex *= samplesperpixel;
                samplesperpixel = 1;
                if (plex > 1) {
                    dimslow = dimmid;
                    dimmid = dimfast;
                    dimfast = plex;
                }
                break;
            case PLANARCONFIG_SEPARATE:
                if (plex > 1 && samplesperpixel > 1) {
                    dimfast = dimfast*plex;
                    dimslow = samplesperpixel;
                } else if (plex == 1 && samplesperpixel > 1 ) {
                    dimslow = samplesperpixel;
                } else if (plex > 1 && samplesperpixel == 1) {
                    dimslow = dimmid;
                    dimmid = dimfast;
                    dimfast = plex;                    
                }
        }
            
        npixels = dimslow*dimmid*dimfast;
        nstrips = TIFFNumberOfStrips(tif);
        stripsize = TIFFStripSize(tif);
        raster = (unsigned char *) _TIFFmalloc(stripsize*nstrips+stripsize-1);
        elsize = stripsize*nstrips/npixels;
        totread = 0;
        for (strip = 0; strip < nstrips; strip++) {
            totread +=TIFFReadEncodedStrip(tif, strip, raster+strip*stripsize, stripsize);
        }
        if(real){
            cbf_failnez (cbf_set_realarray_wdims_fs (cbf, CBF_NONE, imageno,
                                                        (void *)raster, elsize,
                                                        npixels,
                                                        "little_endian",dimfast,dimmid,dimslow,0 ))
        } else {
            cbf_failnez (cbf_set_integerarray_wdims_fs (cbf, CBF_NONE, imageno,
                                                    (void *)raster, elsize, elsign,
                                                    npixels,
                                                    "little_endian",dimfast,dimmid,dimslow,0 ))
        }
        
        _TIFFfree(raster);
        
    } while (TIFFReadDirectory(tif));
        
        
    
    
    /* Write the new file */
    
    out = fopen (argv [2], "w+b");
    
    if (!out)
    {
        fprintf (stderr, " Couldn't open the CBF file %s\n", argv [2]);
        
        exit (1);
    }
    
    cbf_failnez (cbf_write_file (cbf, out, 1, CBF, MSG_DIGEST | MIME_HEADERS  , 0))
    
    
    
    /* Free the cbf */
    
    cbf_failnez (cbf_free_handle (cbf))
    
    b = clock ();
    
    fprintf (stderr, " Time to write the CBF image: %.3fs\n", 
             ((b - a) * 1.0) / CLOCKS_PER_SEC); 
    
    
      
    
    /* Free the tiff images */
            
    TIFFClose(tif);
    
    
    /* Success */
    
    return 0;
}
