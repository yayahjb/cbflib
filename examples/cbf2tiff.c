/**********************************************************************
 * cbf2tiff -- convert a cbf file to a tiff file                      *
 *                                                                    *
 * Version 0.9.8 23 June 2023                                         *
 *                                                                    *
 * (C) Copyright 2023 Herbert J. Bernstein                            *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 * WHILE YOU MAY ALTERNATIVE DISTRIBUTE THE API UNDER THE LGPL        *
 * YOU MAY ***NOT*** DISTRBUTE THIS PROGRAM UNDER THE LGPL            *
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<cbf.h>
#include	<cbf_string.h>
#include        <tiffio.h>

/****************************************************************/


int local_exit (int status);
int outerror(int err);

int outerror(int err)
{

    if ((err&CBF_FORMAT)==CBF_FORMAT)
        fprintf(stderr, " cif2cbf: The file format is invalid.\n");
    if ((err&CBF_ALLOC)==CBF_ALLOC)
        fprintf(stderr, " cif2cbf Memory allocation failed.\n");
    if ((err&CBF_ARGUMENT)==CBF_ARGUMENT)
        fprintf(stderr, " cif2cbf: Invalid function argument.\n");
    if ((err&CBF_ASCII)==CBF_ASCII)
        fprintf(stderr, " cif2cbf: The value is ASCII (not binary).\n");
    if ((err&CBF_BINARY)==CBF_BINARY)
        fprintf(stderr, " cif2cbf: The value is binary (not ASCII).\n");
    if ((err&CBF_BITCOUNT)==CBF_BITCOUNT)
        fprintf(stderr, " cif2cbf: The expected number of bits does"
                " not match the actual number written.\n");
    if ((err&CBF_ENDOFDATA)==CBF_ENDOFDATA)
        fprintf(stderr, " cif2cbf: The end of the data was reached"
                " before the end of the array.\n");
    if ((err&CBF_FILECLOSE)==CBF_FILECLOSE)
        fprintf(stderr, " cif2cbf: File close error.\n");
    if ((err&CBF_FILEOPEN)==CBF_FILEOPEN)
        fprintf(stderr, " cif2cbf: File open error.\n");
    if ((err&CBF_FILEREAD)==CBF_FILEREAD)
        fprintf(stderr, " cif2cbf: File read error.\n");
    if ((err&CBF_FILESEEK)==CBF_FILESEEK)
        fprintf(stderr, " cif2cbf: File seek error.\n");
    if ((err&CBF_FILETELL)==CBF_FILETELL)
        fprintf(stderr, " cif2cbf: File tell error.\n");
    if ((err&CBF_FILEWRITE)==CBF_FILEWRITE)
        fprintf(stderr, " cif2cbf: File write error.\n");
    if ((err&CBF_IDENTICAL)==CBF_IDENTICAL)
        fprintf(stderr, " cif2cbf: A data block with the new name already exists.\n");
    if ((err&CBF_NOTFOUND)==CBF_NOTFOUND)
        fprintf(stderr, " cif2cbf: The data block, category, column or"
                " row does not exist.\n");
    if ((err&CBF_OVERFLOW)==CBF_OVERFLOW)
        fprintf(stderr, " cif2cbf: The number read cannot fit into the "
                "destination argument.\n        The destination has been set to the nearest value.\n");
    if ((err& CBF_UNDEFINED)==CBF_UNDEFINED)
        fprintf(stderr, " cif2cbf: The requested number is not defined (e.g. 0/0).\n");
    if ((err&CBF_NOTIMPLEMENTED)==CBF_NOTIMPLEMENTED)
        fprintf(stderr, " cif2cbf: The requested functionality is not yet implemented.\n");
    if ((err&CBF_H5ERROR)==CBF_H5ERROR)
        fprintf(stderr, " cif2cbf: HDF5 API error.\n");
    return 0;

}

#undef cbf_failnez
#undef cbf_onfailnez

#ifndef __FILE__

#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d \n", err); outerror(err); local_exit (-1); }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d \n", err); \
{ c; } outerror(err); local_exit (-1); }}
#else
#ifndef __func__
#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d at %s:%d\n", err,__FILE__,__LINE__); outerror(err); local_exit (-1); }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d at %s:%d\n", err,__FILE__,__LINE__); \
{ c; } outerror(err); local_exit (-1); }}
#else
#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d at %s:%d(%s)\n", err,__FILE__,__LINE__,__func__); outerror(err); local_exit (-1); }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d at %s:%d(%s)\n", err,__FILE__,__LINE__,__func__); \
{ c; } outerror(err); local_exit (-1); }}

#endif
#endif


static void usage( void )
{
                fprintf(stderr,"Usage: cbf2tiff [-i]infile.cbf [-o]outfile.tif\n");
}


/*
  Converts a CBF to a tiff preserving only minimal image
  metadata:

*/





int	main(int argc, char *argv[])
{
	FILE		*in;
        TIFF            *out;
        cbf_handle      cif=NULL;
	char *		in_filename;
        char *          out_filename;
        int iarg;
  

	if(argc < 2)
	{
		usage();
		exit(0);
	}

        for (iarg=1; iarg < argc; iarg++) {

          if (argv[iarg][0]=='-' && argv[iarg][1]=='i') {
            if (argv[iarg][2]!=0) {
              in_filename=argv[iarg]+2;
            } else {
              if (iarg +1 < argc) {
                iarg++;
                in_filename=argv[iarg];
              } else {
                usage();
                exit(1);
              }
            }
          } else {
            in_filename=argv[iarg];
          }
          iarg++;
          if (argv[iarg][0]=='-' && argv[iarg][1]=='o') {
            if (argv[iarg][2]!=0) {
              out_filename=argv[iarg]+2;
            } else {
              if (iarg +1 < argc) {
                iarg++;
                out_filename=argv[iarg];
              } else {
                usage();
                exit(1);
              }
            }
          } else {
            out_filename=argv[iarg];
          }
          break;
        }

        fprintf(stdout," cbf2tiff: converting '%s' to '%s' \n",in_filename,out_filename);

        cbf_failnez(cbf_make_handle (&cif));

        if (!(in = fopen (in_filename,"rb"))) {
          fprintf(stderr,"cbf2tiff: could not open the input cbf file '%s'\n", in_filename);
          exit (1);
        }

        cbf_failnez(cbf_read_widefile(cif,in,MSG_DIGEST));

	cbf_failnez(cbf_rewind_datablock(cif));

        cbf_failnez(cbf_find_tag(cif,"_array_data.data"));

        {
          int binary_id, elsigned, elunsigned;
          size_t elements, elsize, row;
          int minelement, maxelement;
          unsigned int cifcompression;
          int realarray;
          const char *byteorder;
          size_t fastdim, middim, slowdim, padding;
          void * array;
          void * strip;
          size_t elements_read;
          int sampleperpixel=1;

          cbf_failnez(cbf_get_arrayparameters_wdims_fs(cif,
            &cifcompression,
            &binary_id,
            &elsize,
            &elsigned,
            &elunsigned,
            &elements,
            &minelement,
            &maxelement,
            &realarray,
            &byteorder,
            &fastdim,
            &middim,
            &slowdim,
            &padding));
          if (fastdim < 1) fastdim = 1;
          if (middim < 1) middim = 1;
          if (slowdim < 1) slowdim = 1;
          if ((array=malloc(elsize*elements))&&(strip=malloc(elsize*fastdim))) {
            if (!realarray) {
              cbf_failnez (cbf_get_integerarray(
                cif, &binary_id, array, elsize, elsigned,
                elements, &elements_read));
            } else  {
              cbf_failnez (cbf_get_realarray(
                cif, &binary_id, array, elsize,
                elements, &elements_read));
              elsigned=1;
            }
            out=TIFFOpen(out_filename, "w");
            TIFFSetField (out, TIFFTAG_IMAGEWIDTH, fastdim);  // set the width of the image
            TIFFSetField(out, TIFFTAG_IMAGELENGTH, middim*slowdim);    // set the height of the image
            TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, sampleperpixel);   // set number of channels per pixel
            TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, 8*elsize);    // set the size of the channels
            TIFFSetField(out, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);    // set the origin of the image.
            if (realarray) {
              TIFFSetField(out, TIFFTAG_DATATYPE, SAMPLEFORMAT_IEEEFP);
            } else if (elsigned) {
              TIFFSetField(out, TIFFTAG_DATATYPE, SAMPLEFORMAT_INT);
            } else {
              TIFFSetField(out, TIFFTAG_DATATYPE, SAMPLEFORMAT_UINT);
            }
            TIFFSetField(out, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
            TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
            TIFFSetField(out, TIFFTAG_ROWSPERSTRIP, TIFFDefaultStripSize(out, fastdim));
            for (row = 0; row < middim*slowdim; row++) {
              memcpy(strip,array+fastdim*elsize*row,fastdim*elsize);
              if (TIFFWriteScanline(out,strip,row,0)<0) {
                cbf_failnez(CBF_FILEWRITE);
                exit(1);
              }
            }
            TIFFClose(out);
            free(strip);
            free(array);
          } else {
            fprintf(stderr,
              "cbf2tiff: Failed to allocate memory %ld bytes\n",
              (long) (elsize*elements+elsize*fastdim));
            exit(1);
          }
}

        exit(0);
}

int local_exit (int status)
{
    exit(status);
    return 1; /* avoid warnings */
}


