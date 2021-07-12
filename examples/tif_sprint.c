/* tif_sprint.c
 
 -- variant of tif_print.c for printing to strings
 using snprintf
 
 H. J. Bernstein, 26 Dec 2010
 Revised for 4.0.6 version, 3 Nov 2016
 
 */

/**********************************************************************
 *                                                                    *
 * PORTIONS OF THIS CODE ARE DERIVED FROM CODE IN LIBTIFF AND ARE     *
 * SUBJECT TO THE FOLLOWING COPYRIGHT NOTICE                          *
 *                                                                    *
 **********************************************************************
 * Copyright (c) 1988-1997 Sam Leffler                                *
 * Copyright (c) 1991-1997 Silicon Graphics, Inc.                     *
 *                                                                    *
 * Permission to use, copy, modify, distribute, and sell this software*
 * and its documentation for any purpose is hereby granted without    *
 * fee, provided that (i) the above copyright notices and this        *
 * permission notice appear in all copies of the software and related *
 * documentation, and (ii) the names of Sam Leffler and Silicon       *
 * Graphics may not be used in any advertising or publicity relating  *
 * to the software without the specific, prior written permission of  *
 * Sam Leffler and Silicon Graphics.                                  *
 *                                                                    *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, *
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY   *
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.   *
 *                                                                    *
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR    *
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY  *
 * KIND, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA   *
 * OR PROFITS, WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE,   *
 * AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION    *
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.                      *
 **********************************************************************/

#include "tiffiop.h"
#include <stdio.h>

#include <ctype.h>

static size_t
_TIFFsnprintAsciiBounded(char * str, const size_t xstrlen, const char* cp, size_t max_chars);

static const char *photoNames[] = {
    "min-is-white",				/* PHOTOMETRIC_MINISWHITE */
    "min-is-black",				/* PHOTOMETRIC_MINISBLACK */
    "RGB color",				/* PHOTOMETRIC_RGB */
    "palette color (RGB from colormap)",	/* PHOTOMETRIC_PALETTE */
    "transparency mask",			/* PHOTOMETRIC_MASK */
    "separated",				/* PHOTOMETRIC_SEPARATED */
    "YCbCr",					/* PHOTOMETRIC_YCBCR */
    "7 (0x7)",
    "CIE L*a*b*",				/* PHOTOMETRIC_CIELAB */
    "ICC L*a*b*",				/* PHOTOMETRIC_ICCLAB */
    "ITU L*a*b*" 				/* PHOTOMETRIC_ITULAB */
};
#define	NPHOTONAMES	(sizeof (photoNames) / sizeof (photoNames[0]))

static const char *orientNames[] = {
    "0 (0x0)",
    "row 0 top, col 0 lhs",			/* ORIENTATION_TOPLEFT */
    "row 0 top, col 0 rhs",			/* ORIENTATION_TOPRIGHT */
    "row 0 bottom, col 0 rhs",			/* ORIENTATION_BOTRIGHT */
    "row 0 bottom, col 0 lhs",			/* ORIENTATION_BOTLEFT */
    "row 0 lhs, col 0 top",			/* ORIENTATION_LEFTTOP */
    "row 0 rhs, col 0 top",			/* ORIENTATION_RIGHTTOP */
    "row 0 rhs, col 0 bottom",			/* ORIENTATION_RIGHTBOT */
    "row 0 lhs, col 0 bottom",			/* ORIENTATION_LEFTBOT */
};
#define	NORIENTNAMES	(sizeof (orientNames) / sizeof (orientNames[0]))

static size_t
_TIFFSNPrintField(char * str, const size_t xstrlen, const TIFFField *fip,
                  uint32 value_count, void *raw_data)
{
	uint32 j;

    size_t chars_used = 0;
		
	chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  %s: ", fip->field_name);

	for(j = 0; j < value_count; j++) {
		if(fip->field_type == TIFF_BYTE)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u", ((uint8 *) raw_data)[j]);
		else if(fip->field_type == TIFF_UNDEFINED)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "0x%x",
			    (unsigned int) ((unsigned char *) raw_data)[j]);
		else if(fip->field_type == TIFF_SBYTE)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%d", ((int8 *) raw_data)[j]);
		else if(fip->field_type == TIFF_SHORT)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u", ((uint16 *) raw_data)[j]);
		else if(fip->field_type == TIFF_SSHORT)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%d", ((int16 *) raw_data)[j]);
		else if(fip->field_type == TIFF_LONG)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%lu",
			    (unsigned long)((uint32 *) raw_data)[j]);
		else if(fip->field_type == TIFF_SLONG)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%ld", (long)((int32 *) raw_data)[j]);
		else if(fip->field_type == TIFF_IFD)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "0x%lx",
				(unsigned long)((uint32 *) raw_data)[j]);
		else if(fip->field_type == TIFF_RATIONAL
			|| fip->field_type == TIFF_SRATIONAL
			|| fip->field_type == TIFF_FLOAT)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%f", ((float *) raw_data)[j]);
		else if(fip->field_type == TIFF_LONG8)
#if defined(__WIN32__) && (defined(_MSC_VER) || defined(__MINGW32__))
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%I64u",
			    (unsigned __int64)((uint64 *) raw_data)[j]);
#else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%llu",
			    (unsigned long long)((uint64 *) raw_data)[j]);
#endif
		else if(fip->field_type == TIFF_SLONG8)
#if defined(__WIN32__) && (defined(_MSC_VER) || defined(__MINGW32__))
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%I64d", (__int64)((int64 *) raw_data)[j]);
#else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%lld", (long long)((int64 *) raw_data)[j]);
#endif
		else if(fip->field_type == TIFF_IFD8)
#if defined(__WIN32__) && (defined(_MSC_VER) || defined(__MINGW32__))
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "0x%I64x",
				(unsigned __int64)((uint64 *) raw_data)[j]);
#else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "0x%llx",
				(unsigned long long)((uint64 *) raw_data)[j]);
#endif
		else if(fip->field_type == TIFF_FLOAT)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%f", ((float *)raw_data)[j]);
		else if(fip->field_type == TIFF_DOUBLE)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%f", ((double *) raw_data)[j]);
		else if(fip->field_type == TIFF_ASCII) {
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%s", (char *) raw_data);
			break;
		}
		else {
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "<unsupported data type in TIFFPrint>");
			break;
		}

		if(j < value_count - 1)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), ",");
	}

	chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\n");
    
    return chars_used;
}

static size_t
_TIFFPrettySNPrintField(TIFF* tif, char * str, const size_t xstrlen, ttag_t tag,
                        uint32 value_count, void *raw_data)
{
	TIFFDirectory *td = &tif->tif_dir;

    const TIFFFieldInfo *fip;
    
    size_t chars_used = 0;

    fip = TIFFFieldWithTag(tif, tag);

	switch (tag)
	{
		case TIFFTAG_INKSET:
			if (value_count == 2 && fip->field_type == TIFF_SHORT) {
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Ink Set: ");
				switch (*((uint16*)raw_data)) {
				case INKSET_CMYK:
					chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "CMYK\n");
					break;
				default:
					chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u (0x%x)\n",
						*((uint16*)raw_data),
						*((uint16*)raw_data));
					break;
				}
				return chars_used;
			}
			return chars_used;

		case TIFFTAG_DOTRANGE:
			if (value_count == 2 && fip->field_type == TIFF_SHORT) {
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Dot Range: %u-%u\n",
					((uint16*)raw_data)[0], ((uint16*)raw_data)[1]);
				return chars_used;
			}
			return chars_used;

		case TIFFTAG_WHITEPOINT:
			if (value_count == 2 && fip->field_type == TIFF_RATIONAL) {
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  White Point: %g-%g\n",
					((float *)raw_data)[0], ((float *)raw_data)[1]);
				return chars_used;
			} 
			return chars_used;

		case TIFFTAG_XMLPACKET:
		{
			uint32 i;

			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  XMLPacket (XMP Metadata):\n" );
			for(i = 0; i < value_count; i++)
                chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0),"%c",(int)((char *)raw_data)[i]);
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\n" );
			return chars_used;
		}
		case TIFFTAG_RICHTIFFIPTC:
			/*
			 * XXX: for some weird reason RichTIFFIPTC tag
			 * defined as array of LONG values.
			 */
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0),
			    "  RichTIFFIPTC Data: <present>, %lu bytes\n",
			    (unsigned long) value_count * 4);
			return chars_used;

		case TIFFTAG_PHOTOSHOP:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Photoshop Data: <present>, %lu bytes\n",
			    (unsigned long) value_count);
			return chars_used;

		case TIFFTAG_ICCPROFILE:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  ICC Profile: <present>, %lu bytes\n",
			    (unsigned long) value_count);
			return chars_used;

		case TIFFTAG_STONITS:
			if (value_count == 1 && fip->field_type == TIFF_DOUBLE) { 
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0),
					"  Sample to Nits conversion factor: %.4e\n",
					*((double*)raw_data));
				return chars_used;
			}
			return chars_used;
	}

	return chars_used;
}

/*
 * Print the contents of the current directory
 * to the specified stdio file stream.
 */
size_t
TIFFSNPrintDirectory(TIFF* tif, char * str, const size_t xstrlen, long flags)
{
	TIFFDirectory *td = &tif->tif_dir;
	char *sep;
	uint16 i;
	long l, n;
    
    size_t chars_used = 0;

#if defined(__WIN32__) && (defined(_MSC_VER) || defined(__MINGW32__))
	chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "TIFF Directory at offset 0x%I64x (%I64u)\n",
		(unsigned __int64) tif->tif_diroff,
		(unsigned __int64) tif->tif_diroff);
#else
	chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "TIFF Directory at offset 0x%llx (%llu)\n",
		(unsigned long long) tif->tif_diroff,
		(unsigned long long) tif->tif_diroff);
#endif
	if (TIFFFieldSet(tif,FIELD_SUBFILETYPE)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Subfile Type:");
		sep = " ";
		if (td->td_subfiletype & FILETYPE_REDUCEDIMAGE) {
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%sreduced-resolution image", sep);
			sep = "/";
		}
		if (td->td_subfiletype & FILETYPE_PAGE) {
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%smulti-page document", sep);
			sep = "/";
		}
		if (td->td_subfiletype & FILETYPE_MASK)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%stransparency mask", sep);
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " (%lu = 0x%lx)\n",
		    (long) td->td_subfiletype, (long) td->td_subfiletype);
	}
	if (TIFFFieldSet(tif,FIELD_IMAGEDIMENSIONS)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Image Width: %lu Image Length: %lu",
		    (unsigned long) td->td_imagewidth, (unsigned long) td->td_imagelength);
		if (TIFFFieldSet(tif,FIELD_IMAGEDEPTH))
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " Image Depth: %lu",
			    (unsigned long) td->td_imagedepth);
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\n");
	}
	if (TIFFFieldSet(tif,FIELD_TILEDIMENSIONS)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Tile Width: %lu Tile Length: %lu",
		    (unsigned long) td->td_tilewidth, (unsigned long) td->td_tilelength);
		if (TIFFFieldSet(tif,FIELD_TILEDEPTH))
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " Tile Depth: %lu",
			    (unsigned long) td->td_tiledepth);
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\n");
	}
	if (TIFFFieldSet(tif,FIELD_RESOLUTION)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Resolution: %g, %g",
		    td->td_xresolution, td->td_yresolution);
		if (TIFFFieldSet(tif,FIELD_RESOLUTIONUNIT)) {
			switch (td->td_resolutionunit) {
			case RESUNIT_NONE:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " (unitless)");
				break;
			case RESUNIT_INCH:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " pixels/inch");
				break;
			case RESUNIT_CENTIMETER:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " pixels/cm");
				break;
			default:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " (unit %u = 0x%x)",
				    td->td_resolutionunit,
				    td->td_resolutionunit);
				break;
			}
		}
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\n");
	}
	if (TIFFFieldSet(tif,FIELD_POSITION))
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Position: %g, %g\n",
		    td->td_xposition, td->td_yposition);
	if (TIFFFieldSet(tif,FIELD_BITSPERSAMPLE))
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Bits/Sample: %u\n", td->td_bitspersample);
	if (TIFFFieldSet(tif,FIELD_SAMPLEFORMAT)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Sample Format: ");
		switch (td->td_sampleformat) {
		case SAMPLEFORMAT_VOID:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "void\n");
			break;
		case SAMPLEFORMAT_INT:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "signed integer\n");
			break;
		case SAMPLEFORMAT_UINT:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "unsigned integer\n");
			break;
		case SAMPLEFORMAT_IEEEFP:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "IEEE floating point\n");
			break;
		case SAMPLEFORMAT_COMPLEXINT:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "complex signed integer\n");
			break;
		case SAMPLEFORMAT_COMPLEXIEEEFP:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "complex IEEE floating point\n");
			break;
		default:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u (0x%x)\n",
			    td->td_sampleformat, td->td_sampleformat);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_COMPRESSION)) {
		const TIFFCodec* c = TIFFFindCODEC(td->td_compression);
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Compression Scheme: ");
		if (c)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%s\n", c->name);
		else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u (0x%x)\n",
			    td->td_compression, td->td_compression);
	}
	if (TIFFFieldSet(tif,FIELD_PHOTOMETRIC)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Photometric Interpretation: ");
		if (td->td_photometric < NPHOTONAMES)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%s\n", photoNames[td->td_photometric]);
		else {
			switch (td->td_photometric) {
			case PHOTOMETRIC_LOGL:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "CIE Log2(L)\n");
				break;
			case PHOTOMETRIC_LOGLUV:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "CIE Log2(L) (u',v')\n");
				break;
			default:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u (0x%x)\n",
				    td->td_photometric, td->td_photometric);
				break;
			}
		}
	}
	if (TIFFFieldSet(tif,FIELD_EXTRASAMPLES) && td->td_extrasamples) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Extra Samples: %u<", td->td_extrasamples);
		sep = "";
		for (i = 0; i < td->td_extrasamples; i++) {
			switch (td->td_sampleinfo[i]) {
			case EXTRASAMPLE_UNSPECIFIED:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%sunspecified", sep);
				break;
			case EXTRASAMPLE_ASSOCALPHA:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%sassoc-alpha", sep);
				break;
			case EXTRASAMPLE_UNASSALPHA:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%sunassoc-alpha", sep);
				break;
			default:
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%s%u (0x%x)", sep,
				    td->td_sampleinfo[i], td->td_sampleinfo[i]);
				break;
			}
			sep = ", ";
		}
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), ">\n");
	}
	if (TIFFFieldSet(tif,FIELD_INKNAMES)) {
		char* cp;
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Ink Names: ");
		i = td->td_samplesperpixel;
		sep = "";
		for (cp = td->td_inknames; 
		     i > 0 && cp < td->td_inknames + td->td_inknameslen; 
		     cp = strchr(cp,'\0')+1, i--) {
			size_t max_chars = 
				td->td_inknameslen - (cp - td->td_inknames);
            chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0),sep);
            chars_used += _TIFFsnprintAsciiBounded(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), cp, max_chars);
			sep = ", ";
		}
        chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0),"\n");
	}
	if (TIFFFieldSet(tif,FIELD_THRESHHOLDING)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Thresholding: ");
		switch (td->td_threshholding) {
		case THRESHHOLD_BILEVEL:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "bilevel art scan\n");
			break;
		case THRESHHOLD_HALFTONE:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "halftone or dithered scan\n");
			break;
		case THRESHHOLD_ERRORDIFFUSE:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "error diffused\n");
			break;
		default:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u (0x%x)\n",
			    td->td_threshholding, td->td_threshholding);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_FILLORDER)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  FillOrder: ");
		switch (td->td_fillorder) {
		case FILLORDER_MSB2LSB:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "msb-to-lsb\n");
			break;
		case FILLORDER_LSB2MSB:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "lsb-to-msb\n");
			break;
		default:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u (0x%x)\n",
			    td->td_fillorder, td->td_fillorder);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_YCBCRSUBSAMPLING))
        {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  YCbCr Subsampling: %u, %u\n",
			td->td_ycbcrsubsampling[0], td->td_ycbcrsubsampling[1] );
	}
	if (TIFFFieldSet(tif,FIELD_YCBCRPOSITIONING)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  YCbCr Positioning: ");
		switch (td->td_ycbcrpositioning) {
		case YCBCRPOSITION_CENTERED:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "centered\n");
			break;
		case YCBCRPOSITION_COSITED:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "cosited\n");
			break;
		default:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u (0x%x)\n",
			    td->td_ycbcrpositioning, td->td_ycbcrpositioning);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_HALFTONEHINTS))
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Halftone Hints: light %u dark %u\n",
		    td->td_halftonehints[0], td->td_halftonehints[1]);
	if (TIFFFieldSet(tif,FIELD_ORIENTATION)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Orientation: ");
		if (td->td_orientation < NORIENTNAMES)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%s\n", orientNames[td->td_orientation]);
		else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u (0x%x)\n",
			    td->td_orientation, td->td_orientation);
	}
	if (TIFFFieldSet(tif,FIELD_SAMPLESPERPIXEL))
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Samples/Pixel: %u\n", td->td_samplesperpixel);
	if (TIFFFieldSet(tif,FIELD_ROWSPERSTRIP)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Rows/Strip: ");
		if (td->td_rowsperstrip == (uint32) -1)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "(infinite)\n");
		else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%lu\n", (unsigned long) td->td_rowsperstrip);
	}
	if (TIFFFieldSet(tif,FIELD_MINSAMPLEVALUE))
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Min Sample Value: %u\n", td->td_minsamplevalue);
	if (TIFFFieldSet(tif,FIELD_MAXSAMPLEVALUE))
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Max Sample Value: %u\n", td->td_maxsamplevalue);
	if (TIFFFieldSet(tif,FIELD_SMINSAMPLEVALUE)) {
		int count = (tif->tif_flags & TIFF_PERSAMPLE) ? td->td_samplesperpixel : 1;
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  SMin Sample Value:");
		for (i = 0; i < count; ++i)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " %g", td->td_sminsamplevalue[i]);
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\n");
	}
	if (TIFFFieldSet(tif,FIELD_SMAXSAMPLEVALUE)) {
		int count = (tif->tif_flags & TIFF_PERSAMPLE) ? td->td_samplesperpixel : 1;
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  SMax Sample Value:");
		for (i = 0; i < count; ++i)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " %g", td->td_smaxsamplevalue[i]);
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\n");
	}
	if (TIFFFieldSet(tif,FIELD_PLANARCONFIG)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Planar Configuration: ");
		switch (td->td_planarconfig) {
		case PLANARCONFIG_CONTIG:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "single image plane\n");
			break;
		case PLANARCONFIG_SEPARATE:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "separate image planes\n");
			break;
		default:
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "%u (0x%x)\n",
			    td->td_planarconfig, td->td_planarconfig);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_PAGENUMBER))
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Page Number: %u-%u\n",
		    td->td_pagenumber[0], td->td_pagenumber[1]);
	if (TIFFFieldSet(tif,FIELD_COLORMAP)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Color Map: ");
		if (flags & TIFFPRINT_COLORMAP) {
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\n");
			n = 1L<<td->td_bitspersample;
			for (l = 0; l < n; l++)
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "   %5lu: %5u %5u %5u\n",
				    l,
				    td->td_colormap[0][l],
				    td->td_colormap[1][l],
				    td->td_colormap[2][l]);
		} else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "(present)\n");
	}
	if (TIFFFieldSet(tif,FIELD_REFBLACKWHITE)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Reference Black/White:\n");
		for (i = 0; i < 3; i++)
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "    %2d: %5g %5g\n", i,
			td->td_refblackwhite[2*i+0],
			td->td_refblackwhite[2*i+1]);
	}
	if (TIFFFieldSet(tif,FIELD_TRANSFERFUNCTION)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  Transfer Function: ");
		if (flags & TIFFPRINT_CURVES) {
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\n");
			n = 1L<<td->td_bitspersample;
			for (l = 0; l < n; l++) {
				chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "    %2lu: %5u",
				    l, td->td_transferfunction[0][l]);
				for (i = 1; i < td->td_samplesperpixel; i++)
					chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " %5u",
					    td->td_transferfunction[i][l]);
                chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0),"\n");

			}
		} else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "(present)\n");
	}
	if (TIFFFieldSet(tif, FIELD_SUBIFD) && (td->td_subifd)) {
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  SubIFD Offsets:");
		for (i = 0; i < td->td_nsubifd; i++)
#if defined(__WIN32__) && (defined(_MSC_VER) || defined(__MINGW32__))
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " %5I64u",
				(unsigned __int64) td->td_subifd[i]);
#else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), " %5llu",
				(unsigned long long) td->td_subifd[i]);
#endif
		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0),"\n");
	}

	/*
	** Custom tag support.
	*/
	{
		int  i;
		short count;

		count = (short) TIFFGetTagListCount(tif);
		for(i = 0; i < count; i++) {
			uint32 tag = TIFFGetTagListEntry(tif, i);
			const TIFFField *fip;
			uint32 value_count;
			int mem_alloc = 0;
			void *raw_data;

			fip = TIFFFieldWithTag(tif, tag);
			if(fip == NULL)
				continue;

			if(fip->field_passcount) {
				if (fip->field_readcount == TIFF_VARIABLE2 ) {
					if(TIFFGetField(tif, tag, &value_count, &raw_data) != 1)
						continue;
				} else if (fip->field_readcount == TIFF_VARIABLE ) {
					uint16 small_value_count;
					if(TIFFGetField(tif, tag, &small_value_count, &raw_data) != 1)
						continue;
					value_count = small_value_count;
				} else {
					assert (fip->field_readcount == TIFF_VARIABLE
						|| fip->field_readcount == TIFF_VARIABLE2);
					continue;
				} 
			} else {
				if (fip->field_readcount == TIFF_VARIABLE
				    || fip->field_readcount == TIFF_VARIABLE2)
					value_count = 1;
				else if (fip->field_readcount == TIFF_SPP)
					value_count = td->td_samplesperpixel;
				else
					value_count = fip->field_readcount;
				if (fip->field_tag == TIFFTAG_DOTRANGE
				    && strcmp(fip->field_name,"DotRange") == 0) {
					/* TODO: This is an evil exception and should not have been
					   handled this way ... likely best if we move it into
					   the directory structure with an explicit field in 
					   libtiff 4.1 and assign it a FIELD_ value */
					static uint16 dotrange[2];
					raw_data = dotrange;
					TIFFGetField(tif, tag, dotrange+0, dotrange+1);
				} else if (fip->field_type == TIFF_ASCII
					   || fip->field_readcount == TIFF_VARIABLE
					   || fip->field_readcount == TIFF_VARIABLE2
					   || fip->field_readcount == TIFF_SPP
					   || value_count > 1) {
					if(TIFFGetField(tif, tag, &raw_data) != 1)
						continue;
				} else {
					raw_data = _TIFFmalloc(
					    _TIFFDataSize(fip->field_type)
					    * value_count);
					mem_alloc = 1;
					if(TIFFGetField(tif, tag, raw_data) != 1) {
						_TIFFfree(raw_data);
						continue;
					}
				}
			}

			/*
			 * Catch the tags which needs to be specially handled
			 * and pretty print them. If tag not handled in
			 * _TIFFPrettyPrintField() fall down and print it as
			 * any other tag.
			 */
            {
                size_t cut;
                if ((cut=_TIFFPrettySNPrintField(tif, str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), tag, value_count, raw_data))) {
                    chars_used+=cut;
                    if(mem_alloc)
                        _TIFFfree(raw_data);
                    continue;
                }
                else
                    chars_used += _TIFFSNPrintField(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), fip, value_count, raw_data);
            }

			if(mem_alloc)
				_TIFFfree(raw_data);
		}
	}
        

        /* _TIFFFillStriles( tif ); */
        
	if ((flags & TIFFPRINT_STRIPS) &&
	    TIFFFieldSet(tif,FIELD_STRIPOFFSETS)) {
		uint32 s;

		chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  %lu %s:\n",
		    (long) td->td_nstrips,
		    isTiled(tif) ? "Tiles" : "Strips");
		for (s = 0; s < td->td_nstrips; s++)
#if defined(__WIN32__) && (defined(_MSC_VER) || defined(__MINGW32__))
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "    %3lu: [%8I64u, %8I64u]\n",
			    (unsigned long) s,
			    (unsigned __int64) td->td_stripoffset[s],
			    (unsigned __int64) td->td_stripbytecount[s]);
#else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "    %3lu: [%8llu, %8llu]\n",
			    (unsigned long) s,
			    (unsigned long long) td->td_stripoffset[s],
			    (unsigned long long) td->td_stripbytecount[s]);
#endif
	}
    return chars_used;
}

size_t
_TIFFsnprintAscii(char * str, const size_t xstrlen, const char* cp)
{
	return _TIFFsnprintAsciiBounded( str, xstrlen, cp, strlen(cp));
}

static size_t
_TIFFsnprintAsciiBounded(char * str, const size_t xstrlen, const char* cp, size_t max_chars)
{
    size_t chars_used=0;
	for (; max_chars > 0 && *cp != '\0'; cp++, max_chars--) {
		const char* tp;

		if (isprint((int)*cp)) {
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0),"%c",*cp);
			continue;
		}
		for (tp = "\tt\bb\rr\nn\vv"; *tp; tp++)
			if (*tp++ == *cp)
				break;
		if (*tp)
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\\%c", *tp);
		else
			chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\\%03o", *cp & 0xff);
	}
    return chars_used;
}

size_t
_TIFFsnprintAsciiTag(char * str, const size_t xstrlen, const char* name, const char* value)
{
    size_t chars_used=0;
	chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "  %s: \"", name);
	chars_used += _TIFFsnprintAscii(str+chars_used, xstrlen, value);
	chars_used += snprintf(str+chars_used, ((xstrlen>chars_used)?xstrlen-chars_used:0), "\"\n");
    return chars_used;
}

/* vim: set ts=8 sts=8 sw=8 noet: */
/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * fill-column: 78
 * End:
 */
