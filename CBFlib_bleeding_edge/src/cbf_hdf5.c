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
 * The term ‘this software’, as used in these Notices, refers to      *
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
    
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
    

    
    /* apply a long attribute to a group or dataset */
    
    int cbf_apply_h5long_attribute(hid_t hid,
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

    
    /* apply an integer attribute to a group or dataset */
    
    int cbf_apply_h5integer_attribute(hid_t hid,
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
    
    /* Write a binary value to an HDF5 file */
    
    int cbf_write_h5binary (cbf_node *column, unsigned int row,
                            cbf_h5handle h5handle)
    {
        hid_t valid, valtype, valprop, valspace;
        
        int errorcode;

        char rownum[10];

        cbf_file *infile;
        
        char digest [25];
        
        long start;
        
        size_t size;
        
        hsize_t hsize[1];
        
        unsigned int compression;
        
        int id, bits, sign, type, checked_digest, realarray;
        
        const char *byteorder;
        
        size_t dimover, dimfast, dimmid, dimslow;
        
        size_t padding;

        /* Check the arguments */
        
        if (!h5handle || !h5handle->hfile)
            
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
        
        /* Create treat the image as an opaque stream of size bytes */
        
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

        if (!infile->temporary) {

            errorcode |= cbf_get_block(infile,size);
            
        }
        
        
            
        errorcode |= cbf_apply_h5text_attribute(valid,"type","bnry",errorcode);
        errorcode |= cbf_apply_h5integer_attribute(valid,"compression",compression,errorcode);
        errorcode |= cbf_apply_h5long_attribute(valid,"size",(long)size,errorcode);
        errorcode |= cbf_apply_h5integer_attribute(valid,"binid",id,errorcode);
        errorcode |= cbf_apply_h5integer_attribute(valid,"real",realarray,errorcode);
        errorcode |= cbf_apply_h5integer_attribute(valid,"sign",sign,errorcode);
        errorcode |= cbf_apply_h5text_attribute(valid,"byteorder",byteorder,errorcode);
        if (cbf_is_base64digest (digest)) {
            errorcode |= cbf_apply_h5text_attribute(valid,"MD5_digest",digest,errorcode);
        }
        errorcode |= cbf_apply_h5long_attribute(valid,"dimover",(long)dimover,errorcode);
        errorcode |= cbf_apply_h5long_attribute(valid,"dimfast",(long)dimfast,errorcode);
        errorcode |= cbf_apply_h5long_attribute(valid,"dimmid",(long)dimmid,errorcode);
        errorcode |= cbf_apply_h5long_attribute(valid,"dimslow",(long)dimslow,errorcode);
        errorcode |= cbf_apply_h5long_attribute(valid,"padding",(long)padding,errorcode);

        cbf_h5reportneg(H5Dwrite(valid,valtype,
                                 H5S_ALL,H5S_ALL,H5P_DEFAULT,infile->characters),
                        CBF_ARGUMENT,errorcode);
        
        
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
        static const char missing [] = { CBF_TOKEN_WORD, '?', '\0' };
        
 
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
                        "type",typecode,errorcode);

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
            
            return cbf_write_h5binary (column, row, h5handle);
        
        
        /* Undecoded MIME? */
        
        if (*text == CBF_TOKEN_MIME_BIN)
        {
            /* Convert the value to a normal binary section */
            
            cbf_failnez (cbf_mime_temp (column, row))
            
            return cbf_write_h5binary (column, row, h5handle);
        }
        
        
        /* Fail */
        
        return CBF_ARGUMENT;
    }
    
    
    /* Write a category to an HDF5 file */
    
    int cbf_write_h5category (cbf_handle handle,
                              const cbf_node *category,
                              cbf_h5handle h5handle)
    {
        unsigned int column, row;

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
        

        /* Write the name under the opem save frame or datablock */
        
        if (h5handle->sfid <0) {
        
          cbf_h5failneg(h5handle->catid=H5Gcreatex(h5handle->dbid,
                    (category->name)?(category->name):"_(null)_"),
                    CBF_FORMAT);
        
            
        } else {
            cbf_h5failneg(h5handle->catid=H5Gcreatex(h5handle->dbid,
                        (category->name)?(category->name):"_(null)_"),
                        CBF_FORMAT);
            
            
        }
        
        
        cbf_failnez(cbf_apply_h5text_attribute(h5handle->catid,
                                    "NX_class","NXcifcat",0));
        
        
        /* now, for each column, make it into a group and
           store each row as a dataset */
        
        
        for (column= 0; column < category->children; column++)
        {
            
            cbf_h5failneg(h5handle->colid=H5Gcreatex(h5handle->catid,
                        (category->child[column])->name?
                        (category->child[column])->name:"_(null)_"),
                        CBF_FORMAT);
           
            cbf_failnez(cbf_apply_h5text_attribute(h5handle->colid,
                                    "NX_class","NXcifcol",0));
            
            /* For each row, create a dataset */

            for (row=0; row < category->child [column]->children; row++)
            {
                cbf_failnez(cbf_write_h5value(handle,
                                              category->child [column],
                                              row,h5handle));
            }
            
            cbf_h5failneg(H5Gclose(h5handle->colid),CBF_ARGUMENT);

            h5handle->colid = CBF_H5FAIL;
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
        
        return CBF_SUCCESS;
        
    }


    /*  Create an HDF5 Group below NX entry or below curnxid */

    int cbf_H5Gcreate(cbf_h5handle h5handle,
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
        cbf_failnez(cbf_H5Gcreate(h5handle, groupname, newgroup));
        
        cbf_failnez(cbf_apply_h5text_attribute(*newgroup,
                    "NX_class",nxclass,0));
               
        return CBF_SUCCESS;
        
    }

 
    /* Free an H5File handle */

    int cbf_free_h5handle(cbf_h5handle h5handle) {
        
        void * memblock;
        
        memblock = (void *) h5handle;
        

        if (h5handle->colid >= 0) {
            
            cbf_h5onfailneg(H5Gclose(h5handle->colid),
                          CBF_UNDEFINED,cbf_free(&memblock,NULL));
            
            
        }

        if (h5handle->catid >= 0) {
            
            cbf_h5onfailneg(H5Gclose(h5handle->catid),
                          CBF_UNDEFINED,cbf_free(&memblock,NULL));
            
            
        }
        

        if (h5handle->sfid >= 0) {
            
            cbf_h5onfailneg(H5Gclose(h5handle->sfid),
                          CBF_UNDEFINED,cbf_free(&memblock,NULL));
            
            
        }

        if (h5handle->dbid >= 0) {
            
            cbf_h5onfailneg(H5Gclose(h5handle->dbid),
                          CBF_UNDEFINED,cbf_free(&memblock,NULL));
            
            
        }

        
        if (h5handle->rootid >= 0) {
            
            cbf_h5onfailneg(H5Gclose(h5handle->rootid),
                          CBF_UNDEFINED,cbf_free(&memblock,NULL));

            
        }

        if (h5handle->curnxid >= 0) {
            
            cbf_h5onfailneg(H5Gclose(h5handle->curnxid),
                          CBF_UNDEFINED,cbf_free(&memblock,NULL));

            
        }

        if (h5handle->nxid >= 0) {
            
            cbf_h5onfailneg(H5Gclose(h5handle->nxid),
                            CBF_UNDEFINED,cbf_free(&memblock,NULL));
            
        }

        if (h5handle->hfile >= 0) {
            
            cbf_h5onfailneg(H5Fclose(h5handle->hfile),
                            CBF_FILECLOSE,cbf_free(&memblock,NULL));
            
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
            (*h5handle)->curnxid = (hid_t)CBF_H5FAIL;
            
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
     Make a new group of NeXus class NXcifsf in the NXcif current datablock
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
                    "NX_class", "NXcifsf",0));
        
        return CBF_SUCCESS;
    }
    

    
    
    /* Write a datablock name to an HDF5 file 
       Make a new group of NeXus class NXcifdb in the NXcif class root
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
        
        /* Write the name */

        cbf_h5failneg(h5handle->dbid=H5Gcreatex(h5handle->rootid,
                                datablock->name),
                                CBF_FORMAT);
        
        cbf_failnez(cbf_apply_h5text_attribute(h5handle->dbid,
                                "NX_class", "NXcifdb",0));
        
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
    

    /* Create an H5File handle */
        
    int cbf_create_h5handle(cbf_h5handle *h5handle,
                              const char * h5filename) {
        
        
        hid_t fcreate_prop_list;
        
        int errorcode;
        
        errorcode = cbf_make_h5handle(h5handle);
        
        if (errorcode) return errorcode;
        
        cbf_h5onfailneg(fcreate_prop_list = H5Pcreate(H5P_FILE_ACCESS),
                        CBF_ALLOC,cbf_free((void**) h5handle, NULL));
        
        cbf_h5onfailneg(H5Pset_fclose_degree(fcreate_prop_list,H5F_CLOSE_STRONG),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
        cbf_h5onfailneg((*h5handle)->hfile = H5Fcreate(h5filename,
                        H5F_ACC_TRUNC, H5P_DEFAULT,fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
        cbf_h5onfailneg(H5Pclose(fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));
        
        cbf_onfailnez(cbf_H5Gcreate(*h5handle,"NXcbf",&((*h5handle)->rootid)),
                                    cbf_free_h5handle(*h5handle)); 
        
        return CBF_SUCCESS;
        
    }
        
    
    /*  Write cbf to HDF5 file hfile */
    
    int cbf_write_hdf5_file (cbf_handle handle, cbf_h5handle h5handle)
    {
        cbf_node *node;
                
        if (!handle || !h5handle)
            
            return CBF_ARGUMENT;
        
        /* Find the root node */
        
        cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))
        
        /* Reset the reference counts */
        
        cbf_failnez( cbf_reset_refcounts(handle->dictionary) )
        
        /* Write the file */
        
        return cbf_write_h5node (handle, node, h5handle);
                        
    }
    
    


    
#ifdef __cplusplus
    
}

#endif
