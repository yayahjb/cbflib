/**********************************************************************
 * cbf_hdf5.h -- read and write HDF5/NeXus files                      *
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

#ifndef CBF_HDF5_H
#define CBF_HDF5_H

#ifdef __cplusplus

extern "C" {

#endif


#include <hdf5.h>
#include "cbf.h"
#include "cbf_tree.h"
#include <string.h>

    /** return the maximum of two numeric values */
#define cbf_max(a,b) ((a)>(b)?(a):(b))
    
	/**
     Put a character into a buffer at a given position.
     
     Ensures the buffer is long enough to hold the new character, realloc'ing if needed, and then inserts it.
     
     \param c The character.
     \param buf A pointer to the realloc'able buffer.
     \param n A pointer to the current length of the buffer.
     \param k The offset to place the character <code>c</code> in.
     
     \return void
	 */
	void cbf_push_buf(const int c, char * * const buf, size_t * const n, size_t *k);

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
     const char * * const string);
    
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
	int cbf_isPilatusDate(const char * str);

    
    /****************************************************************
     The following section of code is extracted from J. Sloan's
     cbf_hdf5_common.h
     ****************************************************************/
    
#define CBF_H5FAIL ((hid_t)(-1))
    
	/** \brief Check the validity of an object identifier
     
     Function to check validity of a HDF5 identifier.
     HDF5's predefined types are never counted as valid by this function,
     so it can't be used to test the validity of a type constant.
     Types obtained by using H5Tcopy are safe to test.
     
     \param ID An HDF5 object identifier.
     
     \return Non-zero if the type is valid, zero otherwise.
     */
	int cbf_H5Ivalid(const hid_t ID);
    
	/** \brief Try to free an object identifier
     
     Function to close any handle without tracking its type.
     Don't use this if a more specific function can be used instead, ie if the
     type is known, as this function will be less efficient.
     
     \param ID An HDF5 object identifier to be closed.
     
     \return An error code.
     */
	int cbf_H5Ifree(const hid_t ID);
    
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
	int cbf_H5Gcreate(hid_t * const group, const char * const name, const hid_t parent);
    
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
	int cbf_H5Grequire(hid_t * const group, const char * const name, const hid_t parent);
    
    /** \brief Close a HDF5 group
     
     Attempt to close a group, but don't modify the identifier that described it.
     
     \param ID The HDF5 group to be closed.
     
     \return An error code.
     */
	int cbf_H5Gfree(const hid_t ID);
    
	/** \brief Close a HDF5 group
     
     Attempt to close a group, clobbering the identifier that described it.
     
     \param ID A pointer to the HDF5 group to be closed.
     
     \return An error code.
     */
	int cbf_H5Gdestroy(hid_t * const ID);
    
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
	int cbf_H5Fopen(hid_t * const file, const char * const name);
    
	/** \brief Close a HDF5 file
     
     Attempt to close a file, but don't modify the identifier that described it.
     
     \param ID The HDF5 file to be closed.
     
     \return An error code.
     */
	int cbf_H5Fclose(const hid_t ID);
    
	/** \brief Close a HDF5 file
     
     Attempt to close a file, clobbering the identifier that described it.
     
     \param ID A pointer to the HDF5 file to be closed.
     
     \return An error code.
     */
	int cbf_H5Fdestroy(hid_t * const ID);
    
	/* Attributes */
    
	/* create an attribute with the given name & ASCII value, try to write it to the HDF5 id */
	int cbf_H5Aset_string(const hid_t ID, const char * const name, const char * const value);
        
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
     int (*cmp)(const void * a, const void * b, size_t N));
    
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
     const char * const value);
    
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
     const hid_t type);
    
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
     const hid_t type);
    
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
	int cbf_H5Dset_extent(const hid_t dataset, const hsize_t * const dim);
    
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
     const void * const value);
    
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
     void * const value);
        
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
     const double value);

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
     const char * const value);
    
	/** \brief Close a HDF5 dataset
     
     Attempt to close a dataset, but don't modify the identifier that described it.
     
     \param ID The HDF5 dataset to be closed.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Ddestroy
     \sa cbf_H5Ivalid
     
     \return An error code.
     */
	int cbf_H5Dfree(const hid_t ID);
	/** \brief Close a given dataset and set the handle to an invalid value. */
	int cbf_H5Ddestroy(hid_t * const ID);
    
	/* Custom HDF5 types - to get the correct string type for datasets in a consistent way */
    
	/** \brief Get a HDF5 string datatype with a specified length
	 */
	int cbf_H5Tcreate_string(hid_t * type, const size_t len);
    
	/**
	 */
	int cbf_H5Tfree(const hid_t ID);
    
	/**
	 */
	int cbf_H5Tdestroy(hid_t * const ID);
    
	/* HDF5 dataspace functions: I need a uniform method of creating data spaces to ensure correct operation of comparison functions */
    
	int cbf_H5Screate
    (hid_t * const ID,
     const int rank,
     const hsize_t * const dim,
     const hsize_t * const max);
    
	/** \brief Close a HDF5 dataset
     
     Attempt to close a dataset, but don't modify the identifier that described it.
     
     \param ID The HDF5 dataset to be closed.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Ddestroy
     \sa cbf_H5Ivalid
     
     \return An error code.
     */
	int cbf_H5Sfree(const hid_t ID);
    
	/** \brief Close a HDF5 dataset
     
     Attempt to close a dataset, clobbering the identifier that described it.
     
     \param ID A pointer to the HDF5 dataset to be closed.
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Dfree
     \sa cbf_H5Ivalid
     
     \return An error code.
     */
	int cbf_H5Sdestroy(hid_t * const ID);
    

    /****************************************************************
     End of section of code extracted from J. Sloan's
     cbf_hdf5_common.h
     ****************************************************************/
    /****************************************************************
     The following section of code is extracted from J. Sloan's
     config.h
     ****************************************************************/
    
    /**
     Tokenise an input stream, returning one token at a time into the given buffer.
     
     \param buf A pointer to a realloc-able buffer for storing the input, free'd when EOF is reached.
     \param n The current size of \c buf.
     \param ln The current line number of the file.
     \param pre The previous character, needed to test for unexpected EOL state.
     \param stream The stream to be tokenised.
     
     \return An error code.
     */
    int cbf_hdf5_parseScan(char * * const buf, size_t * n, size_t * ln, char * const pre, FILE * stream);

    /**
     The returned string is "success" for CBF_SUCCESS, "unknown error" if the given error code is not recognised or a non-empty string briefly describing the error otherwise.
     
     The returned string must not be free'd.
     */
    const char * cbf_hdf5_configParseStrerror(const int error);
    
    /** POD to define a basic set of configuration settings for an axis */
    typedef struct _cbf_hdf5_configItem
    {
        const char * minicbf;
        const char * nexus;
        const char * depends_on;
        double vector[3];
    } cbf_hdf5_configItem;
        
    /**
     Initialises name & depends_on to null, vector to [nan,nan,nan].
     */
    cbf_hdf5_configItem cbf_hdf5_createConfigItem();
    
    /** free any heap memory associated with the given cbf_hdf5_configItem object, doesn't free the object itself because it may be on the stack */
    void cbf_hdf5_destroyConfigItem(const cbf_hdf5_configItem item);
    
    /**
     Should not be manipulated directly, takes ownership of the config items which it contains.
     */
    typedef struct cbf_hdf5_configItemVector
    {
        cbf_hdf5_configItem * item;
        size_t nItems;
        size_t maxItems;
        const char * sample_depends_on;
    } cbf_hdf5_configItemVector;

    /** Opaque handle for a vector of config items */
    typedef struct cbf_hdf5_configItemVector * cbf_hdf5_configItemVectorhandle;
    
    /**
     Initialises size & capacity to 0, doesn't allocate storage immediately.
     */
    cbf_hdf5_configItemVectorhandle cbf_hdf5_createConfigItemVector();
    
    /**
     Destroys any children of the object, free's the memory for the array of children and free's the memory for the vector itself.
     */
    void cbf_hdf5_destroyConfigItemVector(const cbf_hdf5_configItemVectorhandle vector);
    
    /**
     Releases any previously held dependancy and takes ownership of a new one.
     The given string will be free'd by the object when it is no longer needed.
     */
    void cbf_hdf5_configItemVector_setSampleDependsOn(cbf_hdf5_configItemVectorhandle vector, const char * const depends_on);
    
    /**
     \return The current dependancy setting for the sample group, or zero if not set.
     */
    const char * cbf_hdf5_configItemVector_getSampleDependsOn(cbf_hdf5_configItemVectorhandle vector);

    /**
     The vector will take ownership of the item's contents. This may invalidate any previously obtained pointers to items in the vector.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_push(cbf_hdf5_configItemVectorhandle vector, cbf_hdf5_configItem item);
    
    /**
     \return An iterator to a matching entry, or an iterator to the current end element.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_findMinicbf(const cbf_hdf5_configItemVectorhandle vector, const char * const name);
    
    /**
     \return An iterator to a matching entry, or an iterator to the current end element.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_findNexus(const cbf_hdf5_configItemVectorhandle vector, const char * const name);
    
    /**
     Performs bounds-checking, and returns 0 if the given index is out-of-bounds. The index type is unsigned, so only one comparison is needed to do this.
     
     \return A pointer to an item in the vector that may be modified but should not be free'd, subsequent vector operations may invalidate this pointer.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_at(cbf_hdf5_configItemVectorhandle vector, const size_t n);
    
    /**
     \return A pointer to an item in the vector that may be modified but should not be free'd, subsequent vector operations may invalidate this pointer.
     */
    cbf_hdf5_configItem * cbf_hdf5_configItemVector_begin(const cbf_hdf5_configItemVectorhandle vector);
    
    /**
     \return A pointer to an item in the vector that may be modified but should not be free'd, subsequent vector operations may invalidate this pointer.
     */
    const cbf_hdf5_configItem * cbf_hdf5_configItemVector_end(const cbf_hdf5_configItemVectorhandle vector);
    
    int cbf_hdf5_parseExtractVector
    (FILE * const configFile,
     FILE * const logFile,
     cbf_hdf5_configItem * it,
     char * * const buf,
     size_t * n,
     size_t * ln,
     char * const pre);
    
    int cbf_hdf5_parseConfig(FILE * const configFile, FILE * const logFile, cbf_hdf5_configItemVectorhandle vec);
    
    /****************************************************************
     End of section of code extracted from J. Sloan's
     config.h
     ****************************************************************/

    /* Attribute type definition, agrees with CBFlib data convensions */
    
	typedef struct cbf_name_value_pair_def
	{
		const char * name;
		const void * value;
	} cbf_name_value_pair;
    

    
#define cbf_h5failneg(x,code) {int err; err = (x); if (err < 0) {return (code);}}
#define cbf_h5onfailneg(x,code,y) {int err; err = (x); if (err < 0) {{y;} return (code);}}
#define cbf_h5reportneg(x,code,cerr) \
{int err; if (!(cerr)) {err = (x); if (err < 0) {(cerr)|=code;}}}
#define cbf_reportnez(x,cerr) \
{int err; if (!(cerr)) {err = (x); (cerr)|=err;}}

#define reportFail(f, errorCode, errorVar) \
do { \
if (CBF_SUCCESS == errorVar && !(f)) { \
errorVar |= (errorCode); \
fprintf(stderr, "%s:%d: '" #f "' failed.\n", __FILE__, __LINE__); \
} \
} while (0)
    
#define cbf_reportFail(f, errorVar) \
do { \
if (CBF_SUCCESS == errorVar) { \
const int errorCode = (f); \
if (CBF_SUCCESS != errorCode) \
errorVar |= errorCode; \
} \
} while (0)

    
#ifndef H5_VERS_MAJOR
#define H5_VERS_MAJOR 0
#endif
    
#ifndef H5_VERS_MINOR
#define H5_VERS_MINOR 0
#endif
    
    
#if (H5_VERS_MAJOR>1)||((H5_VERS_MAJOR==1)&&(H5_VERS_MINOR>=8))
    
#define H5Acreatex(loc_id,name,type_id,space_id,acpl_id) \
H5Acreate2(loc_id,name,type_id,space_id,acpl_id,H5P_DEFAULT)
#define H5Dcreatex(loc_id,name,type_id,space_id,dcpl_id) \
H5Dcreate2(loc_id,name,type_id,space_id,H5P_DEFAULT,dcpl_id,H5P_DEFAULT)
#define H5Gcreatex(loc_id,name) \
H5Gcreate2(loc_id,name,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT)
#define H5Gopenx(loc_id,name) H5Gopen2(loc_id,name,H5P_DEFAULT)
    
#else
    
#error HDF5 Version >= 1.8 Required
    
#endif
    
    /* CBF Bookmark */
    
    typedef struct {
        const char * datablock;
        const char * category;
        const char * column;
        unsigned int row;
        int haverow;
    } cbf_bookmark;
    
    
    /* H5File structure */
    
    typedef struct
    {
        int   rwmode;  /* 0 for read-only, 1 for read-write */
		unsigned int slice; /* The slice within the HDF5 data arrays where data will be added */
		hid_t hfile;   /* The HDF5 file */
		hid_t nxid;    /* /entry@NXentry */
		hid_t nxdata; /* /entry/data@NXdata */
		hid_t nxsample; /* /entry/sample@NXsample group */
		hid_t nxinst;  /* /entry/instrument@NXinstrument */
		hid_t nxdetector; /* /entry/instrument/detector@NXdetector */
		hid_t nxmonochromator;  /* /entry/instrument/monochromator@NXmonochromator */
        hid_t rootid;  /* The root CBF database group */
        hid_t dbid;    /* The current datablock in the CBF */
        hid_t sfid;    /* The current saveframe in the current datablock or -1 */
        hid_t catid;   /* The current category */
        hid_t colid;   /* The current column */
        hid_t curnxid; /* The current NeXus group */
        hid_t dataid;  /* The NeXus NXdata group */
        int   flags;   /* Flags for read or write */
        cbf_bookmark
        bookmark;/* Read bookmark to save names for paths */
        
    }
    cbf_h5handle_struct;
    
    typedef cbf_h5handle_struct *cbf_h5handle;
    
    typedef struct
    {
        cbf_handle handle;
        cbf_h5handle h5handle;
        hid_t parent_id;
        haddr_t parent_addr;
        const char * grand_parent_name;
        const char * parent_name;
        size_t capacity;
        size_t path_size;
        hid_t *hid_path;
        haddr_t *haddr_path;
        cbf_bookmark bookmark; /* bookmark in the CBF */
        int incbf;     /* set to 1 when we have descended
                        into a NeXus NXcbf    */
        int incbfdb;   /* set to 1 when we have descended
                        into a NeXus NXcbfdb  */
        int incbfcat;  /* set to 1 when we have descended
                        into a NeXus NXcbfcat */
        int incbfcol;  /* set to 1 when we have descended
                        into a NeXus NXcbfcol */
        int innexus;   /* set to 1 shen we have descended
                        into a NeXus NXexntry */
    }
    cbf_h5Ovisit_struct;
    
	typedef cbf_h5Ovisit_struct *cbf_h5Ovisithandle;
    
	/* Ensure I have a file to do stuff with.
     There are 4 possible cases:
     1: I dont have a file, no name is supplied -> fail
     2: I dont have a file, a name is supplied -> open it
     3: I have a file already, no name supplied -> success
     4: I have a file already, a name is supplied -> success if they match, else fail
     */
	int cbf_h5handle_require_file(const cbf_h5handle handle, const char * name);
    
	/* Ensure I have a top-level NXentry group in the handle, below the file
     There are 4 possible cases:
     1: I dont have an entry, no name is supplied -> fail
     2: I dont have an entry, a name is supplied -> ensure the file is valid, try to open the group
     3: I have an entry already, no name supplied -> success
     4: I have an entry already, a name is supplied -> success if they match, else fail
     */
	int cbf_h5handle_require_entry(const cbf_h5handle handle, hid_t * group, const char * name);
    
	/* Ensure I have an NXsample group in the handle called 'sample' below the entry
     There are 4 possible cases:
     1: I have a sample already -> success
     2: I dont have a sample -> ensure the entry is valid, try to open the group
     */
	int cbf_h5handle_require_sample(const cbf_h5handle handle, hid_t * group);
    
	/* Ensure I have an NXinstrument group in the handle called 'instrument' below the entry
     There are 4 possible cases:
     1: I have an instrument already -> success
     2: I dont have an instrument -> ensure the entry is valid, try to open the group
     */
	int cbf_h5handle_require_instrument(const cbf_h5handle handle, hid_t * group);
    
	/* Ensure I have a detector with the given name in the hdf5 handle
     If a detector by the same name exists, success
     If no detector by that name is found, add it
     
     Do not destroy the returned group - the handle owns it.
     */
	int cbf_h5handle_require_detector(const cbf_h5handle handle, hid_t * group);
    
	/* Ensure I have a monochromator in the hdf5 handle
     If a monochromator exists, success
     If no monochromator exists, add it
     
     Do not destroy the returned monochromator group - the handle owns it.
     */
	int cbf_h5handle_require_monochromator(const cbf_h5handle handle, hid_t * group);
    
    /* Create a dotted CBF location string
     returns a newly allocated string that
     must be freed */
    
    int cbf_location_string(const char* datablock,
                            const char* category,
                            const char* column,
                            unsigned int row,
                            char * * stringout);
    
    /* Conatenate two strings, returning a newly allocated string */
    
    int cbf_strcat(const char * string1, const char * string2,
                   char * * stringout);
    
    /* Either open or create a NeXus group*/
    
    
    int cbf_require_nxgroup(cbf_h5handle h5handle,
                            const char * nxgroup,
                            const char * nxclass,
                            hid_t parent_id,
                            hid_t * groupid);

    
    /* get an axis vector and offset */
    
    
    int cbf_get_axis_vector_and_offset(cbf_handle handle,
                                       const char *axis_id,
                                       double vector[3],
                                       double offset[3]);
    
    /* Compute the cross-product of 2 3-vectors */
    
    int cbf_cross_product(double vecin1[3],
                          double vecin2[3],
                          double vecout[3] );
    
    /* compute the L2 norm of a 3-vector */
    
    double cbf_norm(double vector[3]);
    
    /* compute the product of a scalar and a vector */
    
    int cbf_scalar_product(double scalar, double vecin[3], double vecout[3]);
    
    
    /* Apply a matrix to a vector */
    
    int cbf_apply_matrix(double matrix[3][3], double vecin[3], double vecout[3]);
    
    /* compute the transform from CBF vectors to NeXus vectors
     Use the transpose to transfrom from NeXus vectors to CBF*/
    
    int cbf_get_NX_axis_transform(cbf_handle handle,
                                  double matrix [3][3]);
    
    
    /* Write the HDF5 version of the NeXus axis definitions, if
     the original CBF had axis definitions */
    
    int cbf_write_h5nxaxes(cbf_handle handle, cbf_h5handle h5handle);
    
    
    /* apply a double vector attribute to a group or dataset */
    
    int cbf_apply_h5vector_attribute(hid_t hid,
                                     const char* attribname,
                                     const double* attribvec,
                                     const size_t dimension,
                                     int errorcode);
    
    /* apply a long attribute to a group or dataset */
    
    int cbf_apply_h5longasstr_attribute(hid_t hid,
                                        const char* attribname,
                                        const long attriblong,
                                        int errorcode);
    
    /* apply an integer attribute to a group or dataset as a string */
    
    int cbf_apply_h5intasstr_attribute(hid_t hid,
                                       const char* attribname,
                                       const int attribint,
                                       int errorcode);
    
    /* apply a integer attribute to a group or dataset */
    
    int cbf_apply_h5integer_attribute(hid_t hid,
                                      const char* attribname,
                                      const int attribint,
                                      int errorcode);
    
    /* apply a text attribute to a group or dataset */
    
    int cbf_apply_h5text_attribute(hid_t hid,
                                   const char* attribname,
                                   const char* attribtext,
                                   int errorcode);

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
                                      int errorcode);
    
    /* apply a text dataset to a group */
    
    int cbf_apply_h5text_dataset(hid_t hid,
                                 const char* datasetname,
                                 const char* datasettext,
                                 int errorcode);
    
    /* Write a binary value to an HDF5 file */
    
    int cbf_write_h5binary (cbf_handle handle,
                            cbf_node *column,
                            unsigned int row,
                            cbf_h5handle h5handle);
    
    /* Write an ascii value to an HDF5 file */
    
    int cbf_write_h5ascii (cbf_handle handle,
                           unsigned int row,
                           const char *string,
                           cbf_h5handle h5handle);
    
    /* Write a value to an HDF5 file */
    
    int cbf_write_h5value (cbf_handle handle, cbf_node *column, unsigned int row,
                           cbf_h5handle h5handle);
    
    /* Write a category to an HDF5 file */
    
    int cbf_write_h5category (cbf_handle handle,
                              const cbf_node *category,
                              cbf_h5handle h5handle);
    
    /*  create top-level NXentry */
    
    int cbf_create_NXentry(cbf_h5handle h5handle);
    
    /*  Create an HDF5 Group below NX entry or below curnxid */
    
    int cbf_H5Gcreate_in_handle(cbf_h5handle h5handle,
                                const char * groupname,
                                hid_t * newgroup);
    
    /*  Create an HDF5 NeXus Group below NX entry or below curnxid */
    
    int cbf_H5NXGcreate(cbf_h5handle h5handle,
                        const char * groupname,
                        const char * nxclass,
                        hid_t * newgroup);
    
    /* Free an H5File handle */
    
    int cbf_free_h5handle(cbf_h5handle h5handle);
    
    /* Make an (empty) H5File handle */
    
    int cbf_make_h5handle(cbf_h5handle *h5handle);
    
    /* Close the current saveframe in an HDF5 file */
    
    int cbf_close_h5saveframe (cbf_h5handle h5handle);
    
    /* Write a saveframe name to an HDF5 file
     Make a new group of NeXus class NXcifsf in the NXcif current datablock
     */
    
    int cbf_write_h5saveframename (const cbf_node *saveframename,
                                   cbf_h5handle h5handle);
    
    /* Write a datablock name to an HDF5 file
     Make a new group of NeXus class NXcifdb in the NXcif class root
     */
    
    int cbf_write_h5datablockname (const cbf_node *datablock, cbf_h5handle h5handle);
    
    /* Write a node to an HDF5 file */
    
    int cbf_write_h5node (cbf_handle handle, const cbf_node *node,
                          const cbf_h5handle h5handle);
    
    /* Create an H5File handle */
    
    int cbf_create_h5handle(cbf_h5handle *h5handle,
							const char * h5filename);
    
	/* Create an HDF5 File handle without adding an NXcbf group to it */
	int cbf_create_h5handle2(cbf_h5handle *h5handle,const char * h5filename);
    
    /*  Write cbf to HDF5 file hfile */
    
	int cbf_write_h5file (cbf_handle handle, cbf_h5handle h5handle, int flags);
    
	/* Write a minicbf to a nexus file */
	int cbf_write_minicbf_h5file (cbf_handle handle, cbf_h5handle h5handle, cbf_hdf5_configItemVectorhandle axisConfig, int flags);
    
    /* Open an HDF5 File handle */
    
    int cbf_open_h5handle(cbf_h5handle *h5handle,
                          const char * h5filename);
    
    /* Convert an HDF5 typeclass to a string
     and flag for atomic or not
     copies up to n-1 characters of the
     type string to buffer*/
    
    int cbf_h5type_class_string(H5T_class_t type_class,
                                char * buffer,
                                int * atomic, size_t n );
    
    
    /* Store an HDF5 Dataset in CBF handle, using
     category categoryname, ...*/
    
    int cbf_h5ds_store(cbf_handle handle, haddr_t parent,
                       const char * parent_name,
                       const int target_row,
                       const char * categoryname, hid_t obj_id,
                       hid_t space, hid_t type,
                       const char * name,
                       const int readattrib,
                       void ** value);
    
    
    /* Callback routine for objects in a group */
    
    
    herr_t cbf_object_visit(hid_t loc_id, const char *name,
                            const H5L_info_t *info,
                            void *op_data);
    
    
    /* Read an HDF5 file */
    
    int cbf_read_h5file(cbf_handle handle, cbf_h5handle h5handle, int flags);
    
    
    /* go to a bookmark in the cbf handle */
    
    int cbf_goto_bookmark(cbf_handle handle, cbf_bookmark bookmark);
    
    /* get a bookmark from the current information in a cbf handle */
    
    int cbf_get_bookmark(cbf_handle handle, cbf_bookmark * bookmark);
    


#ifdef __cplusplus

}

#endif


#endif /* CBF_HDF5_H */
