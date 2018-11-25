/**********************************************************************
 * cbf_ws.c                                                           *
 *                                                                    *
 * Version 0.9.0 26 April 2009                                        *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006, 2007 Herbert J. Bernstein                      *
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


/*     
 1.	The prefix ws is reserved for special whitespace categories and tags.
 2.	For any given tag, <tag>, in a category, <category>,
 whitespace and comments for the tag and its value(s) will be given by
 _ws_<category>.<tag>.  The category ws_<category> is distinct from
 <catgeory> and from ws__category.  For DDL1 and DDLm when tags not 
 respecting the dotted category notation of DDL2 are used, with a tag
 of the form <tag> the tag for whitepace is _ws_<tag>
 3.	For any given category, <category>, whitespace and comments for 
 the category as a whole will be given by _ws__<category>.ws_ (note 
 the double underscore).  This category ws__<category> is distinct 
 from <category>. 
 4.	For any given data block or save frame, whitespace and comments 
 for the data block or save frame as whole will be given by 
 _ws_.ws_
 5.	Whitespace and comments may be given as a prologue (intended to 
 be presented before the element), zero or more emlogues (intended 
 to be presented between the initial sub-element, e.g. "loop_" or 
 the tag name and the rest of the element) or an epilogue (intended 
 to be presented after the element as a whole).  We use the term 
 "-logues" for prologues, emlogues and epilogue   The -logues for 
 an element may be given as a single string, in which case only an 
 epilogue is intended or as a bracketed construct (using parentheses) 
 with multiple -logues.   If only one -logue is given, it is the 
 epilogue.  If two -logues are given, the first is the prologue and the 
 second id the epilogue.  If more emlogues are given than there are 
 breaks in the element, the extra emolgues are prepended to the epilogue.  
 The emlogues for a bracketed construct may also be bracketed constructs 
 to provide whitespace and comments within bracketed constructs.
 6.	A prologue, emlogue or epilogue is a string of one or more lines 
 starting with a optional colon-terminated column position for that 
 line, followed by optional whitespace, followed by an optional comment.  
 If no column position is given the whitespace begins at the next syntactically 
 valid location.  If a column position is given, then, on writing, a new 
 line will be started if necessary to align to that column.  A column 
 position with no whitespace and no comment simply provides alignment 
 for the next sub-element.  If the end of a -logue line is a comment, 
 whatever follows will be forced to a new line 
 
 */

#ifdef __cplusplus

extern "C" {
    
#endif
    
#include "cbf_alloc.h"
#include "cbf_ws.h"
    
    /* create a handle to a bracket_tree */
    
    /*  A bracket tree is a pointer to a cbf_node of type CBF_BKT, CBF_BRC or
        CBF_PRN
     
        Each child, in order is one of the following:
     
          a bracket tree 
          a value of type CBF_VALUE, in which case the name is a string
            following the CBF value conventions
    */
    
    
    /* insert a column number into the buffer for the commentfile */
    
    int cbf_set_ws_column (cbf_file * commentfile, size_t columnnumber) {
        
        int ii;
        
        char numberstring[21];
        
        if (columnnumber < 1 || columnnumber >= 999999999) return CBF_ARGUMENT;
        
        sprintf (numberstring,"%ld:",(long int)columnnumber);
        
        for (ii=0; ii < (ssize_t)strlen(numberstring); ii++) {
            
            cbf_failnez(cbf_save_character (commentfile, numberstring[ii]))
            
        }
        
        return 0;
        
    	
    }
    
    /* Apply pending whitespace to new node */
    
    int cbf_apply_ws(cbf_handle handle) {
        
        cbf_file * file;
        
        cbf_node * node;
        
        cbf_node * datablock;
        
        cbf_node * category;
        
        cbf_node * column;
        
        const char* catname;
        
        const char* colname;
                
        char * value;
        
        char * tvalue;
        
        void * vvalue;
        
        void * vtvalue;
        
        size_t value_size, value_used, ii, iii, start, end;
        
        int have_single_quote, have_double_quote, doas_scq, doas_scqfold;
        
        if ( !handle ) return CBF_ARGUMENT;
        
        if ( !(node=handle->node) ) return CBF_ARGUMENT;
        
        if ( ! (file=handle->commentfile) || 
            !(file->buffer) || 
            (file->buffer_used == 0)) return 0;
        
        /* swap out the whitespace buffer */
        
        value = file->buffer;
        
        fprintf(stderr," cbf_apply_ws: \n(%s)\n",value);
        
        value_size = file->buffer_size;
        
        value_used = file->buffer_used;
        
        file->buffer = NULL;
        
        file->buffer_size = 0;
        
        file->buffer_used = 0;
        
        /*  
         
         Scan the whitespace and comment string looking for quote marks
         or comments
         
         */
        
        start = 0; end = value_used-1;
        
        while (start < value_used) {
            
            have_single_quote = have_double_quote = doas_scq = doas_scqfold = 0;
            
            for (ii=start; ii < value_used; ii++) {
                
                if (value[ii] == '\'' && 
                    (ii==value_used-1 || !value[ii+1] || (isspace(value[ii+1])) 
                     || value[ii+1] == ','
                     || value[ii+1] == ')'
                     || value[ii+1] == '}'
                     || value[ii+1] == ']' )) have_single_quote = 1;
                
                if (value[ii] == '"'  && 
                    (ii==value_used-1 || !value[ii+1] || (isspace(value[ii+1])) 
                     || value[ii+1] == ','
                     || value[ii+1] == ')'
                     || value[ii+1] == '}'
                     || value[ii+1] == ']' )) {
                    
                    have_double_quote = 1;
                    
                    if ( have_single_quote ) doas_scq = 1;
                    
                }
                
                if (value[ii]=='\n') doas_scq = 1;
                
                if (ii > start && value[ii] == ';' && value[ii-1]=='\n') {
                    
                    doas_scq = doas_scqfold = 1;
                    
                }
                
                if (value[ii] == ',' || value[ii] == ')' || value[ii] == '}' || value[ii] == ']') {
                    
                    break;
                    
                }
                
                if (value[ii] == '#') {
                    
                    doas_scq = 1;
                    
                    for (iii=ii+1; iii < value_used; iii++) {
                        
                        if (value[iii] == '\n') break;
                        
                    }
                    
                    ii = iii;
                    
                    end = iii;
                    
                    continue;
                    
                }
                
                end = ii;
                
            }
            
            if (value[end] != '\n' && doas_scq) doas_scqfold = 1;
            
            if (doas_scq) {
                
                cbf_failnez(cbf_save_character(file,'\n'))
                
                cbf_failnez(cbf_save_character(file,';'))
                
                if (doas_scqfold) {
                    cbf_failnez(cbf_save_character(file,'\\'))
                    cbf_failnez(cbf_save_character(file,'\n'))                  
                }
                
            } else {    
                
                if (have_double_quote) {
                    
                    
                    cbf_failnez(cbf_save_character(file,'\''))
                    
                } else {
                    
                    cbf_failnez(cbf_save_character(file,'"'))
                    
                }
            }
            
            for (ii=start; ii<=end; ii++) {
                
                cbf_failnez(cbf_save_character(file,value[ii]))
                
                if (value[ii] == '\n' && ii > start && ii < end) {
                    
                    cbf_failnez (cbf_set_ws_column  (file, 1))
                    
                }
                
                
                if (doas_scqfold && ii > start && value[ii]==';' && value[ii-1]=='\n') {
                    
                    cbf_failnez(cbf_save_character(file,'\\'))
                    
                    cbf_failnez(cbf_save_character(file,'\n'))
                    
                }
                
                
            }
            
            if (doas_scq) {
                
                if (doas_scqfold ) {
                    cbf_failnez(cbf_save_character(file,'\\'))
                    cbf_failnez(cbf_save_character(file,'\n'))                  
                    cbf_failnez(cbf_save_character(file,';'))
                    cbf_failnez(cbf_save_character(file,'\n'))
                } else {
                    if (value[end]!='\n') {
                        cbf_failnez(cbf_save_character(file,'\n'))
                    }
                    cbf_failnez(cbf_save_character(file,';'))
                }
                
            } else {    
                
                if (have_double_quote) {
                    
                    
                    cbf_failnez(cbf_save_character(file,'\''))
                    
                } else {
                    
                    cbf_failnez(cbf_save_character(file,'"'))
                    
                }
            }
            
            start = end+1;
            
            end = value_used-1;
            
            if (start < value_used) {
                
                cbf_failnez(cbf_save_character(file,value[start]))
                
                start++;
                
                continue;
                
            }
            
        }
        
        
        vvalue = (void *)value;
        
        cbf_failnez(cbf_free ((void **) &vvalue,
                              &value_size))
        
        fprintf(stderr," cbf_apply_ws (converted): \n(%s)\n",file->buffer);

        
        /*  construct the value 
         
         On entry file->buffer[0] is ' ' for a single prologue string or '('
         if there embedded emlogues and epilogues.
         file->buffer[1] is ' ' for a single line or '\\' for multiline
         string, or for a string with embedded double quote marks.
         
         For a single prologue, one line string with no embedded double
         quote marks, the result will be ("string",,)
         
         For a single prologue, one line string with no embedded single
         quote marks, the result will be ('string',,)
         
         For a single prologue, multiline string, or string with embedded
         quote marks, the result will be (\n;\\\nstring\\\n;\n,,) with
         internal ;'s and \'s protected by trailing \\\n sequences.
         
         If there are emlogues the same transformations will be applied.
         
         */
        
        if (node->type == CBF_DATABLOCK || node->type == CBF_SAVEFRAME) {
            
            fprintf(stderr,"in CBF_DATABLOCK or CBF_SAVEFRAME\n");

            catname = cbf_copy_string(NULL,"ws_",0);
            
            colname = cbf_copy_string(NULL,"ws_",0);
            
            if (!catname || !colname) return CBF_ALLOC;
            
            cbf_failnez(cbf_make_child(&category,node,CBF_CATEGORY,catname))
            
            cbf_failnez(cbf_make_child(&column,category,CBF_COLUMN,colname))
            
            value = (char *)cbf_copy_string(NULL,file->buffer,CBF_TOKEN_PRNSTRING);
            
            if (!value) return CBF_ALLOC;
            
            cbf_failnez(cbf_set_columnrow(column,0,value,1))
            
            cbf_failnez(cbf_reset_buffer(file))
            
            return 0;
            
        }
        
        if (node->type == CBF_CATEGORY) {
            
            fprintf(stderr,"in CBF_CATEGORY\n");
          
            catname = cbf_copy_strings(NULL,"ws__",(node->name),0);
            
            if (!catname) return CBF_ALLOC;
            
            /* Find the save frame or data block node */
            
            if (cbf_find_parent (&datablock, node, CBF_SAVEFRAME)) {
                
                cbf_failnez (cbf_find_parent (&datablock, node, CBF_DATABLOCK))
                
            }
            
            cbf_failnez(cbf_make_child(&category,datablock,CBF_CATEGORY,catname))
            
            colname = cbf_copy_string(NULL,"ws_",0);
            
            if (!colname) return CBF_ALLOC;
            
            cbf_failnez(cbf_make_child(&column,category,CBF_COLUMN,colname))
            
            value = (char *)cbf_copy_string(NULL,file->buffer,CBF_TOKEN_PRNSTRING);
            
            if (!value) return CBF_ALLOC;
            
            cbf_failnez(cbf_set_columnrow(column,0,value,1))
            
            cbf_failnez(cbf_reset_buffer(file))
            
            return 0;
            
        }
        
        if (node->type == CBF_COLUMN) {
            
            unsigned int row, rows;
            
            fprintf(stderr,"in CBF_COLUMN\n");
            
            if (node->name[0] != '_') {
                
                colname = cbf_copy_string(NULL,node->name,0);
                
            } else {
                
                colname = cbf_copy_strings(NULL,"_ws",node->name,0);
                
            }
            
            if (!colname) return CBF_ALLOC;
            
            rows = node->children;
            
            row = rows>0?rows-1:0;
            
            /* Find the category and pick up the name */
            
            cbf_failnez(cbf_find_parent(&category, node, CBF_CATEGORY))                
            
            catname = cbf_copy_strings(NULL,"ws_",(node->name),0);
            
            if (!catname) return CBF_ALLOC;
            
            /* Find the save frame or data block node */
            
            if (cbf_find_parent (&datablock, node, CBF_SAVEFRAME)) {
                
                cbf_failnez (cbf_find_parent (&datablock, node, CBF_DATABLOCK))
                
            }
            
            cbf_failnez(cbf_make_child(&category,datablock,CBF_CATEGORY,catname))
            
            cbf_failnez(cbf_make_child(&column,category,CBF_COLUMN,colname))
            
            if (column->children > row && column->child[row]) {
                
                /*  Prior ws loaded, need to combine with the new string */
                
                if (((char *)(column->child[row]))[0] != CBF_TOKEN_PRNSTRING) return CBF_FORMAT;
                
                tvalue = (char *)cbf_copy_strings(NULL,((char *)(column->child[row]))+1,",",0);
                
                if (!tvalue) return CBF_ALLOC;
                
                value = (char *)cbf_copy_strings(NULL,tvalue,file->buffer,CBF_TOKEN_PRNSTRING);
                
                vtvalue = (void *)tvalue;
                
                cbf_failnez(cbf_free ((void **) &vtvalue, NULL))
                
                if (!value) return CBF_ALLOC;
                
            } else {
                
                value = (char *)cbf_copy_string(NULL,file->buffer,CBF_TOKEN_PRNSTRING);
                
                if (!value) return CBF_ALLOC;
                
            }
            
            cbf_failnez(cbf_set_columnrow(column,row,value,1))
            
            cbf_failnez(cbf_reset_buffer(file))
            
            return 0;
            
        }
        
        
        return 0;
        
    }
        
    
    /* Write an ascii whitespace value */
    
    int cbf_write_ws_ascii (const char *string, cbf_file *file) {
        
        int ii, iii, istart;
        
        char initc=' ', termc=' ';
        
        
        
        
        /* Check the arguments */
        
        if (!string) return CBF_ARGUMENT;
        
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
        
        
        /* Write the value */
        
        /* First check if anything other than whitespace appears
         */
        
        switch (*string) {
            case  CBF_TOKEN_WORD:
            case  CBF_TOKEN_NULL:
            case  CBF_TOKEN_SQSTRING:
            case  CBF_TOKEN_DQSTRING:
            case  CBF_TOKEN_SCSTRING:
            case  CBF_TOKEN_TSQSTRING:
            case  CBF_TOKEN_TDQSTRING:
                
                istart = 0;
                
                for (ii=0; ii < (ssize_t)strlen(string+1); ii++) {
                    
                    if ((string+1)[ii] == ' ' || (string+1)[ii] == '\t') {
                        
                        if (file->column+ii >= file->columnlimit) {
                            
                            cbf_failnez (cbf_write_character (file, '\n'))
                            
                            istart = ii+1;
                            
                            continue;
                        }
                    }
                    
                    if ((string+1)[ii] != '#') {
                        
                        if (file->column+ii >= file->columnlimit) {
                            
                            cbf_failnez (cbf_write_character (file, '\n'))
                            
                            istart = ii;
                            
                            continue;
                        }
                        
                        cbf_failnez (cbf_write_character (file, '#'))
                        
                    }
                    
                    for (iii=istart; iii< ii; iii++) {
                        
                        cbf_failnez (cbf_write_character (file, (string+1)[iii]))
                        
                        if (file->column == 0) {
                            
                            cbf_failnez (cbf_write_character (file, '#')) 
                            
                        }
                        
                        
                    }
                    
                    for (iii=ii; iii< (ssize_t)strlen(string+1); iii++) {
                        
                        if (file->column >= file->columnlimit) {
                            
                            cbf_failnez (cbf_write_character (file, '\n'))
                            
                            if ((string+1)[iii] != '#') {
                                
                                cbf_failnez (cbf_write_character (file, '#')) 
                                
                            }
                            
                        }
                        
                        
                        cbf_failnez (cbf_write_character (file, (string+1)[iii]))
                        
                        if (file->column == 0) {
                            
                            cbf_failnez (cbf_write_character (file, '#')) 
                            
                        }
                        
                    }
                    
                    if (file->column > 0) {
                        
                        cbf_failnez (cbf_write_character (file, '\n'))
                        
                    }
                    
                    return 0;
                    
                }
                
                for (iii=istart; iii< ii; iii++) {
                    
                    cbf_failnez (cbf_write_character (file, (string+1)[iii]))
                    
                }
                
                break;
                
            case CBF_TOKEN_PRNSTRING:
            case CBF_TOKEN_BRCSTRING:
            case CBF_TOKEN_BKTSTRING:
                
                switch (*string) {
                    case CBF_TOKEN_PRNSTRING: initc = '('; termc = ')'; break;
                    case CBF_TOKEN_BRCSTRING: initc = '{'; termc = '}'; break;
                    case CBF_TOKEN_BKTSTRING: initc = '['; termc = ']'; break;
                }
                
                
                istart = 0;
                
                for (ii=0; ii < (ssize_t)strlen(string+1); ii++) {
                    
                    if ((string+1)[ii] == ' ' || (string+1)[ii] == '\t') {
                        
                        if (file->column+ii >= file->columnlimit) {
                            
                            cbf_failnez (cbf_write_character (file, '\n'))
                            
                            istart = ii+1;
                            
                            continue;
                        }
                    }
                    
                    for (iii=istart; iii< ii; iii++) {
                        
                        cbf_failnez (cbf_write_character (file, (string+1)[iii]))
                        
                        if (file->column == 0) {
                            
                            cbf_failnez (cbf_write_character (file, '#')) 
                            
                        }
                        
                        
                    }
                    
                    if (file->column+ii >= file->columnlimit-1) {
                        
                        cbf_failnez (cbf_write_character (file, '\n'))
                        
                        continue;
                    }
                    
                    cbf_failnez (cbf_write_character (file, '#'))
                    
                    cbf_failnez (cbf_write_character (file, initc))
                    
                    for (iii=ii; iii< (ssize_t)strlen(string+1); iii++) {
                        
                        if (file->column >= file->columnlimit) {
                            
                            cbf_failnez (cbf_write_character (file, '\n'))
                            
                            if ((string+1)[iii] != '#') {
                                
                                cbf_failnez (cbf_write_character (file, '#')) 
                                
                            }
                            
                        }
                        
                        if (file->column == 0) {
                            
                            cbf_failnez (cbf_write_character (file, '#')) 
                            
                        }
                        
                        if (file->column >= file->columnlimit) {
                            
                            cbf_failnez (cbf_write_character (file, '\n'))
                            
                            if ((string+1)[iii] != '#') {
                                
                                cbf_failnez (cbf_write_character (file, '#')) 
                                
                            }
                            
                        }
                        
                        cbf_failnez (cbf_write_character (file, (string+1)[iii]))
                        
                    }
                    
                    if (file->column >= file->columnlimit-1) {
                        
                        cbf_failnez (cbf_write_character (file, '\n'))
                        
                        if ((string+1)[iii] != '#') {
                            
                            cbf_failnez (cbf_write_character (file, '#')) 
                            
                        }
                        
                        cbf_failnez (cbf_write_character (file, termc))
                        
                    }
                    
                    if (file->column > 0) {
                        
                        cbf_failnez (cbf_write_character (file, '\n'))
                        
                    }
                    
                    return 0;
                    
                }
                
                for (iii=istart; iii< ii; iii++) {
                    
                    cbf_failnez (cbf_write_character (file, (string+1)[iii]))
                    
                }
                
                break;
                
        }
        
        return 0;
        
        
    }
    
    
    /* scan a string for a bracketed substring at level targetdepth
     and index targetindex */
    
    int cbf_find_bracketstring(const char * string, 
                               const char * stringlimit,
                               const char * stringtype,
                               char * * bracketstring,
                               char * * bracketstringlimit,
                               int  * more,
                               size_t   targetdepth, 
                               size_t   targetindex ) {
        
        int *tokentype=NULL;
        
        int **vtokentype;
        
        size_t tokentype_size;
        
        int * state;
        
        int ** vstate;
        
        size_t state_size;
        
        int * index;
        
        int ** vindex;
        
        int depth;
        
        size_t index_size;
        
        
        const char * cptr;
        
        const char * line;
        
        char c, cprev, cprevprev, cprevprevprev;
        
        size_t length;
        
        cptr = string;
        
        tokentype_size = state_size = index_size = 0;
        
        vtokentype = &tokentype;
        
        vstate = &state;
        
        vindex = &index;
        
        depth = 0;
        
        /* validate arguments */
        
        if ((!string)              ||
            (!stringlimit)         ||
            (!stringtype)          ||
            (string > stringlimit) ||
            (!bracketstring)       ||
            (!bracketstringlimit)  ||
            (!more)                 ) return CBF_ARGUMENT;
        
        /* determine if this is a simple string or a bracketed string */
        
        switch (*stringtype) {
                
            case CBF_TOKEN_WORD:
            case CBF_TOKEN_SQSTRING:
            case CBF_TOKEN_DQSTRING:
            case CBF_TOKEN_SCSTRING:
            case CBF_TOKEN_TSQSTRING:
            case CBF_TOKEN_TDQSTRING:
                *more = 0;
                if (targetindex > 0 || targetdepth > 1) {
                    *bracketstring = *bracketstringlimit = NULL;
                } else {
                    *bracketstring = (char *)string;
                    *bracketstringlimit = (char *)stringlimit;
                }
                return 0;
                break;
                
            case CBF_TOKEN_BKTSTRING:
            case CBF_TOKEN_BRCSTRING:
            case CBF_TOKEN_PRNSTRING:
                
                tokentype[depth-1]= *stringtype;
                break;
                
            default:
                *bracketstring = *bracketstringlimit = NULL;
                return CBF_ARGUMENT;
                
        }
        
        
        
        
        /* strip any initial space characters */
        
        
        cprev = cprevprev = cprevprevprev = 0;
        
        while (cptr < stringlimit && isspace(*cptr)) { 
            cprevprev= cprev;
            cprev = *cptr; 
            cptr++; 
        }
        
        c = *cptr;
        
        line = cptr;
        
        depth = 1;
        
        tokentype_size = state_size = index_size = 100;
        
        cbf_failnez(cbf_alloc((void **)vtokentype, NULL, sizeof(int), tokentype_size))
        
        cbf_onfailnez(cbf_alloc((void **)vstate, NULL, sizeof(int), state_size), cbf_free((void **)vtokentype,NULL))
        
        cbf_onfailnez(cbf_alloc((void **)vindex, NULL, sizeof(int), index_size), 
                      {cbf_free((void **)vtokentype, NULL); cbf_free((void **)vstate,NULL);})
        
        state[depth-1] = index[depth-1] = 0;
        
        length = 0;
        
        while (cptr < stringlimit) {
            
            int q3;          /* flag for treble quote */
            
            int savechar=1;  /* flag to save the character */
            
            int breakout=0;  /* flag to break out of the loop */
            
            int string;
            
            q3 = (length > 3 &&  (line[0]=='\'' || line[0]=='"')
                  && line[1] ==  line[0] && line[2] == line[1]);
            
            if (depth == 0) {
                
                string = !(!q3 && cprev == line[0] && length > 1 && isspace(c))
                && !( q3 && length > 5
                     && cprev ==  line[0] 
                     && cprevprev == line[1] 
                     && cprevprevprev == line[2]
                     && isspace(c));
                
                
                if ( string && ( (c == '\n' && !q3)  ) )  {
                    
                    string = 0;
                    
                }
                
                /* handle the bracketed cases */
                
                savechar = 0;
                
                breakout = 0;
                
                switch (state[depth-1]) {
                        
                        /* 
                         In state 1 we have started an item and are looking for
                         the end of the item.
                         
                         The possibilities are that the item we are parsing is
                         1.  A single-quoted string
                         2.  A double-quoted string
                         3.  A triple-single-quoted string (only if PARSE_TRIPLE_QUOTES is set) 
                         4.  A triple-double-quoted string (only if PARSE_TRIPLE_QUOTES is set)
                         5.  A parenthesis-bracketed string
                         6.  A brace-bracketed string
                         7.  A bracket-bracketed string
                         8.  A blank-bracketed string (only if PARSE_LIBERAL_BRACKETS is set)
                         9.  A bracket-bracketed item (only if PARSE_LIBERAL_BRACKETS is not set)
                         
                         In all cases, the depth will have been increased by 1 and the
                         appropriate token type stored in tokentype[depth-1], and
                         index[depth-1] will accumulate the number of characters
                         
                         It is important that this code come before the code
                         for state 2 so that we can fall through.
                         
                         */
                        
                    case (1):
                        
                        /* See if we are looking for a terminal quote mark */
                        
                        if (cbf_token_term(tokentype[depth-1])=='\'' 
                            || cbf_token_term(tokentype[depth-1])=='"' ) {
                            
                            string = ( cprev != cbf_token_term(tokentype[depth-1])
                                      || index[depth-1] < 1 
                                      || !(isspace(c)||c==','||c==cbf_token_term(tokentype[depth-2]) ) );
                            
                            if (index[depth-1] == 2  && c==cprev && cprev==cprevprev ) {
                                tokentype[depth-1] = 
                                tokentype[depth-1]==CBF_TOKEN_SQSTRING?CBF_TOKEN_TSQSTRING:CBF_TOKEN_TDQSTRING;
                            }
                            
                            if (tokentype[depth-1]==CBF_TOKEN_TSQSTRING 
                                || tokentype[depth-1]==CBF_TOKEN_TDQSTRING) {
                                string = !(cprev == cbf_token_term(tokentype[depth-1])
                                           && cprevprev == cprev 
                                           && cprevprevprev==cprevprev
                                           && index[depth-1] > 5
                                           && (isspace(c)||c==','||c==cbf_token_term(tokentype[depth-2])
                                               ));
                                
                            } else {
                                
                                if ( string && ( c == '\n') )  {
                                    
                                    string = 0;
                                    
                                }
                                
                                
                            }
                            
                            if ( !string ) {
                                depth--;  /* drop down from this level */
                                state[depth-1]++;
                                
                                savechar = 0;
                                if (c == '\n') {
                                    
                                    break;
                                    
                                }
                                
                                if (!isspace(c)) savechar = 1;
                                /* intentionally fail to do a break */
                                
                            } else {
                                
                                savechar = 1;
                                breakout = 0;
                                index[depth-1]++;
                                break;
                                
                            }
                            
                        } else {
                            
                            /* We are not looking for a terminal quote mark */
                            
                            /* on a blank-delimited item we may end on a blank,
                             comma or the next level terminator */
                            
                            if (cbf_token_term(tokentype[depth-1])==' ') {
                                
                                
                                /* we are still in a blank-delimited item if the
                                 character is not a space, not a comma and not the
                                 bracket from the next level.  The string also ends
                                 at eol or eof */
                                
                                string = ( !isspace(c)) 
                                && !(c==','||c==cbf_token_term(tokentype[depth-2]) );
                                
                                if ( string && ( c == '\n' ) )  {
                                    
                                    string = 0;
                                    
                                }
                                
                                if ( !string ) {
                                    depth--;
                                    state[depth-1]++;
                                    savechar = 0;
                                    breakout = 0;
                                    break;
                                    
                                } else {
                                    savechar = 1;
                                    breakout = 0;
                                    index[depth-1]++;
                                    break;
                                }
                                
                            } else {
                                
                                if (cbf_token_term(tokentype[depth-1])==';') {
                                    
                                    string = ( cprevprev != '\n'
                                              || cprev != cbf_token_term(tokentype[depth-1]) 
                                              || index[depth-1] < 3 
                                              || !(isspace(c) || c==','||c==cbf_token_term(tokentype[depth-2]) ) );
                                    
                                    
                                    if ( !string ) {
                                        depth--;
                                        state[depth-1]++;
                                        savechar = 0;
                                        breakout = 0;
                                        break;
                                    } else {
                                        savechar = 1;
                                        breakout = 0;
                                        index[depth-1]++;
                                        break;
                                    }
                                    
                                }
                                
                            }
                            
                        }
                        
                        /* 
                         In state 2 we have completed an item and need to scan for
                         a comma or a terminator.  Since we are not breaking out the
                         items, we merge this case with state 0.
                         
                         */
                        
                    case (2):
                        
                        /* In state 0 we are looking for an item for the construct
                         We may encounter a comment, a space, a comma, a terminator 
                         for the construct or the beginning of an item
                         
                         */
                        
                    case (0):
                        
                        if (c=='#') do {
                            
                            cprevprevprev = cprevprev;
                            
                            cprevprev = cprev;
                            
                            cprev = c;
                            
                            c = *(++cptr);
                            
                            if (cptr >= stringlimit) c = EOF;
                            
                        } while (c != '\n' && c != EOF);
                        
                        if (c==EOF) {
                            
                            /* int ttype=tokentype[0]; */
                            
                            /*cbf_log(handle,"file ended before end of bracketed construct", 
                             CBF_LOGWARNING|CBF_LOGSTARTLOC); */
                            
                            cbf_free((void **)vtokentype, NULL);
                            cbf_free((void **)vstate, NULL);
                            cbf_free((void **)vindex, NULL);
                            
                            /* return cbf_return_text (STRING, val, &line [1], ttype ); */
                            
                            
                        }
                        
                        if (isspace(c)) {
                            
                            savechar = 0;
                            breakout = 0;
                            break;
                            
                        }
                        
                        
                        if (c==',' ) {
                            
                            savechar = 1;           /* Keep the comma */
                            breakout = 0;
                            /* depth--; */          /* Stay at this level */
                            index[depth-1]++;
                            state[depth-1] = 0;     /* Search for a non-blank */
                            break;
                            
                        }
                        
                        if (c==cbf_token_term(tokentype[depth-1]) 
                            && (cbf_token_term(tokentype[depth-1])==';'?'\n':cprev)==cprev) {
                            
                            savechar = 1;
                            breakout = 0;
                            /* depth--; */ /* end the token */
                            if (depth > 0)depth--; /* end the bracket */
                            
                            if (depth==0) {
                                
                                /* int ttype=tokentype[0]; */
                                
                                cbf_free((void **)vtokentype, NULL);
                                cbf_free((void **)vstate, NULL);
                                cbf_free((void **)vindex, NULL);
                                
                                /* return cbf_return_text (STRING, val, &line [1], ttype); */
                                
                                
                            }
                            
                            state[depth-1]++;
                            break;
                            
                            
                        }
                        
                        if ( !isspace(c)) {
                            
                            if (state[depth-1]==2) {
                                
                                /*  cbf_onfailnez (cbf_save_character_trim (file, ' '), 
                                 { cbf_free((void **)vtokentype, NULL);
                                 cbf_free((void **)vstate, NULL);
                                 cbf_free((void **)vindex, NULL);}) */
                                
                                state[depth-1]=0;
                                
                            }
                            
                            state[depth-1]++;
                            
                            /* cbf_onfailnez (cbf_save_character_trim (file, c), 
                             { cbf_free((void **)vtokentype, NULL);
                             cbf_free((void **)vstate, NULL);
                             cbf_free((void **)vindex, NULL);}) */
                            
                            savechar = 0;
                            
                            depth++;
                            
                            
                            if (depth > (ssize_t)tokentype_size) {
                                
                                cbf_onfailnez(cbf_realloc((void **)vtokentype, NULL, sizeof(int),tokentype_size*2),
                                              {cbf_free((void **)vtokentype, NULL);
                                              cbf_free((void **)vstate, NULL);
                                              cbf_free((void **)vindex, NULL);})
                                
                                tokentype_size *= 2;
                                
                            }
                            
                            if (depth > (ssize_t)state_size) {
                                
                                cbf_onfailnez(cbf_realloc((void **)vstate, NULL, sizeof(int),state_size*2),
                                              {cbf_free((void **)vtokentype, NULL);
                                              cbf_free((void **)vstate, NULL);
                                              cbf_free((void **)vindex, NULL);})
                                
                                state_size *= 2;
                                
                            }
                            
                            
                            if (depth > (ssize_t)index_size) {
                                
                                cbf_onfailnez(cbf_realloc((void **)vindex, NULL, sizeof(int),index_size*2),
                                              {cbf_free((void **)vtokentype, NULL);
                                              cbf_free((void **)vstate, NULL);
                                              cbf_free((void **)vindex, NULL);})
                                
                                index_size *= 2;
                                
                            }
                            
                            index[depth-1] = state[depth-1] = 0;
                            
                            switch(c) {
                                    
                                case ('\'') : tokentype[depth-1]=CBF_TOKEN_SQSTRING; state[depth-1] = 1; break;
                                case ('"')  : tokentype[depth-1]=CBF_TOKEN_DQSTRING; state[depth-1] = 1; break;
                                case ('[')  : tokentype[depth-1]=CBF_TOKEN_BKTSTRING; break;
                                case ('{')  : tokentype[depth-1]=CBF_TOKEN_BRCSTRING; break;
                                case ('(')  : tokentype[depth-1]=CBF_TOKEN_PRNSTRING; break;
                                case (';')  : 
                                    if (cprev=='\n') {
                                        tokentype[depth-1]=CBF_TOKEN_SCSTRING; state[depth-1] = 1; break;
                                    }
                                default:  tokentype[depth-1]= CBF_TOKEN_WORD; state[depth-1] = 1; break;
                                    
                            }
                            
                            breakout = 0;
                            break;
                            
                        }
                        
                        
                        savechar = 1;
                        breakout = 0;
                        break;
                        
                }
                
                if (savechar) {
                    
                    /* cbf_onerrornez (cbf_save_character_trim (file, c), val,
                     {cbf_free((void **)vtokentype, NULL);
                     cbf_free((void **)vstate, NULL);
                     cbf_free((void **)vindex, NULL);}) */
                    
                }
                
                if (breakout) break;
                
                
                
            }
        }
        
        return 0;
    }
    
    
    /* Write a ws and comment value to a file */
    
    int cbf_write_ws_value (cbf_node *column, unsigned int row,
                            cbf_file *file, int isbuffer, logue whichlogue)
    {
        const char *text;
        
        CBF_UNUSED( isbuffer );
        
        CBF_UNUSED( whichlogue );
        
        /* Check the arguments */
        
        if (!column)
            
            return CBF_ARGUMENT;
        
        if (row >= column->children)
            
            return CBF_NOTFOUND;
        
        
        /* Get the value */
        
        cbf_failnez (cbf_get_columnrow (&text, column, row))
        
        
        /* Missing value? */
        
        if (!text) return 0;
        
        
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
            
            return cbf_write_ws_ascii (text, file);
        
        /* Fail */
        
        return CBF_ARGUMENT;
    }
    
    
    
    int cbf_write_ws_prologue(const cbf_node *node, cbf_file *file, int isbuffer) {
        
        unsigned int row;
        
        cbf_node *subnode;
        
        /* Check the arguments */
        
        if (!node || !file)
            
            return CBF_ARGUMENT;
        
        /* Check if white space is to be processed */
        
        if ( (file->write_headers & CBF_PARSE_WS) == 0 )
            
            return 0;
        
        /* Follow any links */
        
        node = cbf_get_link (node);
        
        
        /* Node type */
        
        switch (node->type) {
                
            case CBF_ROOT:
                
                return 0;  break;
                
            case CBF_DATABLOCK:
                
            case CBF_SAVEFRAME:
                
                if (!cbf_find_typed_child(&subnode, node,"ws_",CBF_CATEGORY)) {
                    
                    if (!cbf_find_child( &subnode, subnode,"ws_")) {
                        
                        for (row = 0; row < subnode->children; row++) {
                            
                            cbf_failnez(cbf_write_ws_value(subnode,row,file,isbuffer,pro))
                            
                        }
                        
                    }
                    
                }
                
                return 0;  break;
                
            case CBF_CATEGORY:
                
                if (!cbf_cistrcmp(node->name,"ws_")) {return 0;}
                
                if (!cbf_find_child( &subnode, node,"ws__prologue")) {
                    
                    for (row = 0; row < subnode->children; row++) {
                        
                        cbf_failnez(cbf_write_ws_value(subnode,row,file,isbuffer,pro))
                        
                    }
                    
                }
                
                
                return 0;  break;
                
                
            default:
                
                return CBF_ARGUMENT;
                
        }
        
        
    }
    
    int cbf_write_ws_emlogue(const cbf_node *node, cbf_file *file, int isbuffer) {
        
        unsigned int row;
        
        cbf_node *subnode;
        
        /* Check the arguments */
        
        if (!node || !file)
            
            return CBF_ARGUMENT;
        
        /* Check if white space is to be processed */
        
        if ( (file->write_headers & CBF_PARSE_WS) == 0 )
            
            return 0;
        
        /* Follow any links */
        
        node = cbf_get_link (node);
        
        
        /* Node type */
        
        switch (node->type) {
                
            case CBF_ROOT:
                
                return 0;  break;
                
            case CBF_DATABLOCK:
                
            case CBF_SAVEFRAME:
                
                if (!cbf_find_typed_child(&subnode, node,"ws_",CBF_CATEGORY)) {
                    
                    if (!cbf_find_child( &subnode, subnode,"emlogue")) {
                        
                        for (row = 0; row < subnode->children; row++) {
                            
                            cbf_failnez(cbf_write_ws_value(subnode,row,file,isbuffer, em))
                            
                        }
                        
                    }
                    
                }
                
                return 0;  break;
                
            case CBF_CATEGORY:
                
                if (!cbf_cistrcmp(node->name,"ws_")) {return 0;}
                
                if (!cbf_find_child( &subnode, node,"ws__emlogue")) {
                    
                    for (row = 0; row < subnode->children; row++) {
                        
                        cbf_failnez(cbf_write_ws_value(subnode,row,file,isbuffer, em))
                        
                    }
                    
                }
                
                
                return 0;  break;
                
                
            default:
                
                return CBF_ARGUMENT;
                
        }
        
        
    }
    
    
    int cbf_write_ws_epilogue(const cbf_node *node, cbf_file *file, int isbuffer) {
        
        unsigned int row;
        
        cbf_node *subnode;
        
        /* Check the arguments */
        
        if (!node || !file)
            
            return CBF_ARGUMENT;
        
        /* Check if white space is to be processed */
        
        if ( (file->write_headers & CBF_PARSE_WS) == 0 )
            
            return 0;
        
        /* Follow any links */
        
        node = cbf_get_link (node);
        
        
        /* Node type */
        
        switch (node->type) {
                
            case CBF_ROOT:
                
                return 0;  break;
                
            case CBF_DATABLOCK:
                
            case CBF_SAVEFRAME:
                
                if (!cbf_find_typed_child(&subnode, node,"ws_",CBF_CATEGORY)) {
                    
                    if (!cbf_find_child( &subnode, subnode,"epilogue")) {
                        
                        for (row = 0; row < subnode->children; row++) {
                            
                            cbf_failnez(cbf_write_ws_value(subnode,row,file,isbuffer, epi))
                            
                        }
                        
                    }
                    
                }
                
                return 0;  break;
                
            case CBF_CATEGORY:
                
                if (!cbf_cistrcmp(node->name,"ws_")) {return 0;}
                
                if (!cbf_find_child( &subnode, node,"ws_")) {
                    
                    for (row = 0; row < subnode->children; row++) {
                        
                        cbf_failnez(cbf_write_ws_value(subnode,row,file,isbuffer, epi))
                        
                    }
                    
                }
                
                
                return 0;  break;
                
                
            default:
                
                return CBF_ARGUMENT;
                
        }
        
        
    }
    
    
#ifdef __cplusplus
    
}

#endif
