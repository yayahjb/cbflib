/**********************************************************************
 * cbf_context -- handle cbf contexts                                 *
 *                                                                    *
 * Version 0.7.6 14 July 2006                                         *
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
#include "cbf_alloc.h"
#include "cbf_string.h"
#include "cbf_context.h"
    
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <fcntl.h>
#ifdef _WIN32
  #include <direct.h>
  #define MKDIR(x) _mkdir(x)
#else
  #include <errno.h>
  #include <sys/stat.h>
  #include <sys/types.h>
  #define MKDIR(x) mkdir(x,CBF_TMP_DIR_PERM)
#endif
    
    /* Create and initialise a context */
    
    int cbf_make_context (cbf_context **context)
    {
        /* Allocate the memory */
        
        cbf_failnez (cbf_alloc ((void **) context, NULL, sizeof (cbf_context), 1))
        
        
        /* Initialise */
        
        (*context)->temporary = NULL;
        
        (*context)->connections = 1;
        
        
        /* Success */
        
        return 0;
    }
    
    
    /* Free a context */
    
    int cbf_free_context (cbf_context **context)
    {
        int errorcode;
        
        errorcode = 0;
        
        if (context)
            
            if (*context)
            {
                if ((*context)->temporary)
                    
                    errorcode = cbf_free_file (&(*context)->temporary);
                
                errorcode |= cbf_free ((void **) context, NULL);
            }
        
        
        /* Success? */
        
        return errorcode;
    }
    
    
    /* Add a context connection */
    
    int cbf_add_contextconnection (cbf_context **context)
    {
        /* Does the context pointer exist? */
        
        if (!context)
            
            return CBF_ARGUMENT;
        
        
        /* Does the context exist? */
        
        if (*context)
        {
            (*context)->connections++;
            
            return 0;
        }
        
        
        /* Create a new context */
        
        return cbf_make_context (context);
    }
    
    
    /* Remove a context connection */
    
    int cbf_delete_contextconnection (cbf_context **context)
    {
        /* Does the context pointer exist? */
        
        if (!context)
            
            return CBF_ARGUMENT;
        
        
        /* Does the context exist? */
        
        if (!*context)
            
            return CBF_ARGUMENT;
        
        
        /* Remove a connection */
        
        (*context)->connections--;
        
        
        /* Delete the context? */
        
        if ((*context)->connections == 0)
            
            return cbf_free_context (context);
        
        
        /* Success */
        
        return 0;
    }
    
    
    /* Convert string that may contain enviroment variables delimited
       by ${...} or by %...% to a string in which those variables have
       be replaced by their values.  This is a non-recursive call.
     
       len is the limit on the size of the destination string dst
     
       returns the length of the destination string unless an evironment
       variable requires more than 1024 characters for its value, in
       which case -1 is returned.
 
     */
    
    static size_t cbf_convert_env(char * dst, char * src, size_t len) {
        
        char envbuf[1025];
        
        char c, e;
        
        char * envval;
        
        size_t isrc, idst, ienv, klen;
        
        isrc = idst = klen = 0;
        
        while ((c=src[isrc++])) {
            
            if (c=='$' && src[isrc]=='{') {
                
                isrc++;
                
                ienv = 0;
                
                while ((e=src[isrc++])) {
                    
                    if (ienv >= 1024) return -1;
                    
                    if (e != '}') {
                    
                        envbuf[ienv++] = e;
                        
                        continue;
                        
                    } else {
                        
                        envbuf[ienv] = '\0';
                        
                        envval = getenv(envbuf);
                        
                        if (!envval) break;
                        
                        if (len > klen) strncpy(dst+idst,envval,len-klen);
                        
                        klen += strlen(envval);
                        
                        if (klen > len) {
                            
                            idst = len;
                            
                        } else {
                            
                            idst = klen;
                            
                        }
                    
                        break;
                        
                    }
                    
                }
                
            } else if (c=='%') {
                
                ienv = 0;
                
                while ((e=src[isrc++])) {
                    
                    if (ienv >= 1024) return -1;
                    
                    if (e != '%') {
                        
                        envbuf[ienv++] = e;
                        
                        continue;
                        
                    } else {
                        
                        envbuf[ienv] = '\0';
                        
                        envval = getenv(envbuf);
                        
                        if (!envval) break;
                        
                        if (len > klen) strncpy(dst+idst,envval,len-idst);
                        
                        klen += strlen(envval);
                        
                        if (klen > len) {
                            
                            idst = len;
                            
                        } else {
                            
                            idst = klen;
                            
                        }
                        
                        break;
                        
                    }
                    
                }
                
            } else {
                
                if (idst < len) dst[idst++] = c;
                
                klen++;
                
            }
            
            
        }
        
        return klen;
        
    }
    
    /* Create the directories specified by the given path */
    
    static int cbf_mkdir(char * path) {
        
        ssize_t ip, kp;

        char c, csave;
        
        struct stat buffer;
        
        /* silently reject NULL, the empty path, and "(NONE)" */
        
        if (!path || !cbf_cistrcmp(path,"") || !cbf_cistrcmp(path,"(NONE)")) return CBF_SUCCESS;
        
        if ( MKDIR(path) && errno != EEXIST ) {
            
            ip = 0;
            
            while ((c=path[ip])) {
                
                if (c==CBF_PATH_DIR_SEP) {
                    
                    ip++;
                    
                    continue;
                
                }
            
                kp = ip++;
                
                while ((csave=path[kp])) {
                    
                    if (csave==CBF_PATH_DIR_SEP) {
                        
                        path[kp] = '\0';
                        
                        if (MKDIR(path) == 0 || errno == EEXIST || errno == EISDIR) {
                            
                            path[kp] = csave;
                            
                            ip = kp+1;
                            
                            break;
                            
                        }
                        
                        path[kp]= csave;
                        
                        return CBF_NOTFOUND;
                        
                    }
                    
                    kp++;
                    
                }
                
             }

        }
        
        
        if ( MKDIR(path) && errno == ENOENT ) return CBF_NOTFOUND;
        
        if ( stat(path, &buffer) < 0 ) return CBF_NOTFOUND;
        
        if (buffer.st_mode && S_IFDIR == 0) return CBF_NOTFOUND;

        return CBF_SUCCESS;
        
        
    }
    
    /* Service routine for cbf_tmpfile
     
       assumes one argument which is a character buffer containing
       a directory path followed by 38 empty character positions.
       The directory path is assumed to have been created already.
     
       If the directory path name is NULL or "", "(NONE)", NULL is returned.
     
     */

    static FILE* cbf_mktmpfile(char * tmpdir, size_t tmpdir_len) {

        char base62[62]={
            '0','1','2','3','4','5','6','7','8','9',
            'a','b','c','d','e','f','g','h','i','j',
            'k','l','m','n','o','p','q','r','s','t',
            'u','v','w','x','y','z',
            'A','B','C','D','E','F','G','H','I','J',
            'K','L','M','N','O','P','Q','R','S','T',
            'U','V','W','X','Y','Z'};
        
        unsigned char last_string[7];
        
        FILE *fp;
        
        int pid;
        
        int fd;
        
        int pass;
        
        size_t ii;
        
        FILE * control_file, temp_file;

        CBF_UNUSED( temp_file );
        
        if (tmpdir) tmpdir[tmpdir_len]='\0';
        
        if (!tmpdir || tmpdir_len == 0 || !cbf_cistrcmp(tmpdir,"(NONE)")) return NULL;
        
        if (tmpdir[tmpdir_len-1] != CBF_PATH_DIR_SEP) {
            
            tmpdir[tmpdir_len++] = CBF_PATH_DIR_SEP;

        }
    
        pid = getpid();
        
        pass = -1;
        
        fd = -1;
        
        do {
            
            sprintf(tmpdir+tmpdir_len,"CBF_TMP_%06d",0x3F&pid);
            
            pass++;
            
            if (!(control_file = fopen(tmpdir,"w+"))) return NULL;
            
            if (fread(last_string,1,6,control_file) < 6) {
                
                last_string[0] = last_string[1] = last_string[2]
                = last_string[3] = last_string[4] = last_string[5] = 0;
                
                
            }
            
            for (ii=0; ii < 6; ii++) {
                
                if (last_string[ii] > 61) last_string[ii] = 61;
            }
            
            for (ii=0; ii < 6; ii++) {
                
                last_string[ii]++;
                
                if (last_string[ii] < 62) break;
                
                last_string[ii] = 0;
                
            }
            
            fseek(control_file,0,SEEK_SET);
            
            fwrite(last_string,1,6,control_file);
            
            fclose(control_file);
            
            for (ii=0; ii < 6; ii++) {
                
                last_string[ii] = base62[last_string[ii]];
                
            }
            
            sprintf(tmpdir+tmpdir_len,"CBF_TMP_%06d_%c%c%c%c%c%c",
                    pid,
                    last_string[5],last_string[4],last_string[3],
                    last_string[2],last_string[1],last_string[0]);
        }
        
        while ((fd = open(tmpdir,O_RDWR|O_CREAT|O_EXCL,0600)) < 0 && pass < 100);
        
        if (fd < 0) return NULL;

        unlink(tmpdir);
        
        fp = fdopen(fd, "w+");
        
        if (fp == NULL) close(fd);
        
        return fp;
        
    }
    
    /* Find a temporary file dir.  If there is a non empty CBF_TMP_DIR environment
     variable, use that.  If not, use CBF_TMP_DIR from cbf.h
     
     If the directory name resolves to "(NONE)", NULL will be returned.
     
     The path returned is a buffer with 39 free characters to provide room for
     a termorary file name and must be freed after being used to create a stream.
     
     */
    static int cbf_find_tmpdir( char * * cbf_tmp_dir_conv, size_t * cbf_tmp_dir_len ) {
        
        
        char * cbf_tmp_dir;
        FILE * result;

        CBF_UNUSED( result );
        
        if (!cbf_tmp_dir_conv || !cbf_tmp_dir_len) return CBF_ARGUMENT;
        
        /* Locate the temporary files directory */
        
        if ((cbf_tmp_dir = getenv("CBF_TMP_DIR"))
            && (* cbf_tmp_dir_len=cbf_convert_env(NULL,cbf_tmp_dir,0)) > 0) {
            
            *cbf_tmp_dir_conv = malloc(*cbf_tmp_dir_len+39);
            memset(*cbf_tmp_dir_conv,0,*cbf_tmp_dir_len+39);
            
            *cbf_tmp_dir_len = cbf_convert_env(*cbf_tmp_dir_conv,cbf_tmp_dir,*cbf_tmp_dir_len+1);
            
            return CBF_SUCCESS;
            
        } else if ((*cbf_tmp_dir_len=cbf_convert_env(NULL,CBF_TMP_DIR,0)) > 0) {
            
            *cbf_tmp_dir_conv = malloc(*cbf_tmp_dir_len+39);
            memset(*cbf_tmp_dir_conv,0,*cbf_tmp_dir_len+39);
            
            *cbf_tmp_dir_len = cbf_convert_env(*cbf_tmp_dir_conv,CBF_TMP_DIR,*cbf_tmp_dir_len+1);
            
            return CBF_SUCCESS;

        }
        
        *cbf_tmp_dir_len = 0;
        *cbf_tmp_dir_conv = NULL;
        return CBF_NOTFOUND;
        
    }

    
    /* Create a temporary file.  If there is a non empty environment variable
     CBF_TMP_DIR use that.  If not, use CBF_TMP_DIR from cbf.h
     
     In that directory, create a file named CBF_TMP_nnnnnn_aaaaaa
     where nnnnnn is the PID and aaaaaa is a unique character string
     base 62, using 0-9,a-z,A-Z, managed by a file named CBF_TMP_mmmmmm
     containing the last aaaaaa string used, where mmmmmm is the PID%64.
     
     If the directory name resolves to "(NONE)", NULL will be returned.
     
     */
    FILE * cbf_tmpfile( void ) {
        
        char *cbf_tmp_dir_conv;
        size_t cbf_tmp_dir_len;
        FILE * result;
        
        if (!cbf_find_tmpdir(&cbf_tmp_dir_conv, & cbf_tmp_dir_len)) {
            result = NULL;
            if (!cbf_mkdir(cbf_tmp_dir_conv)) {
                
                result = cbf_mktmpfile(cbf_tmp_dir_conv, cbf_tmp_dir_len);
                
            }
            
            free(cbf_tmp_dir_conv);
            return result;
            
        }
            
        return NULL;

    }
    
    
    
    /* Open a temporary file connection */
    
    int cbf_open_temporary (cbf_context *context, cbf_file **temporary)
    {
        FILE *stream;
        
        const char * cbf_defer_tmp;
        
        int errorcode;
        
        
        /* Check the arguments */
        
        if (!context || !temporary)
            
            return CBF_ARGUMENT;
        
        
        /* Does a temporary file already exist? */
        
        if (context->temporary)
        {
            cbf_failnez (cbf_add_fileconnection (&context->temporary, NULL))
            
            *temporary = context->temporary;
            
            return 0;
        }
        
        
        /* Create the temporary file */
        
        cbf_defer_tmp = getenv("CBF_DEFER_TMP");
        
        if (!cbf_defer_tmp || !cbf_cistrcmp(cbf_defer_tmp,"no") || !cbf_cistrcmp(CBF_DEFER_TMP,"no")) {
        
            stream = cbf_tmpfile ();

        } else {
            
            stream = NULL;
        }
        
        errorcode = cbf_make_file (&context->temporary, stream);
        
        
        context->temporary->temporary = 1;
        
        
        if (errorcode)
        {
            if (fclose (stream))
                
                errorcode |= CBF_FILECLOSE;
            
            return errorcode;
        }
        
        
        /* Open a connection */
        
        return cbf_open_temporary (context, temporary);
    }
    
    
    /* Close a temporary file connection */
    
    int cbf_close_temporary (cbf_context *context, cbf_file **temporary)
    {
        /* Check the arguments */
        
        if (!context || !temporary)
            
            return CBF_ARGUMENT;
        
        if (!*temporary)
            
            return CBF_ARGUMENT;
        
        
        /* Check that the temporary file matches */
        
        if (context->temporary != *temporary)
            
            return CBF_NOTFOUND;
        
        
        /* Delete the connection */
        
        cbf_failnez (cbf_delete_fileconnection (&context->temporary))
        
        *temporary = NULL;
        
        
        /* Is there only one connection left? */
        
        if (context->temporary)
            
            if (cbf_file_connections (context->temporary) == 1)
                
                cbf_failnez (cbf_free_file (&context->temporary))
                
                
            /* Success */
                
                return 0;
    }
    
    
    /* Copy a string */
    
    const char *cbf_copy_string (cbf_context *context, const char *string,
                                 char type)
    {
        char *new_string;
        
        void *memblock;
        
        size_t n;
        
        CBF_UNUSED( context );
        
        n = strlen(string);
        
        if (string) {
            
            if (type)
            {
                if (cbf_alloc (&memblock, NULL,
                               sizeof (char), n  + 2) == 0)
                {
                    new_string = (char *)memblock;
                    
                    *new_string = type;
                    
                    strncpy (new_string + 1, string, n);
                    
                    new_string[n+1] = '\0';
                    
                    return new_string;
                }
            }
            else
                
                if (cbf_alloc (&memblock, NULL, \
                               sizeof (char),  n + 1) == 0)
                {
                    
                    new_string = (char *)memblock;
                    
                    strncpy (new_string, string, n);
                    
                    new_string[n] = '\0';
                    
                    return new_string;
                }
            
        }
        
        
        /* Fail */
        
        return NULL;
    }
    
    
    /* Copy two strings */
    
    const char *cbf_copy_strings (cbf_context *context, 
                                  const char *string1,
                                  const char *string2,
                                  char type)
    {
        char *new_string;
        
        void *memblock;
        
        if (!string1) return cbf_copy_string(context,string2,type);
        
        if (!string2) return cbf_copy_string(context,string2,type);
        
        if (type)
        {
            if (cbf_alloc (&memblock, NULL,
                           sizeof (char), 
                           strlen (string1) + strlen(string2) + 2) == 0)
            {
                new_string = (char *)memblock;
                
                *new_string = type;
                
                strcpy (new_string + 1, string1);
                
                strcpy (new_string + 1 + strlen(string1),string2);
                
                return new_string;
            }
        }
        
        if (cbf_alloc (&memblock, NULL,
                       sizeof (char), 
                       strlen (string1) + strlen(string2) + 1) == 0)
        {
            
            new_string = (char *)memblock;
            
            strcpy (new_string, string1);
            
            strcpy (new_string + strlen(string1), string2);
            
            return new_string;
        }
        
        return NULL;
        
    }
    
    
    
    
    /* Free a string */
    
    void cbf_free_string (cbf_context *context, const char *string)
    {
        void * memblock;
        
        CBF_UNUSED( context );
        
        memblock = (void *)string;
        
        cbf_free (&memblock, NULL);
    }
    
    
#ifdef __cplusplus
    
}

#endif
