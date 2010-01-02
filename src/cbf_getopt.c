/**********************************************************************
 *  cbf_getopt.c                                                      *
 *                                                                    *
 *                                                                    *
 * Created by Herbert J. Bernstein on 6/8/09.                         *
 * (C) Copyright 2009 Herbert J. Bernstein                            *
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


#ifdef __cplusplus

extern "C" {

#endif

#include <cbf.h>
#include <cbf_getopt.h>
#include <cbf_alloc.h>
#include <cbf_context.h>
#include <string.h>


    static const char * cbf_getopt_locate_option(const char * options, 
                                          char * optchar, 
                                          const char * * longopt,
                                          int * hasvalue) {
        
        char c;
        
        if (optchar) *optchar = '\0';
        
        if (longopt) *longopt = NULL;
        
        if (hasvalue) *hasvalue = 0;
        
        if (*options != '(' && *options != ':' && optchar)*optchar = *options; 
        
        while ((c=*options)) {
            
            if (c=='(') {
                
                ++options;
                
                if (longopt) *longopt = options;
                
                while ((c=*options) && (c!=')')) {
                    
                    options++;
                }
                
                if (!c) {
                    
                    return options;
                }
                
            } else if (c==':') {
                
                if (hasvalue) *hasvalue = 1;
                
                options ++;
                
                if ((c=*options) && (c==':')) {
                    
                    if (hasvalue) *hasvalue = -1;
                    
                    options ++;
                }
                
                return options;
                
            } else if (c) {

                options ++;
                
                if ((c=*options) && (c=='(' || c==':')) continue;
                
                return options;
                
            }
                
        }
        
        return options;
    }
    
    /* create a cbf_getopt handle */
    
    int cbf_make_getopt_handle(cbf_getopt_handle * handle) {
        
        *handle = NULL;
        
        cbf_failnez (cbf_alloc ((void **) handle, NULL,
                                sizeof (cbf_getopt_struct), 1))
        
        (*handle)->optstructs = NULL;
        
        cbf_onfailnez (cbf_alloc ((void **) &((*handle)->optstructs),
                                  &((*handle)->optstructs_capacity),sizeof(cbf_getopt_optstruct), 10),
                       cbf_free((void **) handle, NULL))
 
        (*handle)->optstructs_size = 0; 
        (*handle)->optind = 0; /* ordinal of option in options     */
        (*handle)->options = NULL;

        return 0;	
    }
    

    /* clear the data in a cbf_getopt handle */
    
    static int cbf_clear_getopt_handle(cbf_getopt_handle handle) {
        
        cbf_getopt_optstruct * optstruct;
        
        size_t index;
        
        for (index = 0; index < handle->optstructs_size; index++) {
            
            optstruct = &(handle->optstructs[index]);
            
            if (optstruct->optstr) {
                
                cbf_failnez(cbf_free_text(&(optstruct->optstr),NULL))
            }
            
            if (optstruct->optval) {
                
                cbf_failnez(cbf_free_text(&(optstruct->optval),NULL))
            }
            
        }
        
        handle-> optstructs_size = 0;
        
        handle->optind = 0;
        
        if (handle->options) {
            
            cbf_failnez(cbf_free_text(&(handle->options),NULL))
        }
        
        return 0;
        
        
    }
    
    /* free a cbf_getopt handle */
    
    int cbf_free_getopt_handle(cbf_getopt_handle handle) {
        
        void *memblock;
                
        cbf_failnez( cbf_clear_getopt_handle(handle) )
        
        memblock = (void *) handle;
        
        if (handle) {
            
            return cbf_free(&memblock, NULL);
            
        }
        
        return 0;
        
    }
    

/* parse argc and argv into a newly created cbf_getopt */
    
    int cbf_getopt_parse(cbf_getopt_handle handle, int argc, char ** argv, const char * options) {
        
        int ii, iii, ios;
        
        void * voptstructs;
        
        cbf_getopt_optstruct * optstruct;
        
        const char * opts;
        
        int hasvalue;
        
        const char * longopt;
        
        char optchar;
        
        size_t optlen;
        
        int optord;
        
        int foundopt;
        
        if (handle == NULL || argv == NULL ) return CBF_ARGUMENT;
        
        cbf_failnez( cbf_clear_getopt_handle(handle) )
        
        /* Allocate as many opstruct slots as we have arguments */
        
        if (handle->optstructs) {
            
            voptstructs = (void *)(handle->optstructs);
            
            cbf_failnez(cbf_realloc((void **) &voptstructs,
                                    &(handle->optstructs_capacity),sizeof(cbf_getopt_optstruct),argc))
            
        } else {
            
            cbf_failnez(cbf_alloc((void **) &voptstructs,
                                  &(handle->optstructs_capacity),sizeof(cbf_getopt_optstruct),argc))
            
        }
        
        handle->optstructs = (cbf_getopt_optstruct *)voptstructs;
        
        if (handle->options != NULL) {
            
            cbf_failnez(cbf_free_text((const char * *)&(handle->options),NULL))
            
        }
        
        /*  If options have been specified, use them.  Otherwise
         default to "-"
         */
         if (options) {
            
            handle->options = cbf_copy_string(NULL,options,0);
            
        } else {
            
            handle->options = cbf_copy_string(NULL,"-",0);
            
        }
        
        handle->optind = 0;
        
        for ( ii=1; ii < argc; ii++) {
            
            /* Prepare the next slot */
            
            optstruct = &((handle->optstructs)[(handle->optind)++]);
            
            (handle->optstructs_size)++;
            
            optstruct->optopt = 0;
            
            optstruct->optord = -1;
            
            optstruct->optstr = NULL;
            
            optstruct->optval = NULL;
            

            /* on --, end the options scan */
            
            if ( !strcmp(argv[ii],"--") ) {
                
                /* copy all remaining arguments as unflagged values */
                
                break;
                
            }
            /* process a --option case */
            
            if (!strncmp("--",argv[ii],2)) {
                
                optstruct->optstr = cbf_copy_string(NULL,2+argv[ii],0);
                
                opts = options;
                
                if (*opts=='-' || *opts =='+') opts++;
                
                optlen = strlen(argv[ii]+2);
                
                optord = -1;
                
                foundopt = 0;
                
                do {
                    opts= cbf_getopt_locate_option(opts, &optchar, &longopt, &hasvalue);
                    
                    optord++;
                    
                    if (longopt && !strncmp(longopt,argv[ii]+2,optlen) && longopt[optlen]==')') {
                                                
                        optstruct->optopt = optchar;
                        
                        optstruct->optord = optord;
                        
                        optstruct->optval = NULL;
                        
                        if (ii+1 < argc && (hasvalue >0
                                            || (*(argv[ii+1])!='-'&& hasvalue < 0) )) {
                            
                            optstruct->optval = cbf_copy_string(NULL,argv[ii+1],0);
                            
                            ii++;
                            
                            foundopt++;
                            
                            break;
                            
                        }                   
                        
                    }
                    
                } while (*opts);
                
                if (foundopt) continue;
                
                if (*options == '-') {
                                        
                    optstruct->optopt = '\1';
                    
                    optstruct->optord = -1;
                    
                    optstruct->optval = NULL;
                    
                    if (ii+1 < argc && *(argv[ii+1])!='-' ) {
                        
                        optstruct->optval = cbf_copy_string(NULL,argv[ii+1],0);
                        
                        ii++;
                        
                        break;
                        
                    } 
                    
                    continue;                 
                    
                }
                
                /* this is not an expected long option and the option string
                 does not have a leading '-', therefore this is simply
                 a non-option value
                 
                 */
                
                if (*options=='+') break;
                
                optstruct->optval = cbf_copy_string(NULL,argv[ii],0);
                
                optstruct->optopt = 0;
                
                optstruct->optord = -1;
                
                cbf_failnez(cbf_free_text(&(optstruct->optstr),NULL))
                
                optstruct->optstr = NULL;
                
                continue;
                
            }
            
            /* now for the single '-' case marking a lone letter option */
            
            if (*(argv[ii]) == '-' && strlen(argv[ii]) > 1 ) {
                
                char xc[2];
                                
                xc[0] = argv[ii][1];
                
                xc[1] = '\0';
                
                optstruct->optstr = cbf_copy_string(NULL,xc,0);
                
                opts = options;
                
                if (*opts=='-' || *opts =='+') opts++;
                
                optlen = 1;
                
                optord = -1;
                
                foundopt = 0;
                
                do {
                    opts= cbf_getopt_locate_option(opts, &optchar, &longopt,&hasvalue);
                    
                    optord++;
                    
                    if (xc[0] == optchar) {
                                                
                        optstruct->optopt = optchar;
                        
                        optstruct->optord = optord;
                                               
                        optstruct->optval = NULL;
                        
                        if ((strlen(argv[ii]+2) > 0) ||
                            (ii+1 < argc && (hasvalue >0
                                             || (*(argv[ii+1])!='-'&& hasvalue < 0) ))) {
                            if (strlen(argv[ii]+2) > 0) {
                                
                                optstruct->optval = cbf_copy_string(NULL,argv[ii]+2,0);
                                
                            } else {
                                
                                optstruct->optval = cbf_copy_string(NULL,argv[ii+1],0);
                                
                                ii++;
                                
                            }
                            
                            
                        }                   
                        
                        foundopt++;
                        
                        break;
                    }
                    
                } while (*opts);
                
                if (foundopt) continue;
                
                if (*options == '-')      {
                                        
                    optstruct->optopt = '\1';
                    
                    optstruct->optord = -1;
                    
                    optstruct->optval = NULL;
                    
                    if (ii+1 < argc && *(argv[ii+1])!='-' ) {
                        
                        optstruct->optval = cbf_copy_string(NULL,argv[ii+1],0);
                        
                        ii++;
                        
                        break;
                        
                    } 
                    
                    continue;                 
                    
                };

                
                /* this is not an expected short option and the option string
                 does not have a leading '-', therefore this is simply
                 a non-option value
                 
                 */
                
                if (*options=='+') break;
                                
                optstruct->optval = cbf_copy_string(NULL,argv[ii],0);
                
                optstruct->optopt = 0;
                
                optstruct->optord = -1;
                
                cbf_failnez(cbf_free_text(&(optstruct->optstr),NULL))
                
                optstruct->optstr = NULL;
                
                continue;
                
            }
            
            /* All that is left is to treat this as a non-option value */
                        
            optstruct->optval = cbf_copy_string(NULL,argv[ii],0);
            
            optstruct->optopt = 0;
            
            optstruct->optord = -1;
            
            optstruct->optstr = NULL;
            
            if (*options=='+') break;
            
        }
        

        /*
           ii is the last argument processed, the remaining arguments get added
           at the end
         
         */
        
        ios =  handle->optstructs_size;
        
        for (iii = ii+1; iii < argc; iii++) {
            
            optstruct = &((handle->optstructs)[(handle->optind)++]);
            
            (handle->optstructs_size)++;
            
            optstruct->optval = cbf_copy_string(NULL,argv[iii],0);
            
            optstruct->optopt = 0;
            
            optstruct->optord = -1;
            
            optstruct->optstr = NULL;
            
        }
        
        /* if *options is not '-', then all non-options in obstructs need to
           be sorted to the end of the list
         
         */
        
        iii = ios-1;
        
        while (iii >= 0) {
            
            cbf_getopt_optstruct temp;
            
            optstruct = &((handle->optstructs)[iii]);
            
            /* Any option with no option ordinal moves up to index ios-1 */
            
            if (optstruct->optord < 0) {
                
                if (iii < ios-1) {
                
                  memmove((void *)(&temp),(void *)optstruct,sizeof(cbf_getopt_optstruct));
                
                    for (ii = iii; ii < ios-1; ii++) {
                        
                        memmove((void *)(&((handle->optstructs)[ii])),
                                (void *)(&((handle->optstructs)[ii+1])), sizeof(cbf_getopt_optstruct));
                        
                    }
                    
                  memmove((void *)(&((handle->optstructs)[ios-1])),(void *)(&temp),sizeof(cbf_getopt_optstruct));
 
                }
                
                ios --;
                
            }
            
            iii--;
            
        }
        
        return 0;
        
        
    }
        

    
    /* Get first option from a cbf_getopt handle */
    
    int cbf_rewind_getopt_option ( cbf_getopt_handle handle ) {

        if ( !handle ) return CBF_ARGUMENT;
        
        handle->optind = 0 ;
        
        if ( handle->optind >=  handle->optstructs_size) return CBF_NOTFOUND;
        
        return 0;
        
    }
    
    /* Get next option from a cbf_getopt handle */
    
    int cbf_next_getopt_option ( cbf_getopt_handle handle ){

        if ( !handle ) return CBF_ARGUMENT;
        
        handle->optind++;
        
        if ( handle->optind >=  handle->optstructs_size) return CBF_NOTFOUND;
         
        return 0;
        
    }
    
    /* Get option by number (0 ... ) from a cbf_getopt handle */
    
    int cbf_select_getopt_option ( cbf_getopt_handle handle, unsigned int option ) {
        
        if ( !handle ) return CBF_ARGUMENT;
        
        if ( option < 0 || option >=  handle->optstructs_size) return CBF_ARGUMENT;
        
        handle->optind = option;
        
        return 0;
        
    }
    
    /* Count the options in a cbf_getopt handle */
    
    int cbf_count_getopt_options ( cbf_getopt_handle handle, unsigned int * options ) {
        
        if ( !handle ) return CBF_ARGUMENT;
        
        if ( options ) *options = handle->optstructs_size;
        
        return 0;
    }
    
    /* Get the data for an option */
    
    int cbf_get_getopt_data ( cbf_getopt_handle handle, int * optopt,
                             int * optord, const char * * optstr, const char * * optval) {
        
        cbf_getopt_optstruct * optstruct;
        
        if ( !handle ) return CBF_ARGUMENT;
        
        if ( handle->optind < 0 || handle->optind >= handle->optstructs_size) return CBF_NOTFOUND;
        
        optstruct = &(handle->optstructs[handle->optind]);
        
        if (optopt) *optopt = optstruct->optopt;
        
        if (optord) *optord = optstruct->optord;
        
        if (optstr) *optstr = optstruct->optstr;
        
        if (optval) *optval = optstruct->optval;
        
        return 0;
        
    }
    
#ifdef __cplusplus

}

#endif
