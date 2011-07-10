/*
 *  sequence_match.c
 *
 *  match 2 mmCIF files on sequence and export the matching portions
 *  of their atoms lists
 *  
 *
 *  Created by Herbert J. Bernstein on 2/24/10.
 *  Copyright 2010 Herbert J. Bernstein. All rights reserved.
 *
 */

/**********************************************************************
 *                                SYNOPSIS                            *
 *                                                                    *
 *  seqmatch [-l leftinput] [-r rightinput] \                         *
 *           [-m leftoutput] [-s rightoutput] \                       *
 *           [-a|-c]                                                  *
 *           [leftinput] [rightinput] \                               *
 *           [leftoutput] [rightoutput]                               *
 *                                                                    *
 *                                                                    *
 **********************************************************************/

#include "cbf.h"
#include "cbf_simple.h"
#include "cbf_string.h"
#include "cbf_copy.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include "cbf_getopt.h"
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#define C2CBUFSIZ 8192


#ifdef __MINGW32__
#define NOMKSTEMP
#define NOTMPDIR
#endif

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
    return 0;
    
}

#undef cbf_failnez
#define cbf_failnez(x) \
{int err; \
err = (x); \
if (err) { \
fprintf(stderr,"CBFlib fatal error %d\n",err); \
outerror(err); \
local_exit (-1); \
} \
}


typedef struct {
    int hashchain;              /* next index by residue name        */
    int resnumhashchain;        /* next index by residue number      */
    const char * resname;       /* residue name                      */
    int resnum;                 /* residue number                    */
    int seqmatch;               /* index in another chain of a match */
} residue_entry;

typedef struct {
    int hash_table[256];        /* indices by residue name hash code             */ 
    int hash_end[256];          /* indices of the ends of the name hash chains   */
    int hash_resnum[256];       /* indices by residue number hash code           */
    int hash_resnum_end[256];   /* indices of the ends of the number hash chains */
    size_t size;
    size_t capacity;
    int minresnum;
    int maxresnum;
    residue_entry * residues;
    const char* entity;
} residue_chain;

typedef residue_chain * chainhandle;


int residue_hash(const char * resname) {
    int hashcode;
    int c;
    int j;
    size_t len;
    hashcode = 0;
    if (!resname) return -1;
    len = strlen(resname);
    for (j=0; j<len;j++) {
        c = toupper(resname[j]);
        hashcode = (hashcode << 1) +(c&0xFF);
    }
    return (hashcode &0xff);
}

int create_chainhandle(chainhandle * handle, const char * entity, size_t capacity) {
    int i;
    *handle = (chainhandle)malloc(sizeof(residue_chain));
    if (!*handle) return -1;
    for (i=0;i<256;i++) {
        (*handle)->hash_table[i] = -1;
        (*handle)->hash_end[i] = -1;
        (*handle)->hash_resnum[i] = -1;
        (*handle)->hash_resnum_end[i] = -1;
    }
    (*handle)->size = 0;
    (*handle)->capacity = (!capacity)?10:capacity;
    (*handle)->entity = entity;
    (*handle)->residues = (residue_entry*)malloc(((*handle)->capacity)*sizeof(residue_entry));
    (*handle)->maxresnum = -999999;
    (*handle)->minresnum = 999999;    
    if (!(*handle)->residues) {
        free (*handle);
        *handle = NULL;
        return -1;
    }
    return 0;
}
    
    
int add_residue(chainhandle handle, const char * residue, int resnum) {
    residue_entry * re;
    int hashcode;
    int he;
    if (!handle) return -1;
    if (handle->size >= handle->capacity) {
        if((re=(residue_entry *)malloc(handle->capacity*2*sizeof(residue_entry)))){
            memmove(re,handle->residues,(handle->size)*sizeof(residue_entry));
            free(handle->residues);
            handle->capacity *=2;
            handle->residues = re;
        } else {
            return -1;
        }
    }
    re=handle->residues;
    hashcode = residue_hash(residue);
    re[handle->size].hashchain=-1;
    re[handle->size].resname = residue;
    re[handle->size].resnum = resnum;
    if (resnum < handle->minresnum) handle->minresnum = resnum;
    if (resnum > handle->maxresnum) handle->maxresnum = resnum;
    re[handle->size].seqmatch = -1;
    if ((he=(handle->hash_end)[hashcode]) != -1) {
        re[he].hashchain = handle->size;
    }
    (handle->hash_end)[hashcode] = handle->size;
    if ((handle->hash_table)[hashcode] == -1) {
        (handle->hash_table)[hashcode] = handle->size;
    }
    hashcode=resnum&0xFF;
    re[handle->size].resnumhashchain=-1;
    (handle->hash_resnum_end)[hashcode] = handle->size;
    if ((handle->hash_resnum)[hashcode] == -1) {
        (handle->hash_resnum)[hashcode] = handle->size;
    }    
    (handle->size)++;
    return 0;
}

int get_by_residue(chainhandle chain, int oldindex, const char* residue) {
    int hashcode;
    hashcode = residue_hash(residue);
    if (oldindex < 0 ) {
        oldindex = chain->hash_table[hashcode];
    } else {
        oldindex = (chain->residues)[oldindex].hashchain;
    }
    if (oldindex < 0) return -1;
    while(cbf_cistrcmp((chain->residues)[oldindex].resname,residue) ) {
        oldindex = (chain->residues)[oldindex].hashchain;
        if (oldindex < 0) return -1;
    }
    return oldindex;
}

int get_by_resnum(chainhandle chain, int oldindex, int resnum) {
    if (oldindex < 0) {
        oldindex = chain->hash_resnum[resnum&0xFF];
    } else {
        oldindex = (chain->residues)[oldindex].resnumhashchain;
    }
    if (oldindex < 0) return -1;
    while((chain->residues)[oldindex].resnum != resnum ) {
        oldindex = (chain->residues)[oldindex].resnumhashchain;
        if (oldindex < 0) return -1;
    }
    return oldindex;
}

/* find the next match to a given residue, returning the index,
   not the residue number*/

int find_residue(chainhandle chain, const char * residue, int startnum, int minnum) {
    
    int hashcode;
    int index;
    
    if (startnum < chain->minresnum) {
      hashcode = residue_hash(residue);
      index = chain->hash_table[hashcode];
    } else {
      index = get_by_resnum(chain,-1,startnum);
      index = (chain->residues)[index].hashchain;
    }
    while(index >= 0 ){
        if ( !cbf_cistrcmp((chain->residues)[index].resname,residue)
            &&  (chain->residues)[index].resnum >= minnum) { 
            return index;
        } else {
            index = (chain->residues)[index].hashchain;
        }
    }
    return index;
    
}



/* compare part of two sequences by residue number 
   returns 0 for a match or the relative index of the
   first difference, starting at 1*/

int seq_comp_partial(chainhandle leftseq, chainhandle rightseq,
                     int leftstart, int rightstart, int lentomatch) {
    int ii;
    int left, right;
    int imatch;
    left = -1;
    right = -1;
    for (ii=0; ii<lentomatch; ii++) {
        left = get_by_resnum(leftseq, left, leftstart+ii);
        right = get_by_resnum(rightseq, right, rightstart+ii);
        if (left < 0 ) {
            if (right < 0) return 0;
            return ii+1;
        } else {
            (leftseq->residues)[left].seqmatch = -1;
            if (right < 0) return ii+1;
            if (!cbf_cistrcmp((leftseq->residues)[left].resname,(rightseq->residues)[right].resname)) {
                (leftseq->residues)[left].seqmatch = right; 
                left = right = -1;
                continue;
            }
            while ( (left = get_by_resnum(leftseq, left, leftstart+ii)) >= 0) {
                right = -1;
                imatch = 0;
                while ( (right = get_by_resnum(rightseq, right, rightstart+ii)) >= 0) {
                    if (!cbf_cistrcmp((leftseq->residues)[left].resname,(rightseq->residues)[right].resname)){
                        imatch = 1;
                        break;
                    }
                    if (imatch == 1) {
                        left = right = -1;
                        continue;
                    }
                }
                
            }
            return ii+1;
        }
        left = -1;
        right = -1;
    }
    return 0;
    
}

int seq_comp(chainhandle leftseq, chainhandle rightseq ) {
    int index;
    int seqlim;
    int isl;
    int isr;
    int iii;
    int leftlow;
    int lefthigh;
    int rightlow;
    int righthigh;
    int imatch;
    int lentomatch;
    int kmatched;

    leftlow = leftseq->minresnum;
    lefthigh = leftseq->maxresnum;
    rightlow = rightseq->minresnum;
    righthigh = rightseq->maxresnum;
    
    seqlim = (lefthigh-leftlow+1)/2;
    if (seqlim > 6) seqlim = 6;
    for (iii=0;iii <=leftseq->size; iii++) {
        (leftseq->residues)[iii].seqmatch=-1;
    }
    
    kmatched = 0;
    
    
    /* we will try to match a sequence of length lm from the
       left sequence to some portion of the right sequence,
       accepting any match of at least 4 residues */
    
    isl = leftlow;
    while (isl <= lefthigh) {
        index = get_by_resnum(leftseq,-1,isl);
        if (index < 0) {
            isl++;
            continue;
        }
        iii = find_residue(rightseq,(leftseq->residues)[index].resname,-1,rightlow);
        if (iii < 0) {
            while((index=get_by_resnum(leftseq,index,isl)>=0)){
                iii = find_residue(rightseq,(leftseq->residues)[index].resname,-1,rightlow);
                if (iii < 0) {
                    continue;
                }
                isr = (rightseq->residues)[iii].resnum;
                lentomatch = lefthigh-isl+1;
                if (lentomatch > righthigh-isr+1) {
                    lentomatch = righthigh-isr+1;
                }
                imatch = seq_comp_partial(leftseq, rightseq, isl, isr, lentomatch);
                if (imatch > 0 && imatch < 4) {
                    (leftseq->residues)[index].seqmatch=-1;
                    while((index=get_by_resnum(leftseq,index,isl)>=0)){
                        (leftseq->residues)[index].seqmatch=-1;
                    }
                    continue;
                }
                if (imatch == 0 ) imatch = lentomatch;
                isl += imatch-1;
                rightlow = isr+imatch;
                kmatched +=imatch;
                break;
            }
        } else {
            isr = (rightseq->residues)[iii].resnum;
            lentomatch = lefthigh-isl+1;
            if (lentomatch > righthigh-isr+1) {
                lentomatch = righthigh-isr+1;
            }
            imatch = seq_comp_partial(leftseq, rightseq, isl, isr, lentomatch);
            if (imatch > 0 && imatch < 4) {
                (leftseq->residues)[index].seqmatch=-1;
                while((index=get_by_resnum(leftseq,index,isl)>=0)){
                    (leftseq->residues)[index].seqmatch=-1;
                }                
                isl++; continue;
            }
            if (imatch == 0 ) imatch = lentomatch;
            isl += imatch;
            rightlow = isr+imatch;
            kmatched +=imatch;
            break;
        }
        isl++;
    }
    return kmatched;
}

int main (int argc, char *argv [])
{
    FILE *leftin, *rightin, *leftout=NULL, *rightout=NULL;
    const char * leftinstr, * rightinstr, * leftoutstr, * rightoutstr;
    char * leftintmpstr, *rightintmpstr;
    cbf_handle leftincbf, rightincbf, leftoutcbf, rightoutcbf;
    cbf_getopt_handle opts;
    int doall,doca;
    int nbytes;
    int c;
    int leftdevnull, rightdevnull;
    char buf[C2CBUFSIZ];
    
    chainhandle leftch[256];
    chainhandle rightch[256];
    const char * leftchname[256];
    const char * rightchname[256];
    int left_to_right[256];
    int right_to_left[256];
    int numleftch, numrightch;
    int iii, jjj;
    int errflg = 0;
#ifndef NOMKSTEMP
    int leftintmpfd, rightintmpfd;
#endif
    int leftintmpused, rightintmpused;
    
    const char * optarg;
    
    /* Extract options */
    
    leftinstr = NULL;
    rightinstr = NULL;
    leftoutstr = NULL;
    rightoutstr = NULL;
    leftintmpstr = NULL;
    rightintmpstr = NULL;
    leftdevnull = rightdevnull = 0;
    leftintmpused = rightintmpused = 0;
    errflg = 0;
    numleftch = numrightch = 0;
    doall = doca = 0;
    
    cbf_failnez(cbf_make_getopt_handle(&opts))
    
    cbf_failnez(cbf_getopt_parse(opts, argc, argv, "-l(leftinput):" \
                                 "-r(rightinput):" \
                                 "-m(leftoutput):" \
                                 "-s(rightoutput):" \
                                 "-a(allatoms)" \
                                 "-c(calpha)" \
                                 ))
    
    if (!cbf_rewind_getopt_option(opts))
        for(;!cbf_get_getopt_data(opts,&c,NULL,NULL,&optarg);cbf_next_getopt_option(opts)) {
            if (!c) break;
            switch(c) {
                case 'l':  /* left input file */
                    if (leftinstr) errflg++;
                    else leftinstr = optarg;
                    break;
                    
                case 'r':  /* right input file */
                    if (rightinstr) errflg++;
                    else rightinstr = optarg;
                    break;
                    
                case 'm':  /* left output file */
                    if (leftoutstr) errflg++;
                    else leftoutstr = optarg;
                    break;
                    
                case 's':  /* right output file */
                    if (rightoutstr) errflg++;
                    else rightoutstr = optarg;
                    break;
                    
                case 'a': /* do all atoms */
                    if (doall|doca) errflg++;
                    else doall=1;
                    break;

                case 'c': /* do only carbon alpha */
                    if (doall|doca) errflg++;
                    else doca=1;
                    break;
                    
                default:
                    errflg++;
                    break;
                    
            }
        }
    
    for(;!cbf_get_getopt_data(opts,&c,NULL,NULL,&optarg);cbf_next_getopt_option(opts)) {
        if (!leftinstr) {
            leftinstr = optarg;
        } else {
            if (!rightinstr) {
                rightinstr = optarg;
            } else {
                if (!leftoutstr) {
                    leftoutstr = optarg;
                } else {
                    if (!rightoutstr) {
                        rightoutstr = optarg;
                    } else {
                        errflg++;
                    }
                }    
            }
        }
    }
    if (errflg) {
        fprintf(stderr,"seqmatch:  Usage: \n");
        fprintf(stderr,
                "  seqmatch [-l leftin] [-r rightin] \\\n");
        fprintf(stderr,
                "           [-m leftout] [-s rightout]\\\n");
        fprintf(stderr,
                "           [-a|-c] \\\n");
        fprintf(stderr,
                "           [leftin] [rightin] [leftout] [rightout]\n"); 
        exit(2);
    }
    
    if ( cbf_make_handle (&leftincbf) ) {
        fprintf(stderr,"Failed to create handle for left input cif\n");
        exit(1);
    }
    
    if ( cbf_make_handle (&rightincbf) ) {
        fprintf(stderr,"Failed to create handle for right input cif\n");
        exit(1);
    }
    
    if ( cbf_make_handle (&leftoutcbf) ) {
        fprintf(stderr,"Failed to create handle for left output cif\n");
        exit(1);
    }
    
    if ( cbf_make_handle (&rightoutcbf) ) {
        fprintf(stderr,"Failed to create handle for right output cif\n");
        exit(1);
    }
    
    /* Read the leftin cif */
    
    if (!leftinstr || strcmp(leftinstr?leftinstr:"","-") == 0) {
        leftintmpstr = (char *)malloc(strlen("/tmp/seqmatchlXXXXXX")+1);
#ifdef NOTMPDIR
        strcpy(leftintmpstr, "seqmatchlXXXXXX");
#else
        strcpy(leftintmpstr, "/tmp/seqmatchlXXXXXX");
#endif
#ifdef NOMKSTEMP
        if ((leftintmpstr = mktemp(leftintmpstr)) == NULL ) {
            fprintf(stderr,"\n seqmatch: Can't create temporary file name %s.\n", leftintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
        if ( (leftin = fopen(leftintmpstr,"wb+")) == NULL) {
            fprintf(stderr,"Can't open temporary file %s.\n", leftintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);     	
        }
#else
        if ((leftintmpfd = mkstemp(leftintmpstr)) == -1 ) {
            fprintf(stderr,"\n seqmatch: Can't create temporary file %s.\n", leftintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
        if ( (leftin = fdopen(leftintmpfd, "w+")) == NULL) {
            fprintf(stderr,"Can't open temporary file %s.\n", leftintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
#endif
        while ((nbytes = fread(buf, 1, C2CBUFSIZ, stdin))) {
            if(nbytes != fwrite(buf, 1, nbytes, leftin)) {
                fprintf(stderr,"Failed to write %s.\n", leftintmpstr);
                exit(1);
            }
        }
        fclose(leftin);
        leftinstr = leftintmpstr;
        leftintmpused = 1;
    }
    
    /* Read the left input file */
    if (!(leftin = fopen (leftinstr, "rb"))) {
        fprintf (stderr,"Couldn't open the left input CIF file %s\n", leftinstr);
        exit (1);
    }
    
    if (leftintmpused) {
        if (unlink(leftintmpstr) != 0 ) {
            fprintf(stderr,"seqmatch:  Can't unlink temporary file %s.\n", leftintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
    }
    
    cbf_failnez (cbf_read_widefile (leftincbf, leftin, MSG_DIGEST))
    
    
    /* Read the rightin cif */
    
    if (!rightinstr || strcmp(rightinstr?rightinstr:"","-") == 0) {
        rightintmpstr = (char *)malloc(strlen("/tmp/seqmatchrXXXXXX")+1);
#ifdef NOTMPDIR
        strcpy(rightintmpstr, "seqmatchrXXXXXX");
#else
        strcpy(rightintmpstr, "/tmp/seqmatchrXXXXXX");
#endif
#ifdef NOMKSTEMP
        if ((rightintmpstr = mktemp(rightintmpstr)) == NULL ) {
            fprintf(stderr,"\n seqmatch: Can't create temporary file name %s.\n", rightintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
        if ( (rightin = fopen(rightintmpstr,"wb+")) == NULL) {
            fprintf(stderr,"Can't open temporary file %s.\n", rightintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);     	
        }
#else
        if ((rightintmpfd = mkstemp(rightintmpstr)) == -1 ) {
            fprintf(stderr,"\n seqmatch: Can't create temporary file %s.\n", rightintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
        if ( (rightin = fdopen(rightintmpfd, "w+")) == NULL) {
            fprintf(stderr,"Can't open temporary file %s.\n", rightintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
#endif
        while ((nbytes = fread(buf, 1, C2CBUFSIZ, stdin))) {
            if(nbytes != fwrite(buf, 1, nbytes, rightin)) {
                fprintf(stderr,"Failed to write %s.\n", rightintmpstr);
                exit(1);
            }
        }
        fclose(rightin);
        rightinstr = rightintmpstr;
        rightintmpused = 1;
    }
    
    
    /* Read the right input file */
    if (!(rightin = fopen (rightinstr, "rb"))) {
        fprintf (stderr,"Couldn't open the right input CIF file %s\n", rightinstr);
        exit (1);
    }
    
    if (rightintmpused) {
        if (unlink(rightintmpstr) != 0 ) {
            fprintf(stderr,"seqmatch:  Can't unlink temporary file %s.\n", rightintmpstr);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
    }
    
    cbf_failnez (cbf_read_widefile (rightincbf, rightin, MSG_DIGEST))
    
    leftout = rightout = NULL;
    
    if (!cbf_find_tag(leftincbf,"_entity_poly_seq.entity_id")) {
        if (!cbf_find_tag(rightincbf,"_entity_poly_seq.entity_id")) {
        } else {
            fprintf(stderr,"seqmatch: Unable to find _entity_poly_seq.entity_id in right input %s\n",rightinstr);
            exit(1);
        }
        
    } else {
        fprintf(stderr,"seqmatch: Unable to find _entity_poly_seq.entity_id in left input %s\n",leftinstr);
        if (cbf_find_tag(rightincbf,"_entity_poly_seq.entity_id")) {
            fprintf(stderr,"seqmatch: Unable to find _entity_poly_seq.entity_id in right input %s\n",rightinstr);
            exit(1);
        }
    }
    
    cbf_failnez(cbf_rewind_row(leftincbf));
    cbf_failnez(cbf_rewind_row(rightincbf));
    
    {
        const char * lastleftent = NULL;
        const char * lastrightent = NULL;
        const char * entity, * residue;
        unsigned int leftrows, rightrows;
        int row;
        int seqnum;
        
        cbf_failnez (cbf_count_rows(leftincbf,&leftrows))
        for (row=0;row<leftrows;row++) {
            cbf_failnez(cbf_select_row(leftincbf,row))
            cbf_failnez(cbf_find_column(leftincbf,"entity_id"))
            cbf_failnez(cbf_get_value(leftincbf,&entity))
            if (!entity) entity="";
            if (!lastleftent || cbf_cistrcmp(lastleftent,entity)) {
                if (numleftch <256) {
                    lastleftent = entity;
                    leftchname[numleftch] = entity;
                    cbf_failnez(create_chainhandle(leftch+(numleftch++), entity, leftrows))
                } else {
                    fprintf(stderr,"sequence_match: more than 256 chains in leftincbf\n ");
                    exit(-1);
                }
            }
            cbf_failnez(cbf_find_column(leftincbf,"mon_id"))
            cbf_failnez(cbf_get_value(leftincbf,&residue))
            cbf_failnez(cbf_find_column(leftincbf,"num"))
            cbf_failnez(cbf_get_integervalue(leftincbf,&seqnum))
            cbf_failnez(add_residue(leftch[numleftch-1],residue,seqnum))
        }
        
        cbf_failnez (cbf_count_rows(rightincbf,&rightrows))
        for (row=0;row<rightrows;row++) {
            cbf_failnez(cbf_select_row(rightincbf,row))
            cbf_failnez(cbf_find_column(rightincbf,"entity_id"))
            cbf_failnez(cbf_get_value(rightincbf,&entity))
            if (!entity) entity="";
            if (!lastrightent || cbf_cistrcmp(lastrightent,entity)) {
                if (numrightch <256) {
                    lastrightent = entity;
                    rightchname[numrightch] = entity;
                    cbf_failnez(create_chainhandle(rightch+(numrightch++), entity, rightrows))
                } else {
                    fprintf(stderr,"sequence_match: more than 256 chains in rightincbf\n ");
                    exit(-1);
                }
            }
            cbf_failnez(cbf_find_column(rightincbf,"mon_id"))
            cbf_failnez(cbf_get_value(rightincbf,&residue))
            cbf_failnez(cbf_find_column(rightincbf,"num"))
            cbf_failnez(cbf_get_integervalue(rightincbf,&seqnum))
            cbf_failnez(add_residue(rightch[numrightch-1],residue,seqnum))
        }
    }
    
    for (iii=0; iii<numleftch; iii++) {
        chainhandle chain;
        int minresnum, maxresnum;
        int resnum;
        int index;
        chain = leftch[iii];
        minresnum = chain->minresnum;
        maxresnum = chain->maxresnum;
        for (resnum=minresnum; resnum<=maxresnum; resnum++) {
            index = -1;
            while((index = get_by_resnum(chain, index, resnum))!=-1) {
                fprintf(stderr," left chain %d residue %d %s\n",iii,resnum,(chain->residues)[index].resname);
            }
        }
    }

    for (iii=0; iii<numrightch; iii++) {
        chainhandle chain;
        int minresnum, maxresnum;
        int resnum;
        int index;
        chain = rightch[iii];
        minresnum = chain->minresnum;
        maxresnum = chain->maxresnum;
        for (resnum=minresnum; resnum<=maxresnum; resnum++) {
            index = -1;
            while((index = get_by_resnum(chain, index, resnum))!=-1) {
                fprintf(stderr," right chain %d residue %d %s\n",iii,resnum,(chain->residues)[index].resname);
            }
        }
    }
    
    
    fprintf(stderr,"%d chains in %s, %d chains in %s\n", numleftch, leftinstr, numrightch, rightinstr);
    
    for (iii=0; iii<256; iii++) left_to_right[iii]=-1;
    for (iii=0; iii<256; iii++) right_to_left[iii]=-1;
    
    for (iii=0; iii<numleftch; iii++) {
        for (jjj=0; jjj<numrightch; jjj++) {
            if (right_to_left[jjj]<0) {
                int lr, rl;
                int countl, countr;
                countl = leftch[iii]->maxresnum-leftch[iii]->minresnum+1;
                countr = rightch[iii]->maxresnum-rightch[iii]->minresnum+1;
                if ((lr=seq_comp(leftch[iii],rightch[jjj])) > 1+countl/3
                    && (rl=seq_comp(rightch[jjj],leftch[iii]))> 1+countr/3 ) {left_to_right[iii]=jjj; right_to_left[jjj]=iii;
                    fprintf(stderr,"matched %d to %d, matching %d on the left and %d on the right \n",iii,jjj, lr, rl);
                }
            }
        }
    }
    
    
    cbf_failnez(cbf_copy_cbf(leftoutcbf,leftincbf,CBF_BYTE_OFFSET,CBF_HDR_FINDDIMS))
    cbf_failnez(cbf_copy_cbf(rightoutcbf,rightincbf,CBF_BYTE_OFFSET,CBF_HDR_FINDDIMS))
 
    
    if (!cbf_find_tag(leftoutcbf,"_atom_site.label_seq_id")) {
        if (cbf_find_tag(rightoutcbf,"_atom_site.label_seq_id")) {
            fprintf(stderr,"seqmatch: Unable to find _atom_site.label_seq_id in right input %s\n",rightinstr);
            exit(1);
        }
        
    } else {
        fprintf(stderr,"seqmatch: Unable to find _atom_site.label_seq_id in left input %s\n",leftinstr);
        if (cbf_find_tag(rightoutcbf,"_atom_site.label_seq_id")) {
            fprintf(stderr,"seqmatch: Unable to find _atom_site.label_seq_id in right input %s\n",rightinstr);
            exit(1);
        }
    }
    
    {
        int row;
        unsigned int leftrows;
        int seqnum;
        const char * resname;
        const char * entity;
        const char * atomtype;
        const char * atomname;
        int iii;
        int foundent;
        
        cbf_failnez (cbf_count_rows(leftoutcbf,&leftrows))
        for (row=0;row<leftrows;row++) {
            cbf_failnez(cbf_select_row(leftoutcbf,row))
            
            /* try to get the residue sequence number
               and the residue name */

            cbf_failnez(cbf_find_column(leftoutcbf,"label_seq_id"))
            if (!cbf_get_integervalue(leftoutcbf,&seqnum)
                &&!cbf_find_column(leftoutcbf,"label_comp_id")
                &&!cbf_get_value(leftoutcbf,&resname)){
                
                /* try to get the chain identifier */

                foundent = 0;
                cbf_failnez(cbf_find_column(leftoutcbf,"label_entity_id"))
                if (!cbf_get_value(leftoutcbf,&entity)){
                    
                    for (iii=0; iii<numleftch; iii++) {
                         if (!cbf_cistrcmp(entity,leftchname[iii])) {
                           foundent = 1;
                           if (left_to_right[iii] < 0) {
                                cbf_failnez(cbf_remove_row(leftoutcbf))
                                row--;
                                leftrows--;
                                break;
                            } else {
                                int left;
                                left = -1;
                                left = get_by_resnum(leftch[iii], left, seqnum );
                                if (left >=0 && !cbf_cistrcmp((leftch[iii]->residues[left]).resname,resname)) {
                                    if (doca) {
                                        cbf_failnez(cbf_find_column(leftoutcbf,"type_symbol"))
                                        cbf_failnez(cbf_get_value(leftoutcbf,&atomtype))
                                        cbf_failnez(cbf_find_column(leftoutcbf,"label_atom_id"))
                                        cbf_failnez(cbf_get_value(leftoutcbf,&atomname))
                                        if (!atomtype 
                                            || !atomname 
                                            || cbf_cistrcmp(atomtype,"C")
                                            || cbf_cistrcmp(atomname,"CA")) {
                                            cbf_failnez(cbf_remove_row(leftoutcbf))
                                            row--;
                                            leftrows--;
                                            break;
                                        }
                                    }                                        
                                    break;
                                } 
                                while (left >=0 && (left=(leftch[iii]->residues[left]).resnumhashchain)>=0) {
                                    if (!cbf_cistrcmp((leftch[iii]->residues[left]).resname,resname)) {
                                        if (doca) {
                                            cbf_failnez(cbf_find_column(leftoutcbf,"type_symbol"))
                                            cbf_failnez(cbf_get_value(leftoutcbf,&atomtype))
                                            cbf_failnez(cbf_find_column(leftoutcbf,"label_atom_id"))
                                            cbf_failnez(cbf_get_value(leftoutcbf,&atomname))
                                            if (!atomtype 
                                                || !atomname 
                                                || cbf_cistrcmp(atomtype,"C")
                                                || cbf_cistrcmp(atomname,"CA")) {
                                                cbf_failnez(cbf_remove_row(leftoutcbf))
                                                row--;
                                                leftrows--;
                                                break;
                                            }
                                        }                                        
                                        break;
                                    } 
                                }
                                
                                /* if there is no matching residue on the right, we will just drop
                                   this atom, but even if there is a matching residue, we may be
                                   required to drop to atom if we are only accepting carbon alphas
                                 */
                                
                                 if (left < 0) {
                                    cbf_failnez(cbf_remove_row(leftoutcbf))
                                    row--;
                                    leftrows--;
                                    break;
                                 } else if (doca) {
                                     cbf_failnez(cbf_find_column(leftoutcbf,"type_symbol"))
                                     cbf_failnez(cbf_get_value(leftoutcbf,&atomtype))
                                     cbf_failnez(cbf_find_column(leftoutcbf,"label_atom_id"))
                                     cbf_failnez(cbf_get_value(leftoutcbf,&atomname))
                                     if (!atomtype 
                                         || !atomname 
                                         || cbf_cistrcmp(atomtype,"C")
                                         || cbf_cistrcmp(atomname,"CA")) {
                                         cbf_failnez(cbf_remove_row(leftoutcbf))
                                         row--;
                                         leftrows--;
                                         break;
                                     }
                                 }
                            }
                        }
                    }
                }
                if (!foundent) {
                    cbf_failnez(cbf_remove_row(leftoutcbf))
                    row--;
                    leftrows--;                        
                }                
            }
        }
    }
    
    
    {
        int row;
        unsigned int rightrows;
        int seqnum;
        const char * resname;
        const char * entity;
        const char * atomtype;
        const char * atomname;
        int iii;
        int foundent;
        
        cbf_failnez (cbf_count_rows(rightoutcbf,&rightrows))
        for (row=0;row<rightrows;row++) {
            cbf_failnez(cbf_select_row(rightoutcbf,row))
            
            /* try to get the residue sequence number
             and the residue name */
            
            cbf_failnez(cbf_find_column(rightoutcbf,"label_seq_id"))
            if (!cbf_get_integervalue(rightoutcbf,&seqnum)
                &&!cbf_find_column(rightoutcbf,"label_comp_id")
                &&!cbf_get_value(rightoutcbf,&resname)){
                
                /* try to get the chain identifier */
                
                foundent = 0;
                cbf_failnez(cbf_find_column(rightoutcbf,"label_entity_id"))
                if (!cbf_get_value(rightoutcbf,&entity)){
                    
                    for (iii=0; iii<numrightch; iii++) {
                        if (!cbf_cistrcmp(entity,rightchname[iii])) {
                            foundent = 1;
                            if (right_to_left[iii] < 0) {
                                cbf_failnez(cbf_remove_row(rightoutcbf))
                                row--;
                                rightrows--;
                                break;
                            } else {
                                int right;
                                right = -1;
                                right = get_by_resnum(rightch[iii], right, seqnum );
                                if (right >=0 && !cbf_cistrcmp((rightch[iii]->residues[right]).resname,resname)) {
                                    if (doca) {
                                        cbf_failnez(cbf_find_column(rightoutcbf,"type_symbol"))
                                        cbf_failnez(cbf_get_value(rightoutcbf,&atomtype))
                                        cbf_failnez(cbf_find_column(rightoutcbf,"label_atom_id"))
                                        cbf_failnez(cbf_get_value(rightoutcbf,&atomname))
                                        if (!atomtype 
                                            || !atomname 
                                            || cbf_cistrcmp(atomtype,"C")
                                            || cbf_cistrcmp(atomname,"CA")) {
                                            cbf_failnez(cbf_remove_row(rightoutcbf))
                                            row--;
                                            rightrows--;
                                            break;
                                        }
                                        
                                    }
                                    break;
                                } 
                                while (right >=0 && (right=(rightch[iii]->residues[right]).resnumhashchain)>=0) {
                                    if (!cbf_cistrcmp((rightch[iii]->residues[right]).resname,resname)) {
                                        if (doca) {
                                            cbf_failnez(cbf_find_column(rightoutcbf,"type_symbol"))
                                            cbf_failnez(cbf_get_value(rightoutcbf,&atomtype))
                                            cbf_failnez(cbf_find_column(rightoutcbf,"label_atom_id"))
                                            cbf_failnez(cbf_get_value(rightoutcbf,&atomname))
                                            if (!atomtype 
                                                || !atomname 
                                                || cbf_cistrcmp(atomtype,"C")
                                                || cbf_cistrcmp(atomname,"CA")) {
                                                cbf_failnez(cbf_remove_row(rightoutcbf))
                                                row--;
                                                rightrows--;
                                                break;
                                            }
                                            
                                        }
                                        break;
                                    } 
                                }
                                
                                /* if there is no matching residue on the right, we will just drop
                                 this atom, but even if there is a matching residue, we may be
                                 required to drop to atom if we are only accepting carbon alphas
                                 */
                                
                                if (right < 0) {
                                    cbf_failnez(cbf_remove_row(rightoutcbf))
                                    row--;
                                    rightrows--;
                                    break;
                                } else if (doca) {
                                    cbf_failnez(cbf_find_column(rightoutcbf,"type_symbol"))
                                    cbf_failnez(cbf_get_value(rightoutcbf,&atomtype))
                                    cbf_failnez(cbf_find_column(rightoutcbf,"label_atom_id"))
                                    cbf_failnez(cbf_get_value(rightoutcbf,&atomname))
                                    if (!atomtype 
                                        || !atomname 
                                        || cbf_cistrcmp(atomtype,"C")
                                        || cbf_cistrcmp(atomname,"CA")) {
                                        cbf_failnez(cbf_remove_row(rightoutcbf))
                                        row--;
                                        rightrows--;
                                        break;
                                    }
                                    
                                }
                            }
                        }
                    }
                }
                if (!foundent) {
                    cbf_failnez(cbf_remove_row(rightoutcbf))
                    row--;
                    rightrows--;                        
                }                
            }
        }
    }
    
    
    /* Having pruned just to matching residues and possibly just matching
       CA, now prune to match all atoms by residue name, atom name and
       atom type */
    
    {
        int leftrow, rightrow;
        unsigned int leftrows, rightrows;
        int leftresses, rightresses;
        int leftdels, rightdels;
        int leftainr, rightainr;
        int lastleft, lastright;
        const char * leftresname, * rightresname;
        const char * leftatomtype, * rightatomtype;
        const char * leftatomname, * rightatomname;
        int rightresnum, leftresnum;
        
        cbf_failnez (cbf_count_rows(leftoutcbf,&leftrows))
        cbf_failnez (cbf_count_rows(rightoutcbf,&rightrows))
        
        leftrow=rightrow=0;
        leftdels=rightdels=0;
        lastleft = lastright = -999999;
        leftainr = rightainr = 0;
        leftresses = rightresses = 0;
        
        while (leftrow < leftrows && rightrow < rightrows) {
            cbf_failnez(cbf_select_row(leftoutcbf, leftrow))
            cbf_failnez(cbf_select_row(rightoutcbf, rightrow))

            /* try to get the residue sequence numbers
             and the residue names */
            
            cbf_failnez(cbf_find_column(leftoutcbf,"label_seq_id"))
            cbf_failnez(cbf_find_column(rightoutcbf,"label_seq_id"))
            if (!cbf_get_integervalue(leftoutcbf,&leftresnum)
                &&!cbf_find_column(leftoutcbf,"label_comp_id")
                &&!cbf_get_value(leftoutcbf,&leftresname)
                &&!cbf_get_integervalue(rightoutcbf,&rightresnum)
                &&!cbf_find_column(rightoutcbf,"label_comp_id")
                &&!cbf_get_value(rightoutcbf,&rightresname)) {
                
                if (leftresnum != lastleft) {
                    if (leftainr!=0) leftresses++;
                    lastleft = leftresnum;
                    leftainr = 0;
                }
                
                if (rightresnum != lastright) {
                    if (rightainr!=0) rightresses++;
                    lastright = rightresnum;
                    rightainr = 0;
                }
                
                
                cbf_failnez(cbf_find_column(leftoutcbf,"type_symbol"))
                cbf_failnez(cbf_get_value(leftoutcbf,&leftatomtype))
                cbf_failnez(cbf_find_column(leftoutcbf,"label_atom_id"))
                cbf_failnez(cbf_get_value(leftoutcbf,&leftatomname))
                cbf_failnez(cbf_find_column(rightoutcbf,"type_symbol"))
                cbf_failnez(cbf_get_value(rightoutcbf,&rightatomtype))
                cbf_failnez(cbf_find_column(rightoutcbf,"label_atom_id"))
                cbf_failnez(cbf_get_value(rightoutcbf,&rightatomname))
                
                leftainr++;
                rightainr++;
                
                if (!cbf_cistrcmp(leftatomtype,rightatomtype)
                    &&!cbf_cistrcmp(leftatomname,rightatomname)
                    &&!cbf_cistrcmp(leftresname,rightresname)
                    &&leftainr==rightainr) {
                    leftrow++;
                    rightrow++;
                    continue;
                }
                if (!cbf_cistrcmp(leftresname,rightresname)) {
                if (leftainr > rightainr) {
                    cbf_failnez(cbf_remove_row(leftoutcbf))
                    leftrows--;
                    leftainr--;
                    rightainr--;
                    leftdels++;
                    continue;
                }
                if (leftainr < rightainr) {
                    cbf_failnez(cbf_remove_row(rightoutcbf))
                    rightrows--;
                    rightainr--;
                    leftainr--;
                    rightdels++;
                    continue;
                }
                    cbf_failnez(cbf_remove_row(leftoutcbf))
                    cbf_failnez(cbf_remove_row(rightoutcbf))
                    leftrows--;
                    leftainr--;
                    leftdels++;
                    rightrows--;
                    rightainr--;
                    rightdels++;
                    continue;
                }
                if (rightresnum > leftresnum ) {
                    cbf_failnez(cbf_remove_row(leftoutcbf))
                    leftrows--;
                    leftainr--;
                    rightainr--;
                    leftdels++;
                    continue;
                }
                if (leftresnum > rightresnum ) {
                    cbf_failnez(cbf_remove_row(rightoutcbf))
                    rightrows--;
                    rightainr--;
                    leftainr--;
                    rightdels++;
                    continue;
                }
                cbf_failnez(cbf_remove_row(leftoutcbf))
                cbf_failnez(cbf_remove_row(rightoutcbf))
                leftrows--;
                leftainr--;
                leftdels++;
                rightrows--;
                rightainr--;
                rightdels++;
                
            }
            
        }
        
        while (leftrow < leftrows ) {
            cbf_failnez(cbf_select_row(leftoutcbf, leftrow))
            cbf_failnez(cbf_remove_row(leftoutcbf))
            leftrows--;
        }            
    
        while (rightrow < rightrows ) {
            cbf_failnez(cbf_select_row(rightoutcbf, rightrow))
            cbf_failnez(cbf_remove_row(rightoutcbf))
            rightrows--;
        }            
        
    }
        
    
    if ( ! leftoutstr || strcmp(leftoutstr?leftoutstr:"","-") == 0 ) {
        leftout = stdout;
    } else if ( strcmp(leftoutstr?leftoutstr:"","/dev/null") ==0 ){
        leftdevnull=1;
    } else {
        leftout = fopen (leftoutstr, "w+b");
    }
    
    if ( ! rightoutstr || strcmp(rightoutstr?rightoutstr:"","-") == 0 ) {
        rightout = stdout;
    } else if ( strcmp(rightoutstr?rightoutstr:"","/dev/null") ==0 ){
        rightdevnull=1;
    } else {
        rightout = fopen (rightoutstr, "w+b");
    }
    
   
    cbf_failnez (cbf_write_widefile (leftoutcbf, leftout, 1, CIF, 
                                 MIME_HEADERS|MSG_DIGEST,
                                 0))
    cbf_failnez (cbf_write_widefile (rightoutcbf, rightout, 1, CIF, 
                                 MIME_HEADERS|MSG_DIGEST,
                                 0))
    
    exit(0);
    
    
}    

int local_exit (int status)
{
    exit(status);
    return 1; /* avoid warnings */
}


