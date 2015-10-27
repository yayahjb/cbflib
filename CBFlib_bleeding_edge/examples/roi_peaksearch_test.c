#include "cbf.h"
#include "cbf_simple.h"
#include "cbf_string.h"
#include "dps_peaksearch.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>



void    bin2x2(unsigned short *data, int width, int height, unsigned short *out);
int     convertroi(char *roi, int fastdim, int slowdim,
                   int * fastlow, int * fasthigh, int * slowlow, int * slowhigh);



static void gethd ( char* field, char* value, char* header )
{
    char *hp, *lhp, *fp, *vp;
    int l, j;
    char *newfield;
    
    /*
     * Find the last occurance of "field" in "header"
     */
    
    l = strlen (field);
    newfield = (char*) malloc ( l + 3 );
    *newfield = 10;
    strncpy (newfield+1, field, l);
    *(newfield+l+1) = '=';
    *(newfield+l+2) = (char) 0;
    l += 2;
    
    lhp = 0;
    for (hp=header; *hp != '}'; hp++)
    {
        for (fp=newfield, j=0; j<l && *(hp+j) == *fp; fp++, j++);
        if ( j == l ) lhp=hp;
    }
    
    if ( lhp == 0 )
        value[0] = 0;
    else
    {
        /*
         * Find the '='.  hp will now point to the '='
         */
        for (hp=lhp; *hp!='='; hp++);
        
        /*
         * Copy into the returned value until the ';'
         */
        for (lhp=hp+1, vp=value; *lhp!=';' && *lhp!=0; lhp++, vp++) *vp = *lhp;
        *(vp++)=0;
    }
    free (newfield);
}



#define        MAX_PEAKS    20000

DPS_Peak    peaks[MAX_PEAKS];

int        max_peaks = MAX_PEAKS;
int        min_spacing = 6;
double     ioversig = 2.;

void    usage()
{
    fprintf(stderr,"Usage: roi_peaksearch [--binagain] [--min-i-over-sigma ioversig] \\\n"
            "        [--region_of_interest fastlow,fasthigh,slowlow,showhigh] \\\n"
            "        [--overlay overlay_file.cbf] [--mask mask_file.cbf] \\\n"
            "        file.cbf peaklist \n");
}

int     main (int argc, char **argv)
{
    unsigned short *raw;
    unsigned short *out;
    char * filename = NULL;  /* The input file name */
    char * hp;
    char * maskname = NULL;  /* An optional mask file (e.g. BKGINIT.cbf) */
    char * overlayname = NULL; /* An optional peak/roi overlay file */
    int    dim, size[10], type;
    int    xsize, ysize;
    int    istat, lhead;
    char * roi = NULL;
    char * strstrhit;
    int    n_peaks;
    int    i;
    int    binagain = 0;
    FILE   *fp;
    FILE   *ovlp;
    
    char  field[128];
    char  *header;
    
    int nStat = 0;
    
    cbf_handle hCBFile;
    cbf_handle hCBFmask;
    cbf_handle hCBFoverlay;
    
    int    nBinary_id, mask_nBinary_id;
    unsigned int compression, mask_compression;
    int    binary_id, mask_binary_id;
    size_t elsize, mask_elsize;
    int    elsigned, mask_elsigned;
    int    elunsigned, mask_elunsigned;
    size_t elements, elements_read, mask_elements, mask_elements_read;
    int    minelement, mask_minelement;
    int    maxelement, mask_maxelement;
    char*  byteorder ="little_endian";
    char*  mask_byteorder;
    size_t dim1, mask_dim1;
    size_t dim2, mask_dim2;
    size_t dim3, mask_dim3;
    size_t padding, mask_padding;
    int    nDim0, nDim1;
    int    fastlow, fasthigh, slowlow, slowhigh;
    unsigned short    *ushort_data, *up;
    int*   pnData, *ip;
    int*   pmaskData;
    int    lastgood;
    int    error;
    
    error = 0;
    hCBFile = hCBFmask = hCBFoverlay = NULL;
    
    if ( argc < 3 )
    {
        usage();
        exit(-1);
    }
    
    while (argc > 3) {
        if (argc > 4 && (0 == cbf_cistrcmp("--overlay-image",argv[1])
                         || (0 == cbf_cistrcmp("--ovl",argv[1])))) {
            overlayname = argv[2];
            argv+=2;
            argc-=2;
            ovlp = fopen(overlayname, "w+b");
            if (!ovlp) {
                fprintf(stderr,"roi_peaksearch: unable to open overlay cbf `%s'\n\n",overlayname);
                usage();
                exit(-1);
            }
            continue;
        }
        
        if (argc > 4 && (0 == cbf_cistrcmp("--mask",argv[1])
                         || (0 == cbf_cistrcmp("--BKGINIT",argv[1])))) {
            maskname = argv[2];
            argv+=2;
            argc-=2;
            if ( ( fp = fopen(maskname, "rb" ) ) == NULL ) {
                fprintf(stderr,"Error opening file\n");
                return(1);
            }
            error |= cbf_make_handle(&hCBFmask);
            
            if (0 != error) {
                fprintf(stderr,"roi_peaksearch: ERROR in mask cbf_make_handle\n");
                usage();
                exit(-1);
            }
            
            error |= cbf_read_widefile(hCBFmask, fp, MSG_DIGEST);
            if (0 != error) {
                fprintf(stderr,"roi_peaksearch: ERROR in mask cbf_read_widefile\n");
                usage();
                exit(-1);
            }
            
            error |= cbf_find_tag(hCBFmask, "_array_data.data");
            if (0 != error) {
                fprintf(stderr,"roi_peaksearch: ERROR in cbf_find_tag\n");
                usage();
                exit(-1);
            }
            
            error |= cbf_find_column   (hCBFmask, "data");
            if (0 != error)  {
                fprintf(stderr,"roi_peaksearch: ERROR in cbf_find_column data\n");
                usage();
                exit(-1);
            }

        
            error|= cbf_get_integerarrayparameters_wdims (hCBFmask, &mask_compression,
                                                          &mask_nBinary_id,
                                                          &mask_elsize,
                                                          &mask_elsigned,
                                                          &mask_elunsigned,
                                                          &mask_elements,
                                                          &mask_minelement,
                                                          &mask_maxelement,
                                                          (const char **) &mask_byteorder,
                                                          &mask_dim1, &mask_dim2, &mask_dim3, &mask_padding);
            if (0 != error)  {
                fprintf(stderr,"roi_peaksearch: ERROR in cbf_get_integerarrayparameters_wdims\n");
                usage();
                exit(-1);
            }
            
            if (mask_dim3 == 0) mask_dim3 = 1;
            if (mask_dim2 == 0) mask_dim2 = 1;
            if (mask_dim1 == 0) mask_dim1 = mask_elements;

            continue;
        }

        
        if (argc > 4 && (0 == cbf_cistrcmp("--min-i-over-sigma",argv[1])
                         || (0 == cbf_cistrcmp("--min-peak",argv[1])))) {
            ioversig = atof(argv[2]);
            argv+=2;
            argc-=2;
            continue;
        }
        if (argc > 4 && (0 == cbf_cistrcmp("--overlay",argv[1])
                         || (0 == cbf_cistrcmp("ovl",argv[1])))) {
            overlayname = argv[2];
            argv+=2;
            argc-=2;
            continue;
        }
        if(0 == cbf_cistrcmp("--binagain", argv[1])
           || 0 == cbf_cistrcmp("-binagain",argv[1]))
        {
            binagain = 1;
            argv++;
            argc--;
            continue;
        }
        if(argc > 4 && (0 == cbf_cistrcmp("--region-of-interest", argv[1])
                        || 0==cbf_cistrcmp("roi",argv[1]))) {
            if (roi) {
                fprintf(stderr,"roi_peaksearch: only one roi supported\n\n");
                usage();
                exit(-1);
            }
            roi = argv[2];
            argv += 2;
            argc -= 2;
            continue;
        }
        strstrhit = strstr(argv[1], "--region_of_interest");
        if(NULL != strstrhit && strstrhit==argv[1] ) {
            roi = argv[1]+strlen("--region_of_interest")+1;
            if (*(roi-1)!='='||strlen(roi)<3){
                fprintf(stderr,"roi_peaksearch: %s should be --region_of_interest=fastlow,fasthigh,slowlow,slowhigh\n\n", argv[1]);
                usage();
                exit(-1);
            }
            argv++;
            argc--;
            continue;
        }
        usage();
        exit(-1);
    }
    filename=argv[1];
    if ( ( fp = fopen(filename, "rb" ) ) == NULL ) {
        fprintf(stderr,"Error opening file\n");
        exit(-1);
    }
    error |= cbf_make_handle(&hCBFile);
    
    if (0 != error)  {
        fprintf(stderr,"roi_peaksearch: ERROR %x in cbf_make_handle\n", error);
        exit(-1);
    }
    error |= cbf_read_widefile(hCBFile, fp, MSG_DIGEST);
    if (0 != error)  {
        fprintf(stderr,"roi_peaksearch: ERROR %x in cbf_read_widefile\n", error);
        exit(-1);
    }
    
    error |= cbf_find_tag(hCBFile, "_array_data.data");
    if (0 != error)  {
        fprintf(stderr,"roi_peaksearch: ERROR %x in cbf_find_tag\n", error);
        exit(-1);
    }
    
    error |= cbf_find_column   (hCBFile, "data");
    if (0 != error)  {
        fprintf(stderr,"roi_peaksearch: ERROR %x in cbf_find_column data\n", error);
        exit(-1);
    }
    
    error |= cbf_get_integerarrayparameters_wdims (hCBFile, &compression,
                                                  &nBinary_id,
                                                  &elsize,
                                                  &elsigned,
                                                  &elunsigned,
                                                  &elements,
                                                  &minelement,
                                                  &maxelement,
                                                  (const char **) &byteorder,
                                                  &dim1, &dim2, &dim3, &padding);
    if (dim1 == 0) dim1 = elements;
    if (dim2 == 0) dim2 = 1;
    if (dim3 == 0) dim3 = 1;
    
    if (0 != error)  {
        fprintf(stderr,"roi_peaksearch: ERROR %x in cbf_get_integerarrayparameters_wdims\n", error);
        exit(-1);
    }

    
    nDim0 = (int)dim1;
    nDim1 = (int)dim2;
    xsize = nDim0;
    ysize = nDim1;
    
    if (convertroi ( roi, xsize, ysize, &fastlow, &fasthigh, &slowlow, &slowhigh) ) {
        fprintf(stderr,"roi_peaksearch: invalid region of interest '%s'\n", roi);
        usage();
        exit(-1);
    }
    
    out = (unsigned short *)malloc ((fasthigh-fastlow+1)*(slowhigh-slowlow+1)*sizeof( unsigned short ));
    if (out == NULL) {
        fprintf(stderr,"roi_peaksearch: ERROR can not allocate %ld bytes\n",(long unsigned int) (fasthigh-fastlow+1)*(slowhigh-slowlow+1)*sizeof( unsigned short ));
        fflush(stderr); exit(-1);
    }
    if(NULL == (pnData = (int *) malloc(dim1 * dim2 * dim2 * elsize)))
    {
        fprintf(stderr,"roi_peaksearch: ERROR can not allocate %ld bytes for data\n",(long unsigned int) (fasthigh-fastlow+1)*(slowhigh-slowlow+1)*sizeof( unsigned short ));
        fflush(stderr);
        exit(-1);
    }
    if(NULL == (ushort_data = (unsigned short *) malloc(nDim0 * nDim1 * sizeof (unsigned short))))
    {
        return(1);
    }
    
    if (hCBFmask) {
        
        if(NULL == (pmaskData = (int *) malloc(mask_dim1 * mask_dim2 * mask_dim3 * elsize)))
        {
            fprintf(stderr,"roi_peaksearch: ERROR unable to allocate %ld bytes for mask\n",
                    (unsigned long)(mask_dim1 * mask_dim2 * mask_dim3 * elsize));
            fflush(stderr); exit(-1);
        }

        error |=  cbf_get_integerarray (hCBFmask,
                                        &mask_binary_id, (void *)pmaskData,
                                        mask_elsize, mask_elsigned,
                                        mask_elements, &mask_elements_read);
        
        if (0 != error) {
            fprintf(stderr,"roi_peaksearch: ERROR %x in mask cbf_get_integerarray\n", error);
            fflush(stderr); exit(-1);
        }

        
        
    }
    
    error |=  cbf_get_integerarray (hCBFile,
                                   &binary_id, (void *)pnData,
                                   elsize, elsigned,
                                   elements, &elements_read);
    
    if (0 != error) {
        fprintf(stderr,"roi_peaksearch: ERROR in cbf_get_integerarray\n");
        fflush(stderr); exit(-1);
    }
    
    int k;
    up = ushort_data;
    ip = pnData;
    k = nDim0*nDim1;
    for(i = 0; i < k; i++) {
        if ((0xFFFF0000 &*ip) != 0) {
            *up++ = 0xFFFF;
            ip++; }
        else  {
            lastgood = *ip++;
            if ((0xFFF0 & lastgood) == 0xFFF0) lastgood = 0xFFFF;
            *up++ = (unsigned short) (lastgood &0xFFFF);
        }
    }
    
    
    //    *data = ushort_data;
    if ( !overlayname ) free(pnData);
    
    //          istat = cbf2adscimg_sub(filename, &header, &raw);
    //      istat = rdfile ( filename, &hp, &lhead, (char**) &raw, &dim, size, &type );
    
    
    {
        unsigned short * ip = out;
        int i, j;
        
        for(j = slowlow; j <= slowhigh; j++) {
            for(i = fastlow; i <= fasthigh; i++) {
                *ip = 0x0000ffff &
                (int)ushort_data[j*xsize   +i];
                if (hCBFmask && i < mask_dim1 && j < mask_dim2 && (pmaskData[j*mask_dim1+i]&0xFFFC)==0xFFFC)
                    *ip = 0xFFFF;
                ip++;
            }
        }
        
        
        
    }
    
    n_peaks = dps_peaksearch(out, fasthigh-fastlow+1, slowhigh-slowlow+1, max_peaks, ioversig, min_spacing, peaks);
    //        n_peaks = dps_peaksearch_cn(raw, xsize, ysize, max_peaks, ioversig, min_spacing, peaks);
    
    fprintf(stdout, "Number of peaks found with I/sigma > %.2f is %d\n", ioversig, n_peaks);
    fprintf(stdout, "PeaksCount : %d\n", n_peaks);
    fprintf(stdout, "Top 10 peaks:\n");
    fprintf(stdout, "    x       y       I/sigma\n");
    for(i = 0; i < 10; i++)
        fprintf(stdout, "%7.2f %7.2f  %9.2f\n", peaks[i].x+fastlow, peaks[i].y+slowlow, peaks[i].isigma);
    if(NULL == (fp = fopen(argv[2], "w")))
    {
        fprintf(stderr, "roi_peaksearch: Cannot create %s as output peaklist file\n", argv[2]);
        exit(0);
    }
    for(i = 0; i < n_peaks; i++) {
        int xlow, xhigh, ylow, yhigh, xcen, ycen, ix, iy;
        int heat;
        fprintf(fp, "%7.2f %7.2f  %9.2f\n", peaks[i].x+fastlow, peaks[i].y+slowlow, peaks[i].isigma);
        if (overlayname) {
            xcen = peaks[i].x+fastlow+0.5;
            ycen = peaks[i].y+slowlow+0.5;
            xlow = xcen - 6;
            if (xlow < 0) xlow = 0;
            xhigh = xcen + 6;
            if (xhigh >= nDim0) xhigh = nDim0-1;
            ylow = ycen - 6;
            if (ylow < 0) ylow = 0;
            yhigh = ycen +6;
            if (yhigh >= nDim1) yhigh = nDim1-1;
            heat = (int)(peaks[i].isigma/2.5+0.5);
            if (heat < 1) heat = 1;
            if (heat > 65520) heat = 65520;
            for (ix = xlow; ix <=xhigh; ix++) {
                if (ix < xcen-2 || ix > xcen+2) {
                    pnData[ix+nDim0*ycen] ^= heat;
                }
                if (ix == xlow+1 || ix == xhigh-1) {
                    pnData[ix+nDim0*ycen] ^= 0xFF00;
                }
            }
            for (iy = ylow; iy <=yhigh; iy++) {
                if (iy < ycen-2 || iy > ycen+2) {
                    pnData[xcen+nDim0*iy] ^= heat;
                }
                if (iy == ylow+1 || iy == yhigh-1) {
                    pnData[xcen+nDim0*iy] ^= 0xFF00;
                }

            }
        }
    }
    fclose(fp);
    
    if (overlayname) {
        if (roi) {
            int xlow, xhigh, ylow, yhigh, ix, iy;
            if (fastlow > 0){
                xlow = fastlow-1;
                for (iy = slowlow; iy < slowhigh; iy+=2){
                    pnData[xlow+nDim0*iy] ^= 0xFF00;
                }
            }
            if (fasthigh < nDim0-1){
                xhigh = fasthigh+1;
                for (iy = slowlow; iy < slowhigh; iy+=2){
                    pnData[xhigh+nDim0*iy] ^= 0xFF00;
                }
            }
            if (slowlow > 0){
                ylow = slowlow-1;
                for (ix = fastlow; ix < fasthigh; ix+=2){
                    pnData[ix+nDim0*ylow] ^= 0xFF00;
                }
            }
            if (slowhigh < nDim1-1){
                yhigh = slowhigh+1;
                for (ix = fastlow; ix < fasthigh; ix+=2){
                    pnData[ix+nDim0*yhigh] ^= 0xFF00;
                }
            }

        }
        if (hCBFmask) {
            int i, j;
            
            for(j = slowlow; j <= slowhigh; j++) {
                for(i = fastlow; i <= fasthigh; i++) {
                    if (i < mask_dim1 && j < mask_dim2 && (pmaskData[j*mask_dim1+i]&0xFFFC)==0xFFFC)
                        pnData[i+nDim0*j] = -3;
                    ip++;
                }
            }

            
        }
        nStat = cbf_set_integerarray_wdims_fs(hCBFile,
                                     compression,
                                     binary_id,
                                     (void *)pnData,
                                     elsize,
                                     elsigned,
                                     elements,
                                     byteorder,
                                     dim1,dim2,dim3,padding);
        if (0 != nStat) fprintf(stderr,"roi_peaksearch: ERROR %x in cbf_set_integerarray_wdims_fs\n", nStat);
        else {
            nStat = cbf_write_widefile (hCBFile, ovlp, 1, CBF, MIME_HEADERS | MSG_DIGEST, ENC_NONE|ENC_CRTERM | ENC_LFTERM);
            if (0 != nStat) fprintf(stderr,"roi_peaksearch: ERROR %x in cbf_write_widefile\n", nStat);
        }
        free(pnData);
        
    }
    
    // cbf_free_handle hopefully closes the file?
    
    nStat = cbf_free_handle (hCBFile);
    
    exit(0);
}


int convertroi(char *roi, int fastdim, int slowdim,
               int * fastlow, int * fasthigh, int * slowlow, int * slowhigh) {
    char * endptr;
    char * str;
    if (!fastlow || !fasthigh || !slowlow || !slowhigh) return CBF_ARGUMENT;
    *fastlow = *slowlow = 0;
    *fasthigh = fastdim-1;
    *slowhigh = slowdim-1;
    if (!roi) {
        return CBF_SUCCESS;
    }
    str = roi;
    *fastlow = (int)strtol(str,&endptr,0);
    if (*fastlow < 0) *fastlow = 0;
    if (*fastlow > fastdim-1) *fastlow = fastdim-1;
    if (*endptr == '\0') return CBF_SUCCESS;
    if (*endptr != ',') return CBF_FORMAT;
    str = endptr+1;
    *fasthigh = (int)strtol(str,&endptr,0);
    if (*fasthigh < *fastlow) *fasthigh = *fastlow;
    if (*fasthigh > fastdim-1) *fasthigh = fastdim-1;
    if (*endptr == '\0') return CBF_SUCCESS;
    if (*endptr != ',') return CBF_FORMAT;
    str = endptr+1;
    *slowlow = (int)strtol(str,&endptr,0);
    if (*slowlow < 0) *slowlow = 0;
    if (*slowlow > slowdim-1) *slowlow = slowdim-1;
    if (*endptr == '\0') return CBF_SUCCESS;
    if (*endptr != ',') return CBF_FORMAT;
    str = endptr+1;
    *slowhigh = (int)strtol(str,&endptr,0);
    if (*slowhigh < *slowlow) *slowhigh = *slowlow;
    if (*slowhigh > slowdim-1) *slowhigh = slowdim-1;
    if (*endptr == '\0') return CBF_SUCCESS;
    return CBF_FORMAT;
}



/*
 * bin an image 2x2
 */

void    bin2x2(unsigned short *data, int width, int height, unsigned short *out)
{
    register int j, i;
    int pixel;
    
    for(j=0;j<(height-1);j+=2)
        for(i=0;i<(width-1);i+=2)  {
            pixel = data[j*width + i] +
            data[j*width + i + 1] +
            data[(j+1)*width + i] +
            data[(j+1)*width + i + 1] ;
            if(pixel > 65535)
                *out++ = 65535;
            else
                *out++ = (pixel + 2)/4;
        }
}
