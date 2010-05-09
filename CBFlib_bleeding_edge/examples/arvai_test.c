/*
 *  arvai_test.c
 *  
 *
 *  Created by Herbert J. Bernstein on 3/2/10.
 *  Copyright 2010 __MyCompanyName__. All rights reserved.
 *
 */


/******************* Begin testcbf.c ***********************/

#include <stdio.h>
#include <cbf.h>
#include <string.h>

int main (int argc, char ** argv) {
	char buf[256];
	char *data_buffer;
    char *temp_buffer;
	int data_buffer_len=0;
	int read_count;
	char filename[] = "/tmp/mb_LP_1_001_orig.cbf.bz2";
	FILE *fp;
	int ierr;
	cbf_handle cbf;
	
	cbf_failnez (cbf_make_handle (&cbf))
    
	sprintf(buf,"bunzip2 -c %s 2>/dev/null\n",filename);
	if((fp = popen(buf,"r")) == NULL)
		return -1;
	
    
	data_buffer = (char *)malloc(sizeof(char)*1024*1024);
    if (data_buffer == NULL) {
        fprintf(stderr,"not enough memory.\n");
        fflush(stderr);
        fclose(fp);
        return -1;
    }
    while ((read_count = fread (data_buffer + data_buffer_len, 1, 1024*1024, fp)) != 0) {
        data_buffer_len += read_count;
        if (read_count < 1024*1024) break;
        temp_buffer = (char*)malloc(sizeof(char)*data_buffer_len+1024*1024);
        if (temp_buffer == NULL) {
            fprintf(stderr,"not enough memory.\n");
            fflush(stderr);
            fclose(fp);
            return -1;
        }
        memmove(temp_buffer,data_buffer,data_buffer_len);
        free(data_buffer);
        data_buffer=temp_buffer;
    }
    
    fprintf(stderr,"data_buffer_len=%d\n",data_buffer_len); fflush(stderr);
    ierr = cbf_read_buffered_file (cbf, NULL, MSG_DIGESTNOW, data_buffer, data_buffer_len);
    fprintf(stderr,"ierr=%d \n",ierr); fflush(stderr);
    exit(0);
}

int
big_endian ()
{
	int x=1;
    
	if ( *(char *)&x == 1)
		return 0;
	else
		return 1;
}
