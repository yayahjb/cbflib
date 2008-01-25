#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<cbf.h>
#include	<cbf_string.h>

/****************************************************************/

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

int	endswith(char *haystack, char *needle)
{
	char	*cp;

	if(NULL == (cp = (char *) strstr(haystack, needle)))
		return(0);
	if('\0' == *(cp + strlen(needle)))
		return(1);
	return(0);
}

void usage()
{
		fprintf(stderr,"Usage: cbf2adscimg [--flag] file1.cbf ... filen.cbf     (creates file1.img ... filen.img)\n");
		fprintf(stderr,"       Image files may be compressed on output: (.gz, .bz2) by using the flags below.\n");
		fprintf(stderr,"Flags:\n");
		fprintf(stderr,"\t--gz		Output a .gz file  (e.g., filen.img.gz).\n");
		fprintf(stderr,"\t--bz2		Output a .bz2 file (e.g., filen.img.bz2).\n");
}

/*
 *	Jiffy to take one or more .cbf files, presumably from an ADSC detector, 
 *	convert each in turn to	an adsc .img file (SMV format), and output the files.
 *	The input file is preserved; the output file has the same name as the input 
 *	file with the suffix .cbf replaced by .img.  Compression on output can be
 *	performed.
 */

char	popen_command[1080];

int	main(int argc, char *argv[])
{
	FILE		*fp;
	char		in_filename[1024], out_filename[1024];
	char		field[128];
	char		header_bytes[6];
	int		data_size, header_size_char;
	int		cbf_status;
	int		i, j, k;
	int		size1, size2;
	int		output_packing;
	int		status_pclose;
	char		*header;
	unsigned short	*data;
	 char *    lbo; /* local byte order */   
	static char	*endings[] = {
					".img",
					".img.gz",
					".img.bz2",
					NULL
				     };
	static char	*flags[] = {
					"--gz",
					"--bz2",
					NULL
				   };

	int		cbf2adscimg_sub(char *filename, char **header, unsigned short **data);

	if(argc < 2)
	{
		usage();
		exit(0);
	}

	output_packing = -1;

	while(argc > 1 && argv[1][0] == '-' && argv[1][1] == '-')
	{
		for(j = 0; flags[j] != NULL; j++)
			if(NULL != strstr(argv[1], flags[j]))
				break;
		if(NULL == flags[j])
		{
			fprintf(stderr,"cbf2adscimg: %s is an unknown flag\n\n", argv[1]);
			usage();
			exit(0);
		}
		output_packing = j;
		argc--;
		argv++;
	}
	output_packing++;

	while(argc > 1)
	{
		strcpy(in_filename, argv[1]);
		i = strlen(in_filename);
		k = strlen(".cbf");
		if(0 == endswith(in_filename, ".cbf"))
		{
			fprintf(stderr,"cbf2adscimg: Input file name %s does not end in .cbf\n", in_filename);
			exit(0);
		}
		strcpy(out_filename, in_filename);
		out_filename[i - k] = '\0';
		strcat(out_filename, endings[output_packing]);

		cbf_status = cbf2adscimg_sub(in_filename, &header, &data);

		if(0 != cbf_status)
		{
			fprintf(stderr, "cbf2adscimg: Error converting cbf file %s to .img format\n", in_filename);
			exit(0);
		}
		if(NULL == header)
		{
			fprintf(stderr, "cbf2adscimg: Error: cbf2adscimg_sub returned NULL for header on file %s\n", in_filename);
			exit(0);
		}

		if(0 == output_packing)
		{
			if(NULL == (fp = fopen(out_filename, "wb")))
			{
				fprintf(stderr, "cbf2adscimg: Cannot create %s as output .img file\n", out_filename);
				exit(0);
			}
		}
		else
		{
			if(0 == output_packing)
				sprintf(popen_command, "gzip > %s", out_filename);
			else
				sprintf(popen_command, "bzip2 > %s", out_filename);
			if(NULL == (fp = popen(popen_command, "w")))
			{
				fprintf(stderr, "cbf2adscimg: Cannot exec %s command to compress output image file.\n", popen_command);
				exit(0);
			}
		}

		/*
		 *	Output the header block(s).
		 */

		for(i = 0; i < 5; i++)
			header_bytes[0 + i] = header[15 + i];

		header_bytes[5] = '\0';
		header_size_char = atoi(header_bytes);
		

		if(header_size_char != fwrite(header, sizeof (char), header_size_char, fp))
		{
			fprintf(stderr, "cbf2adscimg: Cannot write header, size %d bytes, of file %s.\n", header_size_char, in_filename);
			if(0 == output_packing)
				fclose(fp);
			else
			{
				status_pclose = pclose(fp);
				if(0 != status_pclose)
				{
					fprintf(stderr, "Status returned from compress command via popen NON-ZERO: %d\n", status_pclose);
					perror("popen command (maybe this will be useful)");
					fprintf(stderr, "Filename being compressed: %s with command: %s\n", out_filename, popen_command);
					fprintf(stderr, "Program exiting.\n");
					exit(0);
				}
			}
			exit(0);
		}

		field[0] = '\0';
		gethd("SIZE1", field, header);
		if('\0' == field[0])
		{
			fprintf(stderr,"cbf2adscimg: keyword SIZE1 not found in header.  Cannot convert file %s\n", in_filename);
			exit(0);
		}
		size1 = atoi(field);
		
		field[0] = '\0';
		gethd("SIZE2", field, header);
		if('\0' == field[0])
		{
			fprintf(stderr,"cbf2adscimg: keyword SIZE2 not found in header.  Cannot convert file %s\n", in_filename);
			exit(0);
		}
		size2 = atoi(field);
		
		data_size = size1 * size2 * sizeof(unsigned short);

		gethd("BYTE_ORDER", field, header);

		cbf_get_local_integer_byte_order(&lbo);

		if (cbf_cistrcmp(field,lbo)) {
                  unsigned char *p;
		  unsigned char temp;
		  size_t ii;
		  p = (unsigned char *)data;
		  for (ii=0; ii<data_size; ii+=2) {
		    temp = p[ii];
		    p[ii] = p[ii+1];
		    p[ii+1] = temp;
                  }
		}

		if(data_size != fwrite(data, sizeof (char), data_size, fp))
		{
			fprintf(stderr, "cbf2adscimg: Cannot write data (size %d bytes) from input file %s.\n", 
					data_size, in_filename);
			if(0 == output_packing)
				fclose(fp);
			else
			{
				status_pclose = pclose(fp);
				if(0 != status_pclose)
				{
					fprintf(stderr, "Status returned from compress command via popen NON-ZERO: %d\n", status_pclose);
					perror("popen command (maybe this will be useful)");
					fprintf(stderr, "Filename being compressed: %s with command: %s\n", out_filename, popen_command);
					fprintf(stderr, "Program exiting.\n");
					exit(0);
				}
			}
			exit(0);
		}
		if(0 == output_packing)
			fclose(fp);
		else
		{
			status_pclose = pclose(fp);
			if(0 != status_pclose)
			{
				fprintf(stderr, "Status returned from compress command via popen NON-ZERO: %d\n", status_pclose);
				perror("popen command (maybe this will be useful)");
				fprintf(stderr, "Filename being compressed: %s with command: %s\n", out_filename, popen_command);
				fprintf(stderr, "Program exiting.\n");
				exit(0);
			}
		}


		free(header);
		free(data);

		argv++;
		argc--;
	}
	exit(0);
}
