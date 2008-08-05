#include	<stdio.h>

#ifdef NO_POPEN_PROTOTYPE
	/*
	 *	This is supposed to be found in stdio.h.
	 */

	FILE	*popen(char *popen_command, const char *type);
	int		pclose(FILE *stream);
#endif

#include	<stdlib.h>
#include	<string.h>
#include	<cbf.h>

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
		fprintf(stderr,"Usage: adscimg2cbf [--flag[,modifier]] file1.img ... filen.img     (creates file1.cbf ... filen.cbf)\n");
		fprintf(stderr,"       Image files may also be compressed (.gz, .bz2, .Z)\n");
		fprintf(stderr,"Flags:\n");
		fprintf(stderr,"\t--cbf_byte_offset		Use BYTE_OFFSET compression (DEFAULT).\n");
		fprintf(stderr,"\t--cbf_packed			Use CCP4 packing (JPA) compression.\n");
		fprintf(stderr,"\t--cbf_packed_v2		Use CCP4 packing version 2 (JPA) compression.\n");
		fprintf(stderr,"\t--no_compression		No compression.\n\n");
		fprintf(stderr,"The following two modifiers can be appended to the flags (syntax: --flag,modifier):\n");
		fprintf(stderr,"\t,flat				Flat (linear) images.\n");
		fprintf(stderr,"\t,uncorrelated			Uncorrelated sections.\n\n");
		fprintf(stderr,"The following describe how to interpret the beam center in the ADSC header:\n");
		fprintf(stderr,"\t--beam_center_from_header	Figure out beam center from ADSC header information (default)\n");
		fprintf(stderr,"\t--beam_center_mosflm		Beam center in ADSC header: MOSFLM coordinates.\n");
		fprintf(stderr,"\t--beam_center_ulhc		Beam center in ADSC header: origin: upper left hand corner of image.(HKL mm)\n");
		fprintf(stderr,"\t--beam_center_llhc		Beam center in ADSC header: origin: lower left hand corner of image.(adxv mm)\n");
}

/*
 *	Jiffy to take one or more adsc .img files, convert each in turn to
 *	.cbf files, and output the files.  The input file is preserved; the
 *	output file has the same name as the input file with the suffix .img
 *	replaced by .cbf.
 */

char	popen_command[1080];

#define BEAM_CENTER_FROM_HEADER 0
#define BEAM_CENTER_MOSFLM      1
#define BEAM_CENTER_ULHC        2
#define BEAM_CENTER_LLHC        3

int	main(int argc, char *argv[])
{
	FILE		*fp;
	char		in_filename[1024], out_filename[1024];
	char		field[128];
	char		*hptr;
	unsigned short	*uptr;
	char		header_bytes[6];
	int		file_size, header_size_char;
	int		cbf_status;
	int		i, j, k=0;
	int		size1, size2;
	int		file_type;
	int		pack_flags;
	int		status_pclose;
	int		beam_center_convention;
	static char	*endings[] = {
					".img",
					".img.gz",
					".img.bz2",
					".img.Z",
					NULL
				     };
	static char	*flags[] = {
					"--cbf_byte_offset",
					"--cbf_packed_v2",
					"--cbf_packed",
					"--no_compression",
					"--beam_center_from_header",
					"--beam_center_mosflm",
					"--beam_center_ulhc",
					"--beam_center_llhc",
					NULL
				   };

	int		adscimg2cbf_sub(char *header, unsigned short *data, char *cbf_filename, int pack_flags, int beam_center_convention);

	if(argc < 2)
	{
		usage();
		exit(0);
	}

	pack_flags = CBF_BYTE_OFFSET;
	beam_center_convention = BEAM_CENTER_FROM_HEADER;

	while(argc > 1 && argv[1][0] == '-' && argv[1][1] == '-')
	{
		for(j = 0; flags[j] != NULL; j++)
			if(NULL != strstr(argv[1], flags[j]))
				break;
		if(NULL == flags[j])
		{
			fprintf(stderr,"adscimg2cbf: %s is an unknown flag\n\n", argv[1]);
			usage();
			exit(0);
		}
		switch(j)
		{
		  case 0:
			pack_flags = CBF_BYTE_OFFSET;
			break;
		  case 1:
			pack_flags = CBF_PACKED_V2;
			break;
		  case 2:
			pack_flags = CBF_PACKED;
			break;
		  case 3:
			pack_flags = CBF_NONE;
			break;
		  case 4:
			beam_center_convention = BEAM_CENTER_FROM_HEADER;
			break;
		  case 5:
			beam_center_convention = BEAM_CENTER_MOSFLM;
			break;
		  case 6:
			beam_center_convention = BEAM_CENTER_ULHC;
			break;
		  case 7:
			beam_center_convention = BEAM_CENTER_LLHC;
			break;
		}
		if(j < 3)
		{
			if(NULL != strstr(argv[1], ",flat"))
				pack_flags |= CBF_FLAT_IMAGE;
			else
			if(NULL != strstr(argv[1], ",uncorrelated"))
				pack_flags |= CBF_UNCORRELATED_SECTIONS;
		}
		argc--;
		argv++;
	}

	while(argc > 1)
	{
		file_type = 0;
		strcpy(in_filename, argv[1]);
		i = strlen(in_filename);
		for(j = 0; endings[j] != NULL; j++)
		{
			k = strlen(endings[j]);
			if(endswith(in_filename, endings[j]))
			{
				file_type = j;
				break;
			}
		}
		if(NULL == endings[j])
		{
			fprintf(stderr,"adscimg2cbf: Input file name %s does not end in .img, .img.gz, or .img.bz2, or .img.Z\n", in_filename);
			exit(0);
		}
		strcpy(out_filename, in_filename);
		out_filename[i - k] = '\0';
		strcat(out_filename, ".cbf");

		if(0 == file_type)
		{
			if(NULL == (fp = fopen(in_filename, "r")))
			{
				fprintf(stderr, "adscimg2cbf: Cannot open %s as input .img file\n", in_filename);
				exit(0);
			}
		}
		else
		{
			if(2 == file_type)
				sprintf(popen_command, "bzcat %s", in_filename);
			else
				sprintf(popen_command, "zcat %s", in_filename);
			if(NULL == (fp = popen(popen_command, "r")))
			{
				fprintf(stderr, "adscimg2cbf: Cannot exec %s command to uncompress input file\n", popen_command);
				exit(0);
			}
		}

		/*
		 *	Get the first header block.  Can't use seeks on input file.
		 */

		if(NULL == (hptr = malloc(512 * sizeof (char))))
		{
			fprintf(stderr,"adscimg2cbf: cannot allocate memory for first 512 bytes of header of input file %s\n", in_filename);
			exit(0);
		}
		if(512 != fread(hptr, sizeof (char), 512, fp))
		{
			fprintf(stderr, "adscimg2cbf: Cannot read first header block of file %s.\n", in_filename);
			if(0 == file_type)
				fclose(fp);
			else
			{
				status_pclose = pclose(fp);
				if(0 != status_pclose)
				{
					fprintf(stderr, "Status returned from uncompress command via popen NON-ZERO: %d\n", status_pclose);
					perror("popen command (maybe this will be useful)");
					fprintf(stderr, "Program exiting.  This may be evidence of a corrupt compressed file!\n");
					fprintf(stderr, "Filename being uncompressed: %s with command: %s\n", in_filename, popen_command);
					exit(0);
				}
			}
			exit(0);
		}
		for(i = 0; i < 5; i++)
			header_bytes[0 + i] = hptr[15 + i];

		header_bytes[5] = '\0';
		header_size_char = atoi(header_bytes);
		
		if(NULL == (hptr = realloc(hptr, header_size_char)))
		{
			fprintf(stderr,"adscimg2cbf: cannot reallocate memory for %d bytes of header of input file %s\n", 
					header_size_char, in_filename);
			exit(0);
		}

		if(header_size_char > 512)
		{
			if((header_size_char - 512) != fread(hptr + 512, sizeof (char), (header_size_char - 512), fp))
			{
				fprintf(stderr, "adscimg2cbf: Cannot read next %d bytes of header of file %s.\n", 
						header_size_char - 512, in_filename);
				if(0 == file_type)
					fclose(fp);
				else
				{
					status_pclose = pclose(fp);
					if(0 != status_pclose)
					{
						fprintf(stderr, "Status returned from uncompress command via popen NON-ZERO: %d\n", status_pclose);
						perror("popen command (maybe this will be useful)");
						fprintf(stderr, "Program exiting.  This may be evidence of a corrupt compressed file!\n");
						fprintf(stderr, "Filename being uncompressed: %s with command: %s\n", in_filename, popen_command);
						exit(0);
					}
				}
				exit(0);
			}
		}
		field[0] = '\0';
		gethd("SIZE1", field, hptr);
		if('\0' == field[0])
		{
			fprintf(stderr,"adscimg2cbf: keyword SIZE1 not found in header.  Cannot convert file %s\n", in_filename);
			exit(0);
		}
		size1 = atoi(field);
		
		field[0] = '\0';
		gethd("SIZE2", field, hptr);
		if('\0' == field[0])
		{
			fprintf(stderr,"adscimg2cbf: keyword SIZE2 not found in header.  Cannot convert file %s\n", in_filename);
			exit(0);
		}
		size2 = atoi(field);
		
		file_size = header_size_char + size1 * size2 * sizeof(unsigned short);

		if(NULL == (hptr = realloc(hptr, file_size)))
		{
			fprintf(stderr,"adscimg2cbf: cannot reallocate memory (size %d) for input file %s\n", 
						file_size, in_filename);
			exit(0);
		}
		if((file_size - header_size_char) != fread(hptr + header_size_char, sizeof (char), (file_size - header_size_char), fp))
		{
			fprintf(stderr, "adscimg2cbf: Cannot read data (size %d bytes) from input file %s.\n", 
					file_size - header_size_char, in_filename);
			if(0 == file_type)
				fclose(fp);
			else
			{
				status_pclose = pclose(fp);
				if(0 != status_pclose)
				{
					fprintf(stderr, "Status returned from uncompress command via popen NON-ZERO: %d\n", status_pclose);
					perror("popen command (maybe this will be useful)");
					fprintf(stderr, "Program exiting.  This may be evidence of a corrupt compressed file!\n");
					fprintf(stderr, "Filename being uncompressed: %s with command: %s\n", in_filename, popen_command);
					exit(0);
				}
			}
			exit(0);
		}
		if(0 == file_type)
			fclose(fp);
		else
		{
			status_pclose = pclose(fp);
			if(0 != status_pclose)
			{
				fprintf(stderr, "Status returned from uncompress command via popen NON-ZERO: %d\n", status_pclose);
				perror("popen command (maybe this will be useful)");
				fprintf(stderr, "Program exiting.  This may be evidence of a corrupt compressed file!\n");
				fprintf(stderr, "Filename being uncompressed: %s with command: %s\n", in_filename, popen_command);
				exit(0);
			}
		}

		uptr = ((unsigned short *) (hptr + header_size_char));

		cbf_status = adscimg2cbf_sub(hptr, uptr, out_filename, pack_flags, beam_center_convention);
		free(hptr);

		argv++;
		argc--;
	}
	exit(0);
}
