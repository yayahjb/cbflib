#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

/*
 *	Jiffy to take one or more adsc .img files, convert each in turn to
 *	.cbf files, and output the files.  The input file is preserved; the
 *	output file has the same name as the input file with the suffix .img
 *	replaced by .cbf.
 */

int	main(int argc, char *argv[])
{
	FILE		*fp;
	char		in_filename[1024], out_filename[1024];
	char		*hptr;
	unsigned short	*uptr;
	char		header_bytes[6];
	int		file_size, header_size_char, data_size_ushort;
	int		cbf_status;
	int		i;
	int		img2cbf_sub(char *header, unsigned short *data, char *cbf_filename);

	if(argc < 2)
	{
		fprintf(stdout,"Usage: adscimg2cbf file1.img ... filen.img     (creates file1.cbf ... filen.cbf)\n");
		exit(0);
	}

	while(argc > 1)
	{
		strcpy(in_filename, argv[1]);
		i = strlen(in_filename);
		if(4 > i || 0 != strcmp(".img", &in_filename[i - strlen(".img")]))
		{
			fprintf(stderr,"img2cbf: Input file name %s does not end in .img\n", in_filename);
			exit(0);
		}
		strcpy(out_filename, in_filename);
		out_filename[i - 3] = 'c';
		out_filename[i - 2] = 'b';
		out_filename[i - 1] = 'f';
		if(NULL == (fp = fopen(in_filename, "r")))
		{
			fprintf(stderr, "img2cbf: Cannot open %s as input .img file\n", in_filename);
			exit(0);
		}

		fseek(fp, 0L, SEEK_END);
		file_size = ftell(fp);
		fseek(fp, 0L, SEEK_SET);
		
		if(NULL == (hptr = malloc(file_size * sizeof (char))))
		{
			fprintf(stderr,"img2cbf: cannot allocate memory for input file %s\n", in_filename);
			exit(0);
		}
		if(file_size != fread(hptr, sizeof (char), file_size, fp))
		{
			fprintf(stderr, "img2cbf: Cannot read input file %s into memory\n", in_filename);
			exit(0);
		}
		fclose(fp);

		for(i = 0; i < 5; i++)
			header_bytes[0 + i] = hptr[15 + i];

		header_bytes[5] = '\0';
		header_size_char = atoi(header_bytes);
		data_size_ushort = (file_size - header_size_char) / sizeof (unsigned short);
		uptr = ((unsigned short *) (hptr + header_size_char));

		cbf_status = img2cbf_sub(hptr, uptr, out_filename);
		free(hptr);

		argv++;
		argc--;
	}
	exit(0);
}
