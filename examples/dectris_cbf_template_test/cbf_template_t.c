/* test program to output a CBF header from a template */

/* gcc -g -Wall -o cbf_template_t cbf_template_t.c */

/*
**
** GPL license declared below.
**
** Copyright (C) 2010 E. F. Eikenberry, DECTRIS, AG
** All rights reserved.
**
**   This program is free software; you can redistribute it and/or modify
**   it under the terms of the GNU General Public License as published by
**   the Free Software Foundation; either version 2 of the License, or
**   (at your option) any later version.
**
**   This program is distributed in the hope that it will be useful,
**   but WITHOUT ANY WARRANTY; without even the implied warranty of
**   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**   GNU General Public License for more details.
**
**   You should have received a copy of the GNU General Public License
**   along with this program; if not, write to the Free Software
**   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
**
** EF Eikenberry, Jun, 2010
** DECTRIS, Ltd.  Neuenhoferstrasse 107, CH-5400 Baden, Switzerland.
*/



#define MX_PARAMS_MAIN		/* turn on space allocations in mx_parms.h */


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "mx_parms.h"
#include "mx_cbf_t_extras.h"


/*
** dummy functions
*/

int set_current_energy(double a, int b, int c, char *d)
{
return 0;
}

double dcb_get_exposure_period(void)
{
return 0.100;
}




/*****************************************************************************\
**                                                                           **
**    Global variables                                                       **
**                                                                           **
\*****************************************************************************/
#define TEXT_SIZE 30

static float mx_beam_x=(float)IMAGE_NCOL/2.0;
static int mx_beam_x_defined=0;

static float mx_beam_y=(float)IMAGE_NROW/2.0;
static int mx_beam_y_defined=0;

static int mx_n_oscilations=1;
static int mx_n_oscilations_defined=0;

static float mx_wavelength=1.54;
static int mx_wavelength_defined=0;

static float mx_det_distance=1.0;
static int mx_det_distance_defined=0;

static float mx_det_voffset=0.0;
static int mx_det_voffset_defined=0;

static float mx_start_angle=0.0;
static int mx_start_angle_defined=0;

static float mx_angle_increment=0.1;
static int mx_angle_increment_defined=0;

static float mx_det_2theta=0.0;
static int mx_det_2theta_defined=0;

static float mx_polarization=0.99;
static int mx_polarization_defined=0;

static float mx_alpha=0.0;
static int mx_alpha_defined=0;

static float mx_kappa=0.0;
static int mx_kappa_defined=0;

static float mx_phi=0.0;
static int mx_phi_defined=0;

static float mx_phi_increment=0.0;
static int mx_phi_increment_defined=0;

static float mx_chi=0.0;
static int mx_chi_defined=0;

static float mx_chi_increment=0.0;
static int mx_chi_increment_defined=0;

static float mx_flux=0.0;
static int mx_flux_defined=0;

static float mx_filter_tx=1.0;
static int mx_filter_tx_defined=0;

static float mx_e_range_low=0.0;
static int mx_e_range_low_defined=0;

static float mx_e_range_hi=0.0;
static int mx_e_range_hi_defined=0;

static char mx_oscillation_axis[TEXT_SIZE]="X, CW";
static int mx_oscillation_axis_defined=0;

static float mx_start_position=0.0;
static int mx_start_position_defined=0;

static float mx_position_increment=0.0;
static int mx_position_increment_defined=0;

static float mx_shutter_time=0.0;
static int mx_shutter_time_defined=0;

static char cbf_template_file[LENFN]="\0";
static int cbf_template_file_defined=0;


static char msg[80];

/*****************************************************************************\
**                                                                           **
**      End of global variables                                              **
**                                                                           **
\*****************************************************************************/


/* ----- Functions defined in this module ---- */
static MX_PARAMS_CMND find_mx_param_command(char **);


static MX_PARAMS_CMND find_mx_param_command(char **pP)
{
int i, j, n, idx=0;
char *p=*pP;

msg[0]='\0';
while(*p && (isspace(*p) || *p==';' || *p==','))	/* space to next command */
	p++;
if (*p == '\0')			/* end of line? */
	return 0;
for (n=0; *(p+n); n++)	/* count characters in command */
	if (!isalnum(*(p+n)) && *(p+n)!='_')	/* allows '_' */
		break;
j = 0;			/* count matching names */
for (i = 0; i < mx_params_count; i++)
	if ( strncasecmp(p, mx_params_list[i].name, n) == 0 )
		{
		idx = i;		/* permit unambiguous abbreviations */
		j++;
		if ( n == strlen(mx_params_list[i].name) )
			{
			j = 1;		/* to skip next block */
			break;		/* exact match is exempt from ambiguity check */
			}
		}
if (j != 1)
	{
	for (i=0; *(p+i) && !isspace(*(p+i)); i++)
		;
	*(p+i) = '\0';		/* isolate command */
	if (j == 0)
		sprintf(msg, "Command not found: %s", p);
	else
		sprintf(msg, "Command '%s' is ambiguous", p);
	return 0;
	}
*pP += n;		/* skip over command name */
return mx_params_list[idx].cmnd;
}



/*****************************************************************************\
**                                                                           **
**    CBF header from template                                               **
**                                                                           **
\*****************************************************************************/

/* Note that CBF_TXT_HEADER_SIZE must fit within CBF_HEADER_SIZE in cbftvx.c */
#define CBF_TXT_HEADER_SIZE 8000
#define MAX_NODES 30

static char cbf_template[CBF_TXT_HEADER_SIZE];
static int cbf_header_initialized=0;

struct CBF_NODE
		{
		MX_PARAMS_CMND cmnd;
		char *subP;
		char *strP;
		struct CBF_NODE *next;
		struct CBF_NODE *prev;
		} ;
static struct CBF_NODE nodes[MAX_NODES];

int setup_cbf_template(char *msg)
{
int i, len, m, nidx=0, count;
FILE *ifp;
char line[120], *p, *q, *sub=NULL, substr[30], *txt=NULL, *ptxt=NULL;
MX_PARAMS_CMND cmnd;
struct CBF_NODE *tnode, *rnode;

cbf_header_initialized=0;

if (!cbf_template_file[0])		/* no template given - no error */
	return 0;

if ( !(ifp=fopen(cbf_template_file, "r")) )
	{
	sprintf(msg, "Could not open template file for reading:\n  %s",
			cbf_template_file);
	printf("%s\n", msg);
	return -1;
	}

/* read and edit the template
   ensure that lines end in CRLF (a CBF standard) */
cbf_template[0]='\0';
m=0;
while( (fgets(line, sizeof(line), ifp)) )
	{
	/* if we find a mini-header, discard it */
	if(strstr(line, "--CIF-BINARY-FORMAT-SECTION--"))
		break;
	len = strlen(line);
	if (len > -4+sizeof(line))
		{
		fclose(ifp);
		strcpy(msg, "Line too long in CBF template");
		printf("%s\n", msg);
		return -1;
		}
	p = line+strlen(line);
	while (p>=line && (*p=='\n' || *p=='\r' || *p=='\0'))	/* cut off ending */
		p--;
	strcpy(p+1, "\r\n");		/* CRLF is the correct line ending for CBF */
	p = line;
	while(!ptxt && (*p==' ' || *p=='\t'))	/* in case of leading space */
		p++;
	q = cbf_template+m;			/* memorize position */
	m+=sprintf(cbf_template+m, "%s", p);		/* put line in buffer */
	if (txt)					/* start of text has been found */
		continue;
	if (ptxt && line[0]=='_')
		txt = q;				/* confirmed start of text */
	if (!sub && *q=='@')
		sub = q;				/* position of first substitution */
	if (*q=='@' || *q=='#')
		continue;
	if (strstr(q, "--- End of preamble"))
		txt = cbf_template+m;	/* start of next line */
	if (!txt)
		ptxt = q;				/* possible start of text - probably CRLF */
	}
fclose(ifp);

if (!txt)
	{
	sprintf(msg, "*** Error in template file format - text not found\n");
	printf("%s\n", msg);
	return -1;
	}

/* the ending is critical for CBF library parsing
   "\r\n;\r\n;\r\n" is required; we add an extra blank line */
    
while(isspace(*(cbf_template+m)) || *(cbf_template+m)=='\0')
	m--;
m++;
m+=sprintf(cbf_template+m, "\r\n;\r\n;\r\n\r\n");

/*
** (1) Identify each defined program parameter in turn (e.g., 'Timestamp')
** (2) Pick up the associated substitution key (e.g., '_timestamp_')
** (3) Search through the text for 1 or more instances of the key
** (4) Make up a 'node' for each instance giving the key type and the
**     position of the instance in the text
** (5) Insert the node in the doubly-linked list such that traversing the
**     linked-list enumerates the nodes in the order they occur in the text.
**     The nodes are in arbitrary order in the array of nodes; only the
**     link pointers keep track of the order.
** (6) After all instances are found, terminate each text segment ('\0')
** 
** The final arrangement is an initial text segment, followed by the
** array of nodes (instances).  Each instance contains a variable value
** to be printed, followed by a text segment.
*/

memset(nodes, 0, sizeof(nodes));	/* doubly linked list */
nodes[0].subP=nodes[0].strP=txt;	/* start of text */

/* parse the substitutions */
while (sub && *sub=='@')
	{
	q=sub+1;
	while(isspace(*q))
		q++;
	cmnd = find_mx_param_command(&q);
	if (!cmnd)
		{
		sprintf(msg, "*** Error - unrecognized variable name (%s)", q);
		printf("%s\n", msg);
		return -1;
		}
	while(isspace(*q))
		q++;
	p = substr;
	while(!isspace(*q) && *q!='\r' && *q!='\n' && *q!='\0')
		*p++ = *q++;
	*p = '\0';						/* substr == string to search for */
	while(isspace(*q) && *q!='\r')
		q++;
	if(isalnum(*q) || *q=='-')
		{
		switch (cmnd)				/* store initial value(s) */
			{
			case Energy_range:
				mx_e_range_low = atof(q);
				while(*q && (isdigit(*q) || *q=='-' || *q=='.'))	/* skip number */
					q++;
				while(*q && (isspace(*q) || *q==','))	/* skip to next number */
					q++;
				if (*q == '\0')							/* end of line? */
					return 0;
				mx_e_range_hi = atof(q);
				mx_e_range_low_defined=1;
				mx_e_range_hi_defined=1;
				break;
			case Detector_distance:
				mx_det_distance = atof(q);
				mx_det_distance_defined=1;
				break;
			case Detector_Voffset:
				mx_det_voffset = atof(q);
				mx_det_voffset_defined=1;
				break;
			case Beam_xy:
				mx_beam_x = atof(q);
				while(*q && (isdigit(*q) || *q=='-' || *q=='.'))	/* skip number */
					q++;
				while(*q && (isspace(*q) || *q==','))	/* skip to next number */
					q++;
				if (*q == '\0')							/* end of line? */
					break;
				mx_beam_y = atof(q);
				mx_beam_x_defined=1;
				mx_beam_y_defined=1;
				break;
			case Beam_x:
				mx_beam_x = atof(q);
				mx_beam_x_defined=1;
				break;
			case Beam_y:
				mx_beam_y = atof(q);
				mx_beam_y_defined=1;
				break;
			case Flux:
				mx_flux = atof(q);
				mx_flux_defined=1;
				break;
			case Filter_transmission:
				mx_filter_tx = atof(q);
				mx_filter_tx_defined=1;
				break;
			case Start_angle:
				mx_start_angle = atof(q);
				mx_start_angle_defined=1;
				break;
			case Angle_increment:
				mx_angle_increment = atof(q);
				mx_angle_increment_defined=1;
				mx_start_angle_defined=1;			/* defaults to 0 */
				break;
			case Detector_2theta:
				mx_det_2theta = atof(q);
				mx_det_2theta_defined=1;
				break;
			case Polarization:
				mx_polarization = atof(q);
				mx_polarization_defined=1;
				break;
			case Alpha:
				mx_alpha = atof(p);
				mx_alpha_defined=1;
				break;
			case Kappa:
				mx_kappa = atof(q);
				mx_kappa_defined=1;
				break;
			case Phi:
				mx_phi = atof(q);
				mx_phi_defined=1;
				break;
			case Phi_increment:
				mx_phi_increment = atof(q);
				mx_phi_increment_defined=1;
				mx_phi_defined=1;					/* defaults to 0 */
				break;
			case Chi:
				mx_chi = atof(q);
				mx_chi_defined=1;
				break;
			case Chi_increment:
				mx_chi_increment = atof(q);
				mx_chi_increment_defined=1;
				mx_chi_defined=1;					/* defaults to 0 */
				break;
			case Oscillation_axis:
				memset(mx_oscillation_axis, 0, TEXT_SIZE);
				strncat(mx_oscillation_axis, q, TEXT_SIZE-2);
				for (i=0; mx_oscillation_axis[i] && i<TEXT_SIZE-1; i++)
					mx_oscillation_axis[i] = toupper(mx_oscillation_axis[i]);
				mx_oscillation_axis_defined=1;
				break;
			case N_oscillations:
				mx_n_oscilations = atoi(q);
				mx_n_oscilations_defined=1;
				break;
			case Start_position:
				mx_start_position = atof(q);
				mx_start_position_defined=1;
				break;
			case Position_increment:
				mx_position_increment = atof(q);
				mx_position_increment_defined=1;
				mx_start_position_defined=1;		/* defaults to 0 */
				break;
			case Shutter_time:
				mx_shutter_time = atof(q);
				mx_shutter_time_defined=1;
				break;

			case Timestamp:				/* these do not accept an initial value */
			case Wavelength:
			case Exposure_period:
			case Exposure_time:
			case Count_cutoff:
			case Compression_type:
			case CBF_template_file:
			case X_dimension:
			case Y_dimension:
			default:
				break;
			}
		}

	/* find places to substitute variables */
	q = nodes[0].strP;					/* start of text */
	count=0;
	while( (p=strstr(q, substr)) )
		{
		count++;
		if ( (*(p-1)!=' ' && *(p-1)!='-' &&  *(p-1)!='+' && *(p-1)!='\n') || 
				(*(p+strlen(substr))!=' ' && *(p+strlen(substr))!='\r'))
			{
			q = p+strlen(substr);		/* avoid false substring match */
			continue;
			}
		/* walk the node list to find where this string fits */
		rnode = &nodes[0];
		tnode = nodes[0].next;
		while(tnode && tnode->subP<p)
			{
			rnode = tnode;				/* remember node */
			tnode = tnode->next;
			}

		if (nidx >= -1+MAX_NODES)
			{
			strcpy(msg, "Too many substitution nodes in CBF template");
			printf("%s\n", line);
			return -1;
			}

		nidx++;									/* allocate a new node */
		nodes[nidx].cmnd = cmnd;				/* name of variable */
		nodes[nidx].subP = p;					/* pointer to substitution string */
		nodes[nidx].strP = p+strlen(substr);	/* start of next string */
												
		nodes[nidx].next = rnode->next;			/* insert new node in chain */
		nodes[nidx].prev = rnode;
		if (rnode->next)
			rnode->next->prev = &nodes[nidx];
		rnode->next = &nodes[nidx];

		q = p+strlen(substr);
		}
	if ( !count )
		{
		sprintf(msg, "*** Error: key string ( %s ) not found", substr);
		printf("%s\n", msg);
		return -1;
		}

#if 0				/* print the doubly linked list */
	{char t1[20], t2[20];
	for(i=0; i<=nidx; i++)
		{
		strcpy(t1, "                   ");
		if (nodes[i].subP)
			strncpy(t1, nodes[i].subP, 20);
		t1[19]='\0';
		while((q=strchr(t1,'\n')))*q='.';
		while((q=strchr(t1,'\r')))*q='.';
		strcpy(t2, "                   ");
		if (nodes[i].strP)
			strncpy(t2, nodes[i].strP, 20);
		t2[19]='\0';
		while((q=strchr(t2,'\n')))*q='.';
		while((q=strchr(t2,'\r')))*q='.';
		printf("%2d %p %2d %20s %20s %p %p\n",
				i, &nodes[i], (int)nodes[i].cmnd, t1, 
				t2, nodes[i].next, nodes[i].prev);
		}
	strncpy(t1, p, 20);
	t1[19]='\0';
	while((q=strchr(t1,'\n')))*q='.';
	while((q=strchr(t1,'\r')))*q='.';
	printf("       p = %p, text = %s\n", p, t1);
	printf("\n");
	}
#endif

	sub = 1+strchr(sub, '\n');		/* next line */
	}

/* terminate the internal strings */
tnode = nodes[0].next;
while(tnode)
	{
	*(tnode->subP) = '\0';
	tnode = tnode->next;
	}

cbf_header_initialized=1;

return 0;
}




/* print the CBF header from the template into the buffer provided
   returns size */
int print_cbf_header(char *dest, int size, char *conv)
{
struct CBF_NODE *tnode=&nodes[0];
int m=0;

if (!cbf_header_initialized)
	return 0;

m += sprintf(dest+m, "%s", tnode->strP);
tnode = tnode->next;
while(tnode && m<size-TEXT_SIZE)
	{
	switch (tnode->cmnd)
		{
		case Wavelength:
			if(mx_wavelength_defined)
				m += sprintf(dest+m, "%.2f", mx_wavelength);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Energy_range:
			if(mx_e_range_low_defined && mx_e_range_hi_defined)
				m += sprintf(dest+m, "%.0f %.0f", mx_e_range_low, mx_e_range_hi);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Detector_distance:
			if(mx_det_distance_defined)
				m += sprintf(dest+m, "%.3f", mx_det_distance);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Detector_Voffset:
			if(mx_det_voffset_defined)
				m += sprintf(dest+m, "%.3f", mx_det_voffset);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Beam_xy:
			if(mx_beam_x_defined && mx_beam_y_defined)
				m += sprintf(dest+m, "%.2f %.2f", mx_beam_x, mx_beam_y);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Beam_x:
			if(mx_beam_x_defined)
				m += sprintf(dest+m, "%.2f", mx_beam_x);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Beam_y:
			if(mx_beam_y_defined)
				m += sprintf(dest+m, "%.2f", mx_beam_y);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Flux:
			if(mx_flux_defined)
				m += sprintf(dest+m, "%g", mx_flux);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Filter_transmission:
			if(mx_filter_tx_defined)
				m += sprintf(dest+m, "%.4f", mx_filter_tx);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Start_angle:
			if(mx_start_angle_defined)
				m += sprintf(dest+m, "%.4f", mx_start_angle);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Angle_increment:
			if(mx_angle_increment_defined)
				m += sprintf(dest+m, "%.4f", mx_angle_increment);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Detector_2theta:
			if(mx_det_2theta_defined)
				m += sprintf(dest+m, "%.4f", mx_det_2theta);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Polarization:
			if(mx_polarization_defined)
				m += sprintf(dest+m, "%.3f", mx_polarization);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Alpha:
			if(mx_alpha_defined)
				m += sprintf(dest+m, "%.4f", mx_alpha);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Kappa:
			if(mx_kappa_defined)
				m += sprintf(dest+m, "%.4f", mx_kappa);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Phi:
			if(mx_phi_defined)
				m += sprintf(dest+m, "%.4f", mx_phi);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Phi_increment:
			if(mx_phi_increment_defined)
				m += sprintf(dest+m, "%.4f", mx_phi_increment);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Chi:
			if(mx_chi_defined)
				m += sprintf(dest+m, "%.4f", mx_chi);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Chi_increment:
			if(mx_chi_increment_defined)
				m += sprintf(dest+m, "%.4f", mx_chi_increment);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Oscillation_axis:
			if(mx_oscillation_axis_defined)
				m += sprintf(dest+m, "%s", mx_oscillation_axis);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case N_oscillations:
			if(mx_n_oscilations_defined)
				m += sprintf(dest+m, "%d", mx_n_oscilations);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Start_position:
			if(mx_start_position_defined)
				m += sprintf(dest+m, "%.4f", mx_start_position);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Position_increment:
			if(mx_position_increment_defined)
				m += sprintf(dest+m,  "%.4f", mx_position_increment);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Shutter_time:
			if(mx_shutter_time_defined)
				m += sprintf(dest+m,  "%.7f", mx_shutter_time);
			else
				m += sprintf(dest+m, "<not_defined>");
			break;
		case Timestamp:
			m += sprintf(dest+m, "%s", image_timestamp);
			break;
		case Exposure_period:
			m += sprintf(dest+m, "%.6f", dcb_get_exposure_period());
			break;
		case Exposure_time:
			m += sprintf(dest+m, "%.6f", exposure_time);
			break;
		case Count_cutoff:
			m += sprintf(dest+m, "%u", count_cutoff);
			break;
		case Compression_type:
			if (strstr(conv, "x-"))
				conv+=2;
			m += sprintf(dest+m, "%s", conv);
			break;
		case X_dimension:
			m += sprintf(dest+m, "%d", camera_wide);
			break;
		case Y_dimension:
			m += sprintf(dest+m, "%d", camera_high);
			break;

		case CBF_template_file:
		default:
			break;
		}
	m += sprintf(dest+m, "%s", tnode->strP);
	tnode = tnode->next;
	}
if (m >= -1+size-TEXT_SIZE)
	printf("Size exceeded - CBF header not completed\n");

return m;
}


/*************************************************\
**                                               **
**    main program for cbf test -or- demo        **
**                                               **
\*************************************************/


int main (int argc, char *argv[])
{
FILE *ofp;
char msg[500]="\0";
char bufr[10000];
char line[80];

strcpy(cbf_template_file, "cbf_6M_template.cbf");
cbf_template_file_defined=1; 

setup_cbf_template(msg);

if (msg[0])
	{
	printf("%s\n", msg);
	return 0;
	}

print_cbf_header(bufr, sizeof(bufr), "CBF_BYTE_OFFSET");


strcpy(line, "cbf_template_t.out");
if (!(ofp = fopen(line, "w+b")))
	{
	printf("Could not open %s for writing\n", line);
	return 0;
	}


/* The first part of a DECTRIS CBF header in an image file is a comment
   header containing detector settings and mx_settings (these are 
   crystallography parameters supplied by the user that are also reproduced 
   in the CBF syntax below)
 */

fprintf(ofp, 
"###CBF: VERSION 1.5, CBFlib v0.7.8 - SLS/DECTRIS PILATUS detectors\r\n"
"\r\n"
"data_test65\r\n"
"\r\n"
"_array_data.header_convention \"SLS/DECTRIS_1.1\"\r\n"
"_array_data.header_contents\r\n"
";\r\n");

fprintf(ofp,
"# Detector: PILATUS 100K, In-house (m141) Test System\r\n"
"# 2010-Jun-16T19:09:48.271\r\n"
"# Pixel_size 172e-6 m x 172e-6 m\r\n"
"# Silicon sensor, thickness 0.000320 m\r\n"
"# Exposure_time 1.0000000 s\r\n"
"# Exposure_period 1.0000000 s\r\n"
"# Tau = 0 s\r\n"
"# Count_cutoff 1048574 counts\r\n"
"# Threshold_setting 0 eV\r\n"
"# Gain_setting not set (vrf = -0.200)\r\n"
"# N_excluded_pixels = 0\r\n"
"# Excluded_pixels: (nil)\r\n"
"# Flat_field: (nil)\r\n"
"# Trim_file: (nil)\r\n"
"# Image_path: /home/det/p2_det/images/\r\n"
";\r\n"
);

/* now the part made up from the template */

fprintf(ofp, "%s", bufr);

/* now the mini-header and the data */

fprintf(ofp,
"_array_data.data\r\n"
";\r\n"
"--CIF-BINARY-FORMAT-SECTION--\r\n"
"Content-Type: application/octet-stream;\r\n"
"     conversions=\"x-CBF_BYTE_OFFSET\"\r\n"
"Content-Transfer-Encoding: BINARY\r\n"
);
fprintf(ofp,
"X-Binary-Size: 94981\r\n"
"X-Binary-ID: 1\r\n"
"X-Binary-Element-Type: \"signed 32-bit integer\"\r\n"
"X-Binary-Element-Byte-Order: LITTLE_ENDIAN\r\n"
"Content-MD5: VwGHOeEVHfClJWkB5v5Geg==\r\n"
"X-Binary-Number-of-Elements: 94965\r\n"
"X-Binary-Size-Fastest-Dimension: 487\r\n"
"X-Binary-Size-Second-Dimension: 195\r\n"
"X-Binary-Size-Padding: 4095\r\n"
"\r\n"
"--- data comes here ---\r\n"
"--CIF-BINARY-FORMAT-SECTION----\r\n"
";\r\n"
);

fclose(ofp);

printf ("%s was written\n", line);

return 0;
}
