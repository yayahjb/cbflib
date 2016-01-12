//
//  cbf_standardize_numbers.c
//  
//
//  Created by Herbert J. Bernstein on 12/31/15.
//
//

#include "cbf_standardize_numbers.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <string.h>


void usage ( void ) {
    
    fprintf(stderr,
            "cbf_standardize_numbers:  Usage: \n"
            "  cbf_standardize_numbers file [digits] \n"
            "  or cbf_standardize_numbers - [digits} for use as a filter for 6 digits");
    
    return;
    
    
}

int main (int argc, char ** argv) {
    
    FILE *file;
    char format[10];
    size_t lineln, ii;
    char * line;
    char c;
    char bracket;
    char termc;
    int numcomp;
    int spacestarted;
    int digits = 6;
    double number, anumber, ascale, fpart;
    
    if (argc > 3 || argc < 2) {
        usage();
        return 1;
    }
    
    file = stdin;
    if (strcmp(argv[1],"-") && ! (file = fopen(argv[1],"r"))) {
        fprintf (stderr,"Couldn't open the input file %s\n", argv[1]);
        exit (1);
    }
    
    if (argc > 2) digits = atoi(argv[2]);
    sprintf(format,"%%.%dg ",(digits>0)?digits+1:0);
    
    ascale = pow(10.,digits);
    
    while (!feof(file) && !ferror(file) && (line=fgetln(file,&lineln))) {
        bracket = '\0';
        termc = '\0';
        spacestarted = 1;
        numcomp = 0;
        while(lineln) {
            c = *line;
            if (isspace(c) || c == '[' || c == '(' || c == '{') {
                bracket = c;
                lineln--;
                line++;
                if (!spacestarted) putchar(' ');
                switch (c) {
                    case '[': termc = ']'; putchar (c); putchar(' '); spacestarted = 1; numcomp = 0; break;
                    case '{': termc = '}'; putchar (c); putchar(' '); spacestarted = 1; numcomp = 0; break;
                    case '(': termc = ')'; putchar (c); putchar(' '); spacestarted = 1; numcomp = 0; break;
                    default: spacestarted = 1; break;
                }
                continue;
            }
            if (isdigit(c) || c == '.' || c == '-' || c == '+') {
                char text[lineln+1];
                char * endptr;
                for (ii=0; ii < lineln; ii++) text[ii] = line[ii];
                text[lineln] = '\0';
                if (!spacestarted) putchar(' ');
                number = strtod(text,&endptr)*ascale;
                anumber = fabs(number);
                if (anumber < 0.5) number = 0.;
                
                if (number < 0.) fpart=modf(number-0.5,&number);
                if (number > 0.) fpart=modf(number+0.5,&number);
                
                number /= ascale;
                
                if (termc && numcomp > 0) fprintf(stdout,", ");
                if (endptr == NULL || endptr != text) {
                    fprintf(stdout,format,number);
                    if (endptr == NULL) {
                        lineln=0;
                    } else {
                        line += (endptr-text);
                        lineln -= (endptr-text);
                        spacestarted = 1;
                    }
                    numcomp++;
                } else {
                    fprintf(stdout,". ");
                    line ++;
                    lineln --;
                    spacestarted = 1;
                    numcomp++;
                }
                continue;
            }
            if (c == ',' || (termc && c == termc)) {
                if (!spacestarted) putchar(' ');
                putchar (c);
                putchar (' ');
                numcomp = 0;
                line ++;
                lineln --;
                spacestarted = 1;
                if (termc && c == termc) termc = '\0';
                continue;
            }
            putchar(c);
            line ++;
            lineln --;
            
        }
        putchar('\n');
        
    }
    return 0;
    
}
