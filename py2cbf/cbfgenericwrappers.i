
// Start of generic functions
%feature("autodoc","1");

/* cfunc cbf_get_local_integer_byte_order   pyfunc get_local_integer_byte_order  
   arg char ** byte_order */

%feature("autodoc", "
Returns : char **bo,int *bolen
*args   : 

C prototype: int cbf_get_local_integer_byte_order (char ** byte_order);

CBFLib documentation:
DESCRIPTION
cbf_get_local_integer_byte_order returns the byte order of integers 
on the machine on which the API is being run in the form of a 
character string returned as the value pointed to by byte_order. 
cbf_get_local_real_byte_order returns the byte order of reals on the 
machine on which the API is being run in the form of a character 
string returned as the value pointed to by byte_order. 
cbf_get_local_real_format returns the format of floats on the machine 
on which the API is being run in the form of a character string 
returned as the value pointed to by real_format. The strings returned 
must not be modified in any way.
The values returned in byte_order may be the strings  \"little_endian 
\" or  \"big-endian \". The values returned in real_format may be the 
strings  \"ieee 754-1985 \" or  \"other \". Additional values may be 
returned by future versions of the API.
ARGUMENTS
byte_order    pointer to the returned string real_format   pointer to 
the returned string
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_local_integer_byte_order;

%cstring_output_allocate_size(char **bo, int *bolen, free(*$1));
  %inline {
  void get_local_integer_byte_order(char **bo, int *bolen) {
        char * byteorder;
        char * bot;
        error_status = cbf_get_local_integer_byte_order(&byteorder);
        *bolen = strlen(byteorder);
        if (!(bot = (char *)malloc(*bolen))) {cbf_failnez(CBF_ALLOC)}
        strncpy(bot,byteorder,*bolen);
        *bo = bot;
  }
  }

/* cfunc cbf_compute_cell_volume   pyfunc compute_cell_volume  
   arg double cell[6]    arg double *volume */

%feature("autodoc", "
Returns : Float volume
*args   : double cell[6]

C prototype: int cbf_compute_cell_volume ( double cell[6], double *volume );

CBFLib documentation:
DESCRIPTION
cbf_compute_cell_volume sets *volume to point to the volume of the 
unit cell computed from the double values in cell[0:2] for the cell 
edge lengths a, b and c in Ångstroms and the double values given in 
cell[3:5] for the cell angles α, β and γ in degrees.
ARGUMENTS
cell     Pointer to the array of 6 doubles giving the cell 
parameters. volume   Pointer to the doubles for cell volume.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")compute_cell_volume;


%apply double *OUTPUT {double *volume};
  %inline {
  void compute_cell_volume(double cell[6], double *volume) {
  cbf_failnez(cbf_compute_cell_volume(cell,volume));
  }
  }

/* cfunc cbf_get_local_real_format   pyfunc get_local_real_format  
   arg char ** real_format */

%feature("autodoc", "
Returns : char **rf,int *rflen
*args   : 

C prototype: int cbf_get_local_real_format (char ** real_format );

CBFLib documentation:
DESCRIPTION
cbf_get_local_integer_byte_order returns the byte order of integers 
on the machine on which the API is being run in the form of a 
character string returned as the value pointed to by byte_order. 
cbf_get_local_real_byte_order returns the byte order of reals on the 
machine on which the API is being run in the form of a character 
string returned as the value pointed to by byte_order. 
cbf_get_local_real_format returns the format of floats on the machine 
on which the API is being run in the form of a character string 
returned as the value pointed to by real_format. The strings returned 
must not be modified in any way.
The values returned in byte_order may be the strings  \"little_endian 
\" or  \"big-endian \". The values returned in real_format may be the 
strings  \"ieee 754-1985 \" or  \"other \". Additional values may be 
returned by future versions of the API.
ARGUMENTS
byte_order    pointer to the returned string real_format   pointer to 
the returned string
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_local_real_format;

%cstring_output_allocate_size(char **rf, int *rflen, free(*$1));
  %inline {
  void get_local_real_format(char **rf, int *rflen) {
        char * real_format;
        char * rft;
        error_status = cbf_get_local_real_format(&real_format);
        *rflen = strlen(real_format);
        if (!(rft = (char *)malloc(*rflen))) {cbf_failnez(CBF_ALLOC)}
        strncpy(rft,real_format,*rflen);
        *rf = rft;
  }
  }

/* cfunc cbf_airy_disk   pyfunc airy_disk  
   arg double x    arg double y    arg double cenx    arg double ceny    arg double      volume    arg double fwhm    arg double * value */

%feature("autodoc", "
Returns : Float value
*args   : double x,double y,double cenx,double ceny,double volume,double fwhm

C prototype: int cbf_airy_disk(double x, double y, double cenx, double ceny,
                 double      volume, double fwhm, double * value);

CBFLib documentation:
DESCRIPTION
cbf_airy_disk sets value to point to the value taken at (x,y) of an 
truncated Airy function approximation to a point-spread function of 
total included volume volume and full width at half max fwhm centered 
on (cenx, ceny).
cbf_airy_disk_volume sets to point to the integral in the box with 
diagonal corners (xlo, ylo) and of (xhi, yhi) of a truncated Airy 
function approximation to a point-spread function of total included 
volume volume and full width at half max fwhm centered on (cenx, 
ceny).
The Airy function used is an 8-digit approximation up to the first 
minimum, after which it is forced to zero, so it cannot be used to 
simulate diffraction rings.
ARGUMENTS
x           the x-coordinate of a point in the real plane y           
the y-coordinate of a point in the real plane xlo         the 
x-coordinate of a point in the real plane marking the left bound of 
integration ylo         the y-coordinate of a point in the real plane 
marking the bottom bound of integration xhi         the x-coordinate 
of a point in the real plane marking the right bound of integration 
yhi         the y-coordinate of a point in the real plane marking the 
top bound of integration cenx        the x-coordinate of a point in 
the real plane marking the PSF center ceny        the y-coordinate of 
a point in the real plane marking the PSF center volume      the 
total volume of the PSF fwhm        the full-width at half max of the 
PSF value       Pointer to the value of the Airy function volumeout   
Pointer to the value of the integral/TR>
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")airy_disk;

%apply double *OUTPUT {double *value};
%inline {
void airy_disk(double x, double y, double cenx, double ceny,
double volume, double fwhm, double *value) {
cbf_failnez(cbf_airy_disk(x,y,cenx,ceny,volume,fwhm,value));
}
}


/* cfunc cbf_get_local_real_byte_order   pyfunc get_local_real_byte_order  
   arg char ** byte_order */

%feature("autodoc", "
Returns : char **bo,int *bolen
*args   : 

C prototype: int cbf_get_local_real_byte_order (char ** byte_order);

CBFLib documentation:
DESCRIPTION
cbf_get_local_integer_byte_order returns the byte order of integers 
on the machine on which the API is being run in the form of a 
character string returned as the value pointed to by byte_order. 
cbf_get_local_real_byte_order returns the byte order of reals on the 
machine on which the API is being run in the form of a character 
string returned as the value pointed to by byte_order. 
cbf_get_local_real_format returns the format of floats on the machine 
on which the API is being run in the form of a character string 
returned as the value pointed to by real_format. The strings returned 
must not be modified in any way.
The values returned in byte_order may be the strings  \"little_endian 
\" or  \"big-endian \". The values returned in real_format may be the 
strings  \"ieee 754-1985 \" or  \"other \". Additional values may be 
returned by future versions of the API.
ARGUMENTS
byte_order    pointer to the returned string real_format   pointer to 
the returned string
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_local_real_byte_order;

%cstring_output_allocate_size(char **bo, int *bolen, free(*$1));
  %inline {
  void get_local_real_byte_order(char **bo, int *bolen) {
        char * byteorder;
        char * bot;
        error_status = cbf_get_local_real_byte_order(&byteorder);
        *bolen = strlen(byteorder);
        if (!(bot = (char *)malloc(*bolen))) {cbf_failnez(CBF_ALLOC)}
        strncpy(bot,byteorder,*bolen);
        *bo = bot;
  }
  }

/* cfunc cbf_compute_reciprocal_cell   pyfunc compute_reciprocal_cell  
   arg double cell[6]    arg double rcell[6] */

%feature("autodoc", "
Returns : Float astar,Float bstar,Float cstar,Float alphastar,Float betastar,
          Float gammastar
*args   : double cell[6]

C prototype: int cbf_compute_reciprocal_cell ( double cell[6],
                 double rcell[6] );

CBFLib documentation:
DESCRIPTION
cbf_compute_reciprocal_cell sets rcell to point to the array of 
reciprocal cell parameters computed from the double values cell[0:2] 
giving the cell edge lengths a, b and c in Ångstroms, and the double 
values cell[3:5] giving the cell angles α, β and γ in degrees. The 
double values rcell[0:2] will be set to the reciprocal cell lengths 
a^*, b^* and c^* in Ångstroms^-1 and the double values rcell[3:5] 
will be set to the reciprocal cell angles α^*, β^* and γ^* in 
degrees.
ARGUMENTS
cell     Pointer to the array of 6 doubles giving the cell 
parameters. rcell    Pointer to the destination array of 6 doubles 
giving the reciprocal cell parameters. volume   Pointer to the 
doubles for cell volume.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")compute_reciprocal_cell;

%apply double *OUTPUT {double *astar, double *bstar, double *cstar,
  double *alphastar, double *betastar, double *gammastar};
  %inline {
  void compute_reciprocal_cell(double cell[6], double *astar, double *bstar, double *cstar,
  double *alphastar, double *betastar, double *gammastar) {
    double rcell[6];
    cbf_failnez(cbf_compute_reciprocal_cell(cell,rcell));
    *astar =      rcell[0];
    *bstar =      rcell[1];
    *cstar =      rcell[2];
    *alphastar =  rcell[3];
    *betastar =   rcell[4];
    *gammastar =  rcell[5];
  }
  }


/* cfunc cbf_airy_disk_volume   pyfunc airy_disk_volume  
   arg double xlo    arg double ylo    arg double xhi    arg double yhi    arg double cenx    arg double ceny    arg double volume    arg double fwhm    arg double *      volumeout */

%feature("autodoc", "
Returns : Float volumeout
*args   : double xlo,double ylo,double xhi,double yhi,double cenx,double ceny,
          double volumein,double fwhm

C prototype: int cbf_airy_disk_volume(double xlo, double ylo, double xhi,
                 double yhi,      double cenx, double ceny, double volume,
                 double fwhm, double *      volumeout);

CBFLib documentation:
DESCRIPTION
cbf_airy_disk sets value to point to the value taken at (x,y) of an 
truncated Airy function approximation to a point-spread function of 
total included volume volume and full width at half max fwhm centered 
on (cenx, ceny).
cbf_airy_disk_volume sets to point to the integral in the box with 
diagonal corners (xlo, ylo) and of (xhi, yhi) of a truncated Airy 
function approximation to a point-spread function of total included 
volume volume and full width at half max fwhm centered on (cenx, 
ceny).
The Airy function used is an 8-digit approximation up to the first 
minimum, after which it is forced to zero, so it cannot be used to 
simulate diffraction rings.
ARGUMENTS
x           the x-coordinate of a point in the real plane y           
the y-coordinate of a point in the real plane xlo         the 
x-coordinate of a point in the real plane marking the left bound of 
integration ylo         the y-coordinate of a point in the real plane 
marking the bottom bound of integration xhi         the x-coordinate 
of a point in the real plane marking the right bound of integration 
yhi         the y-coordinate of a point in the real plane marking the 
top bound of integration cenx        the x-coordinate of a point in 
the real plane marking the PSF center ceny        the y-coordinate of 
a point in the real plane marking the PSF center volume      the 
total volume of the PSF fwhm        the full-width at half max of the 
PSF value       Pointer to the value of the Airy function volumeout   
Pointer to the value of the integral/TR>
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")airy_disk_volume;

%apply double *OUTPUT {double *volumeout};
%inline {
void airy_disk_volume(double xlo, double ylo, double xhi, double yhi,
double cenx, double ceny, double volumein, double fwhm, double * volumeout) {
cbf_failnez(cbf_airy_disk_volume(xlo,ylo,xhi,yhi,cenx,ceny,volumein,fwhm,volumeout));
}
}

// End of generic functions
