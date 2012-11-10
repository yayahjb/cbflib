
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
edge lengths a, b and c in AAngstroms and the double values given in 
cell[3:5] for the cell angles a, b and g in degrees.
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
giving the cell edge lengths a, b and c in AAngstroms, and the double 
values cell[3:5] giving the cell angles a, b and g in degrees. The 
double values rcell[0:2] will be set to the reciprocal cell lengths 
a*, b* and c* in AAngstroms-1 and the double values rcell[3:5] will 
be set to the reciprocal cell angles a*, b* and g* in degrees.
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

// End of generic functions
