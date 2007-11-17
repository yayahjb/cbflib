// Start of generic functions

/* cfunc cbf_get_local_integer_byte_order   pyfunc get_local_integer_byte_order  
   arg char ** byte_order */

%feature("autodoc", "
Returns : string
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
The values returned in byte_order may be the strings 
\"little_endian\" or \"big-endian\". The values returned in 
real_format may be the strings \"ieee 754-1985\" or \"other\". 
Additional values may be returned by future versions of the API.
ARGUMENTS
byte_order    pointer to the returned string real_format   pointer to 
the returned string
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_local_integer_byte_order;
%inline %{
   char* get_local_integer_byte_order(void);
   char* get_local_integer_byte_order(void){
      char *r;
      error_status = cbf_get_local_integer_byte_order(&r);
      return r; }
%}
char* get_local_integer_byte_order(void);
/* cfunc cbf_compute_cell_volume   pyfunc compute_cell_volume  
   arg double cell[6]    arg double *volume */


/* cfunc cbf_get_local_real_format   pyfunc get_local_real_format  
   arg char ** real_format */

%feature("autodoc", "
Returns : string
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
The values returned in byte_order may be the strings 
\"little_endian\" or \"big-endian\". The values returned in 
real_format may be the strings \"ieee 754-1985\" or \"other\". 
Additional values may be returned by future versions of the API.
ARGUMENTS
byte_order    pointer to the returned string real_format   pointer to 
the returned string
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_local_real_format;
%inline %{
   char* get_local_real_format(void);
   char* get_local_real_format(void){
      char *r;
      error_status = cbf_get_local_real_format(&r);
      return r; }
%}
char* get_local_real_format(void);
/* cfunc cbf_get_local_real_byte_order   pyfunc get_local_real_byte_order  
   arg char ** byte_order */

%feature("autodoc", "
Returns : string
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
The values returned in byte_order may be the strings 
\"little_endian\" or \"big-endian\". The values returned in 
real_format may be the strings \"ieee 754-1985\" or \"other\". 
Additional values may be returned by future versions of the API.
ARGUMENTS
byte_order    pointer to the returned string real_format   pointer to 
the returned string
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_local_real_byte_order;
%inline %{
   char* get_local_real_byte_order(void);
   char* get_local_real_byte_order(void){
      char *r;
      error_status = cbf_get_local_real_byte_order(&r);
      return r; }
%}
char* get_local_real_byte_order(void);
/* cfunc cbf_compute_reciprocal_cell   pyfunc compute_reciprocal_cell  
   arg double cell[6]    arg double rcell[6] */

// End of generic functions
