
// Tell SWIG not to make constructor for these objects
%nodefault cbf_positioner_struct;
%nodefault cbf_goniometer;
%nodefault cbf_axis_struct;

// Tell SWIG what the object is, so we can build the class
typedef struct
{
  double matrix [3][4];

  cbf_axis_struct *axis;

  size_t axes;

  int matrix_is_valid, axes_are_connected;
}
cbf_positioner_struct;

typedef cbf_positioner_struct *cbf_goniometer;


%feature("autodoc","1");

%extend cbf_positioner_struct{// Tell SWIG to attach functions to the structure

    cbf_positioner_struct(){  // Constructor
       // DO NOT CONSTRUCT WITHOUT A CBFHANDLE
       cbf_failnez(CBF_ARGUMENT);
       return NULL; /* Should never be executed */
       } 

    ~cbf_positioner_struct(){ // Destructor
       cbf_failnez(cbf_free_goniometer(self));
       }
%feature("autodoc", "
Returns : Float start,Float increment
*args   : 

C prototype: int cbf_get_rotation_range (cbf_goniometer goniometer,
                 unsigned int    reserved, double *start, double *increment);

CBFLib documentation:
DESCRIPTION
cbf_get_rotation_range sets *start and *increment to the 
corresponding values of the goniometer rotation axis used for the 
exposure.
Either of the destination pointers may be NULL.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
goniometer   Goniometer handle. reserved     Unused. Any value other 
than 0 is invalid. start        Pointer to the destination start 
value. increment    Pointer to the destination increment value.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_rotation_range;

%apply double *OUTPUT {double *start,double *increment};

    void get_rotation_range(double *start,double *increment){
       unsigned int reserved;
       reserved = 0;
       cbf_failnez(cbf_get_rotation_range (self,reserved, start,increment));
    }
%feature("autodoc", "
Returns : double final1,double final2,double final3
*args   : double ratio,double initial1,double initial2,double initial3

C prototype: int cbf_rotate_vector (cbf_goniometer goniometer,
                 unsigned int    reserved, double ratio, double initial1,
                 double initial2, double    initial3, double *final1,
                 double *final2, double *final3);

CBFLib documentation:
DESCRIPTION
cbf_rotate_vector sets *final1, *final2, and *final3 to the 3 
components of the of the vector (initial1, initial2, initial3) after 
reorientation by applying the goniometer rotations. The value ratio 
specif ies the goniometer setting and varies from 0.0 at the 
beginning of the exposure to 1.0 at the end, irrespective of the 
actual rotation range.
Any of the destination pointers may be NULL.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
goniometer   Goniometer handle. reserved   Unused. Any value other 
than 0 is invalid. ratio   Goniometer setting. 0 = beginning of 
exposure, 1 = end. initial1   x component of the initial vector. 
initial2   y component of the initial vector. initial3   z component 
of the initial vector. vector1   Pointer to the destination x 
component of the final vector. vector2   Pointer to the destination y 
component of the final vector. vector3   Pointer to the destination z 
component of the final vector.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")rotate_vector;


%apply double *OUTPUT {double *final1, double *final2, double *final3};

    void rotate_vector (double ratio, double initial1,double initial2, 
         double initial3, double *final1, double *final2, double *final3){
       unsigned int reserved;
       reserved = 0;
       cbf_failnez(cbf_rotate_vector (self, reserved, ratio, initial1,
         initial2, initial3, final1, final2, final3));
    }
%feature("autodoc", "
Returns : double reciprocal1,double reciprocal2,double reciprocal3
*args   : double ratio,double wavelength,double real1,double real2,double real3

C prototype: int cbf_get_reciprocal (cbf_goniometer goniometer,
                 unsigned int    reserved, double ratio, double wavelength,
                 double real1, double real2,    double real3,
                 double *reciprocal1, double *reciprocal2,
                 double    *reciprocal3);

CBFLib documentation:
DESCRIPTION
cbf_get_reciprocal sets *reciprocal1, * reciprocal2, and * 
reciprocal3 to the 3 components of the of the reciprocal-space vector 
corresponding to the real-space vector (real1, real2, real3). The 
reciprocal-space vector is oriented to correspond to the goniometer 
setting with all axes at 0. The value wavelength is the wavlength in 
Angstrom and the value ratio specifies the current goniometer setting 
and varies from 0.0 at the beginning of the exposur e to 1.0 at the 
end, irrespective of the actual rotation range.
Any of the destination pointers may be NULL.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
goniometer   Goniometer handle. reserved   Unused. Any value other 
than 0 is invalid. ratio   Goniometer setting. 0 = beginning of 
exposure, 1 = end. wavelength   Wavelength in Angstrom. real1   x 
component of the real-space vector. real2   y component of the 
real-space vector. real3   z component of the real-space vector. 
reciprocal1   Pointer to the destination x component of the 
reciprocal-space vector. reciprocal2   Pointer to the destination y 
component of the reciprocal-space vector. reciprocal3   Pointer to 
the destination z component of the reciprocal-space vector.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_reciprocal;

%apply double *OUTPUT {double *reciprocal1,double *reciprocal2, 
              double *reciprocal3};

    void get_reciprocal (double ratio,double wavelength, 
                         double real1, double real2, double real3, 
                         double *reciprocal1,double *reciprocal2, 
                         double *reciprocal3){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_get_reciprocal(self,reserved, ratio, wavelength, 
                         real1, real2, real3,reciprocal1,
                         reciprocal2,reciprocal3));
    }
%feature("autodoc", "
Returns : double vector1,double vector2,double vector3
*args   : 

C prototype: int cbf_get_rotation_axis (cbf_goniometer goniometer,
                 unsigned int    reserved, double *vector1, double *vector2,
                 double vector3);

CBFLib documentation:
DESCRIPTION
cbf_get_rotation_axis sets *vector1, *vector2, and *vector3 to the 3 
components of the goniometer rotation axis used for the exposure.
Any of the destination pointers may be NULL.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
goniometer   Goniometer handle. reserved   Unused. Any value other 
than 0 is invalid. vector1   Pointer to the destination x component 
of the rotation axis. vector2   Pointer to the destination y 
component of the rotation axis. vector3   Pointer to the destination 
z component of the rotation axis.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_rotation_axis;

%apply double *OUTPUT {double *vector1,double *vector2, double *vector3};

void get_rotation_axis (double *vector1, double *vector2, double *vector3){
     unsigned int reserved;
     reserved = 0;
     cbf_failnez(cbf_get_rotation_axis (self, reserved, 
                                        vector1, vector2, vector3));
    }

}; // End of cbf_positioner
