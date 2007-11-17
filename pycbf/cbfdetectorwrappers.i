
// Tell SWIG not to make constructor for these objects
%nodefault cbf_detector_struct;
%nodefault cbf_detector;

// Tell SWIG what the object is, so we can build the class
typedef struct
{
  cbf_positioner positioner;

  double displacement [2], increment [2];

  size_t axes, index [2];
}
cbf_detector_struct;

typedef cbf_detector_struct *cbf_detector;

%feature("autodoc","1");

%extend cbf_detector_struct{// Tell SWIG to attach functions to the structure

    cbf_detector_struct(){  // Constructor
       // DO NOT CONSTRUCT WITHOUT A CBFHANDLE
       cbf_failnez(CBF_ARGUMENT);
       return NULL; /* Should never be executed */
       } 

    ~cbf_detector_struct(){ // Destructor
       cbf_failnez(cbf_free_detector(self));
       }
%feature("autodoc", "
Returns : double normal1,double normal2,double normal3
*args   : double index1,double index2

C prototype: int cbf_get_pixel_normal (cbf_detector detector, double index1,
                 double    index2, double *normal1, double *normal2,
                 double *normal3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_normal sets *normal1, *normal2, and *normal3 to the 
3 components of the of the normal vector to the pixel at (index1, 
index2). The vector is normalized.
Any of the destination pointers may be NULL.
ARGUMENTS
detector   Detector handle. index1   Slow index. index2   Fast index. 
normal1   Pointer to the destination x component of the normal 
vector. normal2   Pointer to the destination y component of the 
normal vector. normal3   Pointer to the destination z component of 
the normal vector.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_pixel_normal;

%apply double *OUTPUT {double *normal1,double *normal2, double *normal3};
   void get_pixel_normal ( double index1, double index2, 
                          double *normal1,double *normal2, double *normal3){
       cbf_failnez(cbf_get_pixel_normal(self,
                                    index1,index2,normal1,normal2,normal3));
   }

%feature("autodoc", "
Returns : double area,double projected_area
*args   : double index1,double index2

C prototype: int cbf_get_pixel_area (cbf_detector detector, double index1,
                 double    index2, double *area, double *projected_area);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_area sets *area to the area of the pixel at (index1, 
index2) on the detector surface and *projected_area to the apparent 
area of the pixel as viewed from the sample position.
Either of the destination pointers may be NULL.
ARGUMENTS
detector         Detector handle. index1           Slow index. index2 
          Fast index. area             Pointer to the destination 
area in mm2. projected_area   Pointer to the destination apparent 
area in mm2.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_pixel_area;

%apply double *OUTPUT{double *area,double *projected_area};
    void get_pixel_area(double index1, double index2,
                        double *area,double *projected_area){
       cbf_failnez(cbf_get_pixel_area (self,
                                       index1, index2, area,projected_area));
      }
%feature("autodoc", "
Returns : double distance
*args   : 

C prototype: int cbf_get_detector_distance (cbf_detector detector,
                 double    *distance);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_distance sets *distance to the nearest distance from 
the sample position to the detector plane.
ARGUMENTS
detector   Detector handle. distance   Pointer to the destination 
distance.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_detector_distance;

%apply double *OUTPUT {double *distance};
 void get_detector_distance (double *distance){
  cbf_failnez(cbf_get_detector_distance(self,distance));
  }
%feature("autodoc", "
Returns : double normal1,double normal2,double normal3
*args   : 

C prototype: int cbf_get_detector_normal (cbf_detector detector,
                 double *normal1,    double *normal2, double *normal3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_normal sets *normal1, *normal2, and *normal3 to the 
3 components of the of the normal vector to the detector plane. The 
vector is normalized.
Any of the destination pointers may be NULL.
ARGUMENTS
detector   Detector handle. normal1   Pointer to the destination x 
component of the normal vector. normal2   Pointer to the destination 
y component of the normal vector. normal3   Pointer to the 
destination z component of the normal vector.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_detector_normal;

%apply double *OUTPUT {double *normal1, double *normal2, double *normal3};
   void get_detector_normal(double *normal1, 
                            double *normal2,
                            double *normal3){
     cbf_failnez(cbf_get_detector_normal(self,
                    normal1, normal2, normal3));
   }
%feature("autodoc", "
Returns : Float pixel size
*args   : Int axis_number

C prototype: int cbf_get_inferred_pixel_size (cbf_detector detector,
                 unsigned int    axis_number, double *psize);

CBFLib documentation:
DESCRIPTION
cbf_get_inferred_pixel_size sets *psize to point to the double value 
in millimeters of the pixel size for the axis axis_number value for 
pixel at (index1, index2) on the detector surface. The slow index is 
treated as axis 1 and the fast index is treated as axis 2.
ARGUMENTS
detector      Detector handle. axis_number   The number of the axis. 
area          Pointer to the destination pizel size in mm.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_inferred_pixel_size;

%apply double *OUTPUT { double *psize } get_inferred_pixel_size;
void get_inferred_pixel_size(unsigned int axis_number, double* psize){
   cbf_failnez(cbf_get_inferred_pixel_size(self, axis_number, psize));
   }
%feature("autodoc", "
Returns : double coordinate1,double coordinate2,double coordinate3
*args   : double index1,double index2

C prototype: int cbf_get_pixel_coordinates (cbf_detector detector,
                 double index1,    double index2, double *coordinate1,
                 double *coordinate2, double    *coordinate3);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_coordinates sets *coordinate1, *coordinate2, and 
*coordinate3 to the vector position of pixel (index1, index2) on the 
detector surface. If index1 and index2 are integers then the 
coordinates correspond to the center of a pixel.
Any of the destination pointers may be NULL.
ARGUMENTS
detector      Detector handle. index1        Slow index. index2       
 Fast index. coordinate1   Pointer to the destination x component. 
coordinate2   Pointer to the destination y component. coordinate3   
Pointer to the destination z component.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_pixel_coordinates;

%apply double *OUTPUT {double *coordinate1,  
         double *coordinate2, double *coordinate3};
   void get_pixel_coordinates(double index1, double index2, 
             double *coordinate1,   
             double *coordinate2, 
             double *coordinate3){
      cbf_failnez(cbf_get_pixel_coordinates(self,index1,index2,
             coordinate1,coordinate2,coordinate3));
   }
%feature("autodoc", "
Returns : double index1,double index2,double center1,double center2
*args   : 

C prototype: int cbf_get_beam_center (cbf_detector detector, double *index1,
                 double    *index2, double *center1, double *center2);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *center1 and *center2 to the displacements 
in mm along the detector axes from pixel (0, 0) to the point at which 
the beam intersects the detector and *index1 and *index2 to the 
corresponding indices. cbf_set_beam_center sets the offsets in the 
axis category for the detector element axis with precedence 1 to 
place the beam center at the position given in mm by *center1 and 
*center2 as the displacements in mm along the detector axes from 
pixel (0, 0) to the point at which the beam intersects the detector 
at the indices given *index1 and *index2.
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
ARGUMENTS
detector   Detector handle. index1   Pointer to the destination slow 
index. index2   Pointer to the destination fast index. center1   
Pointer to the destination displacement along the slow axis. center2  
 Pointer to the destination displacement along the fast axis.
RETURN VALUE
Returns an error code on failure or 0 for success. 
_________________________________________________________________
")get_beam_center;

%apply double *OUTPUT {double *index1, double *index2, 
 double *center1,double *center2};
    void get_beam_center(double *index1, double *index2, 
                         double *center1,double *center2){
        cbf_failnez(cbf_get_beam_center(self, index1, index2, 
                                       center1, center2));
        }

}; // End of cbf_detector
