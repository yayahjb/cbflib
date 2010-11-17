
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
Returns : 
*args   : double indexfast,double indexslow,double centerfast,double centerslow

C prototype: int cbf_set_reference_beam_center_fs (cbf_detector detector,
                 double      *indexfast, double *indexslow, double *centerfast,
                 double *centerslow);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *centerfast and *centerslow to the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector and *indexfast and 
*indexslow to the corresponding indices. cbf_set_beam_center sets the 
offsets in the axis category for the detector element axis with 
precedence 1 to place the beam center at the position given in mm by 
*centerfast and *centerslow as the displacements in mm along the 
detector axes from pixel (0, 0) to the point at which the beam 
intersects the detector at the indices given *indexfast and 
*indexslow. cbf_set_reference_beam_center sets the displacments in 
the array_structure_list_axis category to place the beam center at 
the position given in mm by *centerfast and *centerslow as the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector at the indices given 
by *indexfast and *indexslow. In order to achieve consistent results, 
a reference detector should be used for detector to have all axes at 
their reference settings.
Note that the precedence 1 axis is the fastest axis, so that 
*centerfast and *indexfast are the fast axis components of the center 
and *centerslow and *indexslow are the slow axis components of the 
center.
The _fs calls give the displacments in a fast-to-slow order. The 
calls with no suffix and the calls _sf calls give the displacements 
in slow-to-fast order
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
For cbf_set_beam_center if the diffrn_data_frame category exists with 
a row for the corresponding element id, the values will be set for 
_diffrn_data_frame.center_fast and _diffrn_data_frame.center_slow in 
millimetres and the value of _diffrn_data_frame.center_units will be 
set to 'mm'.
For cbf_set_reference_beam_center if the diffrn_detector_element 
category exists with a row for the corresponding element id, the 
values will be set for _diffrn_detector_element.reference_center_fast 
and _diffrn_detector_element.reference_center_slow in millimetres and 
the value of _diffrn_detector_element.reference_units will be set to 
'mm'.
ARGUMENTS
detector     Detector handle. indexfast    Pointer to the destination 
fast index. indexslow    Pointer to the destination slow index. 
centerfast   Pointer to the destination displacement along the fast 
axis. centerslow   Pointer to the destination displacement along the 
slow axis.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_reference_beam_center_fs;

    void set_reference_beam_center_fs(double *indexfast, double *indexslow, 
                         double *centerfast,double *centerslow){
        cbf_failnez(cbf_set_reference_beam_center_fs(self, indexfast, indexslow, 
                                       centerfast, centerslow));
        }
%feature("autodoc", "
Returns : double coordinate1,double coordinate2,double coordinate3
*args   : double indexfast,double indexslow

C prototype: int cbf_get_pixel_coordinates_fs (cbf_detector detector,
                 double      indexfast, double indexslow, double *coordinate1,
                 double *coordinate2,      double *coordinate3);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_coordinates, cbf_get_pixel_coordinates_fs and 
cbf_get_pixel_coordinates_sf ses *coordinate1, *coordinate2, and 
*coordinate3 to the vector position of pixel (indexfast, indexslow) 
on the detector surface. If indexslow and indexfast are integers then 
the coordinates correspond to the center of a pixel.
Any of the destination pointers may be NULL.
ARGUMENTS
detector      Detector handle. indexslow     Slow index. indexfast    
 Fast index. coordinate1   Pointer to the destination x component. 
coordinate2   Pointer to the destination y component. coordinate3   
Pointer to the destination z component.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_coordinates_fs;

%apply double *OUTPUT {double *coordinate1,  
         double *coordinate2, double *coordinate3};
   void get_pixel_coordinates_fs(double indexfast, double indexslow, 
             double *coordinate1,   
             double *coordinate2, 
             double *coordinate3){
      cbf_failnez(cbf_get_pixel_coordinates_fs(self, indexfast, indexslow, coordinate1, coordinate2, coordinate3));
   }
%feature("autodoc", "
Returns : 
*args   : double indexfast,double indexslow,double centerfast,double centerslow

C prototype: int cbf_set_beam_center_fs (cbf_detector detector,
                 double *indexfast,      double *indexslow, double *centerfast,
                 double *centerslow);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *centerfast and *centerslow to the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector and *indexfast and 
*indexslow to the corresponding indices. cbf_set_beam_center sets the 
offsets in the axis category for the detector element axis with 
precedence 1 to place the beam center at the position given in mm by 
*centerfast and *centerslow as the displacements in mm along the 
detector axes from pixel (0, 0) to the point at which the beam 
intersects the detector at the indices given *indexfast and 
*indexslow. cbf_set_reference_beam_center sets the displacments in 
the array_structure_list_axis category to place the beam center at 
the position given in mm by *centerfast and *centerslow as the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector at the indices given 
by *indexfast and *indexslow. In order to achieve consistent results, 
a reference detector should be used for detector to have all axes at 
their reference settings.
Note that the precedence 1 axis is the fastest axis, so that 
*centerfast and *indexfast are the fast axis components of the center 
and *centerslow and *indexslow are the slow axis components of the 
center.
The _fs calls give the displacments in a fast-to-slow order. The 
calls with no suffix and the calls _sf calls give the displacements 
in slow-to-fast order
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
For cbf_set_beam_center if the diffrn_data_frame category exists with 
a row for the corresponding element id, the values will be set for 
_diffrn_data_frame.center_fast and _diffrn_data_frame.center_slow in 
millimetres and the value of _diffrn_data_frame.center_units will be 
set to 'mm'.
For cbf_set_reference_beam_center if the diffrn_detector_element 
category exists with a row for the corresponding element id, the 
values will be set for _diffrn_detector_element.reference_center_fast 
and _diffrn_detector_element.reference_center_slow in millimetres and 
the value of _diffrn_detector_element.reference_units will be set to 
'mm'.
ARGUMENTS
detector     Detector handle. indexfast    Pointer to the destination 
fast index. indexslow    Pointer to the destination slow index. 
centerfast   Pointer to the destination displacement along the fast 
axis. centerslow   Pointer to the destination displacement along the 
slow axis.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_beam_center_fs;

    void set_beam_center_fs(double *indexfast, double *indexslow, 
                         double *centerfast,double *centerslow){
        cbf_failnez(cbf_set_beam_center_fs(self, indexfast, indexslow, 
                                       centerfast, centerslow));
        }
%feature("autodoc", "
Returns : Float pixel size
*args   : Int axis_number

C prototype: int cbf_get_inferred_pixel_size (cbf_detector detector,
                 int axis_number,      double *psize);

CBFLib documentation:
DESCRIPTION
cbf_get_inferred_pixel_size, cbf_get_inferred_pixel_size_sf set 
*psize to point to the double value in millimeters of the pixel size 
for the axis axis_number value. The slow index is treated as axis 1 
and the next faster index is treated as axis 2. 
cbf_get_inferred_pixel_size_fs sets *psize to point to the double 
value in millimeters of the pixel size for the axis axis_number 
value. The fast index is treated as axis 1 and the next slower index 
is treated as axis 2.
If the axis number is negative, the axes are used in the reverse 
order so that an axis_number of -1 indicates the fast axes in a call 
to cbf_get_inferred_pixel_size or cbf_get_inferred_pixel_size_sf and 
indicates the fast axis in a call to cbf_get_inferred_pixel_size_fs.
ARGUMENTS
detector      Detector handle. axis_number   The number of the axis. 
area          Pointer to the destination pizel size in mm.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_inferred_pixel_size;

%apply double *OUTPUT { double *psize } get_inferred_pixel_size;
void get_inferred_pixel_size(unsigned int axis_number, double* psize){
   cbf_failnez(cbf_get_inferred_pixel_size(self, axis_number, psize));
   }
%feature("autodoc", "
Returns : double area,double projected_area
*args   : double index1,double index2

C prototype: int cbf_get_pixel_area (cbf_detector detector, double indexslow,
                 double      indexfast, double *area, double *projected_area);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_area, cbf_get_pixel_area_fs and cbf_get_pixel_area_sf 
set *area to the area of the pixel at (indexfast, indexslow) on the 
detector surface and *projected_area to the apparent area of the 
pixel as viewed from the sample position, with indexslow being the 
slow axis and indexfast being the fast axis.
Either of the destination pointers may be NULL.
ARGUMENTS
detector         Detector handle. indexfast        Fast index. 
indexslow        Slow index. area             Pointer to the 
destination area in mm2. projected_area   Pointer to the destination 
apparent area in mm2.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_area;

%apply double *OUTPUT{double *area,double *projected_area};
    void get_pixel_area(double index1, double index2,
                        double *area,double *projected_area){
       cbf_failnez(cbf_get_pixel_area (self,
                                       index1, index2, area,projected_area));
      }
%feature("autodoc", "
Returns : double normal1,double normal2,double normal3
*args   : double indexfast,double indexslow

C prototype: int cbf_get_pixel_normal_fs (cbf_detector detector,
                 double indexfast,      double indexslow, double *normal1,
                 double *normal2, double *normal3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_normal, cbf_get_pixel_normal_fs and 
cbf_get_pixel_normal_sf set *normal1, *normal2, and *normal3 to the 3 
components of the of the normal vector to the pixel at (indexfast, 
indexslow). The vector is normalized.
Any of the destination pointers may be NULL.
ARGUMENTS
detector    Detector handle. indexslow   Slow index. indexfast   Fast 
index. normal1     Pointer to the destination x component of the 
normal vector. normal2     Pointer to the destination y component of 
the normal vector. normal3     Pointer to the destination z component 
of the normal vector.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_normal_fs;

%apply double *OUTPUT {double *normal1,double *normal2, double *normal3};
   void get_pixel_normal_fs ( double indexfast, double indexslow, 
                          double *normal1,double *normal2, double *normal3){
       cbf_failnez(cbf_get_pixel_normal_fs(self,
                                    indexfast,indexslow,normal1,normal2,normal3));
   }

%feature("autodoc", "
Returns : double slowaxis1,double slowaxis2,double slowaxis3,double fastaxis1,
          double fastaxis2,double fastaxis3
*args   : 

C prototype: int cbf_get_detector_axes (cbf_detector detector,
                 double *slowaxis1,      double *slowaxis2, double *slowaxis3,
                 double *fastaxis1, double      *fastaxis2, double *fastaxis3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_axis_slow sets *slowaxis1, *slowaxis2, and 
*slowaxis3 to the 3 components of the slow axis of the specified 
detector at the current settings of all axes. 
cbf_get_detector_axis_slow sets *fastaxis1, *fastaxis2, and 
*fastaxis3 to the 3 components of the fast axis of the specified 
detector at the current settings of all axes. cbf_get_detector_axes, 
cbf_get_detector_axes_fs and int cbf_get_detector_axes_sf set 
*slowaxis1, *slowaxis2, and *slowaxis3 to the 3 components of the 
slow axis and *fastaxis1, *fastaxis2, and *fastaxis3 to the 3 
components of the fast axis of the specified detector at the current 
settings of all axes.
Any of the destination pointers may be NULL.
ARGUMENTS
detector    Detector handle. slowaxis1   Pointer to the destination x 
component of the slow axis vector. slowaxis2   Pointer to the 
destination y component of the slow axis vector. slowaxis3   Pointer 
to the destination z component of the slow axis vector. fastaxis1   
Pointer to the destination x component of the fast axis vector. 
fastaxis2   Pointer to the destination y component of the fast axis 
vector. fastaxis3   Pointer to the destination z component of the 
fast axis vector.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_detector_axes;

%apply double *OUTPUT {double *slowaxis1, double *slowaxis2, double *slowaxis3,
                       double *fastaxis1, double *fastaxis2, double *fastaxis3};
   void get_detector_axes ( double *slowaxis1, double *slowaxis2, double *slowaxis3,
                            double *fastaxis1, double *fastaxis2, double *fastaxis3){
       cbf_failnez(cbf_get_detector_axes(self,
                                    slowaxis1,slowaxis2,slowaxis3,
                                    fastaxis1,fastaxis2,fastaxis3));
   }

%feature("autodoc", "
Returns : 
*args   : double indexslow,double indexfast,double centerslow,double centerfast

C prototype: int cbf_set_reference_beam_center (cbf_detector detector,
                 double      *indexslow, double *indexfast, double *centerslow,
                 double *centerfast);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *centerfast and *centerslow to the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector and *indexfast and 
*indexslow to the corresponding indices. cbf_set_beam_center sets the 
offsets in the axis category for the detector element axis with 
precedence 1 to place the beam center at the position given in mm by 
*centerfast and *centerslow as the displacements in mm along the 
detector axes from pixel (0, 0) to the point at which the beam 
intersects the detector at the indices given *indexfast and 
*indexslow. cbf_set_reference_beam_center sets the displacments in 
the array_structure_list_axis category to place the beam center at 
the position given in mm by *centerfast and *centerslow as the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector at the indices given 
by *indexfast and *indexslow. In order to achieve consistent results, 
a reference detector should be used for detector to have all axes at 
their reference settings.
Note that the precedence 1 axis is the fastest axis, so that 
*centerfast and *indexfast are the fast axis components of the center 
and *centerslow and *indexslow are the slow axis components of the 
center.
The _fs calls give the displacments in a fast-to-slow order. The 
calls with no suffix and the calls _sf calls give the displacements 
in slow-to-fast order
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
For cbf_set_beam_center if the diffrn_data_frame category exists with 
a row for the corresponding element id, the values will be set for 
_diffrn_data_frame.center_fast and _diffrn_data_frame.center_slow in 
millimetres and the value of _diffrn_data_frame.center_units will be 
set to 'mm'.
For cbf_set_reference_beam_center if the diffrn_detector_element 
category exists with a row for the corresponding element id, the 
values will be set for _diffrn_detector_element.reference_center_fast 
and _diffrn_detector_element.reference_center_slow in millimetres and 
the value of _diffrn_detector_element.reference_units will be set to 
'mm'.
ARGUMENTS
detector     Detector handle. indexfast    Pointer to the destination 
fast index. indexslow    Pointer to the destination slow index. 
centerfast   Pointer to the destination displacement along the fast 
axis. centerslow   Pointer to the destination displacement along the 
slow axis.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_reference_beam_center;

    void set_reference_beam_center(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_set_reference_beam_center(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
%feature("autodoc", "
Returns : double slowaxis1,double slowaxis2,double slowaxis3
*args   : 

C prototype: int cbf_get_detector_axis_slow (cbf_detector detector,
                 double      *slowaxis1, double *slowaxis2, double *slowaxis3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_axis_slow sets *slowaxis1, *slowaxis2, and 
*slowaxis3 to the 3 components of the slow axis of the specified 
detector at the current settings of all axes. 
cbf_get_detector_axis_slow sets *fastaxis1, *fastaxis2, and 
*fastaxis3 to the 3 components of the fast axis of the specified 
detector at the current settings of all axes. cbf_get_detector_axes, 
cbf_get_detector_axes_fs and int cbf_get_detector_axes_sf set 
*slowaxis1, *slowaxis2, and *slowaxis3 to the 3 components of the 
slow axis and *fastaxis1, *fastaxis2, and *fastaxis3 to the 3 
components of the fast axis of the specified detector at the current 
settings of all axes.
Any of the destination pointers may be NULL.
ARGUMENTS
detector    Detector handle. slowaxis1   Pointer to the destination x 
component of the slow axis vector. slowaxis2   Pointer to the 
destination y component of the slow axis vector. slowaxis3   Pointer 
to the destination z component of the slow axis vector. fastaxis1   
Pointer to the destination x component of the fast axis vector. 
fastaxis2   Pointer to the destination y component of the fast axis 
vector. fastaxis3   Pointer to the destination z component of the 
fast axis vector.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_detector_axis_slow;

%apply double *OUTPUT {double *slowaxis1, double *slowaxis2, double *slowaxis3};
   void get_detector_axis_slow ( double *slowaxis1, double *slowaxis2, double *slowaxis3){
       cbf_failnez(cbf_get_detector_axis_slow(self,
                                    slowaxis1,slowaxis2,slowaxis3));
   }

%feature("autodoc", "
Returns : double distance
*args   : 

C prototype: int cbf_get_detector_distance (cbf_detector detector,
                 double *distance);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_distance sets *distance to the nearest distance from 
the sample position to the detector plane.
ARGUMENTS
detector   Detector handle. distance   Pointer to the destination 
distance.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_detector_distance;

%apply double *OUTPUT {double *distance};
 void get_detector_distance (double *distance){
  cbf_failnez(cbf_get_detector_distance(self,distance));
  }
%feature("autodoc", "
Returns : Float pixel size
*args   : Int axis_number

C prototype: int cbf_get_inferred_pixel_size_fs(cbf_detector detector,
                 int      axis_number, double *psize);

CBFLib documentation:
DESCRIPTION
cbf_get_inferred_pixel_size, cbf_get_inferred_pixel_size_sf set 
*psize to point to the double value in millimeters of the pixel size 
for the axis axis_number value. The slow index is treated as axis 1 
and the next faster index is treated as axis 2. 
cbf_get_inferred_pixel_size_fs sets *psize to point to the double 
value in millimeters of the pixel size for the axis axis_number 
value. The fast index is treated as axis 1 and the next slower index 
is treated as axis 2.
If the axis number is negative, the axes are used in the reverse 
order so that an axis_number of -1 indicates the fast axes in a call 
to cbf_get_inferred_pixel_size or cbf_get_inferred_pixel_size_sf and 
indicates the fast axis in a call to cbf_get_inferred_pixel_size_fs.
ARGUMENTS
detector      Detector handle. axis_number   The number of the axis. 
area          Pointer to the destination pizel size in mm.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_inferred_pixel_size_fs;

%apply double *OUTPUT { double *psize } get_inferred_pixel_size;
void get_inferred_pixel_size_fs(unsigned int axis_number, double* psize){
   cbf_failnez(cbf_get_inferred_pixel_size_fs(self, axis_number, psize));
   }
%feature("autodoc", "
Returns : double normal1,double normal2,double normal3
*args   : 

C prototype: int cbf_get_detector_normal (cbf_detector detector,
                 double *normal1,      double *normal2, double *normal3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_normal sets *normal1, *normal2, and *normal3 to the 
3 components of the of the normal vector to the detector plane. The 
vector is normalized.
Any of the destination pointers may be NULL.
ARGUMENTS
detector   Detector handle. normal1    Pointer to the destination x 
component of the normal vector. normal2    Pointer to the destination 
y component of the normal vector. normal3    Pointer to the 
destination z component of the normal vector.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_detector_normal;

%apply double *OUTPUT {double *normal1, double *normal2, double *normal3};
   void get_detector_normal(double *normal1, 
                            double *normal2,
                            double *normal3){
     cbf_failnez(cbf_get_detector_normal(self,
                    normal1, normal2, normal3));
   }
%feature("autodoc", "
Returns : double fastaxis1,double fastaxis2,double fastaxis3
*args   : 

C prototype: int cbf_get_detector_axis_fast (cbf_detector detector,
                 double      *fastaxis1, double *fastaxis2, double *fastaxis3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_axis_slow sets *slowaxis1, *slowaxis2, and 
*slowaxis3 to the 3 components of the slow axis of the specified 
detector at the current settings of all axes. 
cbf_get_detector_axis_slow sets *fastaxis1, *fastaxis2, and 
*fastaxis3 to the 3 components of the fast axis of the specified 
detector at the current settings of all axes. cbf_get_detector_axes, 
cbf_get_detector_axes_fs and int cbf_get_detector_axes_sf set 
*slowaxis1, *slowaxis2, and *slowaxis3 to the 3 components of the 
slow axis and *fastaxis1, *fastaxis2, and *fastaxis3 to the 3 
components of the fast axis of the specified detector at the current 
settings of all axes.
Any of the destination pointers may be NULL.
ARGUMENTS
detector    Detector handle. slowaxis1   Pointer to the destination x 
component of the slow axis vector. slowaxis2   Pointer to the 
destination y component of the slow axis vector. slowaxis3   Pointer 
to the destination z component of the slow axis vector. fastaxis1   
Pointer to the destination x component of the fast axis vector. 
fastaxis2   Pointer to the destination y component of the fast axis 
vector. fastaxis3   Pointer to the destination z component of the 
fast axis vector.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_detector_axis_fast;

%apply double *OUTPUT {double *fastaxis1, double *fastaxis2, double *fastaxis3};
   void get_detector_axis_fast ( double *fastaxis1, double *fastaxis2, double *fastaxis3){
       cbf_failnez(cbf_get_detector_axis_fast(self,
                                    fastaxis1,fastaxis2,fastaxis3));
   }

%feature("autodoc", "
Returns : double fastaxis1,double fastaxis2,double fastaxis3,double slowaxis1,
          double slowaxis2,double slowaxis3
*args   : 

C prototype: int cbf_get_detector_axes_fs (cbf_detector detector,
                 double *fastaxis1,      double *fastaxis2, double *fastaxis3,
                 double *slowaxis1, double      *slowaxis2, double *slowaxis3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_axis_slow sets *slowaxis1, *slowaxis2, and 
*slowaxis3 to the 3 components of the slow axis of the specified 
detector at the current settings of all axes. 
cbf_get_detector_axis_slow sets *fastaxis1, *fastaxis2, and 
*fastaxis3 to the 3 components of the fast axis of the specified 
detector at the current settings of all axes. cbf_get_detector_axes, 
cbf_get_detector_axes_fs and int cbf_get_detector_axes_sf set 
*slowaxis1, *slowaxis2, and *slowaxis3 to the 3 components of the 
slow axis and *fastaxis1, *fastaxis2, and *fastaxis3 to the 3 
components of the fast axis of the specified detector at the current 
settings of all axes.
Any of the destination pointers may be NULL.
ARGUMENTS
detector    Detector handle. slowaxis1   Pointer to the destination x 
component of the slow axis vector. slowaxis2   Pointer to the 
destination y component of the slow axis vector. slowaxis3   Pointer 
to the destination z component of the slow axis vector. fastaxis1   
Pointer to the destination x component of the fast axis vector. 
fastaxis2   Pointer to the destination y component of the fast axis 
vector. fastaxis3   Pointer to the destination z component of the 
fast axis vector.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_detector_axes;

%apply double *OUTPUT {double *slowaxis1, double *slowaxis2, double *slowaxis3,
                       double *fastaxis1, double *fastaxis2, double *fastaxis3};
   void get_detector_axes_fs ( double *fastaxis1, double *fastaxis2, double *fastaxis3,
                               double *slowaxis1, double *slowaxis2, double *slowaxis3){
       cbf_failnez(cbf_get_detector_axes(self,
                                    slowaxis1,slowaxis2,slowaxis3,
                                    fastaxis1,fastaxis2,fastaxis3));
   }

%feature("autodoc", "
Returns : double slowaxis1,double slowaxis2,double slowaxis3,double fastaxis1,
          double fastaxis2,double fastaxis3
*args   : 

C prototype: int cbf_get_detector_axes_sf (cbf_detector detector,
                 double *slowaxis1,      double *slowaxis2, double *slowaxis3,
                 double *fastaxis1, double      *fastaxis2, double *fastaxis3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_axis_slow sets *slowaxis1, *slowaxis2, and 
*slowaxis3 to the 3 components of the slow axis of the specified 
detector at the current settings of all axes. 
cbf_get_detector_axis_slow sets *fastaxis1, *fastaxis2, and 
*fastaxis3 to the 3 components of the fast axis of the specified 
detector at the current settings of all axes. cbf_get_detector_axes, 
cbf_get_detector_axes_fs and int cbf_get_detector_axes_sf set 
*slowaxis1, *slowaxis2, and *slowaxis3 to the 3 components of the 
slow axis and *fastaxis1, *fastaxis2, and *fastaxis3 to the 3 
components of the fast axis of the specified detector at the current 
settings of all axes.
Any of the destination pointers may be NULL.
ARGUMENTS
detector    Detector handle. slowaxis1   Pointer to the destination x 
component of the slow axis vector. slowaxis2   Pointer to the 
destination y component of the slow axis vector. slowaxis3   Pointer 
to the destination z component of the slow axis vector. fastaxis1   
Pointer to the destination x component of the fast axis vector. 
fastaxis2   Pointer to the destination y component of the fast axis 
vector. fastaxis3   Pointer to the destination z component of the 
fast axis vector.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_detector_axes_sf;

%apply double *OUTPUT {double *slowaxis1, double *slowaxis2, double *slowaxis3,
                       double *fastaxis1, double *fastaxis2, double *fastaxis3};
   void get_detector_axes_sf ( double *slowaxis1, double *slowaxis2, double *slowaxis3,
                            double *fastaxis1, double *fastaxis2, double *fastaxis3){
       cbf_failnez(cbf_get_detector_axes(self,
                                    slowaxis1,slowaxis2,slowaxis3,
                                    fastaxis1,fastaxis2,fastaxis3));
   }

%feature("autodoc", "
Returns : double coordinate1,double coordinate2,double coordinate3
*args   : double indexslow,double indexfast

C prototype: int cbf_get_pixel_coordinates_sf (cbf_detector detector,
                 double      indexslow, double indexfast, double *coordinate1,
                 double *coordinate2,      double *coordinate3);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_coordinates, cbf_get_pixel_coordinates_fs and 
cbf_get_pixel_coordinates_sf ses *coordinate1, *coordinate2, and 
*coordinate3 to the vector position of pixel (indexfast, indexslow) 
on the detector surface. If indexslow and indexfast are integers then 
the coordinates correspond to the center of a pixel.
Any of the destination pointers may be NULL.
ARGUMENTS
detector      Detector handle. indexslow     Slow index. indexfast    
 Fast index. coordinate1   Pointer to the destination x component. 
coordinate2   Pointer to the destination y component. coordinate3   
Pointer to the destination z component.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_coordinates_sf;

%apply double *OUTPUT {double *coordinate1,  
         double *coordinate2, double *coordinate3};
   void get_pixel_coordinates_sf(double indexslow, double indexfast, 
             double *coordinate1,   
             double *coordinate2, 
             double *coordinate3){
      cbf_failnez(cbf_get_pixel_coordinates_sf(self, indexslow, indexfast, coordinate1, coordinate2, coordinate3));
   }
%feature("autodoc", "
Returns : 
*args   : double indexslow,double indexfast,double centerslow,double centerfast

C prototype: int cbf_set_beam_center (cbf_detector detector,
                 double *indexslow,      double *indexfast, double *centerslow,
                 double *centerfast);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *centerfast and *centerslow to the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector and *indexfast and 
*indexslow to the corresponding indices. cbf_set_beam_center sets the 
offsets in the axis category for the detector element axis with 
precedence 1 to place the beam center at the position given in mm by 
*centerfast and *centerslow as the displacements in mm along the 
detector axes from pixel (0, 0) to the point at which the beam 
intersects the detector at the indices given *indexfast and 
*indexslow. cbf_set_reference_beam_center sets the displacments in 
the array_structure_list_axis category to place the beam center at 
the position given in mm by *centerfast and *centerslow as the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector at the indices given 
by *indexfast and *indexslow. In order to achieve consistent results, 
a reference detector should be used for detector to have all axes at 
their reference settings.
Note that the precedence 1 axis is the fastest axis, so that 
*centerfast and *indexfast are the fast axis components of the center 
and *centerslow and *indexslow are the slow axis components of the 
center.
The _fs calls give the displacments in a fast-to-slow order. The 
calls with no suffix and the calls _sf calls give the displacements 
in slow-to-fast order
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
For cbf_set_beam_center if the diffrn_data_frame category exists with 
a row for the corresponding element id, the values will be set for 
_diffrn_data_frame.center_fast and _diffrn_data_frame.center_slow in 
millimetres and the value of _diffrn_data_frame.center_units will be 
set to 'mm'.
For cbf_set_reference_beam_center if the diffrn_detector_element 
category exists with a row for the corresponding element id, the 
values will be set for _diffrn_detector_element.reference_center_fast 
and _diffrn_detector_element.reference_center_slow in millimetres and 
the value of _diffrn_detector_element.reference_units will be set to 
'mm'.
ARGUMENTS
detector     Detector handle. indexfast    Pointer to the destination 
fast index. indexslow    Pointer to the destination slow index. 
centerfast   Pointer to the destination displacement along the fast 
axis. centerslow   Pointer to the destination displacement along the 
slow axis.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_beam_center;

    void set_beam_center(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_set_beam_center(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
%feature("autodoc", "
Returns : double area,double projected_area
*args   : double indexfast,double indexslow

C prototype: int cbf_get_pixel_area_fs(cbf_detector detector,
                 double indexfast,      double indexslow, double *area,
                 double *projected_area);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_area, cbf_get_pixel_area_fs and cbf_get_pixel_area_sf 
set *area to the area of the pixel at (indexfast, indexslow) on the 
detector surface and *projected_area to the apparent area of the 
pixel as viewed from the sample position, with indexslow being the 
slow axis and indexfast being the fast axis.
Either of the destination pointers may be NULL.
ARGUMENTS
detector         Detector handle. indexfast        Fast index. 
indexslow        Slow index. area             Pointer to the 
destination area in mm2. projected_area   Pointer to the destination 
apparent area in mm2.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_area_fs;

%apply double *OUTPUT{double *area,double *projected_area};
    void get_pixel_area_fs(double indexfast, double indexslow,
                        double *area,double *projected_area){
       cbf_failnez(cbf_get_pixel_area_fs (self,
                                       indexfast, indexslow, area,projected_area));
      }
%feature("autodoc", "
Returns : double indexfast,double indexslow,double centerfast,double centerslow
*args   : 

C prototype: int cbf_get_beam_center_fs (cbf_detector detector,
                 double *indexfast,      double *indexslow, double *centerfast,
                 double *centerslow);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *centerfast and *centerslow to the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector and *indexfast and 
*indexslow to the corresponding indices. cbf_set_beam_center sets the 
offsets in the axis category for the detector element axis with 
precedence 1 to place the beam center at the position given in mm by 
*centerfast and *centerslow as the displacements in mm along the 
detector axes from pixel (0, 0) to the point at which the beam 
intersects the detector at the indices given *indexfast and 
*indexslow. cbf_set_reference_beam_center sets the displacments in 
the array_structure_list_axis category to place the beam center at 
the position given in mm by *centerfast and *centerslow as the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector at the indices given 
by *indexfast and *indexslow. In order to achieve consistent results, 
a reference detector should be used for detector to have all axes at 
their reference settings.
Note that the precedence 1 axis is the fastest axis, so that 
*centerfast and *indexfast are the fast axis components of the center 
and *centerslow and *indexslow are the slow axis components of the 
center.
The _fs calls give the displacments in a fast-to-slow order. The 
calls with no suffix and the calls _sf calls give the displacements 
in slow-to-fast order
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
For cbf_set_beam_center if the diffrn_data_frame category exists with 
a row for the corresponding element id, the values will be set for 
_diffrn_data_frame.center_fast and _diffrn_data_frame.center_slow in 
millimetres and the value of _diffrn_data_frame.center_units will be 
set to 'mm'.
For cbf_set_reference_beam_center if the diffrn_detector_element 
category exists with a row for the corresponding element id, the 
values will be set for _diffrn_detector_element.reference_center_fast 
and _diffrn_detector_element.reference_center_slow in millimetres and 
the value of _diffrn_detector_element.reference_units will be set to 
'mm'.
ARGUMENTS
detector     Detector handle. indexfast    Pointer to the destination 
fast index. indexslow    Pointer to the destination slow index. 
centerfast   Pointer to the destination displacement along the fast 
axis. centerslow   Pointer to the destination displacement along the 
slow axis.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_beam_center_fs;

%apply double *OUTPUT {double *indexfast, double *indexslow, 
 double *centerfast,double *centerslow};
    void get_beam_center_fs(double *indexfast, double *indexslow, 
                         double *centerfast,double *centerslow){
        cbf_failnez(cbf_get_beam_center_fs(self, indexfast, indexslow, 
                                       centerfast, centerslow));
        }
%feature("autodoc", "
Returns : Float pixel size
*args   : Int axis_number

C prototype: int cbf_get_inferred_pixel_size_sf(cbf_detector detector,
                 int      axis_number, double *psize);

CBFLib documentation:
DESCRIPTION
cbf_get_inferred_pixel_size, cbf_get_inferred_pixel_size_sf set 
*psize to point to the double value in millimeters of the pixel size 
for the axis axis_number value. The slow index is treated as axis 1 
and the next faster index is treated as axis 2. 
cbf_get_inferred_pixel_size_fs sets *psize to point to the double 
value in millimeters of the pixel size for the axis axis_number 
value. The fast index is treated as axis 1 and the next slower index 
is treated as axis 2.
If the axis number is negative, the axes are used in the reverse 
order so that an axis_number of -1 indicates the fast axes in a call 
to cbf_get_inferred_pixel_size or cbf_get_inferred_pixel_size_sf and 
indicates the fast axis in a call to cbf_get_inferred_pixel_size_fs.
ARGUMENTS
detector      Detector handle. axis_number   The number of the axis. 
area          Pointer to the destination pizel size in mm.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_inferred_pixel_size_sf;

%apply double *OUTPUT { double *psize } get_inferred_pixel_size;
void get_inferred_pixel_size_sf(unsigned int axis_number, double* psize){
   cbf_failnez(cbf_get_inferred_pixel_size_sf(self, axis_number, psize));
   }
%feature("autodoc", "
Returns : double coordinate1,double coordinate2,double coordinate3
*args   : double index1,double index2

C prototype: int cbf_get_pixel_coordinates (cbf_detector detector,
                 double indexslow,      double indexfast, double *coordinate1,
                 double *coordinate2, double      *coordinate3);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_coordinates, cbf_get_pixel_coordinates_fs and 
cbf_get_pixel_coordinates_sf ses *coordinate1, *coordinate2, and 
*coordinate3 to the vector position of pixel (indexfast, indexslow) 
on the detector surface. If indexslow and indexfast are integers then 
the coordinates correspond to the center of a pixel.
Any of the destination pointers may be NULL.
ARGUMENTS
detector      Detector handle. indexslow     Slow index. indexfast    
 Fast index. coordinate1   Pointer to the destination x component. 
coordinate2   Pointer to the destination y component. coordinate3   
Pointer to the destination z component.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_coordinates;

%apply double *OUTPUT {double *coordinate1,  
         double *coordinate2, double *coordinate3};
   void get_pixel_coordinates(double index1, double index2, 
             double *coordinate1,   
             double *coordinate2, 
             double *coordinate3){
      cbf_failnez(cbf_get_pixel_coordinates(self, index1, index2,
             coordinate1, coordinate2, coordinate3));
   }
%feature("autodoc", "
Returns : double indexslow,double indexfast,double centerslow,double centerfast
*args   : 

C prototype: int cbf_get_beam_center_sf (cbf_detector detector,
                 double *indexslow,      double *indexfast, double *centerslow,
                 double *centerfast);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *centerfast and *centerslow to the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector and *indexfast and 
*indexslow to the corresponding indices. cbf_set_beam_center sets the 
offsets in the axis category for the detector element axis with 
precedence 1 to place the beam center at the position given in mm by 
*centerfast and *centerslow as the displacements in mm along the 
detector axes from pixel (0, 0) to the point at which the beam 
intersects the detector at the indices given *indexfast and 
*indexslow. cbf_set_reference_beam_center sets the displacments in 
the array_structure_list_axis category to place the beam center at 
the position given in mm by *centerfast and *centerslow as the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector at the indices given 
by *indexfast and *indexslow. In order to achieve consistent results, 
a reference detector should be used for detector to have all axes at 
their reference settings.
Note that the precedence 1 axis is the fastest axis, so that 
*centerfast and *indexfast are the fast axis components of the center 
and *centerslow and *indexslow are the slow axis components of the 
center.
The _fs calls give the displacments in a fast-to-slow order. The 
calls with no suffix and the calls _sf calls give the displacements 
in slow-to-fast order
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
For cbf_set_beam_center if the diffrn_data_frame category exists with 
a row for the corresponding element id, the values will be set for 
_diffrn_data_frame.center_fast and _diffrn_data_frame.center_slow in 
millimetres and the value of _diffrn_data_frame.center_units will be 
set to 'mm'.
For cbf_set_reference_beam_center if the diffrn_detector_element 
category exists with a row for the corresponding element id, the 
values will be set for _diffrn_detector_element.reference_center_fast 
and _diffrn_detector_element.reference_center_slow in millimetres and 
the value of _diffrn_detector_element.reference_units will be set to 
'mm'.
ARGUMENTS
detector     Detector handle. indexfast    Pointer to the destination 
fast index. indexslow    Pointer to the destination slow index. 
centerfast   Pointer to the destination displacement along the fast 
axis. centerslow   Pointer to the destination displacement along the 
slow axis.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_beam_center_sf;

%apply double *OUTPUT {double *indexslow, double *indexfast, 
 double *centerslow,double *centerfast};
    void get_beam_center_sf(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_get_beam_center_sf(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
%feature("autodoc", "
Returns : double area,double projected_area
*args   : double indexslow,double indexfast

C prototype: int cbf_get_pixel_area_sf(cbf_detector detector,
                 double indexslow,      double indexfast, double *area,
                 double *projected_area);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_area, cbf_get_pixel_area_fs and cbf_get_pixel_area_sf 
set *area to the area of the pixel at (indexfast, indexslow) on the 
detector surface and *projected_area to the apparent area of the 
pixel as viewed from the sample position, with indexslow being the 
slow axis and indexfast being the fast axis.
Either of the destination pointers may be NULL.
ARGUMENTS
detector         Detector handle. indexfast        Fast index. 
indexslow        Slow index. area             Pointer to the 
destination area in mm2. projected_area   Pointer to the destination 
apparent area in mm2.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_area_sf;

%apply double *OUTPUT{double *area,double *projected_area};
    void get_pixel_area_sf(double indexslow, double indexfast,
                        double *area,double *projected_area){
       cbf_failnez(cbf_get_pixel_area_sf (self,
                                       indexslow, indexfast, area,projected_area));
      }
%feature("autodoc", "
Returns : double index1,double index2,double center1,double center2
*args   : 

C prototype: int cbf_get_beam_center (cbf_detector detector,
                 double *indexslow,      double *indexfast, double *centerslow,
                 double *centerfast);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *centerfast and *centerslow to the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector and *indexfast and 
*indexslow to the corresponding indices. cbf_set_beam_center sets the 
offsets in the axis category for the detector element axis with 
precedence 1 to place the beam center at the position given in mm by 
*centerfast and *centerslow as the displacements in mm along the 
detector axes from pixel (0, 0) to the point at which the beam 
intersects the detector at the indices given *indexfast and 
*indexslow. cbf_set_reference_beam_center sets the displacments in 
the array_structure_list_axis category to place the beam center at 
the position given in mm by *centerfast and *centerslow as the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector at the indices given 
by *indexfast and *indexslow. In order to achieve consistent results, 
a reference detector should be used for detector to have all axes at 
their reference settings.
Note that the precedence 1 axis is the fastest axis, so that 
*centerfast and *indexfast are the fast axis components of the center 
and *centerslow and *indexslow are the slow axis components of the 
center.
The _fs calls give the displacments in a fast-to-slow order. The 
calls with no suffix and the calls _sf calls give the displacements 
in slow-to-fast order
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
For cbf_set_beam_center if the diffrn_data_frame category exists with 
a row for the corresponding element id, the values will be set for 
_diffrn_data_frame.center_fast and _diffrn_data_frame.center_slow in 
millimetres and the value of _diffrn_data_frame.center_units will be 
set to 'mm'.
For cbf_set_reference_beam_center if the diffrn_detector_element 
category exists with a row for the corresponding element id, the 
values will be set for _diffrn_detector_element.reference_center_fast 
and _diffrn_detector_element.reference_center_slow in millimetres and 
the value of _diffrn_detector_element.reference_units will be set to 
'mm'.
ARGUMENTS
detector     Detector handle. indexfast    Pointer to the destination 
fast index. indexslow    Pointer to the destination slow index. 
centerfast   Pointer to the destination displacement along the fast 
axis. centerslow   Pointer to the destination displacement along the 
slow axis.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_beam_center;

%apply double *OUTPUT {double *index1, double *index2, 
 double *center1,double *center2};
    void get_beam_center(double *index1, double *index2, 
                         double *center1,double *center2){
        cbf_failnez(cbf_get_beam_center(self, index1, index2, 
                                       center1, center2));
        }
%feature("autodoc", "
Returns : 
*args   : double indexslow,double indexfast,double centerslow,double centerfast

C prototype: int cbf_set_reference_beam_center_sf (cbf_detector detector,
                 double      *indexslow, double *indexfast, double *centerslow,
                 double *centerfast);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *centerfast and *centerslow to the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector and *indexfast and 
*indexslow to the corresponding indices. cbf_set_beam_center sets the 
offsets in the axis category for the detector element axis with 
precedence 1 to place the beam center at the position given in mm by 
*centerfast and *centerslow as the displacements in mm along the 
detector axes from pixel (0, 0) to the point at which the beam 
intersects the detector at the indices given *indexfast and 
*indexslow. cbf_set_reference_beam_center sets the displacments in 
the array_structure_list_axis category to place the beam center at 
the position given in mm by *centerfast and *centerslow as the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector at the indices given 
by *indexfast and *indexslow. In order to achieve consistent results, 
a reference detector should be used for detector to have all axes at 
their reference settings.
Note that the precedence 1 axis is the fastest axis, so that 
*centerfast and *indexfast are the fast axis components of the center 
and *centerslow and *indexslow are the slow axis components of the 
center.
The _fs calls give the displacments in a fast-to-slow order. The 
calls with no suffix and the calls _sf calls give the displacements 
in slow-to-fast order
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
For cbf_set_beam_center if the diffrn_data_frame category exists with 
a row for the corresponding element id, the values will be set for 
_diffrn_data_frame.center_fast and _diffrn_data_frame.center_slow in 
millimetres and the value of _diffrn_data_frame.center_units will be 
set to 'mm'.
For cbf_set_reference_beam_center if the diffrn_detector_element 
category exists with a row for the corresponding element id, the 
values will be set for _diffrn_detector_element.reference_center_fast 
and _diffrn_detector_element.reference_center_slow in millimetres and 
the value of _diffrn_detector_element.reference_units will be set to 
'mm'.
ARGUMENTS
detector     Detector handle. indexfast    Pointer to the destination 
fast index. indexslow    Pointer to the destination slow index. 
centerfast   Pointer to the destination displacement along the fast 
axis. centerslow   Pointer to the destination displacement along the 
slow axis.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_reference_beam_center_sf;

    void set_reference_beam_center_sf(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_set_reference_beam_center_sf(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
%feature("autodoc", "
Returns : 
*args   : double indexslow,double indexfast,double centerslow,double centerfast

C prototype: int cbf_set_beam_center_sf (cbf_detector detector,
                 double *indexslow,      double *indexfast, double *centerslow,
                 double *centerfast);

CBFLib documentation:
DESCRIPTION
cbf_get_beam_center sets *centerfast and *centerslow to the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector and *indexfast and 
*indexslow to the corresponding indices. cbf_set_beam_center sets the 
offsets in the axis category for the detector element axis with 
precedence 1 to place the beam center at the position given in mm by 
*centerfast and *centerslow as the displacements in mm along the 
detector axes from pixel (0, 0) to the point at which the beam 
intersects the detector at the indices given *indexfast and 
*indexslow. cbf_set_reference_beam_center sets the displacments in 
the array_structure_list_axis category to place the beam center at 
the position given in mm by *centerfast and *centerslow as the 
displacements in mm along the detector axes from pixel (0, 0) to the 
point at which the beam intersects the detector at the indices given 
by *indexfast and *indexslow. In order to achieve consistent results, 
a reference detector should be used for detector to have all axes at 
their reference settings.
Note that the precedence 1 axis is the fastest axis, so that 
*centerfast and *indexfast are the fast axis components of the center 
and *centerslow and *indexslow are the slow axis components of the 
center.
The _fs calls give the displacments in a fast-to-slow order. The 
calls with no suffix and the calls _sf calls give the displacements 
in slow-to-fast order
Any of the destination pointers may be NULL for getting the beam 
center. For setting the beam axis, either the indices of the center 
must not be NULL.
The indices are non-negative for beam centers within the detector 
surface, but the center for an axis with a negative increment will be 
negative for a beam center within the detector surface.
For cbf_set_beam_center if the diffrn_data_frame category exists with 
a row for the corresponding element id, the values will be set for 
_diffrn_data_frame.center_fast and _diffrn_data_frame.center_slow in 
millimetres and the value of _diffrn_data_frame.center_units will be 
set to 'mm'.
For cbf_set_reference_beam_center if the diffrn_detector_element 
category exists with a row for the corresponding element id, the 
values will be set for _diffrn_detector_element.reference_center_fast 
and _diffrn_detector_element.reference_center_slow in millimetres and 
the value of _diffrn_detector_element.reference_units will be set to 
'mm'.
ARGUMENTS
detector     Detector handle. indexfast    Pointer to the destination 
fast index. indexslow    Pointer to the destination slow index. 
centerfast   Pointer to the destination displacement along the fast 
axis. centerslow   Pointer to the destination displacement along the 
slow axis.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_beam_center_sf;

    void set_beam_center_sf(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_set_beam_center_sf(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
%feature("autodoc", "
Returns : double normal1,double normal2,double normal3
*args   : double index1,double index2

C prototype: int cbf_get_pixel_normal (cbf_detector detector,
                 double indexslow,      double indexfast, double *normal1,
                 double *normal2, double *normal3);

CBFLib documentation:
DESCRIPTION
cbf_get_detector_normal, cbf_get_pixel_normal_fs and 
cbf_get_pixel_normal_sf set *normal1, *normal2, and *normal3 to the 3 
components of the of the normal vector to the pixel at (indexfast, 
indexslow). The vector is normalized.
Any of the destination pointers may be NULL.
ARGUMENTS
detector    Detector handle. indexslow   Slow index. indexfast   Fast 
index. normal1     Pointer to the destination x component of the 
normal vector. normal2     Pointer to the destination y component of 
the normal vector. normal3     Pointer to the destination z component 
of the normal vector.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_normal;

%apply double *OUTPUT {double *normal1,double *normal2, double *normal3};
   void get_pixel_normal ( double index1, double index2, 
                          double *normal1,double *normal2, double *normal3){
       cbf_failnez(cbf_get_pixel_normal(self,
                                    index1,index2,normal1,normal2,normal3));
   }


}; // End of cbf_detector
