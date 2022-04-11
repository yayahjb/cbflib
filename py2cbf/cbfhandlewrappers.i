
// Tell SWIG not to make constructor for these objects
%nodefault cbf_handle;
%nodefault cbf_handle_struct;
%nodefault cbf_node;

// A couple of blockitem functions return CBF_NODETYPE
typedef enum
{
  CBF_UNDEFNODE,        /* Undefined */
  CBF_LINK,             /* Link      */
  CBF_ROOT,             /* Root      */
  CBF_DATABLOCK,        /* Datablock */
  CBF_SAVEFRAME,        /* Saveframe */
  CBF_CATEGORY,         /* Category  */
  CBF_COLUMN            /* Column    */
}
CBF_NODETYPE;


// Tell SWIG what the object is, so we can build the class

typedef struct
{
  cbf_node *node;

  int row, search_row;
}  cbf_handle_struct;

typedef cbf_handle_struct *cbf_handle;

typedef cbf_handle_struct handle;
%feature("autodoc","1");

%extend cbf_handle_struct{   // Tell SWIG to attach functions to the structure

    cbf_handle_struct(){  // Constructor
       cbf_handle handle;
       cbf_failnez(cbf_make_handle(&handle));
       return handle;
       } 

    ~cbf_handle_struct(){ // Destructor
       cbf_failnez(cbf_free_handle(self));
       }

/* cfunc cbf_select_datablock   pyfunc select_datablock  
   arg cbf_handle handle    arg unsigned int datablock */

%feature("autodoc", "
Returns : 
*args   : Integer

C prototype: int cbf_select_datablock (cbf_handle handle,
                 unsigned int datablock);

CBFLib documentation:
DESCRIPTION
cbf_select_datablock selects data block number datablock as the 
current data block.
The first data block is number 0.
If the data block does not exist, the function returns CBF_NOTFOUND.
ARGUMENTS
handle      CBF handle. datablock   Number of the data block to 
select.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")select_datablock;
    void select_datablock(unsigned int arg){
      cbf_failnez(cbf_select_datablock(self,arg));}

/* cfunc cbf_force_new_datablock   pyfunc force_new_datablock  
   arg cbf_handle handle    arg const char    *datablockname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_force_new_datablock (cbf_handle handle,
                 const char    *datablockname);

CBFLib documentation:
DESCRIPTION
cbf_force_new_datablock creates a new data block with name 
datablockname and makes it the current data block. Duplicate data 
block names are allowed. cbf_force_new_saveframe creates a new savew 
frame with name saveframename and makes it the current save frame. 
Duplicate save frame names are allowed.
Even if a save frame with this name already exists, a new save frame 
is created and becomes the current save frame.
ARGUMENTS
handle          CBF handle. datablockname   The name of the new data 
block. saveframename   The name of the new save frame.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")force_new_datablock;
    void force_new_datablock(const char* arg){
      cbf_failnez(cbf_force_new_datablock(self,arg));}
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int elsign,int ndimfast,int ndimmid,
          int ndimslow

C prototype: int cbf_get_3d_image_fs (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 void *array, size_t elsize, int elsign,      size_t ndimfast,
                 size_t ndimmid, size_t ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_3d_image_fs_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_3d_image_fs_as_string;

// Get the length correct

    void get_3d_image_fs_as_string(int element_number, char **s, int *slen,
    int elsize, int elsign, int ndimfast, int ndimmid, int ndimslow){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimmid*ndimslow))) {
               cbf_failnez (cbf_get_3d_image_fs(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize, elsign,
               (size_t) ndimfast, (size_t)ndimmid, (size_t)ndimslow));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimmid*ndimslow;
        *s = (char *) array;
      }

/* cfunc cbf_reset_datablocks   pyfunc reset_datablocks  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_reset_datablocks (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_reset_datablocks deletes all categories from all data blocks.
The current data block does not change.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")reset_datablocks;
    void reset_datablocks(void){
      cbf_failnez(cbf_reset_datablocks(self));}
%feature("autodoc", "
Returns : 
*args   : String tagname,String categoryname_in

C prototype: int cbf_set_tag_category (cbf_handle handle, const char* tagname,
                 const      char* categoryname_in);

CBFLib documentation:
DESCRIPTION
cbf_find_tag_category sets categoryname to the category associated 
with tagname in the dictionary associated with handle. 
cbf_set_tag_category upddates the dictionary associated with handle 
to indicated that tagname is in category categoryname_in.
ARGUMENTS
handle            CBF handle. tagname           tag name. 
categoryname      pointer to a returned category name. 
categoryname_in   input category name.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_tag_category;

   void set_tag_category(const char *tagname, const char* categoryname_in){
     cbf_failnez(cbf_set_tag_category(self,tagname, categoryname_in));
     }
%feature("autodoc", "
Returns : String tagroot
*args   : String tagname

C prototype: int cbf_require_tag_root (cbf_handle handle, const char* tagname,
                 const      char** tagroot);

CBFLib documentation:
DESCRIPTION
cbf_find_tag_root sets *tagroot to the root tag of which tagname is 
an alias. cbf_set_tag_root sets tagname as an alias of tagroot_in in 
the dictionary associated with handle, creating the dictionary if 
necessary. cbf_require_tag_root sets *tagroot to the root tag of 
which tagname is an alias, if there is one, or to the value of 
tagname, if tagname is not an alias.
A returned tagroot string must not be modified in any way.
ARGUMENTS
handle       CBF handle. tagname      tag name which may be an alias. 
tagroot      pointer to a returned tag root name. tagroot_in   input 
tag root name.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")require_tag_root;

const char* require_tag_root(const char* tagname){
 const char* result;
 cbf_failnez(cbf_require_tag_root(self,tagname,&result));
 return result;
 }

/* cfunc cbf_row_number   pyfunc row_number  
   arg cbf_handle handle    arg unsigned int *row */

%feature("autodoc", "
Returns : Integer
*args   : 

C prototype: int cbf_row_number (cbf_handle handle, unsigned int *row);

CBFLib documentation:
DESCRIPTION
cbf_row_number sets *row to the number of the current row of the 
current category.
ARGUMENTS
handle   CBF handle. row      Pointer to the destination row number.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")row_number;
    unsigned int row_number(void){
      unsigned int result;
      cbf_failnez(cbf_row_number(self,&result));
      return result;}
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int elsign,int dimslow,int dimfast

C prototype: int cbf_set_image (cbf_handle handle, unsigned int reserved,
                 unsigned      int element_number, unsigned int compression,
                 void *array, size_t      elsize, int elsign, size_t ndimslow,
                 size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_image;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_image;

    void set_image(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int elsign, int ndimslow, int ndimfast){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_image (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, elsign, (size_t) ndimslow, (size_t)ndimfast)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : 
*args   : Integer element_number,Float slowbinsize_in,Float fastbinsize_in

C prototype: int cbf_set_bin_sizes(cbf_handle handle,
                 unsigned int element_number,      double slowbinsize_in,
                double fastbinsize_in);

CBFLib documentation:
DESCRIPTION
cbf_get_bin_sizes sets slowbinsize to point to the value of the 
number of pixels composing one array element in the dimension that 
changes at the second-fastest rate and fastbinsize to point to the 
value of the number of pixels composing one array element in the 
dimension that changes at the fastest rate for the dectector element 
with the ordinal element_number. cbf_set_bin_sizes sets the the pixel 
bin sizes in the  \"array_intensities \" category to the values of 
slowbinsize_in for the number of pixels composing one array element 
in the dimension that changes at the second-fastest rate and 
fastbinsize_in for the number of pixels composing one array element 
in the dimension that changes at the fastest rate for the dectector 
element with the ordinal element_number.
In order to allow for software binning involving fractions of pixels, 
the bin sizes are doubles rather than ints.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. slowbinsize      Pointer to the 
returned number of pixels composing one array element in the 
dimension that changes at the second-fastest rate. fastbinsize      
Pointer to the returned number of pixels composing one array element 
in the dimension that changes at the fastest rate. slowbinsize_in   
The number of pixels composing one array element in the dimension 
that changes at the second-fastest rate. fastbinsize_in   The number 
of pixels composing one array element in the dimension that changes 
at the fastest rate.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_bin_sizes;

   void set_bin_sizes( int element_number, double slowbinsize_in, double fastbinsize_in) {
     cbf_failnez(cbf_set_bin_sizes(self,element_number,slowbinsize_in,fastbinsize_in));
   }

/* cfunc cbf_new_row   pyfunc new_row  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_new_row (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_new_row adds a new row to the current category and makes it the 
current row.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")new_row;
    void new_row(void){
      cbf_failnez(cbf_new_row(self));}

/* cfunc cbf_rewind_saveframe   pyfunc rewind_saveframe  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_rewind_saveframe (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_rewind_category makes the first category in the current data 
block the current category. cbf_rewind_saveframe makes the first 
saveframe in the current data block the current saveframe. 
cbf_rewind_blockitem makes the first blockitem (category or 
saveframe) in the current data block the current blockitem. The type 
of the blockitem (CBF_CATEGORY or CBF_SAVEFRAME) is returned in type.
If there are no categories, saveframes or blockitems the function 
returns CBF_NOTFOUND.
The current column and row become undefined.
ARGUMENTS
handle   CBF handle. type     CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")rewind_saveframe;
    void rewind_saveframe(void){
      cbf_failnez(cbf_rewind_saveframe(self));}
%feature("autodoc", "
Returns : int compression,int binary_id,int elsize,int elements
*args   : 

C prototype: int cbf_get_realarrayparameters (cbf_handle handle,
                 unsigned int    *compression, int *binary_id, size_t *elsize,
                 size_t *elements);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarrayparameters sets *compression, *binary_id, 
*elsize, *elsigned, *elunsigned, *elements, *minelement and 
*maxelement to values read from the binary value of the item at the 
current column and row. This provides all the arguments needed for a 
subsequent call to cbf_set_integerarray, if a copy of the array is to 
be made into another CIF or CBF. cbf_get_realarrayparameters sets 
*compression, *binary_id, *elsize, *elements to values read from the 
binary value of the item at the current column and row. This provides 
all the arguments needed for a subsequent call to cbf_set_realarray, 
if a copy of the arry is to be made into another CIF or CBF.
The variants cbf_get_integerarrayparameters_wdims, 
cbf_get_integerarrayparameters_wdims_fs, 
cbf_get_integerarrayparameters_wdims_sf, 
cbf_get_realarrayparameters_wdims, 
cbf_get_realarrayparameters_wdims_fs, 
cbf_get_realarrayparameters_wdims_sf set **byteorder, *dimfast, 
*dimmid, *dimslow, and *padding as well, providing the additional 
parameters needed for a subsequent call to cbf_set_integerarray_wdims 
or cbf_set_realarray_wdims.
The value returned in *byteorder is a pointer either to the string  
\"little_endian \" or to the string  \"big_endian \". This should be 
the byte order of the data, not necessarily of the host machine. No 
attempt should be made to modify this string. At this time only  
\"little_endian \" will be returned.
The values returned in *dimfast, *dimmid and *dimslow are the sizes 
of the fastest changing, second fastest changing and third fastest 
changing dimensions of the array, if specified, or zero, if not 
specified.
The value returned in *padding is the size of the post-data padding, 
if any and if specified in the data header. The value is given as a 
count of octets.
If the value is not binary, the function returns CBF_ASCII.
ARGUMENTS
handle        CBF handle. compression   Compression method used. 
elsize        Size in bytes of each array element. binary_id     
Pointer to the destination integer binary identifier. elsigned      
Pointer to an integer. Set to 1 if the elements can be read as signed 
integers. elunsigned    Pointer to an integer. Set to 1 if the 
elements can be read as unsigned integers. elements      Pointer to 
the destination number of elements. minelement    Pointer to the 
destination smallest element. maxelement    Pointer to the 
destination largest element. byteorder     Pointer to the destination 
byte order. dimfast       Pointer to the destination fastest 
dimension. dimmid        Pointer to the destination second fastest 
dimension. dimslow       Pointer to the destination third fastest 
dimension. padding       Pointer to the destination padding size.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_realarrayparameters;

%apply int *OUTPUT {int *compression,int *binary_id, 
                    int *elsize, int *elements} get_realarrayparameters;


    void get_realarrayparameters(int *compression,int *binary_id, 
                                 int *elsize, int *elements){
        unsigned int  comp;
        size_t elsiz, elem;
        cbf_failnez(cbf_get_realarrayparameters(self, 
                                 &comp ,binary_id, &elsiz, &elem ));
        *compression = comp; /* FIXME - does this convert in C? */
        *elsize = elsiz;
        *elements = elem;
        }
%feature("autodoc", "
Returns : Float pixel_size
*args   : Int element_number,Int axis_number

C prototype: int cbf_get_pixel_size_sf(cbf_handle handle,
                 unsigned int      element_number, int axis_number,
                 double *psize);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_size and cbf_get_pixel_size_sf set *psize to point to 
the double value in millimeters of the axis axis_number of the 
detector element element_number. The axis_number is numbered from 1, 
starting with the slowest axis. cbf_get_pixel_size_fs sets *psize to 
point to the double value in millimeters of the axis axis_number of 
the detector element element_number. The axis_number is numbered from 
1, starting with the fastest axis.
If a negative axis number is given, the order of axes is reversed, so 
that -1 specifies the slowest axis for cbf_get_pixel_size_fs and the 
fastest axis for cbf_get_pixel_size_sf.
If the pixel size is not given explcitly in the  \"array_element_size 
\" category, the function returns CBF_NOTFOUND.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. axis_number      The number of the 
axis, starting from 1 for the fastest for cbf_get_pixel_size and 
cbf_get_pixel_size_fs and the slowest for cbf_get_pixel_size_sf. 
psize            Pointer to the destination pixel size.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_size_sf;

%apply double *OUTPUT {double *psize} get_pixel_size;
    void get_pixel_size_sf(unsigned int element_number, 
                        unsigned int axis_number, double *psize){
        cbf_failnez(cbf_get_pixel_size_sf(self, 
                                       element_number, 
                                       axis_number, 
                                       psize));
    }

/* cfunc cbf_force_new_category   pyfunc force_new_category  
   arg cbf_handle handle    arg const char *categoryname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_force_new_category (cbf_handle handle,
                 const char *categoryname);

CBFLib documentation:
DESCRIPTION
cbf_force_new_category creates a new category in the current data 
block with name categoryname and makes it the current category. 
Duplicate category names are allowed.
Even if a category with this name already exists, a new category of 
the same name is created and becomes the current category. The allows 
for the creation of unlooped tag/value lists drawn from the same 
category.
ARGUMENTS
handle         CBF handle. categoryname   The name of the new 
category.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")force_new_category;
    void force_new_category(const char* arg){
      cbf_failnez(cbf_force_new_category(self,arg));}

/* cfunc cbf_force_new_saveframe   pyfunc force_new_saveframe  
   arg cbf_handle handle    arg const char    *saveframename */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_force_new_saveframe (cbf_handle handle,
                 const char    *saveframename);

CBFLib documentation:
DESCRIPTION
cbf_force_new_datablock creates a new data block with name 
datablockname and makes it the current data block. Duplicate data 
block names are allowed. cbf_force_new_saveframe creates a new savew 
frame with name saveframename and makes it the current save frame. 
Duplicate save frame names are allowed.
Even if a save frame with this name already exists, a new save frame 
is created and becomes the current save frame.
ARGUMENTS
handle          CBF handle. datablockname   The name of the new data 
block. saveframename   The name of the new save frame.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")force_new_saveframe;
    void force_new_saveframe(const char* arg){
      cbf_failnez(cbf_force_new_saveframe(self,arg));}

/* cfunc cbf_count_datablocks   pyfunc count_datablocks  
   arg cbf_handle handle    arg unsigned int *datablocks */

%feature("autodoc", "
Returns : Integer
*args   : 

C prototype: int cbf_count_datablocks (cbf_handle handle,
                 unsigned int *datablocks);

CBFLib documentation:
DESCRIPTION
cbf_count_datablocks puts the number of data blocks in *datablocks .
ARGUMENTS
handle       CBF handle. datablocks   Pointer to the destination data 
block count.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")count_datablocks;
    unsigned int count_datablocks(void){
      unsigned int result;
      cbf_failnez(cbf_count_datablocks(self,&result));
      return result;}

/* cfunc cbf_find_row   pyfunc find_row  
   arg cbf_handle handle    arg const char *value */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_find_row (cbf_handle handle, const char *value);

CBFLib documentation:
DESCRIPTION
cbf_find_row makes the first row in the current column with value 
value the current row.
The comparison is case-sensitive.
If a matching row does not exist, the function returns CBF_NOTFOUND.
The current column is not affected.
ARGUMENTS
handle   CBF handle. value    The value of the row to find.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")find_row;
    void find_row(const char* arg){
      cbf_failnez(cbf_find_row(self,arg));}

/* cfunc cbf_select_column   pyfunc select_column  
   arg cbf_handle handle    arg unsigned int column */

%feature("autodoc", "
Returns : 
*args   : Integer

C prototype: int cbf_select_column (cbf_handle handle, unsigned int column);

CBFLib documentation:
DESCRIPTION
cbf_select_column selects column number column in the current 
category as the current column.
The first column is number 0.
The current row is not affected
If the column does not exist, the function returns CBF_NOTFOUND.
ARGUMENTS
handle   CBF handle. column   Number of the column to select.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")select_column;
    void select_column(unsigned int arg){
      cbf_failnez(cbf_select_column(self,arg));}
%feature("autodoc", "
Returns : pycbf detector object
*args   : Integer element_number

C prototype: int cbf_construct_detector (cbf_handle handle,
                 cbf_detector *detector,      unsigned int element_number);

CBFLib documentation:
DESCRIPTION
cbf_construct_detector constructs a detector object for detector 
element number element_number using the description in the CBF object 
handle and initialises the detector handle *detector.
cbf_construct_reference_detector constructs a detector object for 
detector element number element_number using the description in the 
CBF object handle and initialises the detector handle *detector using 
the reference settings of the axes. cbf_require_reference_detector is 
similar, but try to force the creations of missing intermediate 
categories needed to construct a detector object.
ARGUMENTS
handle           CBF handle. detector         Pointer to the 
destination detector handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")construct_detector;

 cbf_detector construct_detector(unsigned int element_number){
    cbf_detector detector;
    cbf_failnez(cbf_construct_detector(self,&detector,element_number));
    return detector;
    }
%feature("autodoc", "
Returns : String
*args   : String axis_id

C prototype: int cbf_get_axis_depends_on (cbf_handle handle,
                 const char *axis_id,      const char * *depends_on);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_depends_on;

   const char *  get_axis_depends_on(const char *axis_id){
        const char* dep_on;
        cbf_failnez(cbf_get_axis_depends_on(self,axis_id,
           &dep_on));
        return dep_on;
        }

/* cfunc cbf_rewind_column   pyfunc rewind_column  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_rewind_column (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_rewind_column makes the first column in the current category the 
current column.
If there are no columns, the function returns CBF_NOTFOUND.
The current row is not affected.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")rewind_column;
    void rewind_column(void){
      cbf_failnez(cbf_rewind_column(self));}
%feature("autodoc", "
Returns : pycbf positioner object
*args   : String axis_id

C prototype: int cbf_construct_reference_positioner (cbf_handle handle,
                      cbf_positioner *positioner, const char *axis_id);

CBFLib documentation:
DESCRIPTION
cbf_construct_positioner constructs a positioner object for the axis 
given by axis_id using the description in the CBF object handle and 
initialises the positioner handle *positioner.
cbf_construct_reference positioner constructs a positioner object for 
the axis given by axis_id using the description in the CBF object 
handle and initialises the detector handle *detector using the 
reference settings of the axes.
ARGUMENTS
handle       CBF handle. positioner   Pointer to the destination 
positioner handle. axis_id      The identifier of the axis in the  
\"axis \" category.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")construct_reference_positioner;

 cbf_positioner construct_reference_positioner(const char* axis_id){
    cbf_positioner positioner;
    cbf_failnez(cbf_construct_reference_positioner(self,&positioner,axis_id));
    return positioner;
    }
%feature("autodoc", "
Returns : Float defaultvalue
*args   : String columnname,Float Value

C prototype: int cbf_require_column_doublevalue (cbf_handle handle,
                 const char      *columnname, double *number,
                 const double defaultvalue);

CBFLib documentation:
DESCRIPTION
cbf_require_column_doublevalue sets *number to the value of the ASCII 
item at the current row for the column given with the name given by 
*columnname, with the value interpreted as a decimal floating-point 
number, or to the number given by defaultvalue if the item cannot be 
found.
ARGUMENTS
handle         CBF handle. columnname     Name of the column 
containing the number. number         pointer to the location to 
receive the floating-point value. defaultvalue   Value to use if the 
requested column and value cannot be found.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")require_column_doublevalue;

%apply double *OUTPUT { double *number} require_column_doublevalue;
void require_column_doublevalue(const char *columnname, double * number,
             const double defaultvalue){
    cbf_failnez(cbf_require_column_doublevalue(self,
                  columnname,number,defaultvalue));
    }
%feature("autodoc", "
Returns : int year,int month,int day,int hour,int minute,double second,
          int timezone
*args   : 

C prototype: int cbf_get_datestamp (cbf_handle handle, unsigned int reserved,
                 int      *year, int *month, int *day, int *hour, int *minute,
                 double *second, int      *timezone);

CBFLib documentation:
DESCRIPTION
cbf_get_datestamp sets *year, *month, *day, *hour, *minute and 
*second to the corresponding values of the collection timestamp. 
*timezone is set to timezone difference from UTC in minutes. The 
parameter < i>reserved is presently unused and should be set to 0.
Any of the destination pointers may be NULL.
ARGUMENTS
handle     CBF handle. reserved   Unused. Any value other than 0 is 
invalid. year       Pointer to the destination timestamp year. month  
    Pointer to the destination timestamp month (1-12). day        
Pointer to the destination timestamp day (1-31). hour       Pointer 
to the destination timestamp hour (0-23). minute     Pointer to the 
destination timestamp minute (0-59). second     Pointer to the 
destination timestamp second (0-60.0). timezone   Pointer to the 
destination timezone difference from UTC in minutes.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_datestamp;

%apply int *OUTPUT {int *year, int *month, int *day, int *hour, 
                    int *minute, double *second, int *timezone} get_datestamp;
   void get_datestamp(int *year, int *month, int *day, int *hour, 
                      int *minute, double *second, int *timezone){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_get_datestamp(self,reserved,
              year,month,day,hour,minute,second,timezone));
        }

/* cfunc cbf_get_integervalue   pyfunc get_integervalue  
   arg cbf_handle handle    arg int *number */

%feature("autodoc", "
Returns : int
*args   : 

C prototype: int cbf_get_integervalue (cbf_handle handle, int *number);

CBFLib documentation:
DESCRIPTION
cbf_get_integervalue sets *number to the value of the ASCII item at 
the current column and row interpreted as a decimal integer. 
cbf_require_integervalue sets *number to the value of the ASCII item 
at the current column and row interpreted as a decimal integer, 
setting it to defaultvalue if necessary.
If the value is not ASCII, the function returns CBF_BINARY.
ARGUMENTS
handle         CBF handle. number         pointer to the number. 
defaultvalue   default number value.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_integervalue;
    int get_integervalue(void){
     int result;
       cbf_failnez(cbf_get_integervalue(self,&result));
       return result;}

/* cfunc cbf_get_crystal_id   pyfunc get_crystal_id  
   arg cbf_handle handle    arg const char **crystal_id */

%feature("autodoc", "
Returns : 
*args   : string

C prototype: int cbf_get_crystal_id (cbf_handle handle,
                 const char **crystal_id);

CBFLib documentation:
DESCRIPTION
cbf_get_crystal_id sets *crystal_id to point to the ASCII value of 
the  \"diffrn.crystal_id \" entry.
If the value is not ASCII, the function returns CBF_BINARY.
The value will be valid as long as the item exists and has not been 
set to a new value.
The value must not be modified by the program in any way.
ARGUMENTS
handle       CBF handle. crystal_id   Pointer to the destination 
value pointer.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_crystal_id;
    const char* get_crystal_id(void){
    const char* result;
    cbf_failnez(cbf_get_crystal_id(self, &result));
    return result;}

/* cfunc cbf_get_doublevalue   pyfunc get_doublevalue  
   arg cbf_handle handle    arg double *number */

%feature("autodoc", "
Returns : double
*args   : 

C prototype: int cbf_get_doublevalue (cbf_handle handle, double *number);

CBFLib documentation:
DESCRIPTION
cbf_get_doublevalue sets *number to the value of the ASCII item at 
the current column and row interpreted as a decimal floating-point 
number. cbf_require_doublevalue sets *number to the value of the 
ASCII item at the current column and row interpreted as a decimal 
floating-point number, setting it to defaultvalue if necessary.
If the value is not ASCII, the function returns CBF_BINARY.
ARGUMENTS
handle         CBF handle. number         Pointer to the destination 
number. defaultvalue   default number value.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_doublevalue;
    double get_doublevalue(void){
     double result;
       cbf_failnez(cbf_get_doublevalue(self,&result));
       return result;}
%feature("autodoc", "
Returns : Float a,Float b,Float c,Float alpha,Float beta,Float gamma
*args   : 

C prototype: int cbf_get_unit_cell (cbf_handle handle, double cell[6],
                 double      cell_esd[6] );

CBFLib documentation:
DESCRIPTION
cbf_get_unit_cell sets cell[0:2] to the double values of the cell 
edge lengths a, b and c in Ångstroms, cell[3:5] to the double values 
of the cell angles α, β and γ in degrees, cell_esd[0:2] to the 
double values of the estimated strandard deviations of the cell edge 
lengths a, b and c in Ångstroms, cell_esd[3:5] to the double values 
of the estimated standard deviations of the the cell angles α, β 
and γ in degrees.
The values returned are retrieved from the first row of the  \"cell 
\" category. The value of  \"_cell.entry_id \" is ignored.
cell or cell_esd may be NULL.
If cell is NULL, the cell parameters are not retrieved.
If cell_esd is NULL, the cell parameter esds are not retrieved.
If the  \"cell \" category is present, but some of the values are 
missing, zeros are returned for the missing values.
ARGUMENTS
handle     CBF handle. cell       Pointer to the destination array of 
6 doubles for the cell parameters. cell_esd   Pointer to the 
destination array of 6 doubles for the cell parameter esds.
RETURN VALUE
Returns an error code on failure or 0 for success. No errors is 
returned for missing values if the  \"cell \" category exists.
SEE ALSO
")get_unit_cell;

%apply double *OUTPUT {double *a, double *b, double *c,
  double *alpha, double *beta, double *gamma} get_unit_cell;
     void get_unit_cell(double *a, double *b, double *c,
  double *alpha, double *beta, double *gamma) {
     double cell[6];
     cbf_failnez(cbf_get_unit_cell(self,cell,NULL));
     *a = cell[0];
     *b = cell[1];
     *c = cell[2];
     *alpha = cell[3];
     *beta = cell[4];
     *gamma = cell[5];
   }
%feature("autodoc", "
Returns : doubleArray cell
*args   : 

C prototype: int cbf_get_unit_cell (cbf_handle handle, double cell[6],
                 double      cell_esd[6] );

CBFLib documentation:
DESCRIPTION
cbf_get_unit_cell sets cell[0:2] to the double values of the cell 
edge lengths a, b and c in Ångstroms, cell[3:5] to the double values 
of the cell angles α, β and γ in degrees, cell_esd[0:2] to the 
double values of the estimated strandard deviations of the cell edge 
lengths a, b and c in Ångstroms, cell_esd[3:5] to the double values 
of the estimated standard deviations of the the cell angles α, β 
and γ in degrees.
The values returned are retrieved from the first row of the  \"cell 
\" category. The value of  \"_cell.entry_id \" is ignored.
cell or cell_esd may be NULL.
If cell is NULL, the cell parameters are not retrieved.
If cell_esd is NULL, the cell parameter esds are not retrieved.
If the  \"cell \" category is present, but some of the values are 
missing, zeros are returned for the missing values.
ARGUMENTS
handle     CBF handle. cell       Pointer to the destination array of 
6 doubles for the cell parameters. cell_esd   Pointer to the 
destination array of 6 doubles for the cell parameter esds.
RETURN VALUE
Returns an error code on failure or 0 for success. No errors is 
returned for missing values if the  \"cell \" category exists.
SEE ALSO
")get_unit_cell;

%apply double *OUTPUT {double *a_esd, double *b_esd, double *c_esd,
  double *alpha_esd, double *beta_esd, double *gamma_esd} get_unit_cell_esd;
     void get_unit_cell_esd(double *a_esd, double *b_esd, double *c_esd,
  double *alpha_esd, double *beta_esd, double *gamma_esd) {
     double cell_esd[6];
     cbf_failnez(cbf_get_unit_cell(self,NULL,cell_esd));
     *a_esd = cell_esd[0];
     *b_esd = cell_esd[1];
     *c_esd = cell_esd[2];
     *alpha_esd = cell_esd[3];
     *beta_esd = cell_esd[4];
     *gamma_esd = cell_esd[5];
   }
%feature("autodoc", "
Returns : String
*args   : String axis_id

C prototype: int cbf_get_axis_type (cbf_handle handle, const char *axis_id,
                      cbf_axis_type *axis_type);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_type;

   const char *  get_axis_type(const char *axis_id){
        cbf_axis_type axis_type;
        cbf_failnez(cbf_get_axis_type(self,axis_id,
           &axis_type));
        if (axis_type == CBF_TRANSLATION_AXIS) return "translation";
        if (axis_type == CBF_ROTATION_AXIS) return "rotation";
        return "general";
        }

/* cfunc cbf_remove_column   pyfunc remove_column  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_remove_column (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_remove_column deletes the current column.
The current column becomes undefined.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")remove_column;
    void remove_column(void){
      cbf_failnez(cbf_remove_column(self));}

/* cfunc cbf_rewind_blockitem   pyfunc rewind_blockitem  
   arg cbf_handle handle    arg CBF_NODETYPE * type */

%feature("autodoc", "
Returns : CBF_NODETYPE
*args   : 

C prototype: int cbf_rewind_blockitem (cbf_handle handle,
                 CBF_NODETYPE * type);

CBFLib documentation:
DESCRIPTION
cbf_rewind_category makes the first category in the current data 
block the current category. cbf_rewind_saveframe makes the first 
saveframe in the current data block the current saveframe. 
cbf_rewind_blockitem makes the first blockitem (category or 
saveframe) in the current data block the current blockitem. The type 
of the blockitem (CBF_CATEGORY or CBF_SAVEFRAME) is returned in type.
If there are no categories, saveframes or blockitems the function 
returns CBF_NOTFOUND.
The current column and row become undefined.
ARGUMENTS
handle   CBF handle. type     CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")rewind_blockitem;
    CBF_NODETYPE rewind_blockitem(void){
     CBF_NODETYPE result;
       cbf_failnez(cbf_rewind_blockitem(self,&result));
       return result;}

/* cfunc cbf_get_value   pyfunc get_value  
   arg cbf_handle handle    arg const char **value */

%feature("autodoc", "
Returns : 
*args   : string

C prototype: int cbf_get_value (cbf_handle handle, const char **value);

CBFLib documentation:
DESCRIPTION
cbf_get_value sets *value to point to the ASCII value of the item at 
the current column and row. cbf_require_value sets *value to point to 
the ASCII value of the item at the current column and row, creating 
the data item if necessary and initializing it to a copy of 
defaultvalue.
If the value is not ASCII, the function returns CBF_BINARY.
The value will be valid as long as the item exists and has not been 
set to a new value.
The value must not be modified by the program in any way.
ARGUMENTS
handle         CBF handle. value          Pointer to the destination 
value pointer. defaultvalue   Default value character string.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_value;
    const char* get_value(void){
    const char* result;
    cbf_failnez(cbf_get_value(self, &result));
    return result;}

/* cfunc cbf_count_categories   pyfunc count_categories  
   arg cbf_handle handle    arg unsigned int *categories */

%feature("autodoc", "
Returns : Integer
*args   : 

C prototype: int cbf_count_categories (cbf_handle handle,
                 unsigned int *categories);

CBFLib documentation:
DESCRIPTION
cbf_count_categories puts the number of categories in the current 
data block in *categories.
ARGUMENTS
handle       CBF handle. categories   Pointer to the destination 
category count.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")count_categories;
    unsigned int count_categories(void){
      unsigned int result;
      cbf_failnez(cbf_count_categories(self,&result));
      return result;}
%feature("autodoc", "
Returns : 
*args   : String filename,Integer headers

C prototype: int cbf_read_widefile (cbf_handle handle, FILE *file, int flags);

CBFLib documentation:
DESCRIPTION
cbf_read_file reads the CBF or CIF file file into the CBF object 
specified by handle, using the CIF 1.0 convention of 80 character 
lines. cbf_read_widefile reads the CBF or CIF file file into the CBF 
object specified by handle, using the CIF 1.1 convention of 2048 
character lines. A warning is issued to stderr for ascii lines over 
the limit. No test is performed on binary sections.
Validation is performed in three ways levels: during the lexical 
scan, during the parse, and, if a dictionary was converted, against 
the value types, value enumerations, categories and parent-child 
relationships specified in the dictionary.
flags controls the interpretation of binary section headers, the 
parsing of brackets constructs and the parsing of treble-quoted 
strings.
MSG_DIGEST:               Instructs CBFlib to check that the digest 
of the binary section matches any header digest value. If the digests 
do not match, the call will return CBF_FORMAT. This evaluation and 
comparison is delayed (a  \"lazy \" evaluation) to ensure maximal 
processing efficiency. If an immediately evaluation is required, see 
MSG_DIGESTNOW, below. MSG_DIGESTNOW:            Instructs CBFlib to 
check that the digest of the binary section matches any header 
digeste value. If the digests do not match, the call will return 
CBF_FORMAT. This evaluation and comparison is performed during 
initial parsing of the section to ensure timely error reporting at 
the expense of processing efficiency. If a more efficient delayed ( 
\"lazy \") evaluation is required, see MSG_DIGEST, above. 
MSG_DIGESTWARN:           Instructs CBFlib to check that the digest 
of the binary section matches any header digeste value. If the 
digests do not match, a warning message will be sent to stderr, but 
processing will attempt to continue. This evaluation and comparison 
is first performed during initial parsing of the section to ensure 
timely error reporting at the expense of processing efficiency. An 
mismatch of the message digest usually indicates a serious error, but 
it is sometimes worth continuing processing to try to isolate the 
cause of the error. Use this option with caution. MSG_NODIGEST:       
      Do not check the digest (default). PARSE_BRACKETS:           
Accept DDLm bracket-delimited [item,item,...item] or 
{item,item,...item} or (item,item,...item) constructs as valid, 
stripping non-quoted embedded whitespace and comments. These 
constructs may span multiple lines. PARSE_LIBERAL_BRACKETS:   Accept 
DDLm bracket-delimited [item,item,...item] or {item,item,...item} or 
(item,item,...item) constructs as valid, stripping embedded 
non-quoted, non-separating whitespace and comments. These constructs 
may span multiple lines. In this case, whitespace may be used as an 
alternative to the comma. PARSE_TRIPLE_QUOTES:      Accept DDLm 
triple-quoted  \" \" \"item,item,...item \" \" \" or 
'''item,item,...item''' constructs as valid, stripping embedded 
whitespace and comments. These constructs may span multiple lines. If 
this flag is set, then ''' will not be interpreted as a quoted 
apoptrophe and  \" \" \" will not be interpreted as a quoted double 
quote mark and PARSE_NOBRACKETS:         Do not accept DDLm 
bracket-delimited [item,item,...item] or {item,item,...item} or 
(item,item,...item) constructs as valid, stripping non-quoted 
embedded whitespace and comments. These constructs may span multiple 
lines. PARSE_NOTRIPLE_QUOTES:    No not accept DDLm triple-quoted  \" 
\" \"item,item,...item \" \" \" or '''item,item,...item''' constructs 
as valid, stripping embedded whitespace and comments. These 
constructs may span multiple lines. If this flag is set, then ''' 
will be interpreted as a quoted apostrophe and  \" \" \" will be 
interpreted as a quoted double quote mark.
CBFlib defers reading binary sections as long as possible. In the 
current version of CBFlib, this means that:
1. The file must be a random-access file opened in binary mode (fopen 
( ,
")read_widefile;

    void read_widefile(char* filename, int headers){
       /* CBFlib needs a stream that will remain open 
          hence DO NOT open from python */
       FILE *stream;
       if ( ! ( stream = fopen (filename, "rb")) ){
         cbf_failnez(CBF_FILEOPEN);
        }
        else{
         cbf_failnez(cbf_read_widefile(self, stream, headers)); 
    }
       }

/* cfunc cbf_set_wavelength   pyfunc set_wavelength  
   arg cbf_handle handle    arg double wavelength */

%feature("autodoc", "
Returns : double wavelength
*args   : 

C prototype: int cbf_set_wavelength (cbf_handle handle, double wavelength);

CBFLib documentation:
DESCRIPTION
cbf_set_wavelength sets the current wavelength in Å to wavelength.
ARGUMENTS
handle       CBF handle. wavelength   Wavelength in Å.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_wavelength;
     void set_wavelength(double wavelength){
        cbf_failnez(cbf_set_wavelength(self,wavelength));}
%feature("autodoc", "
Returns : Float vector1,Float vector2,Float vector3
*args   : String axis_id

C prototype: int cbf_get_axis_vector (cbf_handle handle, const char *axis_id,
                 double      *vector1, double *vector2, double *vector3);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_vector;

%apply double *OUTPUT {double *vector1, double *vector2, double vector3} get_axis_vector;
   void get_axis_vector(const char *axis_id,
                    double *vector1, double *vector2, double *vector3){
        cbf_failnez(cbf_get_axis_vector(self,axis_id,
                         vector1, vector2,vector3));
        }
%feature("autodoc", "
Returns : 
*args   : Int element_number,Int axis_number,Float pixel size

C prototype: int cbf_set_pixel_size_sf(cbf_handle handle,
                 unsigned int      element_number, int axis_number,
                 double psize);

CBFLib documentation:
DESCRIPTION
cbf_set_pixel_size and cbf_set_pixel_size_sf set the item in the  
\"size \" column of the  \"array_structure_list \" category at the 
row which matches axis axis_number of the detector element 
element_number converting the double pixel size psize from meters to 
millimeters in storing it in the  \"size \" column for the axis 
axis_number of the detector element element_number. The axis_number 
is numbered from 1, starting with the slowest axis. 
cbf_set_pixel_size_fs sets the item in the  \"size \" column of the  
\"array_structure_list \" category at the row which matches axis 
axis_number of the detector element element_number converting the 
double pixel size psize from meters to millimeters in storing it in 
the  \"size \" column for the axis axis_number of the detector 
element element_number. The axis_number is numbered from 1, starting 
with the fastest axis.
If a negative axis number is given, the order of axes is reversed, so 
that -1 specifies the slowest axis for cbf_get_pixel_size_fs and the 
fastest axis for cbf_get_pixel_size_sf.
If the  \"array_structure_list \" category does not already exist, it 
is created.
If the appropriate row in the  \"array_structure_list \" catgeory 
does not already exist, it is created.
If the pixel size is not given explcitly in the  \"array_element_size 
category \", the function returns CBF_NOTFOUND.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. axis_number      The number of the 
axis, fastest first, starting from 1.
")set_pixel_size_sf;

     void set_pixel_size_sf (unsigned int element_number, 
                          unsigned int axis_number, double psize){
         cbf_failnez(cbf_set_pixel_size_sf(self, 
                                        element_number, 
                                        axis_number, 
                                        psize));
     }

/* cfunc cbf_get_diffrn_id   pyfunc get_diffrn_id  
   arg cbf_handle handle    arg const char **diffrn_id */

%feature("autodoc", "
Returns : 
*args   : string

C prototype: int cbf_get_diffrn_id (cbf_handle handle,
                 const char **diffrn_id);

CBFLib documentation:
DESCRIPTION
cbf_get_diffrn_id sets *diffrn_id to point to the ASCII value of the  
\"diffrn.id \" entry. cbf_require_diffrn_id also sets *diffrn_id to 
point to the ASCII value of the  \"diffrn.id \" entry, but, if the  
\"diffrn.id \" entry does not exist, it sets the value in the CBF and 
in*diffrn_id to the character string given by default_id, creating 
the category and column is necessary.
The diffrn_id will be valid as long as the item exists and has not 
been set to a new value.
The diffrn_id must not be modified by the program in any way.
ARGUMENTS
handle       CBF handle. diffrn_id    Pointer to the destination 
value pointer. default_id   Character string default value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_diffrn_id;
    const char* get_diffrn_id(void){
    const char* result;
    cbf_failnez(cbf_get_diffrn_id(self, &result));
    return result;}
%feature("autodoc", "
Returns : Float
*args   : String axis_id

C prototype: int cbf_get_axis_rotation (cbf_handle handle,
                 const char *axis_id,      double *rotation);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_rotation;

%apply double *OUTPUT {double *rotation} get_axis_rotation;
   void get_axis_rotation(const char *axis_id,
                    double *rotation){
        cbf_failnez(cbf_get_axis_rotation(self,axis_id,
                         rotation));
        }

/* cfunc cbf_find_datablock   pyfunc find_datablock  
   arg cbf_handle handle    arg const char *datablockname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_find_datablock (cbf_handle handle,
                 const char *datablockname);

CBFLib documentation:
DESCRIPTION
cbf_find_datablock makes the data block with name datablockname the 
current data block.
The comparison is case-insensitive.
If the data block does not exist, the function returns CBF_NOTFOUND.
The current category becomes undefined.
ARGUMENTS
handle          CBF handle. datablockname   The name of the data 
block to find.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")find_datablock;
    void find_datablock(const char* arg){
      cbf_failnez(cbf_find_datablock(self,arg));}
%feature("autodoc", "
Returns : float polarizn_source_ratio,float polarizn_source_norm
*args   : 

C prototype: int cbf_get_polarization (cbf_handle handle,
                 double      *polarizn_source_ratio,
                 double *polarizn_source_norm);

CBFLib documentation:
DESCRIPTION
cbf_get_polarization sets *polarizn_source_ratio and 
*polarizn_source_norm to the corresponding source polarization 
parameters.
Either destination pointer may be NULL.
ARGUMENTS
handle                  CBF handle. polarizn_source_ratio   Pointer 
to the destination polarizn_source_ratio. polarizn_source_norm    
Pointer to the destination polarizn_source_norm.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_polarization;

     /* Returns a pair of double values */
%apply double *OUTPUT { double *in1, double *in2 };
     void get_polarization(double *in1,double *in2){
        cbf_failnez(cbf_get_polarization (self, in1, in2));
     }

/* cfunc cbf_select_category   pyfunc select_category  
   arg cbf_handle handle    arg unsigned int category */

%feature("autodoc", "
Returns : 
*args   : Integer

C prototype: int cbf_select_category (cbf_handle handle,
                 unsigned int category);

CBFLib documentation:
DESCRIPTION
cbf_select_category selects category number category in the current 
data block as the current category.
The first category is number 0.
The current column and row become undefined.
If the category does not exist, the function returns CBF_NOTFOUND.
ARGUMENTS
handle     CBF handle. category   Number of the category to select.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")select_category;
    void select_category(unsigned int arg){
      cbf_failnez(cbf_select_category(self,arg));}
%feature("autodoc", "
Returns : Float pixel_size
*args   : Int element_number,Int axis_number

C prototype: int cbf_get_pixel_size_fs(cbf_handle handle,
                 unsigned int      element_number, int axis_number,
                 double *psize);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_size and cbf_get_pixel_size_sf set *psize to point to 
the double value in millimeters of the axis axis_number of the 
detector element element_number. The axis_number is numbered from 1, 
starting with the slowest axis. cbf_get_pixel_size_fs sets *psize to 
point to the double value in millimeters of the axis axis_number of 
the detector element element_number. The axis_number is numbered from 
1, starting with the fastest axis.
If a negative axis number is given, the order of axes is reversed, so 
that -1 specifies the slowest axis for cbf_get_pixel_size_fs and the 
fastest axis for cbf_get_pixel_size_sf.
If the pixel size is not given explcitly in the  \"array_element_size 
\" category, the function returns CBF_NOTFOUND.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. axis_number      The number of the 
axis, starting from 1 for the fastest for cbf_get_pixel_size and 
cbf_get_pixel_size_fs and the slowest for cbf_get_pixel_size_sf. 
psize            Pointer to the destination pixel size.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_size_fs;

%apply double *OUTPUT {double *psize} get_pixel_size;
    void get_pixel_size_fs(unsigned int element_number, 
                        unsigned int axis_number, double *psize){
        cbf_failnez(cbf_get_pixel_size_fs(self, 
                                       element_number, 
                                       axis_number, 
                                       psize));
    }
%feature("autodoc", "
Returns : Float vector1,Float vector2,Float vector3,Float offset1,Float offset2,
          Float offset3,Float angle
*args   : Float ratio,String axis_id,String frame_id

C prototype: int cbf_get_axis_poise(cbf_handle handle, double ratio,
                 double *      vector1, double * vector2, double * vector3,
                 double * offset1, double *      offset2, double * offset3,
                 double * angle, const char * axis_id,
                 const      char * frame_id);

CBFLib documentation:
DESCRIPTION
cbf_get_axis_poise sets vector1, vector2, vector3 to point to the 
components of the axis vector for axis axis_id, offset1, offset2, 
offset3 to point to the components of the axis base offset vector for 
axis axis_id, and angle to point to the angle of rotation of axis 
axis_id after application of the axis settings for frame frame_id, 
using ratio, a value between 0 and 1, indicating how far into the 
internal motion in the frame to go. If frame_id is the string  \". 
\", the first frame found is used. If there is more than one frame, 
which frame will be found is indeterminate. If frame_id is NULL, the 
overall setting for the scan are used, rather than those for any 
particular frame. The vector and offset reported are the reference 
vector and offset of the axis axis_id transformed by application of 
all motions of the axes on which axis_id depends.
cbf_get_goniometer_poise vector1, vector2, vector3 to point to the 
components of the axis vector for the goniometer axis, offset1, 
offset2, offset3 to point to the components of the axis base offset 
vector for the goniometer axis, and angle to point to the angle of 
rotation of the goniometer axis after application of all axis 
settings in the goniometer deriving the vector, offset and angle from 
the resulting matrix. Calculation of the vector is indeterminate if 
the angle is zero.
cbf_get_axis_reference_poise sets vector1, vector2, vector3 to point 
to the components of the axis vector for axis axis_id, offset1, 
offset2, offset3 to point to the components of the axis base offset 
vector for axis axis_id unmodified by axis rotations. Any of the 
pointers may be specified as NULL.
ARGUMENTS
handle       CBF handle. ratio        A number between 0 and 1 
indication how far into the frame to go vector1      Pointer to the 
first component of the axis vector vector2      Pointer to the second 
component of the axis vector vector3      Pointer to the third 
component of the axis vector offset1      Pointer to the first 
component of the axis offset offset2      Pointer to the second 
component of the axis offset offset3      Pointer to the third 
component of the axis offset angle        Pointer to the rotation 
angle axis_id      The specified axis frame_id     The specified 
frame positioner   CBF goniometer
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_poise;

  %apply double *OUTPUT {double *vector1, double *vector2, double *vector3, 
    double *offset1, double *offset2, double *offset3, double *angle};
  
  void get_axis_poise(double ratio, 
      double *vector1, double *vector2, double *vector3,
      double *offset1, double *offset2, double *offset3,
      double *angle,
      const char *axis_id, const char *frame_id){
        cbf_failnez(cbf_get_axis_poise(self, ratio,
          vector1, vector2, vector3,
          offset1, offset2, offset3, angle,
          axis_id, frame_id));
      }

%feature("autodoc", "
Returns : 
*args   : String filename,Integer headers

C prototype: int cbf_read_file (cbf_handle handle, FILE *file, int flags);

CBFLib documentation:
DESCRIPTION
cbf_read_file reads the CBF or CIF file file into the CBF object 
specified by handle, using the CIF 1.0 convention of 80 character 
lines. cbf_read_widefile reads the CBF or CIF file file into the CBF 
object specified by handle, using the CIF 1.1 convention of 2048 
character lines. A warning is issued to stderr for ascii lines over 
the limit. No test is performed on binary sections.
Validation is performed in three ways levels: during the lexical 
scan, during the parse, and, if a dictionary was converted, against 
the value types, value enumerations, categories and parent-child 
relationships specified in the dictionary.
flags controls the interpretation of binary section headers, the 
parsing of brackets constructs and the parsing of treble-quoted 
strings.
MSG_DIGEST:               Instructs CBFlib to check that the digest 
of the binary section matches any header digest value. If the digests 
do not match, the call will return CBF_FORMAT. This evaluation and 
comparison is delayed (a  \"lazy \" evaluation) to ensure maximal 
processing efficiency. If an immediately evaluation is required, see 
MSG_DIGESTNOW, below. MSG_DIGESTNOW:            Instructs CBFlib to 
check that the digest of the binary section matches any header 
digeste value. If the digests do not match, the call will return 
CBF_FORMAT. This evaluation and comparison is performed during 
initial parsing of the section to ensure timely error reporting at 
the expense of processing efficiency. If a more efficient delayed ( 
\"lazy \") evaluation is required, see MSG_DIGEST, above. 
MSG_DIGESTWARN:           Instructs CBFlib to check that the digest 
of the binary section matches any header digeste value. If the 
digests do not match, a warning message will be sent to stderr, but 
processing will attempt to continue. This evaluation and comparison 
is first performed during initial parsing of the section to ensure 
timely error reporting at the expense of processing efficiency. An 
mismatch of the message digest usually indicates a serious error, but 
it is sometimes worth continuing processing to try to isolate the 
cause of the error. Use this option with caution. MSG_NODIGEST:       
      Do not check the digest (default). PARSE_BRACKETS:           
Accept DDLm bracket-delimited [item,item,...item] or 
{item,item,...item} or (item,item,...item) constructs as valid, 
stripping non-quoted embedded whitespace and comments. These 
constructs may span multiple lines. PARSE_LIBERAL_BRACKETS:   Accept 
DDLm bracket-delimited [item,item,...item] or {item,item,...item} or 
(item,item,...item) constructs as valid, stripping embedded 
non-quoted, non-separating whitespace and comments. These constructs 
may span multiple lines. In this case, whitespace may be used as an 
alternative to the comma. PARSE_TRIPLE_QUOTES:      Accept DDLm 
triple-quoted  \" \" \"item,item,...item \" \" \" or 
'''item,item,...item''' constructs as valid, stripping embedded 
whitespace and comments. These constructs may span multiple lines. If 
this flag is set, then ''' will not be interpreted as a quoted 
apoptrophe and  \" \" \" will not be interpreted as a quoted double 
quote mark and PARSE_NOBRACKETS:         Do not accept DDLm 
bracket-delimited [item,item,...item] or {item,item,...item} or 
(item,item,...item) constructs as valid, stripping non-quoted 
embedded whitespace and comments. These constructs may span multiple 
lines. PARSE_NOTRIPLE_QUOTES:    No not accept DDLm triple-quoted  \" 
\" \"item,item,...item \" \" \" or '''item,item,...item''' constructs 
as valid, stripping embedded whitespace and comments. These 
constructs may span multiple lines. If this flag is set, then ''' 
will be interpreted as a quoted apostrophe and  \" \" \" will be 
interpreted as a quoted double quote mark.
CBFlib defers reading binary sections as long as possible. In the 
current version of CBFlib, this means that:
1. The file must be a random-access file opened in binary mode (fopen 
( ,
")read_file;

    void read_file(char* filename, int headers){
       /* CBFlib needs a stream that will remain open 
          hence DO NOT open from python */
       FILE *stream;
       if ( ! ( stream = fopen (filename, "rb")) ){
         cbf_failnez(CBF_FILEOPEN);
        }
        else{
         cbf_failnez(cbf_read_file(self, stream, headers)); 
    }
       }

/* cfunc cbf_datablock_name   pyfunc datablock_name  
   arg cbf_handle handle    arg const char **datablockname */

%feature("autodoc", "
Returns : 
*args   : string

C prototype: int cbf_datablock_name (cbf_handle handle,
                 const char **datablockname);

CBFLib documentation:
DESCRIPTION
cbf_datablock_name sets *datablockname to point to the name of the 
current data block.
The data block name will be valid as long as the data block exists 
and has not been renamed.
The name must not be modified by the program in any way.
ARGUMENTS
handle          CBF handle. datablockname   Pointer to the 
destination data block name pointer.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")datablock_name;
    const char* datablock_name(void){
    const char* result;
    cbf_failnez(cbf_datablock_name(self, &result));
    return result;}
%feature("autodoc", "
Returns : 
*args   : int compression,int binary_id,(binary) String data,int elsize,
          int elements,String byteorder,int dimfast,int dimmid,int dimslow,
          int padding

C prototype: int cbf_set_realarray_wdims (cbf_handle handle,
                 unsigned int compression,    int binary_id, void *array,
                 size_t elsize, size_t elements, const char    *byteorder,
                 size_t dimfast, size_t dimmid, size_t dimslow,
                 size_t    padding);

CBFLib documentation:
DESCRIPTION
cbf_set_integerarray sets the binary value of the item at the current 
column and row to an integer array. The array consists of elements 
elements of elsize bytes each, starting at array. The elements are 
signed if elsigned is non-0 and unsigned otherwise. binary_id is the 
binary section identifier. cbf_set_realarray sets the binary value of 
the item at the current column and row to an integer array. The array 
consists of elements elements of elsize bytes each, starting at 
array. binary_id is the binary section identifier.
The cbf_set_integerarray_wdims, cbf_set_integerarray_wdims_fs, 
cbf_set_integerarray_wdims_sf, cbf_set_realarray_wdims, 
cbf_set_realarray_wdims_fs and cbf_set_realarray_wdims_sf variants 
allow the data header values of byteorder, dimfast, dimmid, dimslow 
and padding to be set to the data byte order, the fastest, second 
fastest and third fastest array dimensions and the size in byte of 
the post data padding to be used.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL       Canonical-code compression (section 3.3.1) 
CBF_PACKED          CCP4-style packing (section 3.3.2) CBF_PACKED_V2  
     CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET    
 Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE            No compression. 
NOTE: This scheme is by far the slowest of the four and uses much 
more disk space. It is intended for routine use with small arrays 
only. With large arrays (like images) it should be used only for 
debugging.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned), for cbf_set_integerarray, or IEEE doubles or 
floats for cbf_set_realarray. If elsize is not equal to sizeof 
(char), sizeof (short) or sizeof (int), the function returns 
CBF_ARGUMENT.
ARGUMENTS
handle        CBF handle. compression   Compression method to use. 
binary_id     Integer binary identifier. array         Pointer to the 
source array. elsize        Size in bytes of each source array 
element. elsigned      Set to non-0 if the source array elements are 
signed. elements      The number of elements in the array
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_realarray_wdims;

    /* CBFlib must NOT modify the data string nor the byteorder string
       which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_realarray_wdims;
%apply (char *STRING, int LENGTH) { (char *bo, int bolen) } set_realarray_wdims;

    void set_realarray_wdims(unsigned int compression, int binary_id, 
             char *data, int len, int elsize, int elements,
             char *bo, int bolen, int dimfast, int dimmid, int dimslow, int padding){
        /* safety check on args */
        size_t els, ele;
        void *array;
        char byteorder[15];
        if(len == elsize*elements && elements==dimfast*dimmid*dimslow){
           array = data;
           els = elsize;
           ele = elements;
           strncpy(byteorder,bo,bolen<15?bolen:14);
           byteorder[bolen<15?bolen:14] = 0;
           cbf_failnez(cbf_set_realarray_wdims (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, (size_t) elements, (const char *)byteorder,
           (size_t)dimfast, (size_t)dimmid, (size_t)dimslow, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : pycbf detector object
*args   : Integer element_number

C prototype: int cbf_construct_reference_detector (cbf_handle handle,
                 cbf_detector      *detector, unsigned int element_number);

CBFLib documentation:
DESCRIPTION
cbf_construct_detector constructs a detector object for detector 
element number element_number using the description in the CBF object 
handle and initialises the detector handle *detector.
cbf_construct_reference_detector constructs a detector object for 
detector element number element_number using the description in the 
CBF object handle and initialises the detector handle *detector using 
the reference settings of the axes. cbf_require_reference_detector is 
similar, but try to force the creations of missing intermediate 
categories needed to construct a detector object.
ARGUMENTS
handle           CBF handle. detector         Pointer to the 
destination detector handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")construct_reference_detector;

 cbf_detector construct_reference_detector(unsigned int element_number){
    cbf_detector detector;
    cbf_failnez(cbf_construct_reference_detector(self,&detector,element_number));
    return detector;
    }
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int ndimfast,int ndimmid,int ndimslow

C prototype: int cbf_get_real_3d_image_fs (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 void *array, size_t elsize, size_t      ndimfast,
                 size_t ndimmid, size_t ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_real_3d_image_fs_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_real_3d_image_fs_as_string;

// Get the length correct

    void get_real_3d_image_fs_as_string(int element_number, char **s, int *slen,
    int elsize, int ndimfast, int ndimmid, int ndimslow){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimmid*ndimslow))) {
               cbf_failnez (cbf_get_real_3d_image_fs(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize,
               (size_t) ndimfast, (size_t)ndimmid, (size_t)ndimslow));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimmid*ndimslow;
        *s = (char *) array;
      }

/* cfunc cbf_rewind_row   pyfunc rewind_row  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_rewind_row (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_rewind_row makes the first row in the current category the 
current row.
If there are no rows, the function returns CBF_NOTFOUND.
The current column is not affected.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")rewind_row;
    void rewind_row(void){
      cbf_failnez(cbf_rewind_row(self));}
%feature("autodoc", "
Returns : Float start,Float increment
*args   : String axis_id

C prototype: int cbf_get_axis_setting (cbf_handle handle,
                 unsigned int reserved,      const char *axis_id, double *start,
                 double *increment);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_setting;

%apply double *OUTPUT {double *start, double *increment} get_axis_setting;
   void get_axis_setting(const char *axis_id,
                    double *start, double *increment){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_get_axis_setting(self,reserved,axis_id,
                         start,increment));
        }

/* cfunc cbf_require_column   pyfunc require_column  
   arg cbf_handle handle    arg const char *columnname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_require_column (cbf_handle handle,
                 const char *columnname);

CBFLib documentation:
DESCRIPTION
cbf_require_column makes the columns in the current category with 
name columnname the current column, if it exists, or creates it if it 
does not.
The comparison is case-insensitive.
The current row is not affected.
ARGUMENTS
handle       CBF handle. columnname   The name of column to find.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")require_column;
    void require_column(const char* arg){
      cbf_failnez(cbf_require_column(self,arg));}
%feature("autodoc", "
Returns : Float time,Integer timezone
*args   : 

C prototype: int cbf_get_timestamp (cbf_handle handle, unsigned int reserved,
                 double      *time, int *timezone);

CBFLib documentation:
DESCRIPTION
cbf_get_timestamp sets *time to the collection timestamp in seconds 
since January 1 1970. *timezone is set to timezone difference from 
UTC in minutes. The parameter reserved is presently unused and should 
be set to 0.
Either of the destination pointers may be NULL.
ARGUMENTS
handle     CBF handle. reserved   Unused. Any value other than 0 is 
invalid. time       Pointer to the destination collection timestamp. 
timezone   Pointer to the destination timezone difference.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_timestamp;

%apply double *OUTPUT {double *time} get_timestamp;
%apply int *OUTPUT {int *timezone} get_timestamp;
    void get_timestamp(double *time, int *timezone){
        unsigned int reserved;
        reserved = 0; 
        cbf_failnez(cbf_get_timestamp(self,reserved,time,timezone));
        }

/* cfunc cbf_find_nextrow   pyfunc find_nextrow  
   arg cbf_handle handle    arg const char *value */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_find_nextrow (cbf_handle handle, const char *value);

CBFLib documentation:
DESCRIPTION
cbf_find_nextrow makes the makes the next row in the current column 
with value value the current row. The search starts from the row 
following the last row found with cbf_find_row or cbf_find_nextrow, 
or from the current row if the current row was defined using any 
other function.
The comparison is case-sensitive.
If no more matching rows exist, the function returns CBF_NOTFOUND.
The current column is not affected.
ARGUMENTS
handle   CBF handle. value    the value to search for.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")find_nextrow;
    void find_nextrow(const char* arg){
      cbf_failnez(cbf_find_nextrow(self,arg));}
%feature("autodoc", "
Returns : String
*args   : String axis_id

C prototype: int cbf_get_axis_equipment_component (cbf_handle handle,
                 const char      *axis_id, const char * *equipment_component);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_equipment_component;

   const char *  get_axis_equipment_component(const char *axis_id){
        const char* equip_comp;
        cbf_failnez(cbf_get_axis_equipment_component(self,axis_id,
           &equip_comp));
        return equip_comp;
        }
%feature("autodoc", "
Returns : int compression,int binary_id,int elsize,int elements,char **bo,
          int *bolen,int dimslow,int dimmid,int dimfast,int padding
*args   : 

C prototype: int cbf_get_realarrayparameters_wdims_sf (cbf_handle handle,
                 unsigned int    *compression, int *binary_id, size_t *elsize,
                 size_t *elements, const char    **byteorder, size_t *dimslow,
                 size_t *dimmid, size_t *dimfast, size_t    *padding);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarrayparameters sets *compression, *binary_id, 
*elsize, *elsigned, *elunsigned, *elements, *minelement and 
*maxelement to values read from the binary value of the item at the 
current column and row. This provides all the arguments needed for a 
subsequent call to cbf_set_integerarray, if a copy of the array is to 
be made into another CIF or CBF. cbf_get_realarrayparameters sets 
*compression, *binary_id, *elsize, *elements to values read from the 
binary value of the item at the current column and row. This provides 
all the arguments needed for a subsequent call to cbf_set_realarray, 
if a copy of the arry is to be made into another CIF or CBF.
The variants cbf_get_integerarrayparameters_wdims, 
cbf_get_integerarrayparameters_wdims_fs, 
cbf_get_integerarrayparameters_wdims_sf, 
cbf_get_realarrayparameters_wdims, 
cbf_get_realarrayparameters_wdims_fs, 
cbf_get_realarrayparameters_wdims_sf set **byteorder, *dimfast, 
*dimmid, *dimslow, and *padding as well, providing the additional 
parameters needed for a subsequent call to cbf_set_integerarray_wdims 
or cbf_set_realarray_wdims.
The value returned in *byteorder is a pointer either to the string  
\"little_endian \" or to the string  \"big_endian \". This should be 
the byte order of the data, not necessarily of the host machine. No 
attempt should be made to modify this string. At this time only  
\"little_endian \" will be returned.
The values returned in *dimfast, *dimmid and *dimslow are the sizes 
of the fastest changing, second fastest changing and third fastest 
changing dimensions of the array, if specified, or zero, if not 
specified.
The value returned in *padding is the size of the post-data padding, 
if any and if specified in the data header. The value is given as a 
count of octets.
If the value is not binary, the function returns CBF_ASCII.
ARGUMENTS
handle        CBF handle. compression   Compression method used. 
elsize        Size in bytes of each array element. binary_id     
Pointer to the destination integer binary identifier. elsigned      
Pointer to an integer. Set to 1 if the elements can be read as signed 
integers. elunsigned    Pointer to an integer. Set to 1 if the 
elements can be read as unsigned integers. elements      Pointer to 
the destination number of elements. minelement    Pointer to the 
destination smallest element. maxelement    Pointer to the 
destination largest element. byteorder     Pointer to the destination 
byte order. dimfast       Pointer to the destination fastest 
dimension. dimmid        Pointer to the destination second fastest 
dimension. dimslow       Pointer to the destination third fastest 
dimension. padding       Pointer to the destination padding size.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_realarrayparameters_wdims_sf;

%cstring_output_allocate_size(char **bo, int *bolen, free(*$1));
%apply int *OUTPUT {int *compression,int *binary_id, 
                    int *elsize, 
                    int *elements,
                    int *dimslow, int *dimmid, int *dimfast, int *padding} 
                  get_realarrayparameters_wdims_sf;

    void get_realarrayparameters_wdims_sf(int *compression,int *binary_id, 
                        int *elsize, 
                        int *elements, 
                        char **bo, int *bolen,
                        int *dimslow, int *dimmid, int *dimfast, int *padding
                        ){
        unsigned int  comp;
        size_t elsiz, elem, df,dm,ds,pd;
        const char * byteorder;
        char * bot;
        cbf_failnez(cbf_get_realarrayparameters_wdims_sf(self, 
         &comp,binary_id, &elsiz, &elem, 
         &byteorder,&ds,&dm,&df,&pd ));
        *bolen = strlen(byteorder);
        if (!(bot = (char *)malloc(*bolen))) {cbf_failnez(CBF_ALLOC)}
        strncpy(bot,byteorder,*bolen);
        *bo = bot;
        *compression = comp;
        *elsize = elsiz;
        *elements = elem;
        *dimfast = df;
        *dimmid = dm;
        *dimslow = ds;
        *padding = pd;
        
        }

/* cfunc cbf_reset_datablock   pyfunc reset_datablock  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_reset_datablock (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_reset_datablock deletes all categories from the current data 
block. cbf_reset_saveframe deletes all categories from the current 
save frame.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")reset_datablock;
    void reset_datablock(void){
      cbf_failnez(cbf_reset_datablock(self));}
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int elsign,int dimfast,int dimmid,int dimslow

C prototype: int cbf_set_3d_image_fs(cbf_handle handle, unsigned int reserved,
                      unsigned int element_number, unsigned int compression,
                 void *array,      size_t elsize, int elsign, size_t ndimfast,
                 size_t ndimmid, size_t      ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_3d_image_fs;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_3d_image;

    void set_3d_image_fs(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int elsign, int ndimfast, int ndimmid, int ndimslow){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimmid*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_3d_image_fs (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, elsign, (size_t) ndimfast, (size_t) ndimmid, (size_t)ndimslow)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }

/* cfunc cbf_set_saveframename   pyfunc set_saveframename  
   arg cbf_handle handle    arg const char *saveframename */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_set_saveframename (cbf_handle handle,
                 const char *saveframename);

CBFLib documentation:
DESCRIPTION
cbf_set_datablockname changes the name of the current data block to 
datablockname. cbf_set_saveframename changes the name of the current 
save frame to saveframename.
If a data block or save frame with this name already exists 
(comparison is case-insensitive), the function returns CBF_IDENTICAL.
ARGUMENTS
handle          CBF handle. datablockname   The new data block name. 
saveframename   The new save frame name.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_saveframename;
    void set_saveframename(const char* arg){
      cbf_failnez(cbf_set_saveframename(self,arg));}
%feature("autodoc", "
Returns : Int number
*args   : Int thedefault

C prototype: int cbf_require_integervalue (cbf_handle handle, int *number,
                 int    defaultvalue);

CBFLib documentation:
DESCRIPTION
cbf_get_integervalue sets *number to the value of the ASCII item at 
the current column and row interpreted as a decimal integer. 
cbf_require_integervalue sets *number to the value of the ASCII item 
at the current column and row interpreted as a decimal integer, 
setting it to defaultvalue if necessary.
If the value is not ASCII, the function returns CBF_BINARY.
ARGUMENTS
handle         CBF handle. number         pointer to the number. 
defaultvalue   default number value.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")require_integervalue;

%apply int *OUTPUT {int *number} require_integervalue;

     void require_integervalue(int *number, int thedefault){

     cbf_failnez(cbf_require_integervalue(self,number,thedefault));

     }
%feature("autodoc", "
Returns : int compression,int binary_id,int elsize,int elsigned,int elunsigned,
          int elements,int minelement,int maxelement
*args   : 

C prototype: int cbf_get_integerarrayparameters (cbf_handle handle,
                 unsigned int    *compression, int *binary_id, size_t *elsize,
                 int *elsigned, int    *elunsigned, size_t *elements,
                 int *minelement, int *maxelement);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarrayparameters sets *compression, *binary_id, 
*elsize, *elsigned, *elunsigned, *elements, *minelement and 
*maxelement to values read from the binary value of the item at the 
current column and row. This provides all the arguments needed for a 
subsequent call to cbf_set_integerarray, if a copy of the array is to 
be made into another CIF or CBF. cbf_get_realarrayparameters sets 
*compression, *binary_id, *elsize, *elements to values read from the 
binary value of the item at the current column and row. This provides 
all the arguments needed for a subsequent call to cbf_set_realarray, 
if a copy of the arry is to be made into another CIF or CBF.
The variants cbf_get_integerarrayparameters_wdims, 
cbf_get_integerarrayparameters_wdims_fs, 
cbf_get_integerarrayparameters_wdims_sf, 
cbf_get_realarrayparameters_wdims, 
cbf_get_realarrayparameters_wdims_fs, 
cbf_get_realarrayparameters_wdims_sf set **byteorder, *dimfast, 
*dimmid, *dimslow, and *padding as well, providing the additional 
parameters needed for a subsequent call to cbf_set_integerarray_wdims 
or cbf_set_realarray_wdims.
The value returned in *byteorder is a pointer either to the string  
\"little_endian \" or to the string  \"big_endian \". This should be 
the byte order of the data, not necessarily of the host machine. No 
attempt should be made to modify this string. At this time only  
\"little_endian \" will be returned.
The values returned in *dimfast, *dimmid and *dimslow are the sizes 
of the fastest changing, second fastest changing and third fastest 
changing dimensions of the array, if specified, or zero, if not 
specified.
The value returned in *padding is the size of the post-data padding, 
if any and if specified in the data header. The value is given as a 
count of octets.
If the value is not binary, the function returns CBF_ASCII.
ARGUMENTS
handle        CBF handle. compression   Compression method used. 
elsize        Size in bytes of each array element. binary_id     
Pointer to the destination integer binary identifier. elsigned      
Pointer to an integer. Set to 1 if the elements can be read as signed 
integers. elunsigned    Pointer to an integer. Set to 1 if the 
elements can be read as unsigned integers. elements      Pointer to 
the destination number of elements. minelement    Pointer to the 
destination smallest element. maxelement    Pointer to the 
destination largest element. byteorder     Pointer to the destination 
byte order. dimfast       Pointer to the destination fastest 
dimension. dimmid        Pointer to the destination second fastest 
dimension. dimslow       Pointer to the destination third fastest 
dimension. padding       Pointer to the destination padding size.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_integerarrayparameters;

%apply int *OUTPUT {int *compression,int *binary_id, 
                    int *elsize, int *elsigned, int *elunsigned, 
                    int *elements, int *minelement, int *maxelement} 
                  get_integerarrayparameters;

    void get_integerarrayparameters(int *compression,int *binary_id, 
                        int *elsize, int *elsigned, int *elunsigned, 
                        int *elements, int *minelement, int *maxelement){
        unsigned int  comp;
        size_t elsiz, elem;
        cbf_failnez(cbf_get_integerarrayparameters(self, 
         &comp,binary_id, &elsiz, elsigned, elunsigned, &elem, 
          minelement, maxelement));
        *compression = comp; /* FIXME - does this convert in C? */
        *elsize = elsiz;
        *elements = elem;
        }
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int dimslow,int dimmid,int dimfast

C prototype: int cbf_set_real_3d_image_sf(cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 unsigned int compression, void      *array,size_t elsize,
                 size_t ndimslow, size_t ndimmid, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_real_3d_image_sf;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_real_3d_image_sf;

    void set_real_3d_image_sf(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int ndimslow, int ndimmid, int ndimfast){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimmid*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_real_3d_image_sf (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, (size_t) ndimslow, (size_t)ndimmid, (size_t)ndimfast)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : 
*args   : String filename,Integer ciforcbf,Integer Headers,Integer encoding

C prototype: int cbf_write_file (cbf_handle handle, FILE *file, int readable,
                 int    ciforcbf, int flags, int encoding);

CBFLib documentation:
DESCRIPTION
cbf_write_file writes the CBF object specified by handle into the 
file file, following CIF 1.0 conventions of 80 character lines. 
cbf_write_widefile writes the CBF object specified by handle into the 
file file, following CIF 1.1 conventions of 2048 character lines. A 
warning is issued to stderr for ascii lines over the limit, and an 
attempt is made to fold lines to fit. No test is performed on binary 
sections.
If a dictionary has been provided, aliases will be applied on output.
Unlike cbf_read_file, the file does not have to be random-access.
If the file is random-access and readable, readable can be set to 
non-0 to indicate to CBFlib that the file can be used as a buffer to 
conserve disk space. If the file is not random-access or not 
readable, readable must be 0.
")write_file;

    void write_file(const char* filename, int ciforcbf, int headers, 
                    int encoding){
       FILE *stream;
       int readable;
       /* Make readable false so we can close the file immediately */
       readable = 0;
       if ( ! ( stream = fopen (filename, "w+b")) ){
         cbf_failnez(CBF_FILEOPEN);
        }
        else{
        cbf_failnez(cbf_write_file(self, stream, readable, 
                    ciforcbf, headers, encoding));
        fclose(stream);
        }
       }
%feature("autodoc", "
Returns : 
*args   : Float div_x_source,Float div_y_source,Float div_x_y_source

C prototype: int cbf_set_divergence (cbf_handle handle, double div_x_source,
                 double      div_y_source, double div_x_y_source);

CBFLib documentation:
DESCRIPTION
cbf_set_divergence sets the source divergence parameters to the 
values specified by div_x_source, div_y_source and div_x_y_source.
ARGUMENTS
handle           CBF handle. div_x_source     New value of 
div_x_source. div_y_source     New value of div_y_source. 
div_x_y_source   New value of div_x_y_source.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_divergence;

   void set_divergence ( double div_x_source, double div_y_source,
                        double div_x_y_source){
      cbf_failnez(cbf_set_divergence (self, div_x_source, 
                              div_y_source,div_x_y_source));
      }

/* cfunc cbf_remove_datablock   pyfunc remove_datablock  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_remove_datablock (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_remove_datablock deletes the current data block. 
cbf_remove_saveframe deletes the current save frame.
The current data block becomes undefined.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")remove_datablock;
    void remove_datablock(void){
      cbf_failnez(cbf_remove_datablock(self));}

/* cfunc cbf_count_elements   pyfunc count_elements  
   arg cbf_handle handle    arg unsigned int *elements */

%feature("autodoc", "
Returns : Integer
*args   : 

C prototype: int cbf_count_elements (cbf_handle handle,
                 unsigned int *elements);

CBFLib documentation:
DESCRIPTION
cbf_count_elements sets *elements to the number of detector elements.
ARGUMENTS
handle     CBF handle. elements   Pointer to the destination count.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")count_elements;
    unsigned int count_elements(void){
      unsigned int result;
      cbf_failnez(cbf_count_elements(self,&result));
      return result;}
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int elsign,int dimfast,int dimslow

C prototype: int cbf_set_image_fs(cbf_handle handle, unsigned int reserved,
                 unsigned      int element_number, unsigned int compression,
                 void *array, size_t      elsize, int elsign, size_t ndimfast,
                 size_t ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_image_fs;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_image;

    void set_image_fs(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int elsign, int ndimfast, int ndimslow){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_image (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, elsign, (size_t) ndimfast, (size_t)ndimslow)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : pycbf detector object
*args   : Integer element_number

C prototype: int cbf_require_reference_detector (cbf_handle handle,
                 cbf_detector      *detector, unsigned int element_number);

CBFLib documentation:
DESCRIPTION
cbf_construct_detector constructs a detector object for detector 
element number element_number using the description in the CBF object 
handle and initialises the detector handle *detector.
cbf_construct_reference_detector constructs a detector object for 
detector element number element_number using the description in the 
CBF object handle and initialises the detector handle *detector using 
the reference settings of the axes. cbf_require_reference_detector is 
similar, but try to force the creations of missing intermediate 
categories needed to construct a detector object.
ARGUMENTS
handle           CBF handle. detector         Pointer to the 
destination detector handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")require_reference_detector;

 cbf_detector require_reference_detector(unsigned int element_number){
    cbf_detector detector;
    cbf_failnez(cbf_require_reference_detector(self,&detector,element_number));
    return detector;
    }

/* cfunc cbf_next_category   pyfunc next_category  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_next_category (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_next_category makes the category following the current category 
in the current data block the current category.
If there are no more categories, the function returns CBF_NOTFOUND.
The current column and row become undefined.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")next_category;
    void next_category(void){
      cbf_failnez(cbf_next_category(self));}

/* cfunc cbf_set_diffrn_id   pyfunc set_diffrn_id  
   arg cbf_handle handle    arg const char *diffrn_id */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_set_diffrn_id (cbf_handle handle, const char *diffrn_id);

CBFLib documentation:
DESCRIPTION
cbf_set_diffrn_id sets the  \"diffrn.id \" entry of the current 
datablock to the ASCII value diffrn_id.
This function also changes corresponding  \"diffrn_id \" entries in 
the  \"diffrn_source \",  \"diffrn_radiation \",  \"diffrn_detector 
\" and  \"diffrn_measurement \" categories.
ARGUMENTS
handle      CBF handle. diffrn_id   ASCII value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_diffrn_id;
    void set_diffrn_id(const char* arg){
      cbf_failnez(cbf_set_diffrn_id(self,arg));}
%feature("autodoc", "
Returns : 
*args   : Float time,Integer timezone,Float precision

C prototype: int cbf_set_timestamp (cbf_handle handle, unsigned int reserved,
                 double      time, int timezone, double precision);

CBFLib documentation:
DESCRIPTION
cbf_set_timestamp sets the collection timestamp in seconds since 
January 1 1970 to the value specified by time. The timezone 
difference from UTC
")set_timestamp;

    void set_timestamp(double time, int timezone, double precision){
        unsigned int reserved;
        reserved = 0; 
        cbf_failnez(cbf_set_timestamp(self,reserved,time,timezone,precision));
        }
%feature("autodoc", "
Returns : Float matrix_0,Float matrix_1,Float matrix_2,Float matrix_3,
          Float matrix_4,Float matrix_5,Float matrix_6,Float matrix_7,
          Float matrix_8
*args   : 

C prototype: int cbf_get_orientation_matrix (cbf_handle handle,
                 double ub_matrix[9]);

CBFLib documentation:
DESCRIPTION
cbf_get_orientation_matrix sets ub_matrix to point to the array of 
orientation matrix entries in the  \"diffrn \" category in the order 
of columns:
 \"UB[1][1] \"  \"UB[1][2] \"  \"UB[1][3] \"  \"UB[2][1] \"  
\"UB[2][2] \"  \"UB[2][3] \"  \"UB[3][1] \"  \"UB[3][2] \"  
\"UB[3][3] \"
cbf_set_orientation_matrix sets the values in the  \"diffrn \" 
category to the values pointed to by ub_matrix.
ARGUMENTS
handle      CBF handle. ub_matrix   Source or destination array of 9 
doubles giving the orientation matrix parameters.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_orientation_matrix;

%apply double *OUTPUT {double *m0,double *m1,double *m2,
double *m3,double *m4, double *m5,double *m6,
double *m7,double *m8  } get_orientation_matrix;
   void get_orientation_matrix(  double *m0,double *m1,
double *m2,double *m3,double *m4,double *m5,double *m6,
double *m7,double *m8){
        double m[9];
        cbf_failnez(cbf_get_orientation_matrix(self,m));
        *m0 = m[0]; *m1=m[1] ; *m2=m[2] ;
        *m3 = m[3]; *m4=m[4] ; *m5=m[5] ;
        *m6 = m[6]; *m7=m[7] ; *m8=m[8] ;
        }
%feature("autodoc", "
Returns : size_t ndimfast,size_t ndimslow
*args   : Integer element_number

C prototype: int cbf_get_image_size_fs (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 size_t *ndimfast, size_t *ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_get_image_size, cbf_get_image_size_fs and cbf_get_image_size_sf 
set *ndimslow and *ndimfast to the slow and fast dimensions of the 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimfast 
will be set to 1. If the array is 3-dimensional an error code will be 
returned. cbf_get_3d_image_size, cbf_get_3d_image_size_fs and 
cbf_get_3d_image_size_sf set *ndimslow, *ndimmid and *ndimfast to the 
slowest, next fastest and fastest dimensions, respectively, of the 3D 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimmid 
and
")get_image_size_fs;

%apply int *OUTPUT {int *ndimfast, int *ndimslow} get_image_size_fs;
     void get_image_size_fs(unsigned int element_number, int *ndimfast, int *ndimslow){
        unsigned int reserved;
        size_t infast, inslow;
        reserved = 0;
        cbf_failnez(cbf_get_image_size_fs(self,reserved,element_number,&infast,&inslow));
        *ndimfast = (int)infast; /* FIXME - is that how to convert? */
        *ndimslow = (int)inslow; 
        }
%feature("autodoc", "
Returns : Float div_x_source,Float div_y_source,Float div_x_y_source
*args   : 

C prototype: int cbf_get_divergence (cbf_handle handle, double *div_x_source,
                 double      *div_y_source, double *div_x_y_source);

CBFLib documentation:
DESCRIPTION
cbf_get_divergence sets *div_x_source, *div_y_source and 
*div_x_y_source to the corresponding source divergence parameters.
Any of the destination pointers may be NULL.
ARGUMENTS
handle           CBF handle. div_x_source     Pointer to the 
destination div_x_source. div_y_source     Pointer to the destination 
div_y_source. div_x_y_source   Pointer to the destination 
div_x_y_source.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_divergence;

%apply double *OUTPUT {double *div_x_source, double *div_y_source,
                       double *div_x_y_source } get_divergence;
    void get_divergence(double *div_x_source, double *div_y_source,
       double *div_x_y_source){
       cbf_failnez(cbf_get_divergence(self, 
                                     div_x_source, 
                                     div_y_source,
                                     div_x_y_source)); 
       } 

/* cfunc cbf_rewind_category   pyfunc rewind_category  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_rewind_category (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_rewind_category makes the first category in the current data 
block the current category. cbf_rewind_saveframe makes the first 
saveframe in the current data block the current saveframe. 
cbf_rewind_blockitem makes the first blockitem (category or 
saveframe) in the current data block the current blockitem. The type 
of the blockitem (CBF_CATEGORY or CBF_SAVEFRAME) is returned in type.
If there are no categories, saveframes or blockitems the function 
returns CBF_NOTFOUND.
The current column and row become undefined.
ARGUMENTS
handle   CBF handle. type     CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")rewind_category;
    void rewind_category(void){
      cbf_failnez(cbf_rewind_category(self));}
%feature("autodoc", "
Returns : 
*args   : String filename

C prototype: int cbf_read_template (cbf_handle handle, FILE *file);

CBFLib documentation:
DESCRIPTION
cbf_read_template reads the CBF or CIF file file into the CBF object 
specified by handle and selects the first datablock as the current 
datablock.
ARGUMENTS
handle   Pointer to a CBF handle. file     Pointer to a file 
descriptor.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")read_template;

    void read_template(char* filename){
       /* CBFlib needs a stream that will remain open 
        hence DO NOT open from python */
       FILE *stream;
       if ( ! ( stream = fopen (filename, "rb")) ){
         cbf_failnez(CBF_FILEOPEN);
        }
        else{
        cbf_failnez(cbf_read_template (self, stream)); }
    }


/* cfunc cbf_select_row   pyfunc select_row  
   arg cbf_handle handle    arg unsigned int row */

%feature("autodoc", "
Returns : 
*args   : Integer

C prototype: int cbf_select_row (cbf_handle handle, unsigned int row);

CBFLib documentation:
DESCRIPTION
cbf_select_row selects row number row in the current category as the 
current row.
The first row is number 0.
The current column is not affected
If the row does not exist, the function returns CBF_NOTFOUND.
ARGUMENTS
handle   CBF handle. row      Number of the row to select.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")select_row;
    void select_row(unsigned int arg){
      cbf_failnez(cbf_select_row(self,arg));}
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int elsign,int ndimfast,int ndimslow

C prototype: int cbf_get_image_fs (cbf_handle handle, unsigned int reserved,
                 unsigned      int element_number, void *array, size_t elsize,
                 int elsign, size_t      ndimfast, size_t ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_image_fs_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_image_fs_as_string;

// Get the length correct

    void get_image_fs_as_string(int element_number, char **s, int *slen,
    int elsize, int elsign, int ndimfast, int ndimslow){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimslow))) {
               cbf_failnez (cbf_get_image_fs(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize, elsign,
               (size_t) ndimfast, (size_t)ndimslow));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimslow;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : size_t ndimslow,size_t ndimfast
*args   : Integer element_number

C prototype: int cbf_get_image_size_sf (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 size_t *ndimslow, size_t *ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image_size, cbf_get_image_size_fs and cbf_get_image_size_sf 
set *ndimslow and *ndimfast to the slow and fast dimensions of the 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimfast 
will be set to 1. If the array is 3-dimensional an error code will be 
returned. cbf_get_3d_image_size, cbf_get_3d_image_size_fs and 
cbf_get_3d_image_size_sf set *ndimslow, *ndimmid and *ndimfast to the 
slowest, next fastest and fastest dimensions, respectively, of the 3D 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimmid 
and
")get_image_size_sf;

%apply int *OUTPUT {int *ndimslow, int *ndimfast} get_image_size_sf;
     void get_image_size_sf(unsigned int element_number, int *ndimslow, int *ndimfast){
        unsigned int reserved;
        size_t inslow, infast;
        reserved = 0;
        cbf_failnez(cbf_get_image_size(self,reserved,element_number,&inslow,&infast));
        *ndimslow = (int)inslow;
        *ndimfast = (int)infast; 
        }
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int ndimfast,int ndimslow

C prototype: int cbf_get_real_image_fs (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 void *array, size_t elsize, size_t      ndimfast,
                 size_t ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_real_image_fs_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_real_image_fs_as_string;

// Get the length correct

    void get_real_image_fs_as_string(int element_number, char **s, int *slen,
    int elsize, int ndimfast, int ndimslow){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimslow))) {
               cbf_failnez (cbf_get_real_image_fs(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize,
               (size_t) ndimfast, (size_t)ndimslow));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimslow;
        *s = (char *) array;
      }

/* cfunc cbf_count_columns   pyfunc count_columns  
   arg cbf_handle handle    arg unsigned int *columns */

%feature("autodoc", "
Returns : Integer
*args   : 

C prototype: int cbf_count_columns (cbf_handle handle, unsigned int *columns);

CBFLib documentation:
DESCRIPTION
cbf_count_columns puts the number of columns in the current category 
in *columns.
ARGUMENTS
handle    CBF handle. columns   Pointer to the destination column 
count.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")count_columns;
    unsigned int count_columns(void){
      unsigned int result;
      cbf_failnez(cbf_count_columns(self,&result));
      return result;}
%feature("autodoc", "
Returns : int compression,int binary_id,int elsize,int elsigned,int elunsigned,
          int elements,int minelement,int maxelement,char **bo,int *bolen,
          int dimfast,int dimmid,int dimslow,int padding
*args   : 

C prototype: int cbf_get_integerarrayparameters_wdims (cbf_handle handle,
                 unsigned int    *compression, int *binary_id, size_t *elsize,
                 int *elsigned, int    *elunsigned, size_t *elements,
                 int *minelement, int *maxelement, const    char **byteorder,
                 size_t *dimfast, size_t *dimmid, size_t *dimslow,
                 size_t    *padding);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarrayparameters sets *compression, *binary_id, 
*elsize, *elsigned, *elunsigned, *elements, *minelement and 
*maxelement to values read from the binary value of the item at the 
current column and row. This provides all the arguments needed for a 
subsequent call to cbf_set_integerarray, if a copy of the array is to 
be made into another CIF or CBF. cbf_get_realarrayparameters sets 
*compression, *binary_id, *elsize, *elements to values read from the 
binary value of the item at the current column and row. This provides 
all the arguments needed for a subsequent call to cbf_set_realarray, 
if a copy of the arry is to be made into another CIF or CBF.
The variants cbf_get_integerarrayparameters_wdims, 
cbf_get_integerarrayparameters_wdims_fs, 
cbf_get_integerarrayparameters_wdims_sf, 
cbf_get_realarrayparameters_wdims, 
cbf_get_realarrayparameters_wdims_fs, 
cbf_get_realarrayparameters_wdims_sf set **byteorder, *dimfast, 
*dimmid, *dimslow, and *padding as well, providing the additional 
parameters needed for a subsequent call to cbf_set_integerarray_wdims 
or cbf_set_realarray_wdims.
The value returned in *byteorder is a pointer either to the string  
\"little_endian \" or to the string  \"big_endian \". This should be 
the byte order of the data, not necessarily of the host machine. No 
attempt should be made to modify this string. At this time only  
\"little_endian \" will be returned.
The values returned in *dimfast, *dimmid and *dimslow are the sizes 
of the fastest changing, second fastest changing and third fastest 
changing dimensions of the array, if specified, or zero, if not 
specified.
The value returned in *padding is the size of the post-data padding, 
if any and if specified in the data header. The value is given as a 
count of octets.
If the value is not binary, the function returns CBF_ASCII.
ARGUMENTS
handle        CBF handle. compression   Compression method used. 
elsize        Size in bytes of each array element. binary_id     
Pointer to the destination integer binary identifier. elsigned      
Pointer to an integer. Set to 1 if the elements can be read as signed 
integers. elunsigned    Pointer to an integer. Set to 1 if the 
elements can be read as unsigned integers. elements      Pointer to 
the destination number of elements. minelement    Pointer to the 
destination smallest element. maxelement    Pointer to the 
destination largest element. byteorder     Pointer to the destination 
byte order. dimfast       Pointer to the destination fastest 
dimension. dimmid        Pointer to the destination second fastest 
dimension. dimslow       Pointer to the destination third fastest 
dimension. padding       Pointer to the destination padding size.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_integerarrayparameters_wdims;

%cstring_output_allocate_size(char **bo, int *bolen, free(*$1));
%apply int *OUTPUT {int *compression,int *binary_id, 
                    int *elsize, int *elsigned, int *elunsigned, 
                    int *elements, int *minelement, int *maxelement,
                    int *dimfast, int *dimmid, int *dimslow, int *padding} 
                  get_integerarrayparameters_wdims;

    void get_integerarrayparameters_wdims(int *compression,int *binary_id, 
                        int *elsize, int *elsigned, int *elunsigned, 
                        int *elements, int *minelement, int *maxelement,
                        char **bo, int *bolen,
                        int *dimfast, int *dimmid, int *dimslow, int *padding
                        ){
        unsigned int  comp;
        size_t elsiz, elem, df,dm,ds,pd;
        const char * byteorder;
        char * bot;
        cbf_failnez(cbf_get_integerarrayparameters_wdims(self, 
         &comp,binary_id, &elsiz, elsigned, elunsigned, &elem, 
          minelement, maxelement, &byteorder,&df,&dm,&ds,&pd ));
        *bolen = strlen(byteorder);
        if (!(bot = (char *)malloc(*bolen))) {cbf_failnez(CBF_ALLOC)}
        strncpy(bot,byteorder,*bolen);
        *bo = bot;
        *compression = comp;
        *elsize = elsiz;
        *elements = elem;
        *dimfast = df;
        *dimmid = dm;
        *dimslow = ds;
        *padding = pd;
        
        }
%feature("autodoc", "
Returns : Float gain,Float gain_esd
*args   : 

C prototype: int cbf_get_gain (cbf_handle handle, unsigned int element_number,
                 double      *gain, double *gain_esd);

CBFLib documentation:
DESCRIPTION
cbf_get_gain sets *gain and *gain_esd to the corresponding gain 
parameters for element number element_number.
Either of the destination pointers may be NULL.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. gain             Pointer to the 
destination gain. gain_esd         Pointer to the destination 
gain_esd.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_gain;

%apply double *OUTPUT {double *gain, double *gain_esd} get_gain;
    void get_gain (unsigned int element_number, double *gain, 
                   double *gain_esd){
        cbf_failnez(cbf_get_gain (self, element_number, gain, gain_esd));
        }

/* cfunc cbf_new_saveframe   pyfunc new_saveframe  
   arg cbf_handle handle    arg const char *saveframename */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_new_saveframe (cbf_handle handle,
                 const char *saveframename);

CBFLib documentation:
DESCRIPTION
cbf_new_datablock creates a new data block with name datablockname 
and makes it the current data block. cbf_new_saveframe creates a new 
save frame with name saveframename within the current data block and 
makes the new save frame the current save frame.
If a data block or save frame with this name already exists, the 
existing data block or save frame becomes the current data block or 
save frame.
ARGUMENTS
handle          CBF handle. datablockname   The name of the new data 
block. saveframename   The name of the new save frame.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")new_saveframe;
    void new_saveframe(const char* arg){
      cbf_failnez(cbf_new_saveframe(self,arg));}
%feature("autodoc", "
Returns : 
*args   : Float polarizn_source_ratio,Float polarizn_source_norm

C prototype: int cbf_set_polarization (cbf_handle handle,
                 double      polarizn_source_ratio,
                 double polarizn_source_norm);

CBFLib documentation:
DESCRIPTION
cbf_set_polarization sets the source polarization to the values 
specified by polarizn_source_ratio and polarizn_source_norm.
ARGUMENTS
handle                  CBF handle. polarizn_source_ratio   New value 
of polarizn_source_ratio. polarizn_source_norm    New value of 
polarizn_source_norm.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_polarization;

     void set_polarization (double polarizn_source_ratio,
                            double polarizn_source_norm){
         cbf_failnez(cbf_set_polarization(self,
                         polarizn_source_ratio,
                         polarizn_source_norm));
     }
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int dimslow,int dimmid,int dimfast

C prototype: int cbf_set_real_3d_image (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 unsigned int compression, void      *array,size_t elsize,
                 size_t ndimslow, size_t ndimmid, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_real_3d_image;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_real_3d_image_sf;

    void set_real_3d_image(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int ndimslow, int ndimmid, int ndimfast){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimmid*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_real_3d_image (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, (size_t) ndimslow, (size_t)ndimmid, (size_t)ndimfast)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }

/* cfunc cbf_delete_row   pyfunc delete_row  
   arg cbf_handle handle    arg unsigned int rownumber */

%feature("autodoc", "
Returns : 
*args   : Integer

C prototype: int cbf_delete_row (cbf_handle handle, unsigned int rownumber);

CBFLib documentation:
DESCRIPTION
cbf_delete_row deletes a row from the current category. Rows starting 
from rownumber +1 are moved down by 1. If the current row was higher 
than rownumber, or if the current row is the last row, it will also 
move down by 1.
The row numbers start from 0.
ARGUMENTS
handle      CBF handle. rownumber   The number of the row to delete.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")delete_row;
    void delete_row(unsigned int arg){
      cbf_failnez(cbf_delete_row(self,arg));}

/* cfunc cbf_column_name   pyfunc column_name  
   arg cbf_handle handle    arg const char **columnname */

%feature("autodoc", "
Returns : 
*args   : string

C prototype: int cbf_column_name (cbf_handle handle, const char **columnname);

CBFLib documentation:
DESCRIPTION
cbf_column_name sets *columnname to point to the name of the current 
column of the current category.
The column name will be valid as long as the column exists.
The name must not be modified by the program in any way.
cbf_set_column_name sets the name of the current column to 
newcolumnname
ARGUMENTS
handle          CBF handle. columnname      Pointer to the 
destination column name pointer. newcolumnname   New column name 
pointer.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")column_name;
    const char* column_name(void){
    const char* result;
    cbf_failnez(cbf_column_name(self, &result));
    return result;}

/* cfunc cbf_remove_saveframe   pyfunc remove_saveframe  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_remove_saveframe (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_remove_datablock deletes the current data block. 
cbf_remove_saveframe deletes the current save frame.
The current data block becomes undefined.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")remove_saveframe;
    void remove_saveframe(void){
      cbf_failnez(cbf_remove_saveframe(self));}
%feature("autodoc", "
Returns : 
*args   : int compression,int binary_id,(binary) String data,int elsize,
          int elsigned,int elements,String byteorder,int dimslow,int dimmid,
          int dimfast,int padding

C prototype: int cbf_set_integerarray_wdims_sf (cbf_handle handle,
                 unsigned int    compression, int binary_id, void *array,
                 size_t elsize, int elsigned,    size_t elements,
                 const char *byteorder, size_t dimslow, size_t dimmid,
                    size_t dimfast, size_t padding);

CBFLib documentation:
DESCRIPTION
cbf_set_integerarray sets the binary value of the item at the current 
column and row to an integer array. The array consists of elements 
elements of elsize bytes each, starting at array. The elements are 
signed if elsigned is non-0 and unsigned otherwise. binary_id is the 
binary section identifier. cbf_set_realarray sets the binary value of 
the item at the current column and row to an integer array. The array 
consists of elements elements of elsize bytes each, starting at 
array. binary_id is the binary section identifier.
The cbf_set_integerarray_wdims, cbf_set_integerarray_wdims_fs, 
cbf_set_integerarray_wdims_sf, cbf_set_realarray_wdims, 
cbf_set_realarray_wdims_fs and cbf_set_realarray_wdims_sf variants 
allow the data header values of byteorder, dimfast, dimmid, dimslow 
and padding to be set to the data byte order, the fastest, second 
fastest and third fastest array dimensions and the size in byte of 
the post data padding to be used.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL       Canonical-code compression (section 3.3.1) 
CBF_PACKED          CCP4-style packing (section 3.3.2) CBF_PACKED_V2  
     CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET    
 Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE            No compression. 
NOTE: This scheme is by far the slowest of the four and uses much 
more disk space. It is intended for routine use with small arrays 
only. With large arrays (like images) it should be used only for 
debugging.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned), for cbf_set_integerarray, or IEEE doubles or 
floats for cbf_set_realarray. If elsize is not equal to sizeof 
(char), sizeof (short) or sizeof (int), the function returns 
CBF_ARGUMENT.
ARGUMENTS
handle        CBF handle. compression   Compression method to use. 
binary_id     Integer binary identifier. array         Pointer to the 
source array. elsize        Size in bytes of each source array 
element. elsigned      Set to non-0 if the source array elements are 
signed. elements      The number of elements in the array
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_integerarray_wdims_sf;

    /* CBFlib must NOT modify the data string nor the byteorder string
       which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_integerarray_wdims_sf;
%apply (char *STRING, int LENGTH) { (char *bo, int bolen) } set_integerarray_wdims_sf;

    void set_integerarray_wdims_sf(unsigned int compression, int binary_id, 
             char *data, int len, int elsize, int elsigned, int elements,
             char *bo, int bolen, int dimslow, int dimmid, int dimfast, int padding){
        /* safety check on args */
        size_t els, ele;
        void *array;
        char byteorder[15];
        if(len == elsize*elements && elements==dimfast*dimmid*dimslow){
           array = data;
           els = elsize;
           ele = elements;
           strncpy(byteorder,bo,bolen<15?bolen:14);
           byteorder[bolen<15?bolen:14] = 0;
           cbf_failnez(cbf_set_integerarray_wdims_sf (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, elsigned, (size_t) elements, (const char *)byteorder,
           (size_t)dimslow, (size_t)dimmid, (size_t)dimfast, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : String Value
*args   : String defaultvalue

C prototype: int cbf_require_value (cbf_handle handle, const char **value,
                 const char    *defaultvalue );

CBFLib documentation:
DESCRIPTION
cbf_get_value sets *value to point to the ASCII value of the item at 
the current column and row. cbf_require_value sets *value to point to 
the ASCII value of the item at the current column and row, creating 
the data item if necessary and initializing it to a copy of 
defaultvalue.
If the value is not ASCII, the function returns CBF_BINARY.
The value will be valid as long as the item exists and has not been 
set to a new value.
The value must not be modified by the program in any way.
ARGUMENTS
handle         CBF handle. value          Pointer to the destination 
value pointer. defaultvalue   Default value character string.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")require_value;


   const char* require_value(const char* defaultvalue){
     const char * result;
     cbf_failnez(cbf_require_value(self, &result, defaultvalue));
     return result;
    }
%feature("autodoc", "
Returns : Int Value
*args   : String Columnvalue,Int default

C prototype: int cbf_require_column_integervalue (cbf_handle handle,
                 const char      *columnname, int *number,
                 const int defaultvalue);

CBFLib documentation:
DESCRIPTION
cbf_require_column_doublevalue sets *number to the value of the ASCII 
item at the current row for the column given with the name given by 
*columnname, with the value interpreted as an integer number, or to 
the number given by defaultvalue if the item cannot be found.
ARGUMENTS
handle         CBF handle. columnname     Name of the column 
containing the number. number         pointer to the location to 
receive the integer value. defaultvalue   Value to use if the 
requested column and value cannot be found.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")require_column_integervalue;

%apply int *OUTPUT {int *number}  require_column_integervalue;
void require_column_integervalue(const char *columnname, 
                       int *number, const int defaultvalue){
    cbf_failnez(cbf_require_column_integervalue(self,
           columnname, number,defaultvalue));
    }
%feature("autodoc", "
Returns : 
*args   : Int element_number,Int axis_number,Float pixel size

C prototype: int cbf_set_pixel_size (cbf_handle handle,
                 unsigned int element_number,      int axis_number,
                 double psize);

CBFLib documentation:
DESCRIPTION
cbf_set_pixel_size and cbf_set_pixel_size_sf set the item in the  
\"size \" column of the  \"array_structure_list \" category at the 
row which matches axis axis_number of the detector element 
element_number converting the double pixel size psize from meters to 
millimeters in storing it in the  \"size \" column for the axis 
axis_number of the detector element element_number. The axis_number 
is numbered from 1, starting with the slowest axis. 
cbf_set_pixel_size_fs sets the item in the  \"size \" column of the  
\"array_structure_list \" category at the row which matches axis 
axis_number of the detector element element_number converting the 
double pixel size psize from meters to millimeters in storing it in 
the  \"size \" column for the axis axis_number of the detector 
element element_number. The axis_number is numbered from 1, starting 
with the fastest axis.
If a negative axis number is given, the order of axes is reversed, so 
that -1 specifies the slowest axis for cbf_get_pixel_size_fs and the 
fastest axis for cbf_get_pixel_size_sf.
If the  \"array_structure_list \" category does not already exist, it 
is created.
If the appropriate row in the  \"array_structure_list \" catgeory 
does not already exist, it is created.
If the pixel size is not given explcitly in the  \"array_element_size 
category \", the function returns CBF_NOTFOUND.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. axis_number      The number of the 
axis, fastest first, starting from 1.
")set_pixel_size;

     void set_pixel_size (unsigned int element_number, 
                          unsigned int axis_number, double psize){
         cbf_failnez(cbf_set_pixel_size(self, 
                                        element_number, 
                                        axis_number, 
                                        psize));
     }

/* cfunc cbf_next_column   pyfunc next_column  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_next_column (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_next_column makes the column following the current column in the 
current category the current column.
If there are no more columns, the function returns CBF_NOTFOUND.
The current row is not affected.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")next_column;
    void next_column(void){
      cbf_failnez(cbf_next_column(self));}
%feature("autodoc", "
Returns : size_t ndimslow,size_t ndimmid,size_t ndimfast
*args   : Integer element_number

C prototype: int cbf_get_3d_image_size_sf (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 size_t *ndimslow, size_t *ndimmid, size_t      *ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image_size, cbf_get_image_size_fs and cbf_get_image_size_sf 
set *ndimslow and *ndimfast to the slow and fast dimensions of the 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimfast 
will be set to 1. If the array is 3-dimensional an error code will be 
returned. cbf_get_3d_image_size, cbf_get_3d_image_size_fs and 
cbf_get_3d_image_size_sf set *ndimslow, *ndimmid and *ndimfast to the 
slowest, next fastest and fastest dimensions, respectively, of the 3D 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimmid 
and
")get_3d_image_size_sf;

%apply int *OUTPUT {int *ndimslow, int *ndimmid, int *ndimfast} get_3d_image_size_sf;
     void get_3d_image_size_sf(unsigned int element_number, int *ndimslow, int *ndimmid, int *ndimfast){
        unsigned int reserved;
        size_t inslow, inmid, infast;
        reserved = 0;
        cbf_failnez(cbf_get_3d_image_size_sf(self,reserved,element_number,&inslow,&inmid,&infast));
        *ndimslow = (int)inslow; /* FIXME - is that how to convert? */
        *ndimmid = (int)inmid; 
        *ndimfast = (int)infast;
        }
%feature("autodoc", "
Returns : int compression,int binary_id,int elsize,int elements,char **bo,
          int *bolen,int dimfast,int dimmid,int dimslow,int padding
*args   : 

C prototype: int cbf_get_realarrayparameters_wdims_fs (cbf_handle handle,
                 unsigned int    *compression, int *binary_id, size_t *elsize,
                 size_t *elements, const char    **byteorder, size_t *dimfast,
                 size_t *dimmid, size_t *dimslow, size_t    *padding);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarrayparameters sets *compression, *binary_id, 
*elsize, *elsigned, *elunsigned, *elements, *minelement and 
*maxelement to values read from the binary value of the item at the 
current column and row. This provides all the arguments needed for a 
subsequent call to cbf_set_integerarray, if a copy of the array is to 
be made into another CIF or CBF. cbf_get_realarrayparameters sets 
*compression, *binary_id, *elsize, *elements to values read from the 
binary value of the item at the current column and row. This provides 
all the arguments needed for a subsequent call to cbf_set_realarray, 
if a copy of the arry is to be made into another CIF or CBF.
The variants cbf_get_integerarrayparameters_wdims, 
cbf_get_integerarrayparameters_wdims_fs, 
cbf_get_integerarrayparameters_wdims_sf, 
cbf_get_realarrayparameters_wdims, 
cbf_get_realarrayparameters_wdims_fs, 
cbf_get_realarrayparameters_wdims_sf set **byteorder, *dimfast, 
*dimmid, *dimslow, and *padding as well, providing the additional 
parameters needed for a subsequent call to cbf_set_integerarray_wdims 
or cbf_set_realarray_wdims.
The value returned in *byteorder is a pointer either to the string  
\"little_endian \" or to the string  \"big_endian \". This should be 
the byte order of the data, not necessarily of the host machine. No 
attempt should be made to modify this string. At this time only  
\"little_endian \" will be returned.
The values returned in *dimfast, *dimmid and *dimslow are the sizes 
of the fastest changing, second fastest changing and third fastest 
changing dimensions of the array, if specified, or zero, if not 
specified.
The value returned in *padding is the size of the post-data padding, 
if any and if specified in the data header. The value is given as a 
count of octets.
If the value is not binary, the function returns CBF_ASCII.
ARGUMENTS
handle        CBF handle. compression   Compression method used. 
elsize        Size in bytes of each array element. binary_id     
Pointer to the destination integer binary identifier. elsigned      
Pointer to an integer. Set to 1 if the elements can be read as signed 
integers. elunsigned    Pointer to an integer. Set to 1 if the 
elements can be read as unsigned integers. elements      Pointer to 
the destination number of elements. minelement    Pointer to the 
destination smallest element. maxelement    Pointer to the 
destination largest element. byteorder     Pointer to the destination 
byte order. dimfast       Pointer to the destination fastest 
dimension. dimmid        Pointer to the destination second fastest 
dimension. dimslow       Pointer to the destination third fastest 
dimension. padding       Pointer to the destination padding size.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_realarrayparameters_wdims_fs;

%cstring_output_allocate_size(char **bo, int *bolen, free(*$1));
%apply int *OUTPUT {int *compression,int *binary_id, 
                    int *elsize, 
                    int *elements,
                    int *dimslow, int *dimmid, int *dimfast, int *padding} 
                  get_realarrayparameters_wdims_fs;

    void get_realarrayparameters_wdims_fs(int *compression,int *binary_id, 
                        int *elsize, 
                        int *elements, 
                        char **bo, int *bolen,
                        int *dimfast, int *dimmid, int *dimslow, int *padding
                        ){
        unsigned int  comp;
        size_t elsiz, elem, df,dm,ds,pd;
        const char * byteorder;
        char * bot;
        cbf_failnez(cbf_get_realarrayparameters_wdims_fs(self, 
         &comp,binary_id, &elsiz, &elem, 
         &byteorder,&df,&dm,&ds,&pd ));
        *bolen = strlen(byteorder);
        if (!(bot = (char *)malloc(*bolen))) {cbf_failnez(CBF_ALLOC)}
        strncpy(bot,byteorder,*bolen);
        *bo = bot;
        *compression = comp;
        *elsize = elsiz;
        *elements = elem;
        *dimfast = df;
        *dimmid = dm;
        *dimslow = ds;
        *padding = pd;
        
        }
%feature("autodoc", "
Returns : (Binary)String
*args   : 

C prototype: int cbf_get_realarray (cbf_handle handle, int *binary_id,
                 void *array,    size_t elsize, size_t elements,
                 size_t *elements_read);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarray reads the binary value of the item at the 
current column and row into an integer array. The array consists of 
elements elements of elsize bytes each, starting at array. The 
elements are signed if elsigned is non-0 and unsigned otherwise. 
*binary_id is set to the binary section identifier and *elements_read 
to the number of elements actually read. cbf_get_realarray reads the 
binary value of the item at the current column and row into a real 
array. The array consists of elements elements of elsize bytes each, 
starting at array. *binary_id is set to the binary section identifier 
and *elements_read to the number of elements actually read.
If any element in the integer binary data cant fit into the 
destination element, the destination is set the nearest possible 
value.
If the value is not binary, the function returns CBF_ASCII.
If the requested number of elements cant be read, the function will 
read as many as it can and then return CBF_ENDOFDATA.
Currently, the destination array must consist of chars, shorts or 
ints (signed or unsigned). If elsize is not equal to sizeof (char), 
sizeof (short) or sizeof (int), for cbf_get_integerarray, or 
sizeof(double) or sizeof(float), for cbf_get_realarray the function 
returns CBF_ARGUMENT.
An additional restriction in the current version of CBFlib is that 
values too large to fit in an int are not correctly decompressed. As 
an example, if the machine with 32-bit ints is reading an array 
containing a value outside the range 0 .. 2^^32-1 (unsigned) or 
-2^^31 .. 2^^31-1 (signed), the array will not be correctly 
decompressed. This restriction will be removed in a future release. 
For cbf_get_realarray, only IEEE format is supported. No conversion 
to other floating point formats is done at this time.
ARGUMENTS
handle          CBF handle. binary_id       Pointer to the 
destination integer binary identifier. array           Pointer to the 
destination array. elsize          Size in bytes of each destination 
array element. elsigned        Set to non-0 if the destination array 
elements are signed. elements        The number of elements to read. 
elements_read   Pointer to the destination number of elements 
actually read.
RETURN VALUE
Returns an error code on failure or 0 for success. SEE ALSO
")get_realarray_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_realarray_as_string;

// Get the length correct

    void get_realarray_as_string(char **s, int *slen){
        int binary_id;
        size_t elements, elements_read, elsize;
        unsigned int compression;
        void * array;
        *slen = 0; /* Initialise in case of problems */
        cbf_failnez(cbf_get_realarrayparameters(self, &compression,
               &binary_id, &elsize,
               &elements));

        if ((array=malloc(elsize*elements))) {
              /* cbf_failnez (cbf_select_column(cbf,colnum)) */
               cbf_failnez (cbf_get_realarray(self, &binary_id, 
                            (void *)array, elsize,
                            elements, &elements_read));

         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*elements;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : Float slowbinsize,Float fastbinsize
*args   : Integer element_number

C prototype: int cbf_get_bin_sizes(cbf_handle handle,
                 unsigned int element_number,      double * slowbinsize,
                 double * fastbinsize);

CBFLib documentation:
DESCRIPTION
cbf_get_bin_sizes sets slowbinsize to point to the value of the 
number of pixels composing one array element in the dimension that 
changes at the second-fastest rate and fastbinsize to point to the 
value of the number of pixels composing one array element in the 
dimension that changes at the fastest rate for the dectector element 
with the ordinal element_number. cbf_set_bin_sizes sets the the pixel 
bin sizes in the  \"array_intensities \" category to the values of 
slowbinsize_in for the number of pixels composing one array element 
in the dimension that changes at the second-fastest rate and 
fastbinsize_in for the number of pixels composing one array element 
in the dimension that changes at the fastest rate for the dectector 
element with the ordinal element_number.
In order to allow for software binning involving fractions of pixels, 
the bin sizes are doubles rather than ints.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. slowbinsize      Pointer to the 
returned number of pixels composing one array element in the 
dimension that changes at the second-fastest rate. fastbinsize      
Pointer to the returned number of pixels composing one array element 
in the dimension that changes at the fastest rate. slowbinsize_in   
The number of pixels composing one array element in the dimension 
that changes at the second-fastest rate. fastbinsize_in   The number 
of pixels composing one array element in the dimension that changes 
at the fastest rate.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_bin_sizes;

%apply double *OUTPUT {double *slowbinsize,double *fastbinsize};
  void get_bin_sizes(int element_number, double *slowbinsize, double *fastbinsize) {
    cbf_failnez(cbf_get_bin_sizes (self, (unsigned int)element_number, slowbinsize, fastbinsize));
  }

/* cfunc cbf_reset_category   pyfunc reset_category  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_reset_category (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_reset_category deletes all columns and rows from current category.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")reset_category;
    void reset_category(void){
      cbf_failnez(cbf_reset_category(self));}
%feature("autodoc", "
Returns : Integer
*args   : String axis_id

C prototype: int cbf_count_axis_ancestors (cbf_handle handle,
                 const char *axis_id,      unsigned int *ancestors);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")count_axis_ancestors;

%apply int *OUTPUT {int *ancestors} count_axis_ancestors;
   void count_axis_ancestors(const char *axis_id,
                    int *ancestors){
        unsigned int anc;
        cbf_failnez(cbf_count_axis_ancestors(self,axis_id,&anc));
        *ancestors = anc;
        }
%feature("autodoc", "
Returns : pycbf goniometer object
*args   : 

C prototype: int cbf_construct_goniometer (cbf_handle handle,
                 cbf_goniometer      *goniometer);

CBFLib documentation:
DESCRIPTION
cbf_construct_goniometer constructs a goniometer object using the 
description in the CBF object handle and initialises the goniometer 
handle *goniometer.
ARGUMENTS
handle       CBF handle. goniometer   Pointer to the destination 
goniometer handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")construct_goniometer;

 cbf_goniometer construct_goniometer(){
    cbf_goniometer goniometer;
    cbf_failnez(cbf_construct_goniometer(self,&goniometer));
    return goniometer;
    }

/* cfunc cbf_set_datablockname   pyfunc set_datablockname  
   arg cbf_handle handle    arg const char *datablockname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_set_datablockname (cbf_handle handle,
                 const char *datablockname);

CBFLib documentation:
DESCRIPTION
cbf_set_datablockname changes the name of the current data block to 
datablockname. cbf_set_saveframename changes the name of the current 
save frame to saveframename.
If a data block or save frame with this name already exists 
(comparison is case-insensitive), the function returns CBF_IDENTICAL.
ARGUMENTS
handle          CBF handle. datablockname   The new data block name. 
saveframename   The new save frame name.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_datablockname;
    void set_datablockname(const char* arg){
      cbf_failnez(cbf_set_datablockname(self,arg));}

/* cfunc cbf_get_element_number   pyfunc get_element_number  
   arg cbf_handle handle    arg const char *element_id    arg const char *array_id    arg const char *array_section_id    arg unsigned int      *element_number */

     void get_element_number(void){
        cbf_failnez(CBF_NOTIMPLEMENTED);}

/* cfunc cbf_set_crystal_id   pyfunc set_crystal_id  
   arg cbf_handle handle    arg const char *crystal_id */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_set_crystal_id (cbf_handle handle,
                 const char *crystal_id);

CBFLib documentation:
DESCRIPTION
cbf_set_crystal_id sets the  \"diffrn.crystal_id \" entry to the 
ASCII value crystal_id.
ARGUMENTS
handle       CBF handle. crystal_id   ASCII value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_crystal_id;
    void set_crystal_id(const char* arg){
      cbf_failnez(cbf_set_crystal_id(self,arg));}
%feature("autodoc", "
Returns : (Binary)String
*args   : 

C prototype: int cbf_get_integerarray (cbf_handle handle, int *binary_id,
                 void *array,    size_t elsize, int elsigned, size_t elements,
                 size_t *elements_read);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarray reads the binary value of the item at the 
current column and row into an integer array. The array consists of 
elements elements of elsize bytes each, starting at array. The 
elements are signed if elsigned is non-0 and unsigned otherwise. 
*binary_id is set to the binary section identifier and *elements_read 
to the number of elements actually read. cbf_get_realarray reads the 
binary value of the item at the current column and row into a real 
array. The array consists of elements elements of elsize bytes each, 
starting at array. *binary_id is set to the binary section identifier 
and *elements_read to the number of elements actually read.
If any element in the integer binary data cant fit into the 
destination element, the destination is set the nearest possible 
value.
If the value is not binary, the function returns CBF_ASCII.
If the requested number of elements cant be read, the function will 
read as many as it can and then return CBF_ENDOFDATA.
Currently, the destination array must consist of chars, shorts or 
ints (signed or unsigned). If elsize is not equal to sizeof (char), 
sizeof (short) or sizeof (int), for cbf_get_integerarray, or 
sizeof(double) or sizeof(float), for cbf_get_realarray the function 
returns CBF_ARGUMENT.
An additional restriction in the current version of CBFlib is that 
values too large to fit in an int are not correctly decompressed. As 
an example, if the machine with 32-bit ints is reading an array 
containing a value outside the range 0 .. 2^^32-1 (unsigned) or 
-2^^31 .. 2^^31-1 (signed), the array will not be correctly 
decompressed. This restriction will be removed in a future release. 
For cbf_get_realarray, only IEEE format is supported. No conversion 
to other floating point formats is done at this time.
ARGUMENTS
handle          CBF handle. binary_id       Pointer to the 
destination integer binary identifier. array           Pointer to the 
destination array. elsize          Size in bytes of each destination 
array element. elsigned        Set to non-0 if the destination array 
elements are signed. elements        The number of elements to read. 
elements_read   Pointer to the destination number of elements 
actually read.
RETURN VALUE
Returns an error code on failure or 0 for success. SEE ALSO
")get_integerarray_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_integerarray_as_string;

// Get the length correct

    void get_integerarray_as_string(char **s, int *slen){
        int binary_id, elsigned, elunsigned;
        size_t elements, elements_read, elsize;
        int minelement, maxelement;
        unsigned int compression;
        void * array;
        *slen = 0; /* Initialise in case of problems */
        cbf_failnez(cbf_get_integerarrayparameters(self, &compression,
               &binary_id, &elsize, &elsigned, &elunsigned,
               &elements, &minelement, &maxelement));

        if ((array=malloc(elsize*elements))) {
              /* cbf_failnez (cbf_select_column(cbf,colnum)) */
               cbf_failnez (cbf_get_integerarray(self, &binary_id, 
                            (void *)array, elsize, elsigned,
                            elements, &elements_read));

         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*elements;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int elsign,int dimslow,int dimmid,int dimfast

C prototype: int cbf_set_3d_image (cbf_handle handle, unsigned int reserved,
                 unsigned      int element_number, unsigned int compression,
                 void *array, size_t      elsize, int elsign, size_t ndimslow,
                 size_t ndimmid, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_3d_image;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_3d_image;

    void set_3d_image(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int elsign, int ndimslow, int ndimmid, int ndimfast){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimmid*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_3d_image (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, elsign, (size_t) ndimslow, (size_t) ndimmid, (size_t)ndimfast)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : 
*args   : CBFHandle dictionary

C prototype: int cbf_set_dictionary (cbf_handle handle,
                 cbf_handle dictionary_in);

CBFLib documentation:
DESCRIPTION
cbf_get_dictionary sets *dictionary to the handle of a CBF which has 
been associated with the CBF handle by cbf_set_dictionary. 
cbf_set_dictionary associates the CBF handle dictionary_in with 
handle as its dictionary. cbf_require_dictionary sets *dictionary to 
the handle of a CBF which has been associated with the CBF handle by 
cbf_set_dictionary or creates a new empty CBF and associates it with 
handle, returning the new handle in *dictionary.
ARGUMENTS
handle          CBF handle. dictionary      Pointer to CBF handle of 
dictionary. dictionary_in   CBF handle of dcitionary.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_dictionary;

void set_dictionary(cbf_handle other){
   cbf_failnez(cbf_set_dictionary(self,other));
}
%feature("autodoc", "
Returns : String categoryname
*args   : String tagname

C prototype: int cbf_find_tag_category (cbf_handle handle,
                 const char* tagname, const      char** categoryname);

CBFLib documentation:
DESCRIPTION
cbf_find_tag_category sets categoryname to the category associated 
with tagname in the dictionary associated with handle. 
cbf_set_tag_category upddates the dictionary associated with handle 
to indicated that tagname is in category categoryname_in.
ARGUMENTS
handle            CBF handle. tagname           tag name. 
categoryname      pointer to a returned category name. 
categoryname_in   input category name.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")find_tag_category;


   const char * find_tag_category(const char *tagname){
     const char * result;
     cbf_failnez(cbf_find_tag_category(self,tagname, &result));
     return result;
     }
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int ndimslow,int ndimmid,int ndimfast

C prototype: int cbf_get_real_3d_image_sf (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 void *array, size_t elsize, size_t      ndimslow,
                 size_t ndimmid, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_real_3d_image_sf_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_real_3d_image_sf_as_string;

// Get the length correct

    void get_real_3d_image_sf_as_string(int element_number, char **s, int *slen,
    int elsize, int ndimslow, int ndimmid, int ndimfast){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimmid*ndimslow))) {
               cbf_failnez (cbf_get_real_3d_image_sf(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize,
               (size_t) ndimslow, (size_t)ndimmid, (size_t)ndimfast));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimmid*ndimslow;
        *s = (char *) array;
      }

/* cfunc cbf_set_typeofvalue   pyfunc set_typeofvalue  
   arg cbf_handle handle    arg const char *typeofvalue */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_set_typeofvalue (cbf_handle handle,
                 const char *typeofvalue);

CBFLib documentation:
DESCRIPTION
cbf_set_typeofvalue sets the type of the item at the current column 
and row to the type specified by the ASCII character string given by 
typeofvalue. The strings that may be used are:
 \"null \" for a null value indicated by a  \". \" or a  \"? \"  
\"bnry \" for a binary value  \"word \" for an unquoted string  
\"dblq \" for a double-quoted string  \"sglq \" for a single-quoted 
string  \"text \" for a semicolon-quoted string (multiline text 
field)  \"prns \" for a parenthesis-bracketed string (multiline text 
field)  \"brcs \" for a brace-bracketed string (multiline text field) 
 \"bkts \" for a square-bracket-bracketed string (multiline text 
field)  \"tsqs \" for a treble-single-quote quoted string (multiline 
text field)  \"tdqs \" for a treble-double-quote quoted string 
(multiline text field)
Not all types may be used for all values. Not all types are valid for 
all type of CIF files. In partcular the types  \"prns \",  \"brcs \", 
 \"bkts \" were introduced with DDLm and are not valid in DDL1 or 
DDL2 CIFS. The types  \"tsqs \" and  \"tdqs \" are not formally part 
of the CIF syntax. No changes may be made to the type of binary 
values. You may not set the type of a string that contains a single 
quote followed by a blank or a tab or which contains multiple lines 
to  \"sglq \". You may not set the type of a string that contains a 
double quote followed by a blank or a tab or which contains multiple 
lines to  \"dblq \".
ARGUMENTS
handle        CBF handle. typeofvalue   ASCII string for desired type 
of value.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_typeofvalue;
    void set_typeofvalue(const char* arg){
      cbf_failnez(cbf_set_typeofvalue(self,arg));}
%feature("autodoc", "
Returns : String
*args   : String axis_id

C prototype: int cbf_get_axis_rotation_axis (cbf_handle handle,
                 const char *axis_id,      const char * *rotation_axis);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_rotation_axis;

   const char *  get_axis_rotation_axis(const char *axis_id){
        const char* rot_axis;
        cbf_failnez(cbf_get_axis_rotation_axis(self,axis_id,
           &rot_axis));
        return rot_axis;
        }
%feature("autodoc", "
Returns : 
*args   : int compression,int binary_id,(binary) String data,int elsize,
          int elsigned,int elements,String byteorder,int dimfast,int dimmid,
          int dimslow,int padding

C prototype: int cbf_set_integerarray_wdims (cbf_handle handle,
                 unsigned int    compression, int binary_id, void *array,
                 size_t elsize, int elsigned,    size_t elements,
                 const char *byteorder, size_t dimfast, size_t dimmid,
                    size_t dimslow, size_t padding);

CBFLib documentation:
DESCRIPTION
cbf_set_integerarray sets the binary value of the item at the current 
column and row to an integer array. The array consists of elements 
elements of elsize bytes each, starting at array. The elements are 
signed if elsigned is non-0 and unsigned otherwise. binary_id is the 
binary section identifier. cbf_set_realarray sets the binary value of 
the item at the current column and row to an integer array. The array 
consists of elements elements of elsize bytes each, starting at 
array. binary_id is the binary section identifier.
The cbf_set_integerarray_wdims, cbf_set_integerarray_wdims_fs, 
cbf_set_integerarray_wdims_sf, cbf_set_realarray_wdims, 
cbf_set_realarray_wdims_fs and cbf_set_realarray_wdims_sf variants 
allow the data header values of byteorder, dimfast, dimmid, dimslow 
and padding to be set to the data byte order, the fastest, second 
fastest and third fastest array dimensions and the size in byte of 
the post data padding to be used.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL       Canonical-code compression (section 3.3.1) 
CBF_PACKED          CCP4-style packing (section 3.3.2) CBF_PACKED_V2  
     CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET    
 Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE            No compression. 
NOTE: This scheme is by far the slowest of the four and uses much 
more disk space. It is intended for routine use with small arrays 
only. With large arrays (like images) it should be used only for 
debugging.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned), for cbf_set_integerarray, or IEEE doubles or 
floats for cbf_set_realarray. If elsize is not equal to sizeof 
(char), sizeof (short) or sizeof (int), the function returns 
CBF_ARGUMENT.
ARGUMENTS
handle        CBF handle. compression   Compression method to use. 
binary_id     Integer binary identifier. array         Pointer to the 
source array. elsize        Size in bytes of each source array 
element. elsigned      Set to non-0 if the source array elements are 
signed. elements      The number of elements in the array
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_integerarray_wdims;

    /* CBFlib must NOT modify the data string nor the byteorder string
       which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_integerarray_wdims;
%apply (char *STRING, int LENGTH) { (char *bo, int bolen) } set_integerarray_wdims;

    void set_integerarray_wdims(unsigned int compression, int binary_id, 
             char *data, int len, int elsize, int elsigned, int elements,
             char *bo, int bolen, int dimfast, int dimmid, int dimslow, int padding){
        /* safety check on args */
        size_t els, ele;
        void *array;
        char byteorder[15];
        if(len == elsize*elements && elements==dimfast*dimmid*dimslow){
           array = data;
           els = elsize;
           ele = elements;
           strncpy(byteorder,bo,bolen<15?bolen:14);
           byteorder[bolen<15?bolen:14] = 0;
           cbf_failnez(cbf_set_integerarray_wdims (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, elsigned, (size_t) elements, (const char *)byteorder,
           (size_t)dimfast, (size_t)dimmid, (size_t)dimslow, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : 
*args   : Float time

C prototype: int cbf_set_integration_time (cbf_handle handle,
                 unsigned int reserved,      double time);

CBFLib documentation:
DESCRIPTION
cbf_set_integration_time sets the integration time in seconds to the 
value specified by time. The parameter reserved is presently unused 
and should be set to 0.
ARGUMENTS
handle     CBF handle. reserved   Unused. Any value other than 0 is 
invalid. time       Integration time in seconds.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_integration_time;

   void set_integration_time(double time){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_set_integration_time(self,reserved,time));
        }
%feature("autodoc", "
Returns : 
*args   : String axis_id,Float start,Float increment

C prototype: int cbf_set_axis_setting (cbf_handle handle,
                 unsigned int reserved,      const char *axis_id, double start,
                 double increment);

CBFLib documentation:
DESCRIPTION
cbf_set_axis_setting sets the starting and increment values of the 
axis axis_id to start and increment.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle      CBF handle. reserved    Unused. Any value other than 0 is 
invalid. axis_id     Axis id. start       Start value. increment   
Increment value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_axis_setting;

   void set_axis_setting(const char *axis_id,
                    double start, double increment){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_set_axis_setting(self,reserved,
                         axis_id,start,increment));
        }
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int ndimslow,int ndimfast

C prototype: int cbf_get_real_image (cbf_handle handle, unsigned int reserved,
                      unsigned int element_number, void *array, size_t elsize,
                 size_t      ndimslow, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_real_image_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_real_image_as_string;

// Get the length correct

    void get_real_image_as_string(int element_number, char **s, int *slen,
    int elsize, int ndimslow, int ndimfast){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimslow))) {
               cbf_failnez (cbf_get_real_image(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize,
               (size_t) ndimslow, (size_t)ndimfast));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimslow;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : String
*args   : String axis_id,Integer ancestor_index

C prototype: int cbf_get_axis_ancestor (cbf_handle handle,
                 const char *axis_id, const      unsigned int ancestor_index,
                 const char * *ancestor);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_ancestor;

   const char *  get_axis_ancestor(const char *axis_id,
                    int ancestor_index){
        const char* anc;
        cbf_failnez(cbf_get_axis_ancestor(self,axis_id,
           (unsigned int)ancestor_index,&anc));
        return anc;
        }
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int elsign,int ndimslow,int ndimmid,
          int ndimfast

C prototype: int cbf_get_3d_image_sf (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 void *array, size_t elsize, int elsign,      size_t ndimslow,
                 size_t ndimmid, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_3d_image_sf_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_3d_image_sf_as_string;

// Get the length correct

    void get_3d_image_sf_as_string(int element_number, char **s, int *slen,
    int elsize, int elsign, int ndimfast, int ndimmid, int ndimslow){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimmid*ndimslow))) {
               cbf_failnez (cbf_get_3d_image_sf(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize, elsign,
               (size_t) ndimslow, (size_t)ndimmid, (size_t)ndimfast));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimmid*ndimslow;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int dimfast,int dimslow

C prototype: int cbf_set_real_image_fs(cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 unsigned int compression, void      *array,size_t elsize,
                 size_t ndimfast, size_t ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_real_image_fs;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_real_image;

    void set_real_image_fs(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int ndimfast, int ndimslow){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_real_image_fs (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, (size_t) ndimfast, (size_t)ndimslow)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : Float overload
*args   : Integer element_number

C prototype: int cbf_get_overload (cbf_handle handle,
                 unsigned int element_number,      double *overload);

CBFLib documentation:
DESCRIPTION
cbf_get_overload sets *overload to the overload value for element 
number element_number.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. overload         Pointer to the 
destination overload.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_overload;

%apply double *OUTPUT {double *overload} get_overload;
   void get_overload(unsigned int element_number, double *overload){
        cbf_failnez(cbf_get_overload(self,element_number,overload));
        }

/* cfunc cbf_get_wavelength   pyfunc get_wavelength  
   arg cbf_handle handle    arg double *wavelength */

%feature("autodoc", "
Returns : double
*args   : 

C prototype: int cbf_get_wavelength (cbf_handle handle, double *wavelength);

CBFLib documentation:
DESCRIPTION
cbf_get_wavelength sets *wavelength to the current wavelength in Å.
ARGUMENTS
handle       CBF handle. wavelength   Pointer to the destination.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_wavelength;
    double get_wavelength(void){
     double result;
       cbf_failnez(cbf_get_wavelength(self,&result));
       return result;}

/* cfunc cbf_next_datablock   pyfunc next_datablock  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_next_datablock (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_next_datablock makes the data block following the current data 
block the current data block.
If there are no more data blocks, the function returns CBF_NOTFOUND.
The current category becomes undefined.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")next_datablock;
    void next_datablock(void){
      cbf_failnez(cbf_next_datablock(self));}
%feature("autodoc", "
Returns : int compression,int binary_id,int elsize,int elements,char **bo,
          int *bolen,int dimfast,int dimmid,int dimslow,int padding
*args   : 

C prototype: int cbf_get_realarrayparameters_wdims (cbf_handle handle,
                 unsigned int    *compression, int *binary_id, size_t *elsize,
                 size_t *elements, const char    **byteorder, size_t *dimfast,
                 size_t *dimmid, size_t *dimslow, size_t    *padding);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarrayparameters sets *compression, *binary_id, 
*elsize, *elsigned, *elunsigned, *elements, *minelement and 
*maxelement to values read from the binary value of the item at the 
current column and row. This provides all the arguments needed for a 
subsequent call to cbf_set_integerarray, if a copy of the array is to 
be made into another CIF or CBF. cbf_get_realarrayparameters sets 
*compression, *binary_id, *elsize, *elements to values read from the 
binary value of the item at the current column and row. This provides 
all the arguments needed for a subsequent call to cbf_set_realarray, 
if a copy of the arry is to be made into another CIF or CBF.
The variants cbf_get_integerarrayparameters_wdims, 
cbf_get_integerarrayparameters_wdims_fs, 
cbf_get_integerarrayparameters_wdims_sf, 
cbf_get_realarrayparameters_wdims, 
cbf_get_realarrayparameters_wdims_fs, 
cbf_get_realarrayparameters_wdims_sf set **byteorder, *dimfast, 
*dimmid, *dimslow, and *padding as well, providing the additional 
parameters needed for a subsequent call to cbf_set_integerarray_wdims 
or cbf_set_realarray_wdims.
The value returned in *byteorder is a pointer either to the string  
\"little_endian \" or to the string  \"big_endian \". This should be 
the byte order of the data, not necessarily of the host machine. No 
attempt should be made to modify this string. At this time only  
\"little_endian \" will be returned.
The values returned in *dimfast, *dimmid and *dimslow are the sizes 
of the fastest changing, second fastest changing and third fastest 
changing dimensions of the array, if specified, or zero, if not 
specified.
The value returned in *padding is the size of the post-data padding, 
if any and if specified in the data header. The value is given as a 
count of octets.
If the value is not binary, the function returns CBF_ASCII.
ARGUMENTS
handle        CBF handle. compression   Compression method used. 
elsize        Size in bytes of each array element. binary_id     
Pointer to the destination integer binary identifier. elsigned      
Pointer to an integer. Set to 1 if the elements can be read as signed 
integers. elunsigned    Pointer to an integer. Set to 1 if the 
elements can be read as unsigned integers. elements      Pointer to 
the destination number of elements. minelement    Pointer to the 
destination smallest element. maxelement    Pointer to the 
destination largest element. byteorder     Pointer to the destination 
byte order. dimfast       Pointer to the destination fastest 
dimension. dimmid        Pointer to the destination second fastest 
dimension. dimslow       Pointer to the destination third fastest 
dimension. padding       Pointer to the destination padding size.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_realarrayparameters_wdims;

%cstring_output_allocate_size(char **bo, int *bolen, free(*$1));
%apply int *OUTPUT {int *compression,int *binary_id, 
                    int *elsize, 
                    int *elements,
                    int *dimslow, int *dimmid, int *dimfast, int *padding} 
                  get_realarrayparameters_wdims;

    void get_realarrayparameters_wdims(int *compression,int *binary_id, 
                        int *elsize, 
                        int *elements, 
                        char **bo, int *bolen,
                        int *dimfast, int *dimmid, int *dimslow, int *padding
                        ){
        unsigned int  comp;
        size_t elsiz, elem, df,dm,ds,pd;
        const char * byteorder;
        char * bot;
        cbf_failnez(cbf_get_realarrayparameters_wdims(self, 
         &comp,binary_id, &elsiz, &elem, 
         &byteorder,&df,&dm,&ds,&pd ));
        *bolen = strlen(byteorder);
        if (!(bot = (char *)malloc(*bolen))) {cbf_failnez(CBF_ALLOC)}
        strncpy(bot,byteorder,*bolen);
        *bo = bot;
        *compression = comp;
        *elsize = elsiz;
        *elements = elem;
        *dimfast = df;
        *dimmid = dm;
        *dimslow = ds;
        *padding = pd;
        
        }
%feature("autodoc", "
Returns : 
*args   : Float matrix_0,Float matrix_1,Float matrix_2,Float matrix_3,
          Float matrix_4,Float matrix_5,Float matrix_6,Float matrix_7,
          Float matrix_8

C prototype: int cbf_set_orientation_matrix (cbf_handle handle,
                 double ub_matrix[9]);

CBFLib documentation:
DESCRIPTION
cbf_get_orientation_matrix sets ub_matrix to point to the array of 
orientation matrix entries in the  \"diffrn \" category in the order 
of columns:
 \"UB[1][1] \"  \"UB[1][2] \"  \"UB[1][3] \"  \"UB[2][1] \"  
\"UB[2][2] \"  \"UB[2][3] \"  \"UB[3][1] \"  \"UB[3][2] \"  
\"UB[3][3] \"
cbf_set_orientation_matrix sets the values in the  \"diffrn \" 
category to the values pointed to by ub_matrix.
ARGUMENTS
handle      CBF handle. ub_matrix   Source or destination array of 9 
doubles giving the orientation matrix parameters.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_orientation_matrix;

   void set_orientation_matrix(  double m0,double m1,
double  m2,double  m3,double  m4,double m5,double m6,
double  m7,double  m8){
        double m[9];
        m[0] = m0; m[1]=m1 ; m[2]=m2 ;
        m[3] = m3; m[4]=m4 ; m[5]=m5 ;
        m[6] = m6; m[7]=m7 ; m[8]=m8 ;
        cbf_failnez(cbf_get_orientation_matrix(self,m));
        }

/* cfunc cbf_new_category   pyfunc new_category  
   arg cbf_handle handle    arg const char *categoryname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_new_category (cbf_handle handle,
                 const char *categoryname);

CBFLib documentation:
DESCRIPTION
cbf_new_category creates a new category in the current data block 
with name categoryname and makes it the current category.
If a category with this name already exists, the existing category 
becomes the current category.
ARGUMENTS
handle         CBF handle. categoryname   The name of the new 
category.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")new_category;
    void new_category(const char* arg){
      cbf_failnez(cbf_new_category(self,arg));}
%feature("autodoc", "
Returns : 
*args   : Float gain,Float gain_esd

C prototype: int cbf_set_gain (cbf_handle handle, unsigned int element_number,
                 double      gain, double gain_esd);

CBFLib documentation:
DESCRIPTION
cbf_set_gain sets the gain of element number element_number to the 
values specified by gain and gain_esd.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. gain             New gain value. 
gain_esd         New gain_esd value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_gain;

    void set_gain (unsigned int element_number, double gain, double gain_esd){
        cbf_failnez(cbf_set_gain (self, element_number, gain, gain_esd));
        }

/* cfunc cbf_find_column   pyfunc find_column  
   arg cbf_handle handle    arg const char *columnname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_find_column (cbf_handle handle, const char *columnname);

CBFLib documentation:
DESCRIPTION
cbf_find_column makes the columns in the current category with name 
columnname the current column.
The comparison is case-insensitive.
If the column does not exist, the function returns CBF_NOTFOUND.
The current row is not affected.
ARGUMENTS
handle       CBF handle. columnname   The name of column to find.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")find_column;
    void find_column(const char* arg){
      cbf_failnez(cbf_find_column(self,arg));}

/* cfunc cbf_remove_category   pyfunc remove_category  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_remove_category (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_remove_category deletes the current category.
The current category becomes undefined.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")remove_category;
    void remove_category(void){
      cbf_failnez(cbf_remove_category(self));}
%feature("autodoc", "
Returns : int compression,int binary_id,int elsize,int elsigned,int elunsigned,
          int elements,int minelement,int maxelement,char **bo,int *bolen,
          int dimslow,int dimmid,int dimfast,int padding
*args   : 

C prototype: int cbf_get_integerarrayparameters_wdims_sf (cbf_handle handle,
                 unsigned    int *compression, int *binary_id, size_t *elsize,
                 int *elsigned, int    *elunsigned, size_t *elements,
                 int *minelement, int *maxelement, const    char **byteorder,
                 size_t *dimslow, size_t *dimmid, size_t *dimfast,
                 size_t    *padding);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarrayparameters sets *compression, *binary_id, 
*elsize, *elsigned, *elunsigned, *elements, *minelement and 
*maxelement to values read from the binary value of the item at the 
current column and row. This provides all the arguments needed for a 
subsequent call to cbf_set_integerarray, if a copy of the array is to 
be made into another CIF or CBF. cbf_get_realarrayparameters sets 
*compression, *binary_id, *elsize, *elements to values read from the 
binary value of the item at the current column and row. This provides 
all the arguments needed for a subsequent call to cbf_set_realarray, 
if a copy of the arry is to be made into another CIF or CBF.
The variants cbf_get_integerarrayparameters_wdims, 
cbf_get_integerarrayparameters_wdims_fs, 
cbf_get_integerarrayparameters_wdims_sf, 
cbf_get_realarrayparameters_wdims, 
cbf_get_realarrayparameters_wdims_fs, 
cbf_get_realarrayparameters_wdims_sf set **byteorder, *dimfast, 
*dimmid, *dimslow, and *padding as well, providing the additional 
parameters needed for a subsequent call to cbf_set_integerarray_wdims 
or cbf_set_realarray_wdims.
The value returned in *byteorder is a pointer either to the string  
\"little_endian \" or to the string  \"big_endian \". This should be 
the byte order of the data, not necessarily of the host machine. No 
attempt should be made to modify this string. At this time only  
\"little_endian \" will be returned.
The values returned in *dimfast, *dimmid and *dimslow are the sizes 
of the fastest changing, second fastest changing and third fastest 
changing dimensions of the array, if specified, or zero, if not 
specified.
The value returned in *padding is the size of the post-data padding, 
if any and if specified in the data header. The value is given as a 
count of octets.
If the value is not binary, the function returns CBF_ASCII.
ARGUMENTS
handle        CBF handle. compression   Compression method used. 
elsize        Size in bytes of each array element. binary_id     
Pointer to the destination integer binary identifier. elsigned      
Pointer to an integer. Set to 1 if the elements can be read as signed 
integers. elunsigned    Pointer to an integer. Set to 1 if the 
elements can be read as unsigned integers. elements      Pointer to 
the destination number of elements. minelement    Pointer to the 
destination smallest element. maxelement    Pointer to the 
destination largest element. byteorder     Pointer to the destination 
byte order. dimfast       Pointer to the destination fastest 
dimension. dimmid        Pointer to the destination second fastest 
dimension. dimslow       Pointer to the destination third fastest 
dimension. padding       Pointer to the destination padding size.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_integerarrayparameters_wdims_sf;

%cstring_output_allocate_size(char **bo, int *bolen, free(*$1));
%apply int *OUTPUT {int *compression,int *binary_id, 
                    int *elsize, int *elsigned, int *elunsigned, 
                    int *elements, int *minelement, int *maxelement,
                    int *dimslow, int *dimmid, int *dimfast, int *padding} 
                  get_integerarrayparameters_wdims_sf;

    void get_integerarrayparameters_wdims_sf(int *compression,int *binary_id, 
                        int *elsize, int *elsigned, int *elunsigned, 
                        int *elements, int *minelement, int *maxelement,
                        char **bo, int *bolen,
                        int *dimslow, int *dimmid, int *dimfast, int *padding
                        ){
        unsigned int  comp;
        size_t elsiz, elem, df,dm,ds,pd;
        const char * byteorder;
        char * bot;
        cbf_failnez(cbf_get_integerarrayparameters_wdims_sf(self, 
         &comp,binary_id, &elsiz, elsigned, elunsigned, &elem, 
          minelement, maxelement, &byteorder,&ds,&dm,&df,&pd ));
        *bolen = strlen(byteorder);
        if (!(bot = (char *)malloc(*bolen))) {cbf_failnez(CBF_ALLOC)}
        strncpy(bot,byteorder,*bolen);
        *bo = bot;
        *compression = comp;
        *elsize = elsiz;
        *elements = elem;
        *dimfast = df;
        *dimmid = dm;
        *dimslow = ds;
        *padding = pd;
        
        }
%feature("autodoc", "
Returns : Float pixel_size
*args   : Int element_number,Int axis_number

C prototype: int cbf_get_pixel_size (cbf_handle handle,
                 unsigned int element_number,      int axis_number,
                 double *psize);

CBFLib documentation:
DESCRIPTION
cbf_get_pixel_size and cbf_get_pixel_size_sf set *psize to point to 
the double value in millimeters of the axis axis_number of the 
detector element element_number. The axis_number is numbered from 1, 
starting with the slowest axis. cbf_get_pixel_size_fs sets *psize to 
point to the double value in millimeters of the axis axis_number of 
the detector element element_number. The axis_number is numbered from 
1, starting with the fastest axis.
If a negative axis number is given, the order of axes is reversed, so 
that -1 specifies the slowest axis for cbf_get_pixel_size_fs and the 
fastest axis for cbf_get_pixel_size_sf.
If the pixel size is not given explcitly in the  \"array_element_size 
\" category, the function returns CBF_NOTFOUND.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. axis_number      The number of the 
axis, starting from 1 for the fastest for cbf_get_pixel_size and 
cbf_get_pixel_size_fs and the slowest for cbf_get_pixel_size_sf. 
psize            Pointer to the destination pixel size.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_pixel_size;

%apply double *OUTPUT {double *psize} get_pixel_size;
    void get_pixel_size(unsigned int element_number, 
                        unsigned int axis_number, double *psize){
        cbf_failnez(cbf_get_pixel_size(self, 
                                       element_number, 
                                       axis_number, 
                                       psize));
    }
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int dimslow,int dimfast

C prototype: int cbf_set_real_image_sf(cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 unsigned int compression, void      *array,size_t elsize,
                 size_t ndimslow, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_real_image_sf;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_real_image_sf;

    void set_real_image_sf(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int ndimslow, int ndimfast){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_real_image_sf (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, (size_t) ndimslow, (size_t)ndimfast)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }

/* cfunc cbf_require_category   pyfunc require_category  
   arg cbf_handle handle    arg const char *categoryname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_require_category (cbf_handle handle,
                 const char *categoryname);

CBFLib documentation:
DESCRIPTION
cbf_rewuire_category makes the category in the current data block 
with name categoryname the current category, if it exists, or creates 
the catagory if it does not exist.
The comparison is case-insensitive.
The current column and row become undefined.
ARGUMENTS
handle         CBF handle. categoryname   The name of the category to 
find.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")require_category;
    void require_category(const char* arg){
      cbf_failnez(cbf_require_category(self,arg));}
%feature("autodoc", "
Returns : Float astar,Float bstar,Float cstar,Float alphastar,Float betastar,
          Float gammastar
*args   : 

C prototype: int cbf_get_reciprocal_cell (cbf_handle handle, double cell[6],
                 double      cell_esd[6] );

CBFLib documentation:
DESCRIPTION
cbf_get_reciprocal_cell sets cell[0:2] to the double values of the 
reciprocal cell edge lengths a^*, b^* and c^* in Ångstroms^-1, 
cell[3:5] to the double values of the reciprocal cell angles α^*, 
β^* and γ^* in degrees, cell_esd[0:2] to the double values of the 
estimated strandard deviations of the reciprocal cell edge lengths 
a^*, b^* and c^* in Ångstroms^-1, cell_esd[3:5] to the double values 
of the estimated standard deviations of the the reciprocal cell 
angles α^*, β^* and γ^* in degrees.
The values returned are retrieved from the first row of the  \"cell 
\" category. The value of  \"_cell.entry_id \" is ignored.
cell or cell_esd may be NULL.
If cell is NULL, the reciprocal cell parameters are not retrieved.
If cell_esd is NULL, the reciprocal cell parameter esds are not 
retrieved.
If the  \"cell \" category is present, but some of the values are 
missing, zeros are returned for the missing values.
ARGUMENTS
handle     CBF handle. cell       Pointer to the destination array of 
6 doubles for the reciprocal cell parameters. cell_esd   Pointer to 
the destination array of 6 doubles for the reciprocal cell parameter 
esds.
RETURN VALUE
Returns an error code on failure or 0 for success. No errors is 
returned for missing values if the  \"cell \" category exists.
SEE ALSO
")get_reciprocal_cell;

%apply double *OUTPUT {double *astar, double *bstar, double *cstar,
  double *alphastar, double *betastar, double *gammastar} get_reciprocal_cell;
     void get_reciprocal_cell(double *astar, double *bstar, double *cstar,
  double *alphastar, double *betastar, double *gammastar) {
     double rcell[6];
     cbf_failnez(cbf_get_reciprocal_cell(self,rcell,NULL));
    *astar =      rcell[0];
    *bstar =      rcell[1];
    *cstar =      rcell[2];
    *alphastar =  rcell[3];
    *betastar =   rcell[4];
    *gammastar =  rcell[5];
   }
%feature("autodoc", "
Returns : doubleArray cell
*args   : 

C prototype: int cbf_get_reciprocal_cell (cbf_handle handle, double cell[6],
                 double      cell_esd[6] );

CBFLib documentation:
DESCRIPTION
cbf_get_reciprocal_cell sets cell[0:2] to the double values of the 
reciprocal cell edge lengths a^*, b^* and c^* in Ångstroms^-1, 
cell[3:5] to the double values of the reciprocal cell angles α^*, 
β^* and γ^* in degrees, cell_esd[0:2] to the double values of the 
estimated strandard deviations of the reciprocal cell edge lengths 
a^*, b^* and c^* in Ångstroms^-1, cell_esd[3:5] to the double values 
of the estimated standard deviations of the the reciprocal cell 
angles α^*, β^* and γ^* in degrees.
The values returned are retrieved from the first row of the  \"cell 
\" category. The value of  \"_cell.entry_id \" is ignored.
cell or cell_esd may be NULL.
If cell is NULL, the reciprocal cell parameters are not retrieved.
If cell_esd is NULL, the reciprocal cell parameter esds are not 
retrieved.
If the  \"cell \" category is present, but some of the values are 
missing, zeros are returned for the missing values.
ARGUMENTS
handle     CBF handle. cell       Pointer to the destination array of 
6 doubles for the reciprocal cell parameters. cell_esd   Pointer to 
the destination array of 6 doubles for the reciprocal cell parameter 
esds.
RETURN VALUE
Returns an error code on failure or 0 for success. No errors is 
returned for missing values if the  \"cell \" category exists.
SEE ALSO
")get_reciprocal_cell;

%apply double *OUTPUT {double *a_esd, double *b_esd, double *c_esd,
  double *alpha_esd, double *beta_esd, double *gamma_esd} get_reciprocal_cell_esd;
     void get_reciprocal_cell_esd(double *a_esd, double *b_esd, double *c_esd,
  double *alpha_esd, double *beta_esd, double *gamma_esd) {
     double cell_esd[6];
     cbf_failnez(cbf_get_reciprocal_cell(self,NULL,cell_esd));
     *a_esd = cell_esd[0];
     *b_esd = cell_esd[1];
     *c_esd = cell_esd[2];
     *alpha_esd = cell_esd[3];
     *beta_esd = cell_esd[4];
     *gamma_esd = cell_esd[5];
   }
%feature("autodoc", "
Returns : size_t ndimslow,size_t ndimmid,size_t ndimfast
*args   : Integer element_number

C prototype: int cbf_get_3d_image_size (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 size_t *ndimslow, size_t *ndimmid, size_t      *ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image_size, cbf_get_image_size_fs and cbf_get_image_size_sf 
set *ndimslow and *ndimfast to the slow and fast dimensions of the 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimfast 
will be set to 1. If the array is 3-dimensional an error code will be 
returned. cbf_get_3d_image_size, cbf_get_3d_image_size_fs and 
cbf_get_3d_image_size_sf set *ndimslow, *ndimmid and *ndimfast to the 
slowest, next fastest and fastest dimensions, respectively, of the 3D 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimmid 
and
")get_3d_image_size;

%apply int *OUTPUT {int *ndimslow, int *ndimmid, int *ndimfast} get_3d_image_size;
     void get_3d_image_size(unsigned int element_number, int *ndimslow, int *ndimmid, int *ndimfast){
        unsigned int reserved;
        size_t inslow, inmid, infast;
        reserved = 0;
        cbf_failnez(cbf_get_3d_image_size(self,reserved,element_number,&inslow,&inmid,&infast));
        *ndimslow = (int)inslow; /* FIXME - is that how to convert? */
        *ndimmid = (int)inmid; 
        *ndimfast = (int)infast;
        }
%feature("autodoc", "
Returns : String tagroot
*args   : String tagname

C prototype: int cbf_find_tag_root (cbf_handle handle, const char* tagname,
                 const      char** tagroot);

CBFLib documentation:
DESCRIPTION
cbf_find_tag_root sets *tagroot to the root tag of which tagname is 
an alias. cbf_set_tag_root sets tagname as an alias of tagroot_in in 
the dictionary associated with handle, creating the dictionary if 
necessary. cbf_require_tag_root sets *tagroot to the root tag of 
which tagname is an alias, if there is one, or to the value of 
tagname, if tagname is not an alias.
A returned tagroot string must not be modified in any way.
ARGUMENTS
handle       CBF handle. tagname      tag name which may be an alias. 
tagroot      pointer to a returned tag root name. tagroot_in   input 
tag root name.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")find_tag_root;

const char * find_tag_root(const char* tagname){
   const char* result;
   cbf_failnez(cbf_find_tag_root(self,tagname,&result));
   return result;
}
%feature("autodoc", "
Returns : String categoryroot
*args   : String Categoryname

C prototype: int cbf_require_category_root (cbf_handle handle,
                 const char*      categoryname, const char** categoryroot);

CBFLib documentation:
DESCRIPTION
cbf_find_category_root sets *categoryroot to the root category of 
which categoryname is an alias. cbf_set_category_root sets 
categoryname_in as an alias of categoryroot in the dictionary 
associated with handle, creating the dictionary if necessary. 
cbf_require_category_root sets *categoryroot to the root category of 
which categoryname is an alias, if there is one, or to the value of 
categoryname, if categoryname is not an alias.
A returned categoryroot string must not be modified in any way.
ARGUMENTS
handle            CBF handle. categoryname      category name which 
may be an alias. categoryroot      pointer to a returned category 
root name. categoryroot_in   input category root name.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")cbf_require_category_root;

const char* require_category_root (const char* categoryname){
  const char* result;
  cbf_failnez(cbf_require_category_root(self,categoryname, &result));
  return result;
}
%feature("autodoc", "
Returns : 
*args   : int compression,int binary_id,(binary) String data,int elsize,
          int elements,String byteorder,int dimslow,int dimmid,int dimfast,
          int padding

C prototype: int cbf_set_realarray_wdims_sf (cbf_handle handle,
                 unsigned int    compression, int binary_id, void *array,
                 size_t elsize, size_t elements,    const char *byteorder,
                 size_t dimslow, size_t dimmid, size_t dimfast,
                    size_t padding);

CBFLib documentation:
DESCRIPTION
cbf_set_integerarray sets the binary value of the item at the current 
column and row to an integer array. The array consists of elements 
elements of elsize bytes each, starting at array. The elements are 
signed if elsigned is non-0 and unsigned otherwise. binary_id is the 
binary section identifier. cbf_set_realarray sets the binary value of 
the item at the current column and row to an integer array. The array 
consists of elements elements of elsize bytes each, starting at 
array. binary_id is the binary section identifier.
The cbf_set_integerarray_wdims, cbf_set_integerarray_wdims_fs, 
cbf_set_integerarray_wdims_sf, cbf_set_realarray_wdims, 
cbf_set_realarray_wdims_fs and cbf_set_realarray_wdims_sf variants 
allow the data header values of byteorder, dimfast, dimmid, dimslow 
and padding to be set to the data byte order, the fastest, second 
fastest and third fastest array dimensions and the size in byte of 
the post data padding to be used.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL       Canonical-code compression (section 3.3.1) 
CBF_PACKED          CCP4-style packing (section 3.3.2) CBF_PACKED_V2  
     CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET    
 Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE            No compression. 
NOTE: This scheme is by far the slowest of the four and uses much 
more disk space. It is intended for routine use with small arrays 
only. With large arrays (like images) it should be used only for 
debugging.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned), for cbf_set_integerarray, or IEEE doubles or 
floats for cbf_set_realarray. If elsize is not equal to sizeof 
(char), sizeof (short) or sizeof (int), the function returns 
CBF_ARGUMENT.
ARGUMENTS
handle        CBF handle. compression   Compression method to use. 
binary_id     Integer binary identifier. array         Pointer to the 
source array. elsize        Size in bytes of each source array 
element. elsigned      Set to non-0 if the source array elements are 
signed. elements      The number of elements in the array
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_realarray_wdims_sf;

    /* CBFlib must NOT modify the data string nor the byteorder string
       which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_realarray_wdims_sf;
%apply (char *STRING, int LENGTH) { (char *bo, int bolen) } set_realarray_wdims_sf;

    void set_realarray_wdims_sf(unsigned int compression, int binary_id, 
             char *data, int len, int elsize, int elements,
             char *bo, int bolen, int dimslow, int dimmid, int dimfast, int padding){
        /* safety check on args */
        size_t els, ele;
        void *array;
        char byteorder[15];
        if(len == elsize*elements && elements==dimfast*dimmid*dimslow){
           array = data;
           els = elsize;
           ele = elements;
           strncpy(byteorder,bo,bolen<15?bolen:14);
           byteorder[bolen<15?bolen:14] = 0;
           cbf_failnez(cbf_set_realarray_wdims_sf (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, (size_t) elements, (const char *)byteorder,
           (size_t) dimslow, (size_t) dimmid, (size_t) dimfast, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }

/* cfunc cbf_set_integervalue   pyfunc set_integervalue  
   arg cbf_handle handle    arg int number */

%feature("autodoc", "
Returns : int number
*args   : 

C prototype: int cbf_set_integervalue (cbf_handle handle, int number);

CBFLib documentation:
DESCRIPTION
cbf_set_integervalue sets the item at the current column and row to 
the integer value number written as a decimal ASCII string.
ARGUMENTS
handle   CBF handle. number   Integer value.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_integervalue;
     void set_integervalue(int number){
        cbf_failnez(cbf_set_integervalue(self,number));}

/* cfunc cbf_category_name   pyfunc category_name  
   arg cbf_handle handle    arg const char **categoryname */

%feature("autodoc", "
Returns : 
*args   : string

C prototype: int cbf_category_name (cbf_handle handle,
                 const char **categoryname);

CBFLib documentation:
DESCRIPTION
cbf_category_name sets *categoryname to point to the name of the 
current category of the current data block.
The category name will be valid as long as the category exists.
The name must not be modified by the program in any way.
ARGUMENTS
handle         CBF handle. categoryname   Pointer to the destination 
category name pointer.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")category_name;
    const char* category_name(void){
    const char* result;
    cbf_failnez(cbf_category_name(self, &result));
    return result;}

/* cfunc cbf_get_typeofvalue   pyfunc get_typeofvalue  
   arg cbf_handle handle    arg const char **typeofvalue */

%feature("autodoc", "
Returns : 
*args   : string

C prototype: int cbf_get_typeofvalue (cbf_handle handle,
                 const char **typeofvalue);

CBFLib documentation:
DESCRIPTION
cbf_get_value sets *typeofvalue to point an ASCII descriptor of the 
value of the item at the current column and row. The strings that may 
be returned are:
 \"null \" for a null value indicated by a  \". \" or a  \"? \"  
\"bnry \" for a binary value  \"word \" for an unquoted string  
\"dblq \" for a double-quoted string  \"sglq \" for a single-quoted 
string  \"text \" for a semicolon-quoted string (multiline text 
field)  \"prns \" for a parenthesis-bracketed string (multiline text 
field)  \"brcs \" for a brace-bracketed string (multiline text field) 
 \"bkts \" for a square-bracket-bracketed string (multiline text 
field)  \"tsqs \" for a treble-single-quote quoted string (multiline 
text field)  \"tdqs \" for a treble-double-quote quoted string 
(multiline text field)
Not all types are valid for all type of CIF files. In partcular the 
types  \"prns \",  \"brcs \",  \"bkts \" were introduced with DDLm 
and are not valid in DDL1 or DDL2 CIFS. The types  \"tsqs \" and  
\"tdqs \" are not formally part of the CIF syntax. A field for which 
no value has been set sets *typeofvalue to NULL rather than to the 
string  \"null \".
The typeofvalue must not be modified by the program in any way.
ARGUMENTS
handle        CBF handle. typeofvalue   Pointer to the destination 
type-of-value string pointer.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_typeofvalue;
    const char* get_typeofvalue(void){
    const char* result;
    cbf_failnez(cbf_get_typeofvalue(self, &result));
    return result;}
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int dimslow,int dimfast

C prototype: int cbf_set_real_image (cbf_handle handle, unsigned int reserved,
                      unsigned int element_number, unsigned int compression,
                 void      *array,size_t elsize, size_t ndimslow,
                 size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_real_image;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_real_image;

    void set_real_image(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int ndimslow, int ndimfast){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_real_image (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, (size_t) ndimslow, (size_t)ndimfast)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int elsign,int ndimslow,int ndimmid,
          int ndimfast

C prototype: int cbf_get_3d_image (cbf_handle handle, unsigned int reserved,
                 unsigned      int element_number, void *array, size_t elsize,
                 int elsign, size_t      ndimslow, size_t ndimmid,
                 size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_3d_image_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_3d_image_as_string;

// Get the length correct

    void get_3d_image_as_string(int element_number, char **s, int *slen,
    int elsize, int elsign, int ndimfast, int ndimmid, int ndimslow){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimmid*ndimslow))) {
               cbf_failnez (cbf_get_3d_image(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize, elsign,
               (size_t) ndimslow, (size_t)ndimmid, (size_t)ndimfast));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimmid*ndimslow;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : Float vector1,Float vector2,Float vector3,Float offset1,Float offset2,
          Float offset3
*args   : String axis_id

C prototype: int cbf_get_axis_reference_poise(cbf_handle handle,
                 double * vector1,      double * vector2, double * vector3,
                 double * offset1, double * offset2,      double * offset3,
                 const char * axis_id);

CBFLib documentation:
DESCRIPTION
cbf_get_axis_poise sets vector1, vector2, vector3 to point to the 
components of the axis vector for axis axis_id, offset1, offset2, 
offset3 to point to the components of the axis base offset vector for 
axis axis_id, and angle to point to the angle of rotation of axis 
axis_id after application of the axis settings for frame frame_id, 
using ratio, a value between 0 and 1, indicating how far into the 
internal motion in the frame to go. If frame_id is the string  \". 
\", the first frame found is used. If there is more than one frame, 
which frame will be found is indeterminate. If frame_id is NULL, the 
overall setting for the scan are used, rather than those for any 
particular frame. The vector and offset reported are the reference 
vector and offset of the axis axis_id transformed by application of 
all motions of the axes on which axis_id depends.
cbf_get_goniometer_poise vector1, vector2, vector3 to point to the 
components of the axis vector for the goniometer axis, offset1, 
offset2, offset3 to point to the components of the axis base offset 
vector for the goniometer axis, and angle to point to the angle of 
rotation of the goniometer axis after application of all axis 
settings in the goniometer deriving the vector, offset and angle from 
the resulting matrix. Calculation of the vector is indeterminate if 
the angle is zero.
cbf_get_axis_reference_poise sets vector1, vector2, vector3 to point 
to the components of the axis vector for axis axis_id, offset1, 
offset2, offset3 to point to the components of the axis base offset 
vector for axis axis_id unmodified by axis rotations. Any of the 
pointers may be specified as NULL.
ARGUMENTS
handle       CBF handle. ratio        A number between 0 and 1 
indication how far into the frame to go vector1      Pointer to the 
first component of the axis vector vector2      Pointer to the second 
component of the axis vector vector3      Pointer to the third 
component of the axis vector offset1      Pointer to the first 
component of the axis offset offset2      Pointer to the second 
component of the axis offset offset3      Pointer to the third 
component of the axis offset angle        Pointer to the rotation 
angle axis_id      The specified axis frame_id     The specified 
frame positioner   CBF goniometer
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_reference_poise;

  %apply double *OUTPUT {double *vector1, double *vector2, double *vector3, 
    double *offset1, double *offset2, double *offset3};
  
  void get_axis_reference_poise(double *vector1, double *vector2, double *vector3,
      double *offset1, double *offset2, double *offset3,
      const char *axis_id){
        cbf_failnez(cbf_get_axis_reference_poise(self,
          vector1, vector2, vector3,
          offset1, offset2, offset3,
          axis_id));
      }


/* cfunc cbf_remove_row   pyfunc remove_row  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_remove_row (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_remove_row deletes the current row in the current category.
If the current row was the last row, it will move down by 1, 
otherwise, it will remain the same.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")remove_row;
    void remove_row(void){
      cbf_failnez(cbf_remove_row(self));}
%feature("autodoc", "
Returns : 
*args   : Integer element_number,Float overload

C prototype: int cbf_set_overload (cbf_handle handle,
                 unsigned int element_number,      double overload);

CBFLib documentation:
DESCRIPTION
cbf_set_overload sets the overload value of element number 
element_number to overload.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. overload         New overload value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_overload;

   void set_overload(unsigned int element_number, double overload){
        cbf_failnez(cbf_set_overload(self,element_number,overload));
        }
%feature("autodoc", "
Returns : size_t ndim1,size_t ndim2
*args   : Integer element_number

C prototype: int cbf_get_image_size (cbf_handle handle, unsigned int reserved,
                      unsigned int element_number, size_t *ndimslow,
                 size_t *ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image_size, cbf_get_image_size_fs and cbf_get_image_size_sf 
set *ndimslow and *ndimfast to the slow and fast dimensions of the 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimfast 
will be set to 1. If the array is 3-dimensional an error code will be 
returned. cbf_get_3d_image_size, cbf_get_3d_image_size_fs and 
cbf_get_3d_image_size_sf set *ndimslow, *ndimmid and *ndimfast to the 
slowest, next fastest and fastest dimensions, respectively, of the 3D 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimmid 
and
")get_image_size;

%apply int *OUTPUT {int *ndimslow, int *ndimfast} get_image_size;
     void get_image_size(unsigned int element_number, int *ndimslow, int *ndimfast){
        unsigned int reserved;
        size_t inslow, infast;
        reserved = 0;
        cbf_failnez(cbf_get_image_size(self,reserved,element_number,&inslow,&infast));
        *ndimslow = (int)inslow;
        *ndimfast = (int)infast; 
        }
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int elsign,int dimslow,int dimmid,int dimfast

C prototype: int cbf_set_3d_image_sf(cbf_handle handle, unsigned int reserved,
                      unsigned int element_number, unsigned int compression,
                 void *array,      size_t elsize, int elsign, size_t ndimslow,
                 size_t ndimmid, size_t      ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_3d_image_sf;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_3d_image;

    void set_3d_image_sf(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int elsign, int ndimslow, int ndimmid, int ndimfast){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimmid*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_3d_image_sf (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, elsign, (size_t) ndimslow, (size_t) ndimmid, (size_t)ndimfast)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int ndimslow,int ndimfast

C prototype: int cbf_get_real_image_sf (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 void *array, size_t elsize, size_t      ndimslow,
                 size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_real_image_sf_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_real_image_sf_as_string;

// Get the length correct

    void get_real_image_sf_as_string(int element_number, char **s, int *slen,
    int elsize, int ndimslow, int ndimfast){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimslow))) {
               cbf_failnez (cbf_get_real_image_sf(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize,
               (size_t) ndimslow, (size_t)ndimfast));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimslow;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int elsign,int ndimslow,int ndimfast

C prototype: int cbf_get_image (cbf_handle handle, unsigned int reserved,
                 unsigned      int element_number, void *array, size_t elsize,
                 int elsign, size_t      ndimslow, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_image_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_image_as_string;

// Get the length correct

    void get_image_as_string(int element_number, char **s, int *slen,
    int elsize, int elsign, int ndimslow, int ndimfast){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimslow))) {
               cbf_failnez (cbf_get_image(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize, elsign,
               (size_t) ndimslow, (size_t)ndimfast));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimslow;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : 
*args   : String tagname,String tagroot_in

C prototype: int cbf_set_tag_root (cbf_handle handle, const char* tagname,
                 const      char*tagroot_in);

CBFLib documentation:
DESCRIPTION
cbf_find_tag_root sets *tagroot to the root tag of which tagname is 
an alias. cbf_set_tag_root sets tagname as an alias of tagroot_in in 
the dictionary associated with handle, creating the dictionary if 
necessary. cbf_require_tag_root sets *tagroot to the root tag of 
which tagname is an alias, if there is one, or to the value of 
tagname, if tagname is not an alias.
A returned tagroot string must not be modified in any way.
ARGUMENTS
handle       CBF handle. tagname      tag name which may be an alias. 
tagroot      pointer to a returned tag root name. tagroot_in   input 
tag root name.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_tag_root;

void  set_tag_root(const char* tagname, const char* tagroot_in){
   cbf_failnez(cbf_set_tag_root(self,tagname,tagroot_in));
}
%feature("autodoc", "
Returns : 
*args   : String filename,Integer ciforcbf,Integer Headers,Integer encoding

C prototype: int cbf_write_widefile (cbf_handle handle, FILE *file,
                 int readable, int    ciforcbf, int flags, int encoding);

CBFLib documentation:
DESCRIPTION
cbf_write_file writes the CBF object specified by handle into the 
file file, following CIF 1.0 conventions of 80 character lines. 
cbf_write_widefile writes the CBF object specified by handle into the 
file file, following CIF 1.1 conventions of 2048 character lines. A 
warning is issued to stderr for ascii lines over the limit, and an 
attempt is made to fold lines to fit. No test is performed on binary 
sections.
If a dictionary has been provided, aliases will be applied on output.
Unlike cbf_read_file, the file does not have to be random-access.
If the file is random-access and readable, readable can be set to 
non-0 to indicate to CBFlib that the file can be used as a buffer to 
conserve disk space. If the file is not random-access or not 
readable, readable must be 0.
")write_widefile;

    void write_widefile(const char* filename, int ciforcbf, int headers, 
                    int encoding){
       FILE *stream;
       int readable;
       /* Make readable false so we can close the file immediately */
       readable = 0;
       if ( ! ( stream = fopen (filename, "w+b")) ){
         cbf_failnez(CBF_FILEOPEN);
        }
        else{
        cbf_failnez(cbf_write_widefile(self, stream, readable, 
                    ciforcbf, headers, encoding));
        fclose(stream);

        }
       }

/* cfunc cbf_count_rows   pyfunc count_rows  
   arg cbf_handle handle    arg unsigned int *rows */

%feature("autodoc", "
Returns : Integer
*args   : 

C prototype: int cbf_count_rows (cbf_handle handle, unsigned int *rows);

CBFLib documentation:
DESCRIPTION
cbf_count_rows puts the number of rows in the current category in 
*rows .
ARGUMENTS
handle   CBF handle. rows     Pointer to the destination row count.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")count_rows;
    unsigned int count_rows(void){
      unsigned int result;
      cbf_failnez(cbf_count_rows(self,&result));
      return result;}

/* cfunc cbf_require_datablock   pyfunc require_datablock  
   arg cbf_handle handle    arg const char      *datablockname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_require_datablock (cbf_handle handle,
                 const char      *datablockname);

CBFLib documentation:
DESCRIPTION
cbf_require_datablock makes the data block with name datablockname 
the current data block, if it exists, or creates it if it does not.
The comparison is case-insensitive.
The current category becomes undefined.
ARGUMENTS
handle          CBF handle. datablockname   The name of the data 
block to find or create.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")require_datablock;
    void require_datablock(const char* arg){
      cbf_failnez(cbf_require_datablock(self,arg));}
%feature("autodoc", "
Returns : 
*args   : int compression,int binary_id,(binary) String data,int elsize,
          int elsigned,int elements

C prototype: int cbf_set_integerarray (cbf_handle handle,
                 unsigned int compression, int    binary_id, void *array,
                 size_t elsize, int elsigned, size_t elements);

CBFLib documentation:
DESCRIPTION
cbf_set_integerarray sets the binary value of the item at the current 
column and row to an integer array. The array consists of elements 
elements of elsize bytes each, starting at array. The elements are 
signed if elsigned is non-0 and unsigned otherwise. binary_id is the 
binary section identifier. cbf_set_realarray sets the binary value of 
the item at the current column and row to an integer array. The array 
consists of elements elements of elsize bytes each, starting at 
array. binary_id is the binary section identifier.
The cbf_set_integerarray_wdims, cbf_set_integerarray_wdims_fs, 
cbf_set_integerarray_wdims_sf, cbf_set_realarray_wdims, 
cbf_set_realarray_wdims_fs and cbf_set_realarray_wdims_sf variants 
allow the data header values of byteorder, dimfast, dimmid, dimslow 
and padding to be set to the data byte order, the fastest, second 
fastest and third fastest array dimensions and the size in byte of 
the post data padding to be used.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL       Canonical-code compression (section 3.3.1) 
CBF_PACKED          CCP4-style packing (section 3.3.2) CBF_PACKED_V2  
     CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET    
 Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE            No compression. 
NOTE: This scheme is by far the slowest of the four and uses much 
more disk space. It is intended for routine use with small arrays 
only. With large arrays (like images) it should be used only for 
debugging.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned), for cbf_set_integerarray, or IEEE doubles or 
floats for cbf_set_realarray. If elsize is not equal to sizeof 
(char), sizeof (short) or sizeof (int), the function returns 
CBF_ARGUMENT.
ARGUMENTS
handle        CBF handle. compression   Compression method to use. 
binary_id     Integer binary identifier. array         Pointer to the 
source array. elsize        Size in bytes of each source array 
element. elsigned      Set to non-0 if the source array elements are 
signed. elements      The number of elements in the array
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_integerarray;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_integerarray;

    void set_integerarray(unsigned int compression, int binary_id, 
             char *data, int len, int elsize, int elsigned, int elements){
        /* safety check on args */
        size_t els, ele;
        void *array;
        if(len == elsize*elements){
           array = data;
           els = elsize;
           ele = elements;
           cbf_failnez(cbf_set_integerarray (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, elsigned, (size_t) elements)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }

/* cfunc cbf_new_datablock   pyfunc new_datablock  
   arg cbf_handle handle    arg const char *datablockname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_new_datablock (cbf_handle handle,
                 const char *datablockname);

CBFLib documentation:
DESCRIPTION
cbf_new_datablock creates a new data block with name datablockname 
and makes it the current data block. cbf_new_saveframe creates a new 
save frame with name saveframename within the current data block and 
makes the new save frame the current save frame.
If a data block or save frame with this name already exists, the 
existing data block or save frame becomes the current data block or 
save frame.
ARGUMENTS
handle          CBF handle. datablockname   The name of the new data 
block. saveframename   The name of the new save frame.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")new_datablock;
    void new_datablock(const char* arg){
      cbf_failnez(cbf_new_datablock(self,arg));}
%feature("autodoc", "
Returns : 
*args   : int year,int month,int day,int hour,int minute,double second,
          int timezone,Float precision

C prototype: int cbf_set_datestamp (cbf_handle handle, unsigned int reserved,
                 int      year, int month, int day, int hour, int minute,
                 double second, int      timezone, double precision);

CBFLib documentation:
DESCRIPTION
cbf_set_datestamp sets the collection timestamp in seconds since 
January 1 1970 to the value specified by time. The timezone 
difference from UTC
")set_datestamp;

   void set_datestamp(int year, int month, int day, int hour, 
                      int minute, double second, int timezone, 
                      double precision){
        unsigned int reserved;
        reserved = 0; 
        cbf_failnez(cbf_set_datestamp(self,reserved, 
              year,month,day,hour,minute,second,timezone,precision));
        }

/* cfunc cbf_next_row   pyfunc next_row  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_next_row (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_next_row makes the row following the current row in the current 
category the current row.
If there are no more rows, the function returns CBF_NOTFOUND.
The current column is not affected.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")next_row;
    void next_row(void){
      cbf_failnez(cbf_next_row(self));}
%feature("autodoc", "
Returns : 
*args   : String categoryname,String categoryroot

C prototype: int cbf_set_category_root (cbf_handle handle,
                 const char*      categoryname_in, const char*categoryroot);

CBFLib documentation:
DESCRIPTION
cbf_find_category_root sets *categoryroot to the root category of 
which categoryname is an alias. cbf_set_category_root sets 
categoryname_in as an alias of categoryroot in the dictionary 
associated with handle, creating the dictionary if necessary. 
cbf_require_category_root sets *categoryroot to the root category of 
which categoryname is an alias, if there is one, or to the value of 
categoryname, if categoryname is not an alias.
A returned categoryroot string must not be modified in any way.
ARGUMENTS
handle            CBF handle. categoryname      category name which 
may be an alias. categoryroot      pointer to a returned category 
root name. categoryroot_in   input category root name.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_category_root;

void  set_category_root(const char* categoryname, const char* categoryroot){
   cbf_failnez(cbf_set_category_root(self,categoryname,categoryroot));
}
%feature("autodoc", "
Returns : Float offset1,Float offset2,Float offset3
*args   : String axis_id

C prototype: int cbf_get_axis_offset (cbf_handle handle, const char *axis_id,
                 double      *offset1, double *offset2, double *offset3);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_offset;

%apply double *OUTPUT {double *offset1, double *offset2, double offset3} get_axis_offset;
   void get_axis_offset(const char *axis_id,
                    double *offset1, double *offset2, double*offset3){
        cbf_failnez(cbf_get_axis_offset(self,axis_id,
                         offset1, offset2,offset3));
        }
%feature("autodoc", "
Returns : 
*args   : Int element_number,Int axis_number,Float pixel size

C prototype: int cbf_set_pixel_size_fs(cbf_handle handle,
                 unsigned int      element_number, int axis_number,
                 double psize);

CBFLib documentation:
DESCRIPTION
cbf_set_pixel_size and cbf_set_pixel_size_sf set the item in the  
\"size \" column of the  \"array_structure_list \" category at the 
row which matches axis axis_number of the detector element 
element_number converting the double pixel size psize from meters to 
millimeters in storing it in the  \"size \" column for the axis 
axis_number of the detector element element_number. The axis_number 
is numbered from 1, starting with the slowest axis. 
cbf_set_pixel_size_fs sets the item in the  \"size \" column of the  
\"array_structure_list \" category at the row which matches axis 
axis_number of the detector element element_number converting the 
double pixel size psize from meters to millimeters in storing it in 
the  \"size \" column for the axis axis_number of the detector 
element element_number. The axis_number is numbered from 1, starting 
with the fastest axis.
If a negative axis number is given, the order of axes is reversed, so 
that -1 specifies the slowest axis for cbf_get_pixel_size_fs and the 
fastest axis for cbf_get_pixel_size_sf.
If the  \"array_structure_list \" category does not already exist, it 
is created.
If the appropriate row in the  \"array_structure_list \" catgeory 
does not already exist, it is created.
If the pixel size is not given explcitly in the  \"array_element_size 
category \", the function returns CBF_NOTFOUND.
ARGUMENTS
handle           CBF handle. element_number   The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. axis_number      The number of the 
axis, fastest first, starting from 1.
")set_pixel_size_fs;

     void set_pixel_size_fs (unsigned int element_number, 
                          unsigned int axis_number, double psize){
         cbf_failnez(cbf_set_pixel_size_fs(self, 
                                        element_number, 
                                        axis_number, 
                                        psize));
     }

/* cfunc cbf_insert_row   pyfunc insert_row  
   arg cbf_handle handle    arg unsigned int rownumber */

%feature("autodoc", "
Returns : 
*args   : Integer

C prototype: int cbf_insert_row (cbf_handle handle, unsigned int rownumber);

CBFLib documentation:
DESCRIPTION
cbf_insert_row adds a new row to the current category. The new row is 
inserted as row rownumber and existing rows starting from rownumber 
are moved up by 1. The new row becomes the current row.
If the category has fewer than rownumber rows, the function returns 
CBF_NOTFOUND.
The row numbers start from 0.
ARGUMENTS
handle      CBF handle. rownumber   The row number of the new row.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")insert_row;
    void insert_row(unsigned int arg){
      cbf_failnez(cbf_insert_row(self,arg));}

/* cfunc cbf_new_column   pyfunc new_column  
   arg cbf_handle handle    arg const char *columnname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_new_column (cbf_handle handle, const char *columnname);

CBFLib documentation:
DESCRIPTION
cbf_new_column creates a new column in the current category with name 
columnname and makes it the current column.
If a column with this name already exists, the existing column 
becomes the current category.
ARGUMENTS
handle       CBF handle. columnname   The name of the new column.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")new_column;
    void new_column(const char* arg){
      cbf_failnez(cbf_new_column(self,arg));}
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int ndimslow,int ndimmid,int ndimfast

C prototype: int cbf_get_real_3d_image (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 void *array, size_t elsize, size_t      ndimslow,
                 size_t ndimmid, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_real_3d_image_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_real_3d_image_as_string;

// Get the length correct

    void get_real_3d_image_as_string(int element_number, char **s, int *slen,
    int elsize, int ndimslow, int ndimmid, int ndimfast){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimmid*ndimslow))) {
               cbf_failnez (cbf_get_real_3d_image(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize,
               (size_t) ndimslow, (size_t)ndimmid, (size_t)ndimfast));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimmid*ndimslow;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : Float time
*args   : 

C prototype: int cbf_get_integration_time (cbf_handle handle,
                 unsigned int reserved,      double *time);

CBFLib documentation:
DESCRIPTION
cbf_get_integration_time sets *time to the integration time in 
seconds. The parameter reserved is presently unused and should be set 
to 0.
ARGUMENTS
handle     CBF handle. reserved   Unused. Any value other than 0 is 
invalid. time       Pointer to the destination time.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_integration_time;

%apply double *OUTPUT {double *time} get_integration_time;
   void get_integration_time( double *time ){
        unsigned int reserved;
        double tim;
        reserved = 0;
        cbf_failnez(cbf_get_integration_time(self,reserved,&tim));
        *time = tim;
        }
%feature("autodoc", "
Returns : 
*args   : int compression,int binary_id,(binary) String data,int elsize,
          int elements

C prototype: int cbf_set_realarray (cbf_handle handle,
                 unsigned int compression, int    binary_id, void *array,
                 size_t elsize, size_t elements);

CBFLib documentation:
DESCRIPTION
cbf_set_integerarray sets the binary value of the item at the current 
column and row to an integer array. The array consists of elements 
elements of elsize bytes each, starting at array. The elements are 
signed if elsigned is non-0 and unsigned otherwise. binary_id is the 
binary section identifier. cbf_set_realarray sets the binary value of 
the item at the current column and row to an integer array. The array 
consists of elements elements of elsize bytes each, starting at 
array. binary_id is the binary section identifier.
The cbf_set_integerarray_wdims, cbf_set_integerarray_wdims_fs, 
cbf_set_integerarray_wdims_sf, cbf_set_realarray_wdims, 
cbf_set_realarray_wdims_fs and cbf_set_realarray_wdims_sf variants 
allow the data header values of byteorder, dimfast, dimmid, dimslow 
and padding to be set to the data byte order, the fastest, second 
fastest and third fastest array dimensions and the size in byte of 
the post data padding to be used.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL       Canonical-code compression (section 3.3.1) 
CBF_PACKED          CCP4-style packing (section 3.3.2) CBF_PACKED_V2  
     CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET    
 Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE            No compression. 
NOTE: This scheme is by far the slowest of the four and uses much 
more disk space. It is intended for routine use with small arrays 
only. With large arrays (like images) it should be used only for 
debugging.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned), for cbf_set_integerarray, or IEEE doubles or 
floats for cbf_set_realarray. If elsize is not equal to sizeof 
(char), sizeof (short) or sizeof (int), the function returns 
CBF_ARGUMENT.
ARGUMENTS
handle        CBF handle. compression   Compression method to use. 
binary_id     Integer binary identifier. array         Pointer to the 
source array. elsize        Size in bytes of each source array 
element. elsigned      Set to non-0 if the source array elements are 
signed. elements      The number of elements in the array
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_realarray;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_realarray;

    void set_realarray(unsigned int compression, int binary_id, 
             char *data, int len, int elsize, int elements){
        /* safety check on args */
        size_t els, ele;
        void *array;
        if(len == elsize*elements){
           array = data;
           els = elsize;
           ele = elements;
           cbf_failnez(cbf_set_realarray (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, (size_t) elements)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : String
*args   : Integer element_number

C prototype: int cbf_get_element_id (cbf_handle handle,
                 unsigned int element_number,      const char **element_id);

CBFLib documentation:
DESCRIPTION
cbf_get_element_number sets element_number to a number that can be 
used in other cbf_simple calls to identify the detector element 
element_id and optionally the specific array_id> and 
array_section_id. cbf_get_element_id sets *element_id to point to the 
ASCII value of the element_number'th  
\"diffrn_data_frame.detector_element_id \" entry, counting from 0. 
The element_number is the ordinal of the detector element in the 
DIFFRN_DETECTOR_ELEMENT category. If an array_section_id is specified 
(i.e. is not NULL), the element_number is the sum of the ordinal of 
the detector element plus the number of detector elements multiplied 
by the ordinal of array_section_id for the specified array_id> in the 
ARRAY_STRUCTURE_LIST_SECTION category.
If the detector element does not exist, the function returns 
CBF_NOTFOUND.
The element_id will be valid as long as the item exists and has not 
been set to a new value.
The element_id must not be modified by the program in any way.
ARGUMENTS
handle             CBF handle. element_number     The number of the 
detector element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. element_id         Pointer to the 
destination string for cbf_get_element_id, but the string itself for 
cbf_get_element_number. array_id           The optional array id or 
NULL. array_section_id   The optional array_section_id or NULL.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_element_id;

   const char * get_element_id(unsigned int element_number){
       const char * result;
       cbf_failnez(cbf_get_element_id (self, element_number, &result));
       return result;
       }
%feature("autodoc", "
Returns : (Binary)String
*args   : int element_number,int elsize,int elsign,int ndimslow,int ndimfast

C prototype: int cbf_get_image_sf (cbf_handle handle, unsigned int reserved,
                 unsigned      int element_number, void *array, size_t elsize,
                 int elsign, size_t      ndimslow, size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_get_image, cbf_get_image_fs and cbf_get_image_sf read the image 
array for element number element_number into an array. The array 
consists of ndimslow×ndimfast elements of elsize bytes each, 
starting at array. The elements are signed if elsign is non-0 and 
unsigned otherwise. cbf_get_real_image, cbf_get_real_image_fs and 
cbf_get_real_image_sf read the image array of IEEE doubles or floats 
for element number element_number into an array. A real array is 
always signed. cbf_get_3d_image, cbf_get_3d_image_fs and 
cbf_get_3d_image_sf read the 3D image array for element number 
element_number into an array. The array consists of 
ndimslow×ndimmid×ndimfast elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_get_real_3d_image, cbf_get_real_3d_image_fs, 
cbf_get_real_3d_image_sf reads the 3D image array of IEEE doubles or 
floats for element number element_number into an array. A real array 
is always signed.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
The structure of the array as a 1-, 2- or 3-dimensional array should 
agree with the structure of the array given in the 
ARRAY_STRUCTURE_LIST category. If the array is 1-dimensional, 
ndimslow should be the array size and ndimfast and, for the 3D calls, 
ndimmid, should be set to 1 both in the call and in the imgCIF data 
being processed. If the array is 2-dimensional and a 3D call is used, 
ndimslow and ndimmid should be the
")get_image_sf_as_string;

// Ensure we free the local temporary

%cstring_output_allocate_size(char ** s, int *slen, free(*$1))
       get_image_fs_as_string;

// Get the length correct

    void get_image_sf_as_string(int element_number, char **s, int *slen,
    int elsize, int elsign, int ndimslow, int ndimfast){
        void *array;
        int reserved = 0;
        *slen = 0; /* Initialise in case of problems */
        if ((array=malloc(elsize*ndimfast*ndimslow))) {
               cbf_failnez (cbf_get_image_sf(self, 
               reserved, (unsigned int)element_number,
               (void *)array, (size_t)elsize, elsign,
               (size_t) ndimslow, (size_t)ndimfast));
         }else{
               cbf_failnez(CBF_ALLOC);
         }
        *slen = elsize*ndimfast*ndimslow;
        *s = (char *) array;
      }
%feature("autodoc", "
Returns : pycbf positioner object
*args   : String axis_id

C prototype: int cbf_construct_positioner (cbf_handle handle,
                 cbf_positioner      *positioner, const char *axis_id);

CBFLib documentation:
DESCRIPTION
cbf_construct_positioner constructs a positioner object for the axis 
given by axis_id using the description in the CBF object handle and 
initialises the positioner handle *positioner.
cbf_construct_reference positioner constructs a positioner object for 
the axis given by axis_id using the description in the CBF object 
handle and initialises the detector handle *detector using the 
reference settings of the axes.
ARGUMENTS
handle       CBF handle. positioner   Pointer to the destination 
positioner handle. axis_id      The identifier of the axis in the  
\"axis \" category.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")construct_positioner;

 cbf_positioner construct_positioner(const char* axis_id){
    cbf_positioner positioner;
    cbf_failnez(cbf_construct_positioner(self,&positioner,axis_id));
    return positioner;
    }
%feature("autodoc", "
Returns : size_t ndimfast,size_t ndimmid,size_t ndimslow
*args   : Integer element_number

C prototype: int cbf_get_3d_image_size_fs (cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 size_t *ndimfast, size_t *ndimmid, size_t      *ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_get_image_size, cbf_get_image_size_fs and cbf_get_image_size_sf 
set *ndimslow and *ndimfast to the slow and fast dimensions of the 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimfast 
will be set to 1. If the array is 3-dimensional an error code will be 
returned. cbf_get_3d_image_size, cbf_get_3d_image_size_fs and 
cbf_get_3d_image_size_sf set *ndimslow, *ndimmid and *ndimfast to the 
slowest, next fastest and fastest dimensions, respectively, of the 3D 
image array for element number element_number. If the array is 
1-dimensional, *ndimslow will be set to the array size and *ndimmid 
and
")get_3d_image_size;

%apply int *OUTPUT {int *ndimslow, int *ndimmid, int *ndimfast} get_3d_image_size;
     void get_3d_image_size_fs(unsigned int element_number, int *ndimfast, int *ndimmid, int *ndimslow){
        unsigned int reserved;
        size_t inslow, inmid, infast;
        reserved = 0;
        cbf_failnez(cbf_get_3d_image_size_fs(self,reserved,element_number,&infast,&inmid,&inslow));
        *ndimslow = (int)inslow; /* FIXME - is that how to convert? */
        *ndimmid = (int)inmid; 
        *ndimfast = (int)infast;
        }

/* cfunc cbf_set_value   pyfunc set_value  
   arg cbf_handle handle    arg const char *value */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_set_value (cbf_handle handle, const char *value);

CBFLib documentation:
DESCRIPTION
cbf_set_value sets the item at the current column and row to the 
ASCII value value.
ARGUMENTS
handle   CBF handle. value    ASCII value.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_value;
    void set_value(const char* arg){
      cbf_failnez(cbf_set_value(self,arg));}
%feature("autodoc", "
Returns : 
*args   : Integer timezone

C prototype: int cbf_set_current_timestamp (cbf_handle handle,
                 unsigned int reserved,      int timezone);

CBFLib documentation:
DESCRIPTION
cbf_set_current_timestamp sets the collection timestamp to the 
current time. The timezone difference from UTC in minutes is set to 
timezone. If no timezone is desired, timezone should be 
CBF_NOTIMEZONE. If no timezone is used, the timest amp will be UTC. 
The parameter reserved is presently unused and should be set to 0.
The new timestamp will have a precision of 1 second.
ARGUMENTS
handle     CBF handle. reserved   Unused.   Any value other than 0 is 
invalid. timezone   Timezone difference from UTC in minutes or 
CBF_NOTIMEZONE.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_current_timestamp;

    void set_current_timestamp(int timezone){
        unsigned int reserved;
        reserved = 0; 
        cbf_failnez(cbf_set_current_timestamp(self,reserved,timezone));
        }
%feature("autodoc", "
Returns : Float Number
*args   : Float Default

C prototype: int cbf_require_doublevalue (cbf_handle handle, double *number,
                 double    defaultvalue);

CBFLib documentation:
DESCRIPTION
cbf_get_doublevalue sets *number to the value of the ASCII item at 
the current column and row interpreted as a decimal floating-point 
number. cbf_require_doublevalue sets *number to the value of the 
ASCII item at the current column and row interpreted as a decimal 
floating-point number, setting it to defaultvalue if necessary.
If the value is not ASCII, the function returns CBF_BINARY.
ARGUMENTS
handle         CBF handle. number         Pointer to the destination 
number. defaultvalue   default number value.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")require_doublevalue;

%apply double *OUTPUT {double *number} require_doublevalue;
void require_doublevalue(double *number, double defaultvalue){
   cbf_failnez(cbf_require_doublevalue(self,number,defaultvalue));
}

/* cfunc cbf_rewind_datablock   pyfunc rewind_datablock  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_rewind_datablock (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_rewind_datablock makes the first data block the current data 
block.
If there are no data blocks, the function returns CBF_NOTFOUND.
The current category becomes undefined.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")rewind_datablock;
    void rewind_datablock(void){
      cbf_failnez(cbf_rewind_datablock(self));}
%feature("autodoc", "
Returns : String Name
*args   : String columnnanme,String Default

C prototype: int cbf_require_column_value (cbf_handle handle,
                 const char *columnname,      const char **value,
                 const char *defaultvalue);

CBFLib documentation:
DESCRIPTION
cbf_require_column_doublevalue sets *value to the ASCII item at the 
current row for the column given with the name given by *columnname, 
or to the string given by defaultvalue if the item cannot be found.
ARGUMENTS
handle         CBF handle. columnname     Name of the column 
containing the number. value          pointer to the location to 
receive the value. defaultvalue   Value to use if the requested 
column and value cannot be found.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")require_column_value;

 const char* require_column_value(const char *columnname,
                                  const char *defaultvalue){
   const char * result;
   cbf_failnez(cbf_require_column_value(self,columnname,
                                    &result,defaultvalue));
   return result;
}
%feature("autodoc", "
Returns : CBFHandle dictionary
*args   : 

C prototype: int cbf_get_dictionary (cbf_handle handle,
                 cbf_handle * dictionary);

CBFLib documentation:
DESCRIPTION
cbf_get_dictionary sets *dictionary to the handle of a CBF which has 
been associated with the CBF handle by cbf_set_dictionary. 
cbf_set_dictionary associates the CBF handle dictionary_in with 
handle as its dictionary. cbf_require_dictionary sets *dictionary to 
the handle of a CBF which has been associated with the CBF handle by 
cbf_set_dictionary or creates a new empty CBF and associates it with 
handle, returning the new handle in *dictionary.
ARGUMENTS
handle          CBF handle. dictionary      Pointer to CBF handle of 
dictionary. dictionary_in   CBF handle of dcitionary.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_dictionary;

cbf_handle get_dictionary(){
   cbf_handle temp;
   cbf_failnez(cbf_get_dictionary(self,&temp));
   return temp;
}

/* cfunc cbf_reset_saveframe   pyfunc reset_saveframe  
   arg cbf_handle handle */

%feature("autodoc", "
Returns : 
*args   : 

C prototype: int cbf_reset_saveframe (cbf_handle handle);

CBFLib documentation:
DESCRIPTION
cbf_reset_datablock deletes all categories from the current data 
block. cbf_reset_saveframe deletes all categories from the current 
save frame.
ARGUMENTS
handle   CBF handle.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")reset_saveframe;
    void reset_saveframe(void){
      cbf_failnez(cbf_reset_saveframe(self));}
%feature("autodoc", "
Returns : 
*args   : double cell[6]

C prototype: int cbf_set_reciprocal_cell (cbf_handle handle, double cell[6],
                 double      cell_esd[6] );

CBFLib documentation:
DESCRIPTION
cbf_set_reciprocal_cell sets the reciprocal cell parameters to the 
double values given in cell[0:2] for the reciprocal cell edge lengths 
a^*, b^* and c^* in Ångstroms^-1, the double values given in 
cell[3:5] for the reciprocal cell angles α^*, β^* and γ^* in 
degrees, the double values given in cell_esd[0:2] for the estimated 
strandard deviations of the reciprocal cell edge lengths a^*, b^* and 
c^* in Ångstroms, and the double values given in cell_esd[3:5] for 
the estimated standard deviations of the reciprocal cell angles α^*, 
β^* and γ^* in degrees.
The values are placed in the first row of the  \"cell \" category. If 
no value has been given for  \"_cell.entry_id \", it is set to the 
value of the  \"diffrn.id \" entry of the current data block.
cell or cell_esd may be NULL.
If cell is NULL, the reciprocal cell parameters are not set.
If cell_esd is NULL, the reciprocal cell parameter esds are not set.
If the  \"cell \" category is not present, it is created. If any of 
the necessary columns are not present, they are created.
ARGUMENTS
handle     CBF handle. cell       Pointer to the array of 6 doubles 
for the reciprocal cell parameters. cell_esd   Pointer to the array 
of 6 doubles for the reciprocal cell parameter esds.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_reciprocal_cell;

   void set_reciprocal_cell(double cell[6]) {
     cbf_failnez(cbf_set_reciprocal_cell(self,cell,NULL));
   }
%feature("autodoc", "
Returns : 
*args   : double cell_esd[6]

C prototype: int cbf_set_reciprocal_cell (cbf_handle handle, double cell[6],
                 double      cell_esd[6] );

CBFLib documentation:
DESCRIPTION
cbf_set_reciprocal_cell sets the reciprocal cell parameters to the 
double values given in cell[0:2] for the reciprocal cell edge lengths 
a^*, b^* and c^* in Ångstroms^-1, the double values given in 
cell[3:5] for the reciprocal cell angles α^*, β^* and γ^* in 
degrees, the double values given in cell_esd[0:2] for the estimated 
strandard deviations of the reciprocal cell edge lengths a^*, b^* and 
c^* in Ångstroms, and the double values given in cell_esd[3:5] for 
the estimated standard deviations of the reciprocal cell angles α^*, 
β^* and γ^* in degrees.
The values are placed in the first row of the  \"cell \" category. If 
no value has been given for  \"_cell.entry_id \", it is set to the 
value of the  \"diffrn.id \" entry of the current data block.
cell or cell_esd may be NULL.
If cell is NULL, the reciprocal cell parameters are not set.
If cell_esd is NULL, the reciprocal cell parameter esds are not set.
If the  \"cell \" category is not present, it is created. If any of 
the necessary columns are not present, they are created.
ARGUMENTS
handle     CBF handle. cell       Pointer to the array of 6 doubles 
for the reciprocal cell parameters. cell_esd   Pointer to the array 
of 6 doubles for the reciprocal cell parameter esds.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_reciprocal_cell_esd;

   void set_reciprocal_cell_esd(double cell_esd[6]) {
     cbf_failnez(cbf_set_reciprocal_cell(self,NULL,cell_esd));
   }
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int dimfast,int dimmid,int dimslow

C prototype: int cbf_set_real_3d_image_fs(cbf_handle handle,
                 unsigned int reserved,      unsigned int element_number,
                 unsigned int compression, void      *array,size_t elsize,
                 size_t ndimfast, size_t ndimmid, size_t ndimslow);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_real_3d_image_fs;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_real_3d_image_fs;

    void set_real_3d_image_fs(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int ndimfast, int ndimmid, int ndimslow){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimmid*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_real_3d_image_fs (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, (size_t) ndimfast, (size_t)ndimmid, (size_t)ndimslow)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : 
*args   : String format,Float number

C prototype: int cbf_set_doublevalue (cbf_handle handle, const char *format,
                 double    number);

CBFLib documentation:
DESCRIPTION
cbf_set_doublevalue sets the item at the current column and row to 
the floating-point value number written as an ASCII string with the 
format specified by format as appropriate for the printf function.
ARGUMENTS
handle   CBF handle. format   Format for the number. number   
Floating-point value.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_doublevalue;

     void set_doublevalue(const char *format, double number){
        cbf_failnez(cbf_set_doublevalue(self,format,number));}

/* cfunc cbf_find_category   pyfunc find_category  
   arg cbf_handle handle    arg const char *categoryname */

%feature("autodoc", "
Returns : string
*args   : 

C prototype: int cbf_find_category (cbf_handle handle,
                 const char *categoryname);

CBFLib documentation:
DESCRIPTION
cbf_find_category makes the category in the current data block with 
name categoryname the current category.
The comparison is case-insensitive.
If the category does not exist, the function returns CBF_NOTFOUND.
The current column and row become undefined.
ARGUMENTS
handle         CBF handle. categoryname   The name of the category to 
find.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")find_category;
    void find_category(const char* arg){
      cbf_failnez(cbf_find_category(self,arg));}
%feature("autodoc", "
Returns : int compression,int binary_id,int elsize,int elsigned,int elunsigned,
          int elements,int minelement,int maxelement,char **bo,int *bolen,
          int dimfast,int dimmid,int dimslow,int padding
*args   : 

C prototype: int cbf_get_integerarrayparameters_wdims_fs (cbf_handle handle,
                 unsigned    int *compression, int *binary_id, size_t *elsize,
                 int *elsigned, int    *elunsigned, size_t *elements,
                 int *minelement, int *maxelement, const    char **byteorder,
                 size_t *dimfast, size_t *dimmid, size_t *dimslow,
                 size_t    *padding);

CBFLib documentation:
DESCRIPTION
cbf_get_integerarrayparameters sets *compression, *binary_id, 
*elsize, *elsigned, *elunsigned, *elements, *minelement and 
*maxelement to values read from the binary value of the item at the 
current column and row. This provides all the arguments needed for a 
subsequent call to cbf_set_integerarray, if a copy of the array is to 
be made into another CIF or CBF. cbf_get_realarrayparameters sets 
*compression, *binary_id, *elsize, *elements to values read from the 
binary value of the item at the current column and row. This provides 
all the arguments needed for a subsequent call to cbf_set_realarray, 
if a copy of the arry is to be made into another CIF or CBF.
The variants cbf_get_integerarrayparameters_wdims, 
cbf_get_integerarrayparameters_wdims_fs, 
cbf_get_integerarrayparameters_wdims_sf, 
cbf_get_realarrayparameters_wdims, 
cbf_get_realarrayparameters_wdims_fs, 
cbf_get_realarrayparameters_wdims_sf set **byteorder, *dimfast, 
*dimmid, *dimslow, and *padding as well, providing the additional 
parameters needed for a subsequent call to cbf_set_integerarray_wdims 
or cbf_set_realarray_wdims.
The value returned in *byteorder is a pointer either to the string  
\"little_endian \" or to the string  \"big_endian \". This should be 
the byte order of the data, not necessarily of the host machine. No 
attempt should be made to modify this string. At this time only  
\"little_endian \" will be returned.
The values returned in *dimfast, *dimmid and *dimslow are the sizes 
of the fastest changing, second fastest changing and third fastest 
changing dimensions of the array, if specified, or zero, if not 
specified.
The value returned in *padding is the size of the post-data padding, 
if any and if specified in the data header. The value is given as a 
count of octets.
If the value is not binary, the function returns CBF_ASCII.
ARGUMENTS
handle        CBF handle. compression   Compression method used. 
elsize        Size in bytes of each array element. binary_id     
Pointer to the destination integer binary identifier. elsigned      
Pointer to an integer. Set to 1 if the elements can be read as signed 
integers. elunsigned    Pointer to an integer. Set to 1 if the 
elements can be read as unsigned integers. elements      Pointer to 
the destination number of elements. minelement    Pointer to the 
destination smallest element. maxelement    Pointer to the 
destination largest element. byteorder     Pointer to the destination 
byte order. dimfast       Pointer to the destination fastest 
dimension. dimmid        Pointer to the destination second fastest 
dimension. dimslow       Pointer to the destination third fastest 
dimension. padding       Pointer to the destination padding size.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")get_integerarrayparameters_wdims_fs;

%cstring_output_allocate_size(char **bo, int *bolen, free(*$1));
%apply int *OUTPUT {int *compression,int *binary_id, 
                    int *elsize, int *elsigned, int *elunsigned, 
                    int *elements, int *minelement, int *maxelement,
                    int *dimfast, int *dimmid, int *dimslow, int *padding} 
                  get_integerarrayparameters_wdims_fs;

    void get_integerarrayparameters_wdims_fs(int *compression,int *binary_id, 
                        int *elsize, int *elsigned, int *elunsigned, 
                        int *elements, int *minelement, int *maxelement,
                        char **bo, int *bolen,
                        int *dimfast, int *dimmid, int *dimslow, int *padding
                        ){
        unsigned int  comp;
        size_t elsiz, elem, df,dm,ds,pd;
        const char * byteorder;
        char * bot;
        cbf_failnez(cbf_get_integerarrayparameters_wdims_fs(self, 
         &comp,binary_id, &elsiz, elsigned, elunsigned, &elem, 
          minelement, maxelement, &byteorder,&df,&dm,&ds,&pd ));
        *bolen = strlen(byteorder);
        if (!(bot = (char *)malloc(*bolen))) {cbf_failnez(CBF_ALLOC)}
        strncpy(bot,byteorder,*bolen);
        *bo = bot;
        *compression = comp; 
        *elsize = elsiz;
        *elements = elem;
        *dimfast = df;
        *dimmid = dm;
        *dimslow = ds;
        *padding = pd;
        
        }
%feature("autodoc", "
Returns : 
*args   : int compression,int binary_id,(binary) String data,int elsize,
          int elements,String byteorder,int dimfast,int dimmid,int dimslow,
          int padding

C prototype: int cbf_set_realarray_wdims_fs (cbf_handle handle,
                 unsigned int    compression, int binary_id, void *array,
                 size_t elsize, size_t elements,    const char *byteorder,
                 size_t dimfast, size_t dimmid, size_t dimslow,
                    size_t padding);

CBFLib documentation:
DESCRIPTION
cbf_set_integerarray sets the binary value of the item at the current 
column and row to an integer array. The array consists of elements 
elements of elsize bytes each, starting at array. The elements are 
signed if elsigned is non-0 and unsigned otherwise. binary_id is the 
binary section identifier. cbf_set_realarray sets the binary value of 
the item at the current column and row to an integer array. The array 
consists of elements elements of elsize bytes each, starting at 
array. binary_id is the binary section identifier.
The cbf_set_integerarray_wdims, cbf_set_integerarray_wdims_fs, 
cbf_set_integerarray_wdims_sf, cbf_set_realarray_wdims, 
cbf_set_realarray_wdims_fs and cbf_set_realarray_wdims_sf variants 
allow the data header values of byteorder, dimfast, dimmid, dimslow 
and padding to be set to the data byte order, the fastest, second 
fastest and third fastest array dimensions and the size in byte of 
the post data padding to be used.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL       Canonical-code compression (section 3.3.1) 
CBF_PACKED          CCP4-style packing (section 3.3.2) CBF_PACKED_V2  
     CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET    
 Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE            No compression. 
NOTE: This scheme is by far the slowest of the four and uses much 
more disk space. It is intended for routine use with small arrays 
only. With large arrays (like images) it should be used only for 
debugging.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned), for cbf_set_integerarray, or IEEE doubles or 
floats for cbf_set_realarray. If elsize is not equal to sizeof 
(char), sizeof (short) or sizeof (int), the function returns 
CBF_ARGUMENT.
ARGUMENTS
handle        CBF handle. compression   Compression method to use. 
binary_id     Integer binary identifier. array         Pointer to the 
source array. elsize        Size in bytes of each source array 
element. elsigned      Set to non-0 if the source array elements are 
signed. elements      The number of elements in the array
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_realarray_wdims_fs;

    /* CBFlib must NOT modify the data string nor the byteorder string
       which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_realarray_wdims_fs;
%apply (char *STRING, int LENGTH) { (char *bo, int bolen) } set_realarray_wdims_fs;

    void set_realarray_wdims_fs(unsigned int compression, int binary_id, 
             char *data, int len, int elsize, int elements,
             char *bo, int bolen, int dimfast, int dimmid, int dimslow, int padding){
        /* safety check on args */
        size_t els, ele;
        void *array;
        char byteorder[15];
        if(len == elsize*elements && elements==dimfast*dimmid*dimslow){
           array = data;
           els = elsize;
           ele = elements;
           strncpy(byteorder,bo,bolen<15?bolen:14);
           byteorder[bolen<15?bolen:14] = 0;
           cbf_failnez(cbf_set_realarray_wdims_fs (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, (size_t) elements, (const char *)byteorder,
           (size_t) dimfast, (size_t) dimmid, (size_t) dimslow, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : String categoryroot
*args   : String categoryname

C prototype: int cbf_find_category_root (cbf_handle handle,
                 const char* categoryname,      const char** categoryroot);

CBFLib documentation:
DESCRIPTION
cbf_find_category_root sets *categoryroot to the root category of 
which categoryname is an alias. cbf_set_category_root sets 
categoryname_in as an alias of categoryroot in the dictionary 
associated with handle, creating the dictionary if necessary. 
cbf_require_category_root sets *categoryroot to the root category of 
which categoryname is an alias, if there is one, or to the value of 
categoryname, if categoryname is not an alias.
A returned categoryroot string must not be modified in any way.
ARGUMENTS
handle            CBF handle. categoryname      category name which 
may be an alias. categoryroot      pointer to a returned category 
root name. categoryroot_in   input category root name.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")find_category_root;

const char*  find_category_root(const char* categoryname){
   const char * result;
   cbf_failnez(cbf_find_category_root(self,categoryname,&result));
   return result;
}
%feature("autodoc", "
Returns : 
*args   : int compression,int binary_id,(binary) String data,int elsize,
          int elsigned,int elements,String byteorder,int dimfast,int dimmid,
          int dimslow,int padding

C prototype: int cbf_set_integerarray_wdims_fs (cbf_handle handle,
                 unsigned int    compression, int binary_id, void *array,
                 size_t elsize, int elsigned,    size_t elements,
                 const char *byteorder, size_t dimfast, size_t dimmid,
                    size_t dimslow, size_t padding);

CBFLib documentation:
DESCRIPTION
cbf_set_integerarray sets the binary value of the item at the current 
column and row to an integer array. The array consists of elements 
elements of elsize bytes each, starting at array. The elements are 
signed if elsigned is non-0 and unsigned otherwise. binary_id is the 
binary section identifier. cbf_set_realarray sets the binary value of 
the item at the current column and row to an integer array. The array 
consists of elements elements of elsize bytes each, starting at 
array. binary_id is the binary section identifier.
The cbf_set_integerarray_wdims, cbf_set_integerarray_wdims_fs, 
cbf_set_integerarray_wdims_sf, cbf_set_realarray_wdims, 
cbf_set_realarray_wdims_fs and cbf_set_realarray_wdims_sf variants 
allow the data header values of byteorder, dimfast, dimmid, dimslow 
and padding to be set to the data byte order, the fastest, second 
fastest and third fastest array dimensions and the size in byte of 
the post data padding to be used.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL       Canonical-code compression (section 3.3.1) 
CBF_PACKED          CCP4-style packing (section 3.3.2) CBF_PACKED_V2  
     CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET    
 Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE            No compression. 
NOTE: This scheme is by far the slowest of the four and uses much 
more disk space. It is intended for routine use with small arrays 
only. With large arrays (like images) it should be used only for 
debugging.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned), for cbf_set_integerarray, or IEEE doubles or 
floats for cbf_set_realarray. If elsize is not equal to sizeof 
(char), sizeof (short) or sizeof (int), the function returns 
CBF_ARGUMENT.
ARGUMENTS
handle        CBF handle. compression   Compression method to use. 
binary_id     Integer binary identifier. array         Pointer to the 
source array. elsize        Size in bytes of each source array 
element. elsigned      Set to non-0 if the source array elements are 
signed. elements      The number of elements in the array
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_integerarray_wdims_fs;

    /* CBFlib must NOT modify the data string nor the byteorder string
       which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_integerarray_wdims_fs;
%apply (char *STRING, int LENGTH) { (char *bo, int bolen) } set_integerarray_wdims_fs;

    void set_integerarray_wdims_fs(unsigned int compression, int binary_id, 
             char *data, int len, int elsize, int elsigned, int elements,
             char *bo, int bolen, int dimfast, int dimmid, int dimslow, int padding){
        /* safety check on args */
        size_t els, ele;
        void *array;
        char byteorder[15];
        if(len == elsize*elements && elements==dimfast*dimmid*dimslow){
           array = data;
           els = elsize;
           ele = elements;
           strncpy(byteorder,bo,bolen<15?bolen:14);
           byteorder[bolen<15?bolen:14] = 0;
           cbf_failnez(cbf_set_integerarray_wdims_fs (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, elsigned, (size_t) elements, (const char *)byteorder,
           (size_t)dimfast, (size_t)dimmid, (size_t)dimslow, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : 
*args   : int element_number,int compression,(binary) String data,int elsize,
          int elsign,int dimslow,int dimfast

C prototype: int cbf_set_image_sf(cbf_handle handle, unsigned int reserved,
                 unsigned      int element_number, unsigned int compression,
                 void *array, size_t      elsize, int elsign, size_t ndimslow,
                 size_t ndimfast);

CBFLib documentation:
DESCRIPTION
cbf_set_image, cbf_set_image_fs and cbf_set_image_sf write the image 
array for element number element_number. The array consists of 
ndimfast×ndimslow elements of elsize bytes each, starting at array. 
The elements are signed if elsign is non-zero and unsigned otherwise. 
cbf_set_real_image, cbf_set_real_image_fs and cbf_set_real_image_sf 
write the image array for element number element_number. The array 
consists of ndimfast×ndimslow IEEE double or float elements of 
elsize bytes each, starting at array. cbf_set_3d_image, 
cbf_set_3d_image_fs and cbf_set_3d_image_sf write the 3D image array 
for element number element_number. The array consists of 
ndimfast×ndimmid×ndimslow elements of elsize bytes each, starting 
at array. The elements are signed if elsign is non-0 and unsigned 
otherwise. cbf_set_real_3d_image, cbf_set_real_3d_image_fs and 
cbf_set_real_3d_image_sf writes the 3D image array for element number 
element_number. The array consists of ndimfast×ndimmid×ndimslow 
IEEE double or float elements of elsize bytes each, starting at 
array.
The _fs calls give the dimensions in a fast-to-slow order. The calls 
with no suffix and the calls _sf calls give the dimensions in 
slow-to-fast order
If the array is 1-dimensional, ndimslow should be the array size and 
ndimfast and, for the 3D calls, ndimmid, should be set to 1. If the 
array is 2-dimensional and the 3D calls are used, ndimslow and 
ndimmid should be used for the array dimensions and ndimfast should 
be set to 1.
The array will be compressed using the compression scheme specifed by 
compression. Currently, the available schemes are:
CBF_CANONICAL     Canonical-code compression (section 3.3.1) 
CBF_PACKED        CCP4-style packing (section 3.3.2) CBF_PACKED_V2    
   CCP4-style packing, version 2 (section 3.3.2) CBF_BYTE_OFFSET     
Simple  \"byte_offset \" compression. CBF_NIBBLE_OFFSET   Simple  
\"nibble_offset \" compression. CBF_NONE          No compression.
The values compressed are limited to 64 bits. If any element in the 
array is larger than 64 bits, the value compressed is the nearest 
64-bit value.
Currently, the source array must consist of chars, shorts or ints 
(signed or unsigned)for cbf_set_image, or IEEE doubles or floats for 
cbf_set_real_image. If elsize is not equal to sizeof (short), sizeof 
(int), sizeof(double) or sizeof(float), the function returns 
CBF_ARGUMENT.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle           CBF handle. reserved         Unused. Any value other 
than 0 is invalid. element_number   The number of the detector 
element counting from 0 by order of appearance in the  
\"diffrn_data_frame \" category. compression      Compression type. 
array            Pointer to the image array. elsize           Size in 
bytes of each image array element. elsigned         Set to non-0 if 
the image array elements are signed. ndimslow         Slowest array 
dimension. ndimmid          Second slowest array dimension. ndimfast  
       Fastest array dimension.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")set_image_sf;

    /* CBFlib must NOT modify the data string which belongs to the scripting 
       language we will get and check the length via a typemap */

%apply (char *STRING, int LENGTH) { (char *data, int len) } set_image_sf;

    void set_image_sf(unsigned int element_number,
             unsigned int compression, 
             char *data, int len, int elsize, int elsign, int ndimslow, int ndimfast){
        /* safety check on args */
        size_t els;
        unsigned int reserved;
        void *array;
        if(len == elsize*ndimslow*ndimfast){
           array = data;
           els = elsize;
           reserved = 0;
           cbf_failnez(cbf_set_image_sf (self, reserved, element_number, compression,
           (void *) data,  (size_t) elsize, elsign, (size_t) ndimslow, (size_t)ndimfast)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
%feature("autodoc", "
Returns : String
*args   : String axis_id

C prototype: int cbf_get_axis_equipment (cbf_handle handle,
                 const char *axis_id,      const char * *equipment);

CBFLib documentation:
DESCRIPTION
cbf_count_axis_ancestors sets ancestors to the number of ancestors of 
axis axis_id. cbf_get_axis_ancestor sets *ancestor to the ancestor 
axis of index ancestor_index of axis axis_id, starting with axis_id 
for ancestor_index 0.
cbf_get_axis_depends_on sets *depends_on to the immediate ancestor of 
axis_id or to  \". \" if there is no such ancestor. 
cbf_get_axis_equipment sets *equipment to the equipment of axis_id or 
to  \". \" if there is no such equipment. 
cbf_get_axis_equipment_component sets *equipment_component to the 
equipment_component of axis_id or to  \". \" if there is no such 
equipment_component.
cbf_get_axis_offset sets *offset1, *offset2 and *offset3 to the 
components of the ofset of axis_id.
cbf_get_axis_rotation sets rotation to the rotation of axis_id or to 
0 if there is no such rotation. cbf_get_axis_rotation_axis sets 
*rotation_axis to the rotation_axis of axis_id or to  \". \" if there 
is no such rotation_axis.
cbf_get_axis_setting sets *start and *increment to the corresponding 
values of the axis axis_id. Any of the destination pointers may be 
NULL.
cbf_get_axis_type sets axis_type to the type of axis_id.
cbf_get_axis_vector sets *vector1, *vector2 and *vector3 to the 
components of the vector of axis_id.
The parameter reserved is presently unused and should be set to 0.
ARGUMENTS
handle                CBF handle. reserved              Unused. Any 
value other than 0 is invalid. axis_id               Axis id. 
ancestor_index        Integer index of the desired ancestor, starting 
with 0 for the current axis_id. ancestor              Pointer to 
destination ancestor name pointer. depends_on            Pointer to 
destination depends_on name pointer. equipment             Pointer to 
destination equipment name pointer. equipment_component   Pointer to 
destination equipment_component name pointer. offset1               
Pointer to destination first offset component value. offset2          
     Pointer to destination second offset component value. offset3    
           Pointer to destination third offset component value. 
rotation              Pointer to destination rotation value. 
rotation_axis         Pointer to destination rotation_axisn name 
pointer. start                 Pointer to the destination start 
value. increment             Pointer to the destination increment 
value. type                  Pointer to destination axis type of type 
. vector1               Pointer to destination first vector component 
value. vector2               Pointer to destination second vector 
component value. vector3               Pointer to destination third 
vector component value.
RETURN VALUE
Returns an error code on failure or 0 for success.
----------------------------------------------------------------------
")get_axis_equipment;

   const char *  get_axis_equipment(const char *axis_id){
        const char* equip;
        cbf_failnez(cbf_get_axis_equipment(self,axis_id,
           &equip));
        return equip;
        }
%feature("autodoc", "
Returns : 
*args   : double cell[6]

C prototype: int cbf_set_unit_cell (cbf_handle handle, double cell[6],
                 double      cell_esd[6] );

CBFLib documentation:
DESCRIPTION
cbf_set_unit_cell sets the cell parameters to the double values given 
in cell[0:2] for the cell edge lengths a, b and c in Ångstroms, the 
double values given in cell[3:5] for the cell angles α, β and γ in 
degrees, the double values given in cell_esd[0:2] for the estimated 
strandard deviations of the cell edge lengths a, b and c in 
Ångstroms, and the double values given in cell_esd[3:5] for the 
estimated standard deviations of the the cell angles α, β and γ in 
degrees.
The values are placed in the first row of the  \"cell \" category. If 
no value has been given for  \"_cell.entry_id \", it is set to the 
value of the  \"diffrn.id \" entry of the current data block.
cell or cell_esd may be NULL.
If cell is NULL, the cell parameters are not set.
If cell_esd is NULL, the cell parameter esds are not set.
If the  \"cell \" category is not present, it is created. If any of 
the necessary columns are not present, they are created.
ARGUMENTS
handle     CBF handle. cell       Pointer to the array of 6 doubles 
for the cell parameters. cell_esd   Pointer to the array of 6 doubles 
for the cell parameter esds.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_unit_cell;

   void set_unit_cell(double cell[6]) {
     cbf_failnez(cbf_set_unit_cell(self,cell,NULL));
   }
%feature("autodoc", "
Returns : 
*args   : double cell_esd[6]

C prototype: int cbf_set_unit_cell (cbf_handle handle, double cell[6],
                 double      cell_esd[6] );

CBFLib documentation:
DESCRIPTION
cbf_set_unit_cell sets the cell parameters to the double values given 
in cell[0:2] for the cell edge lengths a, b and c in Ångstroms, the 
double values given in cell[3:5] for the cell angles α, β and γ in 
degrees, the double values given in cell_esd[0:2] for the estimated 
strandard deviations of the cell edge lengths a, b and c in 
Ångstroms, and the double values given in cell_esd[3:5] for the 
estimated standard deviations of the the cell angles α, β and γ in 
degrees.
The values are placed in the first row of the  \"cell \" category. If 
no value has been given for  \"_cell.entry_id \", it is set to the 
value of the  \"diffrn.id \" entry of the current data block.
cell or cell_esd may be NULL.
If cell is NULL, the cell parameters are not set.
If cell_esd is NULL, the cell parameter esds are not set.
If the  \"cell \" category is not present, it is created. If any of 
the necessary columns are not present, they are created.
ARGUMENTS
handle     CBF handle. cell       Pointer to the array of 6 doubles 
for the cell parameters. cell_esd   Pointer to the array of 6 doubles 
for the cell parameter esds.
RETURN VALUE
Returns an error code on failure or 0 for success.
SEE ALSO
")set_unit_cell_esd;

   void set_unit_cell_esd(double cell_esd[6]) {
     cbf_failnez(cbf_set_unit_cell(self,NULL,cell_esd));
   }

}; // End of cbf_handle_struct
