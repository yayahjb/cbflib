
// Tell SWIG not to make constructor for these objects
%nodefault cbf_positioner_struct;
%nodefault cbf_positioner;

// Tell SWIG what the object is, so we can build the class
typedef struct
{
  double matrix [3][4];

  cbf_axis_struct *axis;

  size_t axes;

  int matrix_is_valid, axes_are_connected;
}
cbf_positioner_struct;

typedef cbf_positioner_struct *cbf_positioner;

%feature("autodoc","1");

%extend cbf_positioner_struct{// Tell SWIG to attach functions to the structure

    cbf_positioner_struct(){  // Constructor
       // DO NOT CONSTRUCT WITHOUT A CBFHANDLE
       cbf_failnez(CBF_ARGUMENT);
       return NULL; /* Should never be executed */
       }

    ~cbf_positioner_struct(){ // Destructor
       cbf_failnez(cbf_free_positioner(self));
       }

}; // End of cbf_positioner
