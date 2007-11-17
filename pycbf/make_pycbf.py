
print "\\begin{verbatim}"
print "This output comes from make_pycbf.py which generates the wrappers"

# Get the ascii text as a list of strings 
lines = open("CBFlib.txt","r").readlines()

# Variables to hold the useful things we find in the file
docstring = "\n"
name=""

# Flag to indicate we have not read anything useful yet
on=0

# Dictionary of function prototypes and documentation, keyed by name in C.
name_dict = {}
i=-1
debug = 0
# Parse the text
while i<len(lines)-1:
   i=i+1
   line=lines[i]
   nfunc = 0
   if line.find("PROTOTYPE")>=0 and on==1:
      on=10 # Only try for ten lines after it say PROTOTYPE
      continue
   if line.find("#include")>=0: # why?
      continue 
   if line.find("int cbf_")>=0: # We found a function
      # keep going up to DESCRIPTION
      prototypes=""+lines[i].rstrip()+" "
      check=0
      while lines[i+1].find("DESCRIPTION")==-1:
         i=i+1
         prototypes+=lines[i].rstrip()+" " # lose the \n
         check+=1
         if check>20:
            raise Exception("Runaway prototype "+prototypes)
      on=1 # Keep reading docstring
      continue
   if on > 1: # why?
      on=on-1
   if line.find("3. File format")>=0 and on==1:
      # Stop processing at section 3
      i=len(lines)
   if on==1:
      # Docstring ends at 2.xxx for next function or see also
      # We are losing the see also information for now (needed the section
      # breaks in the rtf file)
      if len(line.strip())==0:
         docstring+="\n"
         continue
      else:
         if docstring[-1]=="\n":
            docstring += line.lstrip().rstrip()
         else:
            docstring =docstring+" "+line.lstrip().rstrip()
      if line.strip()[0] in [str(j) for j in range(9)] or \
            line.find("SEE ALSO")>=0 or\
            line.find("________")>=0:
         if len(docstring)>0:
            docstring = docstring.replace("\"", "\\\"") # escape the quotes
            for prototype in prototypes.strip().split(";")[:-1]:
                name = prototype.split("(")[0].strip()
                cname = name.split()[1].strip()
                prototype = prototype.strip()+";"
                name_dict[cname]=[prototype,docstring]
      #  print "Found ",prototype
            docstring="\n"
            prototype=""
            cname=""
            on=0
         else:
            raise Exception("bad docstring")
   


# End of CBFlib.txt file - now generate wrapper code for swig



def myformat(s,l,indent=0,breakon=" "):
   """
   Try to pretty print lines - this is a pain...
   """
   lines = s.rstrip().split("\n")
   out=""
   for line in lines:
      if len(line)==0:
         continue # skip blank lines
      if len(line)>l:
         words = line.split(breakon)
         newline=words[0]
         if len(words)>1:
            for word in words[1:]:
               if len(newline)+len(word)+1 < l:
                  newline=newline+breakon+word
               else:
                  out = out+newline+breakon+"\n"+indent*" "
                  newline=word       
            out += newline+"\n"
         else:
            out += "\n"
      else:
         out += line+"\n" # Last one
   if out == "":
      return "\n"
   else:
      return out


def docstringwrite(pyfunc,input,output,prototype,cbflibdoc):
   doc = "%feature(\"autodoc\", \"\nReturns : "
   returns = ""
   for out in output:
      returns += out+","
   if len(returns)>0:
      doc += myformat(returns[:-1],70,indent = 10,breakon=",")
   else:
      doc += "\n"
   doc += "*args   : "
   takes = ""
   for inp in input:
      takes += inp+","
   if len(takes)>0:
      doc += myformat(takes[:-1],70,indent = 10,breakon=",")   
   else:
      doc += "\n"
   doc += "\nC prototype: "+myformat(prototype,65,indent=16,breakon=",")
   doc += "\nCBFLib documentation:\n"+myformat(cbflibdoc,70)+"\")"
   doc += pyfunc+";\n"
   return doc




cbfhandle_specials = {

"cbf_get_integerarrayparameters":["""
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
""","get_integerarrayparameters",[],["int compression","int binary_id", 
     "int elsize", "int elsigned", "int elunsigned", 
     "int elements", "int minelement", "int maxelement"]],


"cbf_get_realarrayparameters":["""
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
""","get_realarrayparameters",[],["int compression","int binary_id", 
     "int elsize", "int elements"]],



"cbf_get_integerarray":["""
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
""","get_integerarray_as_string",[],["(Binary)String"] ],

"cbf_set_integerarray":["""
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
""","set_integerarray",
[ "int compression", "int binary_id","(binary) String data", 
 "int elsize", "int elsigned","int elements"],[]],


"cbf_get_image_size": ["""
%apply int *OUTPUT {int *ndim1, int *ndim2} get_image_size;
     void get_image_size(unsigned int element_number, int *ndim1, int *ndim2){
        unsigned int reserved;
        size_t in1, in2;
        reserved = 0;
        cbf_failnez(cbf_get_image_size(self,reserved,element_number,&in1,&in2));
        *ndim1 = in1; /* FIXME - is that how to convert? */
        *ndim2 = in2; 
        }
""","get_image_size",["Integer element_number"],["size_t ndim1","size_t ndim2"]],



"cbf_get_pixel_size" : ["""
%apply double *OUTPUT {double *psize} get_pixel_size;
    void get_pixel_size(unsigned int element_number, 
                        unsigned int axis_number, double *psize){
        cbf_failnez(cbf_get_pixel_size(self, 
                                       element_number, 
                                       axis_number, 
                                       psize));
    }
""","get_pixel_size",["Int element_number","Int axis_number"],
                     ["Float pixel_size"]] ,



"cbf_set_pixel_size":["""
     void set_pixel_size (unsigned int element_number, 
                          unsigned int axis_number, double psize){
         cbf_failnez(cbf_set_pixel_size(self, 
                                        element_number, 
                                        axis_number, 
                                        psize));
     }
""","set_pixel_size",
   ["Int element_number","Int axis_number","Float pixel size"],[]],




"cbf_write_file" : ["""
    void write_file(const char* filename, int ciforcbf, int headers, 
                    int encoding){
       FILE *stream;
       int readable;
       /* Make the file non-0 to make CBFlib close the file */
       readable = 1;
       if ( ! ( stream = fopen (filename, "w+b")) ){
         cbf_failnez(CBF_FILEOPEN);
        }
        else{
        cbf_failnez(cbf_write_file(self, stream, readable, 
                    ciforcbf, headers, encoding));

        }
       }
""","write_file",["String filename","Integer ciforcbf","Integer Headers", 
                  "Integer encoding"],[]],


"cbf_read_template":["""
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

""","read_template",["String filename"],[]],


"cbf_read_file" : ["""
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
""","read_file",["String filename","Integer headers"],[]],


"cbf_set_doublevalue":["""
     void set_doublevalue(const char *format, double number){
        cbf_failnez(cbf_set_doublevalue(self,format,number));}
""","set_doublevalue",["String format","Float number"],[]],


"cbf_require_integervalue":["""
%apply int *OUTPUT {int *number} require_integervalue;

     void require_integervalue(int *number, int thedefault){

     cbf_failnez(cbf_require_integervalue(self,number,thedefault));

     }
""","require_integervalue", ["Int thedefault"],["Int number"]],


"cbf_require_doublevalue":["""
%apply double *OUTPUT {double *number} require_doublevalue;
void require_doublevalue(double *number, double defaultvalue){
   cbf_failnez(cbf_require_doublevalue(self,number,defaultvalue));
}
""","require_doublevalue",["Float Default"],["Float Number"]],


"cbf_require_column_value":["""
 const char* require_column_value(const char *columnname,
                                  const char *defaultvalue){
   const char * result;
   cbf_failnez(cbf_require_column_value(self,columnname,
                                    &result,defaultvalue));
   return result;
}
""","require_column_value",
    ["String columnnanme","String Default"],["String Name"]],


"cbf_require_column_doublevalue":["""
%apply double *OUTPUT { double *number} require_column_doublevalue;
void require_column_doublevalue(const char *columnname, double * number,
             const double defaultvalue){
    cbf_failnez(cbf_require_column_doublevalue(self,
                  columnname,number,defaultvalue));
    }
""","require_column_doublevalue",["String columnname","Float Value"],
                                 ["Float defaultvalue"]],


"cbf_require_column_integervalue":["""
%apply int *OUTPUT {int *number}  require_column_integervalue;
void require_column_integervalue(const char *columnname, 
                       int *number, const int defaultvalue){
    cbf_failnez(cbf_require_column_integervalue(self,
           columnname, number,defaultvalue));
    }
""","require_column_integervalue",["String Columnvalue","Int default"],
 ["Int Value"]],

           


"cbf_require_value" : ["""

   const char* require_value(const char* defaultvalue){
     const char * result;
     cbf_failnez(cbf_require_value(self, &result, defaultvalue));
     return result;
    }
""","require_value",["String defaultvalue"],['String Value']],


"cbf_require_diffrn_id":["""
   const char* require_diffrn_id(const char* defaultid){
     const char * id;
     cbf_failnez(cbf_require_diffrn_id(self,&id,defaultid));
     return id;
     }
""","require_diffrn_id", ["String Default_id"],["String diffrn_id"]],



"cbf_get_polarization":["""
     /* Returns a pair of double values */
%apply double *OUTPUT { double *in1, double *in2 };
     void get_polarization(double *in1,double *in2){
        cbf_failnez(cbf_get_polarization (self, in1, in2));
     }
""","get_polarization",[],
    ["float polarizn_source_ratio","float polarizn_source_norm"]],


"cbf_set_polarization":["""
     void set_polarization (double polarizn_source_ratio,
                            double polarizn_source_norm){
         cbf_failnez(cbf_set_polarization(self,
                         polarizn_source_ratio,
                         polarizn_source_norm));
     }
""","set_polarization",
   ["Float polarizn_source_ratio","Float polarizn_source_norm"],[]],


"cbf_get_divergence":["""
%apply double *OUTPUT {double *div_x_source, double *div_y_source,
                       double *div_x_y_source } get_divergence;
    void get_divergence(double *div_x_source, double *div_y_source,
       double *div_x_y_source){
       cbf_failnez(cbf_get_divergence(self, 
                                     div_x_source, 
                                     div_y_source,
                                     div_x_y_source)); 
       } 
""","get_divergence",[],
     ["Float div_x_source","Float div_y_source","Float div_x_y_source"]],


"cbf_set_divergence":["""
   void set_divergence ( double div_x_source, double div_y_source,
                        double div_x_y_source){
      cbf_failnez(cbf_set_divergence (self, div_x_source, 
                              div_y_source,div_x_y_source));
      }
""","set_divergence",
    ["Float div_x_source","Float div_y_source","Float div_x_y_source"],[]],

"cbf_get_gain":["""
%apply double *OUTPUT {double *gain, double *gain_esd} get_gain;
    void get_gain (unsigned int element_number, double *gain, 
                   double *gain_esd){
        cbf_failnez(cbf_get_gain (self, element_number, gain, gain_esd));
        }
""","get_gain",
    [],["Float gain", "Float gain_esd"]],


"cbf_set_gain":["""
    void set_gain (unsigned int element_number, double gain, double gain_esd){
        cbf_failnez(cbf_set_gain (self, element_number, gain, gain_esd));
        }
""","set_gain",["Float gain", "Float gain_esd"],[]],

"cbf_get_element_id":["""
   const char * get_element_id(unsigned int element_number){
       const char * result;
       cbf_failnez(cbf_get_element_id (self, element_number, &result));
       return result;
       }
""","get_element_id", ["Integer element_number"],["String"]],


"cbf_set_axis_setting":["""
   void set_axis_setting(const char *axis_id,
                    double start, double increment){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_set_axis_setting(self,reserved,
                         axis_id,start,increment));
        }
""","set_axis_setting",["String axis_id", "Float start", "Float increment"],
 []],


"cbf_get_axis_setting":["""
%apply double *OUTPUT {double *start, double *increment} get_axis_setting;
   void get_axis_setting(const char *axis_id,
                    double *start, double *increment){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_get_axis_setting(self,reserved,axis_id,
                         start,increment));
        }
""","get_axis_setting",["String axis_id"],["Float start", "Float increment"],],



"cbf_get_datestamp":["""
%apply int *OUTPUT {int *year, int *month, int *day, int *hour, 
                    int *minute, double *second, int *timezone} get_datestamp;
   void get_datestamp(int *year, int *month, int *day, int *hour, 
                      int *minute, double *second, int *timezone){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_get_datestamp(self,reserved,
              year,month,day,hour,minute,second,timezone));
        }
""","get_datestamp",[],["int year", "int month", "int day", "int hour", 
"int minute", "double second", "int timezone"]],



"cbf_set_datestamp":["""
   void set_datestamp(int year, int month, int day, int hour, 
                      int minute, double second, int timezone, 
                      double precision){
        unsigned int reserved;
        reserved = 0; 
        cbf_failnez(cbf_set_datestamp(self,reserved, 
              year,month,day,hour,minute,second,timezone,precision));
        }
""","set_datestamp",["int year", "int month", "int day", "int hour", 
"int minute", "double second", "int timezone","Float precision"],[]],


"cbf_get_timestamp":["""
%apply double *OUTPUT {double *time} get_timestamp;
%apply int *OUTPUT {int *timezone} get_timestamp;
    void get_timestamp(double *time, int *timezone){
        unsigned int reserved;
        reserved = 0; 
        cbf_failnez(cbf_get_timestamp(self,reserved,time,timezone));
        }
""","get_timestamp",[],["Float time","Integer timezone"]],



"cbf_set_timestamp":["""
    void set_timestamp(double time, int timezone, double precision){
        unsigned int reserved;
        reserved = 0; 
        cbf_failnez(cbf_set_timestamp(self,reserved,time,timezone,precision));
        }
""","set_timestamp",["Float time","Integer timezone","Float precision"],[]],


"cbf_set_current_timestamp":["""
    void set_current_timestamp(int timezone){
        unsigned int reserved;
        reserved = 0; 
        cbf_failnez(cbf_set_current_timestamp(self,reserved,timezone));
        }
""","set_current_timestamp",["Integer timezone"],[]],



"cbf_get_overload":["""
%apply double *OUTPUT {double *overload} get_overload;
   void get_overload(unsigned int element_number, double *overload){
        cbf_failnez(cbf_get_overload(self,element_number,overload));
        }
""","get_overload",["Integer element_number"],["Float overload"]],

"cbf_set_overload":["""
   void set_overload(unsigned int element_number, double overload){
        cbf_failnez(cbf_set_overload(self,element_number,overload));
        }
""","set_overload",["Integer element_number","Float overload"],[]],


"cbf_set_integration_time":["""
   void set_integration_time(double time){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_set_integration_time(self,reserved,time));
        }
""","set_integration_time",["Float time"],[]],



"cbf_get_integration_time":["""
%apply double *OUTPUT {double *time} get_integration_time;
   void get_integration_time(double *time){
        unsigned int reserved;
        reserved = 0;
        cbf_failnez(cbf_get_integration_time(self,reserved,time));
        }
""","get_integration_time",[],["Float time"]],

"cbf_get_orientation_matrix":["""
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
""","get_orientation_matrix",
    [],[ "Float matrix_%d"%(ind) for ind in range(9) ]],


"cbf_set_tag_category":["""
   void set_tag_category(const char *tagname, const char* categoryname_in){
     cbf_failnez(cbf_set_tag_category(self,tagname, categoryname_in));
     }
""","set_tag_category",["String tagname","String categoryname_in"],[] ],



"cbf_find_tag_category":["""

   const char * find_tag_category(const char *tagname){
     const char * result;
     cbf_failnez(cbf_find_tag_category(self,tagname, &result));
     return result;
     }
""","find_tag_category",["String tagname"],["String categoryname_in"] ],


"cbf_require_tag_root":["""
const char* require_tag_root(const char* tagname){
 const char* result;
 cbf_failnez(cbf_require_tag_root(self,tagname,&result));
 return result;
 }
""","require_tag_root",["String tagname"],["String tagroot"]],

"cbf_find_tag_root":["""
const char * find_tag_root(const char* tagname){
   const char* result;
   cbf_failnez(cbf_find_tag_root(self,tagname,&result));
   return result;
}
""","find_tag_root",["String tagname"],["String tagroot"]],


"cbf_set_tag_root":["""
void  set_tag_root(const char* tagname, const char* tagroot_in){
   cbf_failnez(cbf_set_tag_root(self,tagname,tagroot_in));
}
""","set_tag_root",["String tagname","String tagroot_in"],[]],

"cbf_set_category_root":["""
void  set_category_root(const char* categoryname, const char* categoryroot){
   cbf_failnez(cbf_set_category_root(self,categoryname,categoryroot));
}
""","set_category_root",["String categoryname","String categoryroot"],[]],


"cbf_find_category_root":["""
const char*  find_category_root(const char* categoryname){
   const char * result;
   cbf_failnez(cbf_find_category_root(self,categoryname,&result));
   return result;
}
""","find_category_root",["String categoryname"],["String categoryroot"]],






"cbf_require_category_root":["""
const char* require_category_root (const char* categoryname){
  const char* result;
  cbf_failnez(cbf_require_category_root(self,categoryname, &result));
  return result;
}
""","cbf_require_category_root",["String Categoryname"],["String categoryroot"]],
  

"cbf_set_orientation_matrix":["""
   void set_orientation_matrix(  double m0,double m1,
double  m2,double  m3,double  m4,double m5,double m6,
double  m7,double  m8){
        double m[9];
        m[0] = m0; m[1]=m1 ; m[2]=m2 ;
        m[3] = m3; m[4]=m4 ; m[5]=m5 ;
        m[6] = m6; m[7]=m7 ; m[8]=m8 ;
        cbf_failnez(cbf_get_orientation_matrix(self,m));
        }
""","set_orientation_matrix",
    [ "Float matrix_%d"%(ind) for ind in range(9) ] ,[]],


# cbfhandle dict functions UNTESTED


"cbf_require_dictionary":["""
cbf_handle require_dictionary(){
   cbf_handle temp;
   cbf_failnez(cbf_require_dictionary(self,&temp));
   return temp;
}
""","require_dictionary",[],["CBFHandle dictionary"]],


"cbf_get_dictionary":["""
cbf_handle get_dictionary(){
   cbf_handle temp;
   cbf_failnez(cbf_get_dictionary(self,&temp));
   return temp;
}
""","get_dictionary",[],["CBFHandle dictionary"]],



"cbf_set_dictionary":["""
void set_dictionary(cbf_handle other){
   cbf_failnez(cbf_set_dictionary(self,other));
}
""","set_dictionary",["CBFHandle dictionary"],[]],



"cbf_convert_dictionary":["""
void convert_dictionary(cbf_handle other){
   cbf_failnez(cbf_convert_dictionary(self,other));
}
""","convert_dictionary",["CBFHandle dictionary"],[]],




# Prelude to the next but one section of the nuweb doc


"cbf_construct_detector":["""
 cbf_detector construct_detector(unsigned int element_number){
    cbf_detector detector;
    cbf_failnez(cbf_construct_detector(self,&detector,element_number));
    return detector;
    }
""","construct_detector",["Integer element_number"],["pycbf detector object"]],


# Prelude to the next section of the nuweb doc

"cbf_construct_goniometer":["""
 cbf_goniometer construct_goniometer(){
    cbf_goniometer goniometer;
    cbf_failnez(cbf_construct_goniometer(self,&goniometer));
    return goniometer;
    }
""","construct_goniometer",[],["pycbf goniometer object"]],

}


class cbfhandlewrapper:
   def __init__(self):
      self.code = """
// Tell SWIG not to make constructor for these objects
%nodefault cbf_handle;
%nodefault cbf_handle_struct;
%nodefault cbf_node;

// A couple of blockitem functions return CBF_NODETYPE
typedef enum
{
  CBF_UNDEFINED,        /* Undefined */
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
"""
      self.tail = """
}; // End of cbf_handle_struct
"""
   # End of init function
   def get_code(self):
       return self.code+self.tail
   def wrap(self,cfunc,prototype,args,docstring):
       pyfunc = cfunc.replace("cbf_","")
       # Insert a comment for debugging this script
       code = "\n/* cfunc %s   pyfunc %s  \n"%(cfunc,pyfunc)
       for a in args:
          code += "   arg %s "%(a)
       code += "*/\n\n"
       # Make and free handle are done in the header so skip
       if cfunc.find("cbf_make_handle")>-1 or cfunc.find("cbf_free_handle")>-1:
          # Constructor and destructor done in headers
          return
       if args[0] != "cbf_handle handle": # Must be for cbfhandle
          print "problem",cfunc,pyfunc,args
          return
       if len(args)==1: # Only takes CBFhandle arg
          code+= docstringwrite(pyfunc,[],[],prototype,docstring)
          code+= "    void %s(void){\n"%(pyfunc)
          code+= "      cbf_failnez(%s(self));}\n"%(cfunc) 
          self.code=self.code+code
          return
       # Now case by case rather than writing a proper parser
       # Special cases ...
       not_found=0
       try:
           code, pyname, input, output = cbfhandle_specials[cfunc]
           self.code +=  docstringwrite(pyname,input,output,
                                              prototype,docstring)+ code
           return
       except KeyError:
           not_found = 1
       except ValueError:
           print "problem in",cfunc
           for item in cbfhandle_specials[cfunc]:
              print "***",item
           raise
       if len(args)==2:
          if args[1].find("const char")>-1 and \
             args[1].find("*")>-1          and \
             args[1].find("**")==-1            :
             # 1 input string
             code += docstringwrite(pyfunc,[],["string"],prototype,docstring)
             code += "    void %s(const char* arg){\n"%(pyfunc)
             code +="      cbf_failnez(%s(self,arg));}\n"%(cfunc)
             self.code=self.code+code
             return
          if args[1].find("const char")>-1 and \
             args[1].find("**")>-1                :# return string
             code += docstringwrite(pyfunc,["string"],[],prototype,docstring)
             code += "    const char* %s(void){\n"%(pyfunc)
             code += "    const char* result;\n"
             code += "    cbf_failnez(%s(self, &result));\n"%(cfunc)
             code += "    return result;}\n"
             self.code=self.code+code
             return
          if args[1].find("unsigned int")>-1 and args[1].find("*")==-1:
             # set uint
             if args[1].find("reserved")>-1:
                raise Exception("Setting reserved??? %s %s %s"%(pyfunc,
                                                           cfunc,str(args)))
             code += docstringwrite(pyfunc,["Integer"],[],prototype,docstring)
             code +="    void %s(unsigned int arg){\n"%(pyfunc)
             code +="      cbf_failnez(%s(self,arg));}\n"%(cfunc)
             self.code=self.code+code
             return
          if args[1].find("unsigned int *")>-1 and args[1].find("**")==-1:
             # output uint
             if args[1].find("reserved")>-1:
                raise Exception("Setting reserved??? %s %s %s"%(pyfunc,
                                                           cfunc,str(args)))
             code += docstringwrite(pyfunc,[],["Integer"],prototype,docstring)
             code +="    unsigned int %s(void){\n"%(pyfunc)
             code +="      unsigned int result;\n"
             code +="      cbf_failnez(%s(self,&result));\n"%(cfunc)
             code +="      return result;}\n"
             self.code=self.code+code
             return
          # For the rest attempt to guess
          if args[1].find("cbf")==-1: # but do not try the goniometer constructor
             if args[1].find("*")>-1 and args[1].find("cbf")==-1:
                # pointer used for returning something
                type = args[1].split(" ")[0]
                code += docstringwrite(pyfunc,[],[type.replace("*","")],
                                                          prototype,docstring)
                code+= "    "+type+" "+pyfunc+"(void){\n"
                code+= "     "+type+" result;\n"
                code+= "       cbf_failnez(%s(self,&result));\n"%(cfunc)
                code+= "       return result;}\n"
                self.code=self.code+code
                return
             else:
                var = args[1].split(" ")[-1]
                code += docstringwrite(pyfunc,[],[args[1]],prototype,docstring)
                code+= "     void %s(%s){\n"%(pyfunc,args[1])
                code +="        cbf_failnez(%s(self,%s));}\n"%(cfunc,var)
                self.code=self.code+code
                return
       if not_found:
             code+= "     void %s(void){\n"%(pyfunc)
             code +="        cbf_failnez(CBF_NOTIMPLEMENTED);}\n"
             self.code=self.code+code
             print "Have not implemented: cbfhandle.%s"%(pyfunc)
             print "   ",cfunc
             print "    args:"
             for a in args:
                 print "       ",a 
             print
             return



cbf_handle_wrapper = cbfhandlewrapper()




cbf_goniometer_specials = {
"cbf_get_rotation_range":["""
%apply double *OUTPUT {double *start,double *increment};

    void get_rotation_range(double *start,double *increment){
       unsigned int reserved;
       reserved = 0;
       cbf_failnez(cbf_get_rotation_range (self,reserved, start,increment));
    }
""","get_rotation_range",[],["Float start","Float increment"]],

"cbf_rotate_vector":["""

%apply double *OUTPUT {double *final1, double *final2, double *final3};

    void rotate_vector (double ratio, double initial1,double initial2, 
         double initial3, double *final1, double *final2, double *final3){
       unsigned int reserved;
       reserved = 0;
       cbf_failnez(cbf_rotate_vector (self, reserved, ratio, initial1,
         initial2, initial3, final1, final2, final3));
    }
""", "rotate_vector",
 [ "double ratio", "double initial1","double initial2", "double initial3" ] , 
                  [ "double final1"  ,"double final2"  , "double final3" ] ],



"cbf_get_reciprocal":["""
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
""", "get_reciprocal",
    ["double ratio","double wavelength",
     "double real1","double real2","double real3"],
    ["double reciprocal1","double reciprocal2", "double reciprocal3" ]],

"cbf_get_rotation_axis":["""
%apply double *OUTPUT {double *vector1,double *vector2, double *vector3};

void get_rotation_axis (double *vector1, double *vector2, double *vector3){
     unsigned int reserved;
     reserved = 0;
     cbf_failnez(cbf_get_rotation_axis (self, reserved, 
                                        vector1, vector2, vector3));
    }
""","get_rotation_axis", [] , 
 ["double vector1", "double vector2", "double vector3"] ],

}



class cbfgoniometerwrapper:
   def __init__(self):
      self.code = """
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
"""
      self.tail = """
}; // End of cbf_positioner
"""
   def wrap(self,cfunc,prototype,args,docstring):
     if cfunc.find("cbf_free_goniometer")>-1:
        return 
     try:
        code, pyname, input, output = cbf_goniometer_specials[cfunc]
        self.code +=  docstringwrite(pyname,input,output,
                                     prototype,docstring)+ code
     except KeyError:
        print "TODO: Goniometer:",prototype
   def get_code(self):
     return self.code+self.tail


cbf_goniometer_wrapper = cbfgoniometerwrapper()



cbf_detector_specials = {
"cbf_get_pixel_normal":["""
%apply double *OUTPUT {double *normal1,double *normal2, double *normal3};
   void get_pixel_normal ( double index1, double index2, 
                          double *normal1,double *normal2, double *normal3){
       cbf_failnez(cbf_get_pixel_normal(self,
                                    index1,index2,normal1,normal2,normal3));
   }

""","get_pixel_normal",["double index1","double index2"] ,
 ["double normal1","double normal2", "double normal3" ] ],

"cbf_get_pixel_area":["""
%apply double *OUTPUT{double *area,double *projected_area};
    void get_pixel_area(double index1, double index2,
                        double *area,double *projected_area){
       cbf_failnez(cbf_get_pixel_area (self,
                                       index1, index2, area,projected_area));
      }
""","get_pixel_area",["double index1", "double index2"],
     ["double area","double projected_area"] ],

"cbf_get_detector_distance":["""
%apply double *OUTPUT {double *distance};
 void get_detector_distance (double *distance){
  cbf_failnez(cbf_get_detector_distance(self,distance));
  }
""","get_detector_distance",[],["double distance"]],

"cbf_get_detector_normal":["""
%apply double *OUTPUT {double *normal1, double *normal2, double *normal3};
   void get_detector_normal(double *normal1, 
                            double *normal2,
                            double *normal3){
     cbf_failnez(cbf_get_detector_normal(self,
                    normal1, normal2, normal3));
   }
""","get_detector_normal",[],
["double normal1", "double normal2", "double normal3"]],

"cbf_get_pixel_coordinates":["""
%apply double *OUTPUT {double *coordinate1,  
         double *coordinate2, double *coordinate3};
   void get_pixel_coordinates(double index1, double index2, 
             double *coordinate1,   
             double *coordinate2, 
             double *coordinate3){
      cbf_failnez(cbf_get_pixel_coordinates(self,index1,index2,
             coordinate1,coordinate2,coordinate3));
   }
""","get_pixel_coordinates",["double index1","double index2"],
["double coordinate1", "double coordinate2", "double coordinate3"] ],

"cbf_get_beam_center":["""
%apply double *OUTPUT {double *index1, double *index2, 
 double *center1,double *center2};
    void get_beam_center(double *index1, double *index2, 
                         double *center1,double *center2){
        cbf_failnez(cbf_get_beam_center(self, index1, index2, 
                                       center1, center2));
        }
""","get_beam_center",[],
["double index1", "double index2", "double center1","double center2"]],



"cbf_get_inferred_pixel_size" : ["""
%apply double *OUTPUT { double *psize } get_inferred_pixel_size;
void get_inferred_pixel_size(unsigned int axis_number, double* psize){
   cbf_failnez(cbf_get_inferred_pixel_size(self, axis_number, psize));
   }
""","get_inferred_pixel_size",["Int axis_number"],["Float pixel size"] ]


}



class cbfdetectorwrapper:
   def __init__(self):
      self.code = """
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
"""
      self.tail = """
}; // End of cbf_detector
"""
   def wrap(self,cfunc,prototype,args,docstring):
     if cfunc.find("cbf_free_detector")>-1:
        return 
     try:
        code, pyname, input, output = cbf_detector_specials[cfunc]
        self.code +=  docstringwrite(pyname,input,output,
                                     prototype,docstring)+ code
     except KeyError:
        print "TODO: Detector:",prototype
   def get_code(self):
     return self.code+self.tail


cbf_detector_wrapper = cbfdetectorwrapper()



class genericwrapper:
   def __init__(self):
       self.code = "// Start of generic functions\n"
       self.tail = "// End of generic functions\n"
   def get_code(self):
       return self.code + self.tail
   def wrap(self,cfunc,prototype,args,docstring):
       pyfunc = cfunc.replace("cbf_","")
       # Insert a comment for debugging this script
       code = "\n/* cfunc %s   pyfunc %s  \n"%(cfunc,pyfunc)
       for a in args:
           code += "   arg %s "%(a)
       code += "*/\n\n"
       self.code+=code
       code = ""
       if len(args)==1 and args[0].find("char")>-1 and \
                           args[0].find("**")>-1                :# return string
           # first write the c code and inline it
           code += docstringwrite(pyfunc,[],["string"],prototype,docstring)
           code += "%%inline %%{\n   char* %s(void);\n"%(pyfunc)
           code += "   char* %s(void){\n"%(pyfunc)
           code += "      char *r;\n"
           code += "      error_status = %s(&r);\n"%(cfunc)
           code += "      return r; }\n%}\n"
           # now the thing to wrap is:
           code += "char* %s(void);"%(pyfunc)
           self.code=self.code+code
           return
           
#       code+= "     void %s(void){\n"%(pyfunc)
#       code +="        cbf_failnez(CBF_NOTIMPLEMENTED);}\n"
#       self.code=self.code+code
       print "Have not implemented:"
       for s in [cfunc, pyfunc] + args:
           print "\t",s 
       print
       return



generic_wrapper = genericwrapper()

def generate_wrappers(name_dict):
   names = name_dict.keys()
   for cname in names:
      prototype = name_dict[cname][0]
      docstring = name_dict[cname][1]
      # Check prototype begins with "int cbf_"
      if prototype.find("int cbf_")!=0:
         print "problem with:",prototype
      # Get arguments from prototypes
      try:
         args = prototype.split("(")[1].split(")")[0].split(",")
         args = [ s.lstrip().rstrip() for s in args ] # strip spaces off ends
      except:
         print cname
         print prototype
         raise
      if args[0].find("cbf_handle")>=0: # This is for the cbfhandle object
         cbf_handle_wrapper.wrap(cname,prototype,args,docstring)
         continue
      if args[0].find("cbf_goniometer")>=0: # This is for the cbfgoniometer
         cbf_goniometer_wrapper.wrap(cname,prototype,args,docstring)
         continue
      if args[0].find("cbf_detector")>=0: # This is for the cbfdetector
         cbf_detector_wrapper.wrap(cname,prototype,args,docstring)
         continue
      generic_wrapper.wrap(cname,prototype,args,docstring)
 

generate_wrappers(name_dict)
open("cbfgoniometerwrappers.i","w").write(cbf_goniometer_wrapper.get_code())
open("cbfdetectorwrappers.i","w").write(cbf_detector_wrapper.get_code())
open("cbfhandlewrappers.i","w").write(cbf_handle_wrapper.get_code())
open("cbfgenericwrappers.i","w").write(generic_wrapper.get_code())

print "End of output from make_pycbf.py"
print "\\end{verbatim}"

