% make_pycbf.w
% nuweb source file used to create
% make_pycbf.py and to document it in pycbf.w
%
% pycbf - python binding to the CBFlib library
%
% Copyright (C) 2005  Jonathan Wright
%     ESRF, Grenoble, France
%     email: wright@@esrf.fr
%
% Revised for CBFlib 0.9 releases, Herbert J. Bernstein, 23 Aug 2010
%
%######################################################################
%#                                                                    #
%# YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE INCLUDING PYCBF UNDER THE  #
%# TERMS OF THE GPL                                                   #
%#                                                                    #
%# ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API INCLUDING PYCBF  #
%# UNDER THE TERMS OF THE LGPL                                        #
%#                                                                    #
%######################################################################
%
%########################### GPL NOTICES ##############################
%#                                                                    #
%# This program is free software; you can redistribute it and/or      #
%# modify it under the terms of the GNU General Public License as     #
%# published by the Free Software Foundation; either version 2 of     #
%# (the License, or (at your option) any later version.               #
%#                                                                    #
%# This program is distributed in the hope that it will be useful,    #
%# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
%# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      #
%# GNU General Public License for more details.                       #
%#                                                                    #
%# You should have received a copy of the GNU General Public License  #
%# along with this program; if not, write to the Free Software        #
%# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           #
%# 02111-1307  USA                                                    #
%#                                                                    #
%######################################################################
%
%######################### LGPL NOTICES ###############################
%#                                                                    #
%# This library is free software; you can redistribute it and/or      #
%# modify it under the terms of the GNU Lesser General Public         #
%# License as published by the Free Software Foundation; either       #
%# version 2.1 of the License, or (at your option) any later version. #
%#                                                                    #
%# This library is distributed in the hope that it will be useful,    #
%# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
%# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  #
%# Lesser General Public License for more details.                    #
%#                                                                    #
%# You should have received a copy of the GNU Lesser General Public   #
%# License along with this library; if not, write to the Free         #
%# Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    #
%# MA  02110-1301  USA                                                #
%#                                                                    #
%######################################################################
%
\section{Wrappers}

The program that does the conversion from CBFlib.txt to the SWIG
input files is a python script named make\_pycbf.py.

@O make_pycbf.py
@{
print "\\begin{verbatim}"
print "This output comes from make_pycbf.py which generates the wrappers"
print "pycbf Copyright (C) 2005  Jonathan Wright, no warranty, LGPL"


######################################################################
#                                                                    #
# YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE INCLUDING PYCBF UNDER THE  #
# TERMS OF THE GPL                                                   #
#                                                                    #
# ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API INCLUDING PYCBF  #
# UNDER THE TERMS OF THE LGPL                                        #
#                                                                    #
######################################################################


########################### GPL NOTICES ##############################
#                                                                    #
# This program is free software; you can redistribute it and/or      #
# modify it under the terms of the GNU General Public License as     #
# published by the Free Software Foundation; either version 2 of     #
# (the License, or (at your option) any later version.               #
#                                                                    #
# This program is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      #
# GNU General Public License for more details.                       #
#                                                                    #
# You should have received a copy of the GNU General Public License  #
# along with this program; if not, write to the Free Software        #
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           #
# 02111-1307  USA                                                    #
#                                                                    #
######################################################################


######################### LGPL NOTICES ###############################
#                                                                    #
# This library is free software; you can redistribute it and/or      #
# modify it under the terms of the GNU Lesser General Public         #
# License as published by the Free Software Foundation; either       #
# version 2.1 of the License, or (at your option) any later version. #
#                                                                    #
# This library is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  #
# Lesser General Public License for more details.                    #
#                                                                    #
# You should have received a copy of the GNU Lesser General Public   #
# License along with this library; if not, write to the Free         #
# Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    #
# MA  02110-1301  USA                                                #
#                                                                    #
######################################################################


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
prototypes = ""
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
      prototypes+=""+lines[i].rstrip()+" "
      # print lines[i].rstrip()
      check=0
      while lines[i+1].find("DESCRIPTION")==-1 and lines[i+1].find("int cbf_")==-1:
         i=i+1
         prototypes+=lines[i].rstrip()+" " # lose the \n
         # print lines[i].rstrip()
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
            line.find("SEE ALSO")>=0 or \
            line.find("________")>=0 or \
            line.find("--------")>=0:
         if len(docstring)>0:
            # print "Prototypes: ",prototypes
            docstring = docstring.replace("\"", " \\\"") # escape the quotes
            for prototype in prototypes.strip().split(";")[:-1]:
                name = prototype.split("(")[0].strip()
                cname = name.split()[1].strip()
                prototype = prototype.strip()+";"
                name_dict[cname]=[prototype,docstring]
                # print "Prototype: ","::",cname,"::",name,"::", prototype
            prototypes = ""
            # print "Found ",prototype
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


"cbf_get_integerarrayparameters_wdims":["""
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
""","get_integerarrayparameters_wdims",[],["int compression","int binary_id", 
     "int elsize", "int elsigned", "int elunsigned", 
     "int elements", "int minelement", "int maxelement", "char **bo", "int *bolen",
     "int dimfast", "int dimmid", "int dimslow", "int padding"]],


"cbf_get_integerarrayparameters_wdims_fs":["""
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
""","get_integerarrayparameters_wdims_fs",[],["int compression","int binary_id", 
     "int elsize", "int elsigned", "int elunsigned", 
     "int elements", "int minelement", "int maxelement", "char **bo", "int *bolen",
      "int dimfast", "int dimmid", "int dimslow", "int padding"]],


"cbf_get_integerarrayparameters_wdims_sf":["""
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
""","get_integerarrayparameters_wdims_sf",[],["int compression","int binary_id", 
     "int elsize", "int elsigned", "int elunsigned", 
     "int elements", "int minelement", "int maxelement", "char **bo", "int *bolen",
      "int dimslow", "int dimmid", "int dimfast", "int padding"]],


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


"cbf_get_realarrayparameters_wdims":["""
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
         &byteorder,&ds,&dm,&ds,&pd ));
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
""","get_realarrayparameters_wdims",[],["int compression","int binary_id", 
     "int elsize", 
     "int elements", "char **bo", "int *bolen",
     "int dimfast", "int dimmid", "int dimslow", "int padding"]],


"cbf_get_realarrayparameters_wdims_fs":["""
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
         &byteorder,&ds,&dm,&ds,&pd ));
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
""","get_realarrayparameters_wdims_fs",[],["int compression","int binary_id", 
     "int elsize", 
     "int elements", "char **bo", "int *bolen",
      "int dimfast", "int dimmid", "int dimslow", "int padding"]],


"cbf_get_realarrayparameters_wdims_sf":["""
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
""","get_realarrayparameters_wdims_sf",[],["int compression","int binary_id", 
     "int elsize", 
     "int elements", "char **bo", "int *bolen",
      "int dimslow", "int dimmid", "int dimfast", "int padding"]],


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


"cbf_get_image":["""
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
""","get_image_as_string",["int element_number", 
    "int elsize", "int elsign", "int ndimslow", "int ndimfast"],["(Binary)String"] ],


"cbf_get_image_fs":["""
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
""","get_image_fs_as_string",["int element_number", 
    "int elsize", "int elsign", "int ndimfast", "int ndimslow"],["(Binary)String"] ],


"cbf_get_image_sf":["""
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
""","get_image_sf_as_string",["int element_number", 
    "int elsize", "int elsign", "int ndimslow", "int ndimfast"],["(Binary)String"] ],


"cbf_get_real_image":["""
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
""","get_real_image_as_string",["int element_number", 
    "int elsize", "int ndimslow", "int ndimfast"],["(Binary)String"] ],


"cbf_get_real_image_fs":["""
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
""","get_real_image_fs_as_string",["int element_number", 
    "int elsize", "int ndimfast", "int ndimslow"],["(Binary)String"] ],


"cbf_get_real_image_sf":["""
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
""","get_real_image_sf_as_string",["int element_number", 
    "int elsize", "int ndimslow", "int ndimfast"],["(Binary)String"] ],


"cbf_get_3d_image":["""
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
""","get_3d_image_as_string",["int element_number", 
    "int elsize", "int elsign", "int ndimslow", "int ndimmid", "int ndimfast"],["(Binary)String"] ],


"cbf_get_3d_image_fs":["""
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
""","get_3d_image_fs_as_string",["int element_number", 
    "int elsize", "int elsign", "int ndimfast", "int ndimmid", "int ndimslow"],["(Binary)String"] ],


"cbf_get_3d_image_sf":["""
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
""","get_3d_image_sf_as_string",["int element_number", 
    "int elsize", "int elsign", "int ndimslow", "int ndimmid", "int ndimfast"],["(Binary)String"] ],


"cbf_get_real_3d_image":["""
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
""","get_real_3d_image_as_string",["int element_number", 
    "int elsize", "int ndimslow", "int ndimmid", "int ndimfast"],["(Binary)String"] ],


"cbf_get_real_3d_image_fs":["""
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
""","get_real_3d_image_fs_as_string",["int element_number", 
    "int elsize", "int ndimfast", "int ndimmid", "int ndimslow"],["(Binary)String"] ],

"cbf_get_real_3d_image_sf":["""
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
""","get_real_3d_image_sf_as_string",["int element_number", 
    "int elsize", "int ndimslow", "int ndimmid", "int ndimfast"],["(Binary)String"] ],


"cbf_get_realarray":["""
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
""","get_realarray_as_string",[],["(Binary)String"] ],


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


"cbf_set_integerarray_wdims":["""
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
           byteorder[bolen<15?14:bolen] = 0;
           cbf_failnez(cbf_set_integerarray_wdims (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, elsigned, (size_t) elements, (const char *)byteorder,
           (size_t)dimfast, (size_t)dimmid, (size_t)dimslow, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
""","set_integerarray_wdims",
[ "int compression", "int binary_id","(binary) String data", 
 "int elsize","int elements", "String byteorder", "int dimfast", "int dimmid", "int dimslow", "int padding"],[]],


"cbf_set_integerarray_wdims_sf":["""
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
           byteorder[bolen<15?14:bolen] = 0;
           cbf_failnez(cbf_set_integerarray_wdims_sf (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, elsigned, (size_t) elements, (const char *)byteorder,
           (size_t)dimslow, (size_t)dimmid, (size_t)dimfast, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
""","set_integerarray_wdims_sf",
[ "int compression", "int binary_id","(binary) String data", 
 "int elsize","int elements", "String byteorder", "int dimslow", "int dimmid", "int dimfast", "int padding"],[]],

"cbf_set_integerarray_wdims_fs":["""
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
           byteorder[bolen<15?14:bolen] = 0;
           cbf_failnez(cbf_set_integerarray_wdims_fs (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, elsigned, (size_t) elements, (const char *)byteorder,
           (size_t)dimfast, (size_t)dimmid, (size_t)dimslow, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
""","set_integerarray_wdims_fs",
[ "int compression", "int binary_id","(binary) String data", 
 "int elsize","int elements", "String byteorder", "int dimfast", "int dimmid", "int dimslow", "int padding"],[]],


"cbf_set_realarray":["""
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
""","set_realarray",
[ "int compression", "int binary_id","(binary) String data", 
 "int elsize","int elements"],[]],


"cbf_set_realarray_wdims":["""
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
           byteorder[bolen<15?14:bolen] = 0;
           cbf_failnez(cbf_set_realarray_wdims (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, (size_t) elements, (const char *)byteorder,
           (size_t)dimfast, (size_t)dimmid, (size_t)dimslow, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
""","set_realarray_wdims",
[ "int compression", "int binary_id","(binary) String data", 
 "int elsize","int elements", "String byteorder", "int dimfast", "int dimmid", "int dimslow", "int padding"],[]],


"cbf_set_realarray_wdims_sf":["""
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
           byteorder[bolen<15?14:bolen] = 0;
           cbf_failnez(cbf_set_realarray_wdims_sf (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, (size_t) elements, (const char *)byteorder,
           (size_t) dimslow, (size_t) dimmid, (size_t) dimfast, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
""","set_realarray_wdims_sf",
[ "int compression", "int binary_id","(binary) String data", 
 "int elsize","int elements", "String byteorder", "int dimslow", "int dimmid", "int dimfast", "int padding"],[]],


"cbf_set_realarray_wdims_fs":["""
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
           byteorder[bolen<15?14:bolen] = 0;
           cbf_failnez(cbf_set_realarray_wdims_fs (self, compression, binary_id, 
           (void *) data,  (size_t) elsize, (size_t) elements, (const char *)byteorder,
           (size_t) dimfast, (size_t) dimmid, (size_t) dimslow, (size_t)padding)); 
        }else{
           cbf_failnez(CBF_ARGUMENT);
        }
    }
""","set_realarray_wdims_fs",
[ "int compression", "int binary_id","(binary) String data", 
 "int elsize","int elements", "String byteorder", "int dimfast", "int dimmid", "int dimslow", "int padding"],[]],


"cbf_set_image":["""
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
""","set_image",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int elsign", "int dimslow", "int dimfast"],[]],


"cbf_set_image_fs":["""
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
""","set_image_fs",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int elsign", "int dimfast", "int dimslow"],[]],


"cbf_set_image_sf":["""
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
""","set_image_sf",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int elsign", "int dimslow", "int dimfast"],[]],


"cbf_set_real_image":["""
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
""","set_real_image",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int dimslow", "int dimfast"],[]],


"cbf_set_real_image_fs":["""
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
""","set_real_image_fs",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int dimfast", "int dimslow"],[]],


"cbf_set_real_image_sf":["""
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
""","set_real_image_sf",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int dimslow", "int dimfast"],[]],


"cbf_set_3d_image":["""
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
""","set_3d_image",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int elsign", "int dimslow", "int dimmid", "int dimfast"],[]],


"cbf_set_3d_image_fs":["""
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
""","set_3d_image_fs",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int elsign", "int dimfast", "int dimmid", "int dimslow"],[]],


"cbf_set_3d_image_sf":["""
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
""","set_3d_image_sf",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int elsign", "int dimslow", "int dimmid", "int dimfast"],[]],


"cbf_set_real_3d_image":["""
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
""","set_real_3d_image",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int dimslow", "int dimmid", "int dimfast"],[]],


"cbf_set_real_3d_image_fs":["""
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
""","set_real_3d_image_fs",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int dimfast", "int dimmid", "int dimslow"],[]],


"cbf_set_real_3d_image_sf":["""
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
""","set_real_3d_image_sf",
[ "int element_number","int compression","(binary) String data", 
 "int elsize", "int dimslow", "int dimmid", "int dimfast"],[]],


"cbf_get_image_size": ["""
%apply int *OUTPUT {int *ndimslow, int *ndimfast} get_image_size;
     void get_image_size(unsigned int element_number, int *ndimslow, int *ndimfast){
        unsigned int reserved;
        size_t inslow, infast;
        reserved = 0;
        cbf_failnez(cbf_get_image_size(self,reserved,element_number,&inslow,&infast));
        *ndimslow = (int)inslow;
        *ndimfast = (int)infast; 
        }
""","get_image_size",["Integer element_number"],["size_t ndim1","size_t ndim2"]],


"cbf_get_image_size_fs": ["""
%apply int *OUTPUT {int *ndimfast, int *ndimslow} get_image_size_fs;
     void get_image_size_fs(unsigned int element_number, int *ndimfast, int *ndimslow){
        unsigned int reserved;
        size_t infast, inslow;
        reserved = 0;
        cbf_failnez(cbf_get_image_size_fs(self,reserved,element_number,&infast,&inslow));
        *ndimfast = (int)infast; /* FIXME - is that how to convert? */
        *ndimslow = (int)inslow; 
        }
""","get_image_size_fs",["Integer element_number"],["size_t ndimfast","size_t ndimslow"]],


"cbf_get_image_size_sf": ["""
%apply int *OUTPUT {int *ndimslow, int *ndimfast} get_image_size_sf;
     void get_image_size_sf(unsigned int element_number, int *ndimslow, int *ndimfast){
        unsigned int reserved;
        size_t inslow, infast;
        reserved = 0;
        cbf_failnez(cbf_get_image_size(self,reserved,element_number,&inslow,&infast));
        *ndimslow = (int)inslow;
        *ndimfast = (int)infast; 
        }
""","get_image_size_sf",["Integer element_number"],["size_t ndimslow","size_t ndimfast"]],


"cbf_get_3d_image_size": ["""
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
""","get_3d_image_size",["Integer element_number"],["size_t ndimslow","size_t ndimmid","size_t ndimfast"]],


"cbf_get_3d_image_size_fs": ["""
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
""","get_3d_image_size",["Integer element_number"],["size_t ndimfast","size_t ndimmid","size_t ndimslow"]],


"cbf_get_3d_image_size_sf": ["""
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
""","get_3d_image_size_sf",["Integer element_number"],["size_t ndimslow","size_t ndimmid","size_t ndimfast"]],


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


"cbf_get_pixel_size_fs" : ["""
%apply double *OUTPUT {double *psize} get_pixel_size;
    void get_pixel_size_fs(unsigned int element_number, 
                        unsigned int axis_number, double *psize){
        cbf_failnez(cbf_get_pixel_size_fs(self, 
                                       element_number, 
                                       axis_number, 
                                       psize));
    }
""","get_pixel_size_fs",["Int element_number","Int axis_number"],
                     ["Float pixel_size"]] ,


"cbf_get_pixel_size_sf" : ["""
%apply double *OUTPUT {double *psize} get_pixel_size;
    void get_pixel_size_sf(unsigned int element_number, 
                        unsigned int axis_number, double *psize){
        cbf_failnez(cbf_get_pixel_size_sf(self, 
                                       element_number, 
                                       axis_number, 
                                       psize));
    }
""","get_pixel_size_sf",["Int element_number","Int axis_number"],
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


"cbf_set_pixel_size_fs":["""
     void set_pixel_size_fs (unsigned int element_number, 
                          unsigned int axis_number, double psize){
         cbf_failnez(cbf_set_pixel_size_fs(self, 
                                        element_number, 
                                        axis_number, 
                                        psize));
     }
""","set_pixel_size_fs",
   ["Int element_number","Int axis_number","Float pixel size"],[]],


"cbf_set_pixel_size_sf":["""
     void set_pixel_size_sf (unsigned int element_number, 
                          unsigned int axis_number, double psize){
         cbf_failnez(cbf_set_pixel_size_sf(self, 
                                        element_number, 
                                        axis_number, 
                                        psize));
     }
""","set_pixel_size_sf",
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


"cbf_write_widefile" : ["""
    void write_widefile(const char* filename, int ciforcbf, int headers, 
                    int encoding){
       FILE *stream;
       int readable;
       /* Make the file non-0 to make CBFlib close the file */
       readable = 1;
       if ( ! ( stream = fopen (filename, "w+b")) ){
         cbf_failnez(CBF_FILEOPEN);
        }
        else{
        cbf_failnez(cbf_write_widefile(self, stream, readable, 
                    ciforcbf, headers, encoding));

        }
       }
""","write_widefile",["String filename","Integer ciforcbf","Integer Headers", 
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



"cbf_read_widefile" : ["""
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
""","read_widefile",["String filename","Integer headers"],[]],


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
   void get_integration_time( double *time ){
        unsigned int reserved;
        double tim;
        reserved = 0;
        cbf_failnez(cbf_get_integration_time(self,reserved,&tim));
        *time = tim;
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

"cbf_get_unit_cell":["""
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
""","get_unit_cell",
    [],["Float a", "Float b", "Float c", "Float alpha", "Float beta", "Float gamma" ] ],

"cbf_get_unit_cell_esd":["""
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
""","get_unit_cell",
    [],["doubleArray cell"] ],


"cbf_get_reciprocal_cell":["""
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
""","get_reciprocal_cell",
    [],["Float astar", "Float bstar", "Float cstar", "Float alphastar", "Float betastar", "Float gammastar"] ],

"cbf_get_reciprocal_cell_esd":["""
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
""","get_reciprocal_cell",
    [],["doubleArray cell"] ],



"cbf_set_unit_cell":["""
   void set_unit_cell(double cell[6]) {
     cbf_failnez(cbf_set_unit_cell(self,cell,NULL));
   }
""","set_unit_cell",
    ["double cell[6]"],[] ],

"cbf_set_unit_cell_esd":["""
   void set_unit_cell_esd(double cell_esd[6]) {
     cbf_failnez(cbf_set_unit_cell(self,NULL,cell_esd));
   }
""","set_unit_cell_esd",
    ["double cell_esd[6]"],[] ],


"cbf_set_reciprocal_cell":["""
   void set_reciprocal_cell(double cell[6]) {
     cbf_failnez(cbf_set_reciprocal_cell(self,cell,NULL));
   }
""","set_reciprocal_cell",
    ["double cell[6]"],[] ],

"cbf_set_reciprocal_cell_esd":["""
   void set_reciprocal_cell_esd(double cell_esd[6]) {
     cbf_failnez(cbf_set_reciprocal_cell(self,NULL,cell_esd));
   }
""","set_reciprocal_cell_esd",
    ["double cell_esd[6]"],[] ],


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
""","find_tag_category",["String tagname"],["String categoryname"] ],


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
    
"cbf_set_bin_sizes":["""
   void set_bin_sizes( int element_number, double slowbinsize_in, double fastbinsize_in) {
     cbf_failnez(cbf_set_bin_sizes(self,element_number,slowbinsize_in,fastbinsize_in));
   }
""","set_bin_sizes",["Integer element_number","Float slowbinsize_in","Float fastbinsize_in"],[] ],


"cbf_get_bin_sizes":["""
%apply double *OUTPUT {double *slowbinsize,double *fastbinsize};
  void get_bin_sizes(int element_number, double *slowbinsize, double *fastbinsize) {
    cbf_failnez(cbf_get_bin_sizes (self, (unsigned int)element_number, slowbinsize, fastbinsize));
  }
""","get_bin_sizes",["Integer element_number"],["Float slowbinsize","Float fastbinsize"] ],



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


"cbf_construct_detector":["""
 cbf_detector construct_detector(unsigned int element_number){
    cbf_detector detector;
    cbf_failnez(cbf_construct_detector(self,&detector,element_number));
    return detector;
    }
""","construct_detector",["Integer element_number"],["pycbf detector object"]],

"cbf_construct_reference_detector":["""
 cbf_detector construct_reference_detector(unsigned int element_number){
    cbf_detector detector;
    cbf_failnez(cbf_construct_reference_detector(self,&detector,element_number));
    return detector;
    }
""","construct_reference_detector",["Integer element_number"],["pycbf detector object"]],

"cbf_require_reference_detector":["""
 cbf_detector require_reference_detector(unsigned int element_number){
    cbf_detector detector;
    cbf_failnez(cbf_require_reference_detector(self,&detector,element_number));
    return detector;
    }
""","require_reference_detector",["Integer element_number"],["pycbf detector object"]],


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
"""
      self.tail = """
}; // End of cbf_handle_struct
"""
   # End of init function
   def get_code(self):
       return self.code+self.tail
   def wrap(self,cfunc,prototype,args,docstring):
       # print "cfunc: ", cfunc
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
           # print "KeyError"
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

"cbf_get_pixel_normal_fs":["""
%apply double *OUTPUT {double *normalfast,double *normalslow, double *normal3};
   void get_pixel_normal_fs ( double indexfast, double indexslow, 
                          double *normal1,double *normal2, double *normal3){
       cbf_failnez(cbf_get_pixel_normal_fs(self,
                                    indexfast,indexslow,normal1,normal2,normal3));
   }

""","get_pixel_normal_fs",["double indexfast","double indexslow"] ,
 ["double normal1","double normal2", "double normal3" ] ],


"cbf_get_pixel_normal_sf":["""
%apply double *OUTPUT {double *normalslow,double *normalfast, double *normal3};
   void get_pixel_normal_sf ( double indexslow, double indexfast, 
                          double *normal1,double *normal2, double *normal3){
       cbf_failnez(cbf_get_pixel_normal_sf(self,
                                    indexslow,indexfast,normal1,normal2,normal3));
   }

""","get_pixel_normal_sf",["double indexslow","double indexfast"] ,
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


"cbf_get_pixel_area_fs":["""
%apply double *OUTPUT{double *area,double *projected_area};
    void get_pixel_area_fs(double indexfast, double indexslow,
                        double *area,double *projected_area){
       cbf_failnez(cbf_get_pixel_area_fs (self,
                                       indexfast, indexslow, area,projected_area));
      }
""","get_pixel_area_fs",["double indexfast", "double indexslow"],
     ["double area","double projected_area"] ],


"cbf_get_pixel_area_sf":["""
%apply double *OUTPUT{double *area,double *projected_area};
    void get_pixel_area_sf(double indexslow, double indexfast,
                        double *area,double *projected_area){
       cbf_failnez(cbf_get_pixel_area_sf (self,
                                       indexslow, indexfast, area,projected_area));
      }
""","get_pixel_area_sf",["double indexslow", "double indexfast"],
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
      cbf_failnez(cbf_get_pixel_coordinates(self, index1, index2,
             coordinate1, coordinate2, coordinate3));
   }
""","get_pixel_coordinates",["double index1","double index2"],
["double coordinate1", "double coordinate2", "double coordinate3"] ],


"cbf_get_pixel_coordinates_fs":["""
%apply double *OUTPUT {double *coordinate1,  
         double *coordinate2, double *coordinate3};
   void get_pixel_coordinates_fs(double indexfast, double indexslow, 
             double *coordinate1,   
             double *coordinate2, 
             double *coordinate3){
      cbf_failnez(cbf_get_pixel_coordinates_fs(self, indexfast, indexslow, coordinate1, coordinate2, coordinate3));
   }
""","get_pixel_coordinates_fs",["double indexfast","double indexslow"],
["double coordinate1", "double coordinate2", "double coordinate3"] ],


"cbf_get_pixel_coordinates_sf":["""
%apply double *OUTPUT {double *coordinate1,  
         double *coordinate2, double *coordinate3};
   void get_pixel_coordinates_sf(double indexslow, double indexfast, 
             double *coordinate1,   
             double *coordinate2, 
             double *coordinate3){
      cbf_failnez(cbf_get_pixel_coordinates_sf(self, indexslow, indexfast, coordinate1, coordinate2, coordinate3));
   }
""","get_pixel_coordinates_sf",["double indexslow","double indexfast"],
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


"cbf_get_beam_center_fs":["""
%apply double *OUTPUT {double *indexfast, double *indexslow, 
 double *centerfast,double *centerslow};
    void get_beam_center_fs(double *indexfast, double *indexslow, 
                         double *centerfast,double *centerslow){
        cbf_failnez(cbf_get_beam_center_fs(self, indexfast, indexslow, 
                                       centerfast, centerslow));
        }
""","get_beam_center_fs",[],
["double indexfast", "double indexslow", "double centerfast","double centerslow"]],


"cbf_get_beam_center_sf":["""
%apply double *OUTPUT {double *indexslow, double *indexfast, 
 double *centerslow,double *centerfast};
    void get_beam_center_sf(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_get_beam_center_sf(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
""","get_beam_center_sf",[],
["double indexslow", "double indexfast", "double centerslow","double centerfast"]],


"cbf_set_beam_center":["""
    void set_beam_center(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_set_beam_center(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
""","set_beam_center",
["double indexslow", "double indexfast", "double centerslow","double centerfast"],[]],


"cbf_set_beam_center_fs":["""
    void set_beam_center_fs(double *indexfast, double *indexslow, 
                         double *centerfast,double *centerslow){
        cbf_failnez(cbf_set_beam_center_fs(self, indexfast, indexslow, 
                                       centerfast, centerslow));
        }
""","set_beam_center_fs",
["double indexfast", "double indexslow", "double centerfast","double centerslow"],[]],


"cbf_set_beam_center_sf":["""
    void set_beam_center_sf(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_set_beam_center_sf(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
""","set_beam_center_sf",
["double indexslow", "double indexfast", "double centerslow","double centerfast"],[]],


"cbf_set_reference_beam_center":["""
    void set_reference_beam_center(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_set_reference_beam_center(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
""","set_reference_beam_center",
["double indexslow", "double indexfast", "double centerslow","double centerfast"],[]],


"cbf_set_reference_beam_center_fs":["""
    void set_reference_beam_center_fs(double *indexfast, double *indexslow, 
                         double *centerfast,double *centerslow){
        cbf_failnez(cbf_set_reference_beam_center_fs(self, indexfast, indexslow, 
                                       centerfast, centerslow));
        }
""","set_reference_beam_center_fs",
["double indexfast", "double indexslow", "double centerfast","double centerslow"],[]],


"cbf_set_reference_beam_center_sf":["""
    void set_reference_beam_center_sf(double *indexslow, double *indexfast, 
                         double *centerslow,double *centerfast){
        cbf_failnez(cbf_set_reference_beam_center_sf(self, indexslow, indexfast, 
                                       centerslow, centerfast));
        }
""","set_reference_beam_center_sf",
["double indexslow", "double indexfast", "double centerslow","double centerfast"],[]],


"cbf_get_inferred_pixel_size" : ["""
%apply double *OUTPUT { double *psize } get_inferred_pixel_size;
void get_inferred_pixel_size(unsigned int axis_number, double* psize){
   cbf_failnez(cbf_get_inferred_pixel_size(self, axis_number, psize));
   }
""","get_inferred_pixel_size",["Int axis_number"],["Float pixel size"] ],


"cbf_get_inferred_pixel_size_fs" : ["""
%apply double *OUTPUT { double *psize } get_inferred_pixel_size;
void get_inferred_pixel_size_fs(unsigned int axis_number, double* psize){
   cbf_failnez(cbf_get_inferred_pixel_size_fs(self, axis_number, psize));
   }
""","get_inferred_pixel_size_fs",["Int axis_number"],["Float pixel size"] ],


"cbf_get_inferred_pixel_size_sf" : ["""
%apply double *OUTPUT { double *psize } get_inferred_pixel_size;
void get_inferred_pixel_size_sf(unsigned int axis_number, double* psize){
   cbf_failnez(cbf_get_inferred_pixel_size_sf(self, axis_number, psize));
   }
""","get_inferred_pixel_size_sf",["Int axis_number"],["Float pixel size"] ]


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


cbfgeneric_specials = {
"cbf_get_local_integer_byte_order":["""
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
""","get_local_integer_byte_order",[],["char **bo", "int *bolen"]],


"cbf_get_local_real_format":["""
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
""","get_local_real_format",[],["char **rf", "int *rflen"]],


"cbf_get_local_real_byte_order":["""
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
""","get_local_real_byte_order",[],["char **bo", "int *bolen"]],


"cbf_compute_cell_volume":["""

%apply double *OUTPUT {double *volume};
  %inline {
  void compute_cell_volume(double cell[6], double *volume) {
  cbf_failnez(cbf_compute_cell_volume(cell,volume));
  }
  }
""","compute_cell_volume",["double cell[6]"],["Float volume"]],


"cbf_compute_reciprocal_cell":["""
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

""","compute_reciprocal_cell",["double cell[6]"],
["Float astar", "Float bstar", "Float cstar", "Float alphastar", "Float betastar", "Float gammastar"] ]

}


class genericwrapper:
   def __init__(self):
       self.code = """
// Start of generic functions
%feature("autodoc","1");
"""
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
       not_found = 0
       try:
           code, pyname, input, output = cbfgeneric_specials[cfunc]
           self.code +=  docstringwrite(pyname,input,output,
                                              prototype,docstring)+ code
           return
       except KeyError:
           not_found = 1
           # print "KeyError"
       except ValueError:
           print "problem in generic",cfunc
           for item in cbfgeneric_specials[cfunc]:
              print "***",item
           raise
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
      # print "Generate wrappers: ", "::",cname,"::", prototype,"::", docstring
      # Check prototype begins with "int cbf_"
      if prototype.find("int cbf_")!=0:
         print "problem with:",prototype
      # Get arguments from prototypes
      try:
         args = prototype.split("(")[1].split(")")[0].split(",")
         args = [ s.lstrip().rstrip() for s in args ] # strip spaces off ends
         # print "Args: ", args
      except:
         # print cname
         # print prototype
         raise
      if args[0].find("cbf_handle")>=0: # This is for the cbfhandle object
         cbf_handle_wrapper.wrap(cname,prototype,args,docstring)
         if (cname=="cbf_get_unit_cell"):
           cbf_handle_wrapper.wrap("cbf_get_unit_cell_esd",prototype,args,docstring)
         if (cname=="cbf_get_reciprocal_cell"):
           cbf_handle_wrapper.wrap("cbf_get_reciprocal_cell_esd",prototype,args,docstring)
         if (cname=="cbf_set_unit_cell"):
           cbf_handle_wrapper.wrap("cbf_set_unit_cell_esd",prototype,args,docstring)
         if (cname=="cbf_set_reciprocal_cell"):
           cbf_handle_wrapper.wrap("cbf_set_reciprocal_cell_esd",prototype,args,docstring)
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
@}

