
#
#  py3setup_py.m4
#

`# Import the things to build python binary extensions

from distutils.core import setup, Extension

# Make our extension module

e = Extension(''`_pycbf''`,
              sources = ["pycbf_wrap.c","../src/cbf_simple.c"],
         extra_compile_args=["-g", "-DSWIG_PYTHON_STRICT_BYTE_CHAR"],
         'm4_ifelse(regexlibdir,`NOREGEXLIBDIR',`library_dirs=["../solib/","../lib/"],',`library_dirs=["../solib/","../lib/","'regexlibdir`"],')`
         'm4_ifelse(regexlib,`',`libraries=["cbf"],', `m4_ifelse(regexlib2,`',`libraries=["cbf","'regexlib`"],',`libraries=["cbf","'regexlib`","'regexlib2`"],')'  )`
         include_dirs = ["../include","'hdf5_prefix`/include"] )
            
# Build it
setup(name="_pycbf",ext_modules=[e],)'
