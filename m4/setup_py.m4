#
#  pycbf/setup.py generated from m4/setup_py.m4
# 

`# Import the things to build python binary extensions

from distutils.core import setup, Extension

# Make our extension module

e = Extension(''`_pycbf''`,
              sources = ["pycbf_wrap.c","../src/cbf_simple.c"],
         extra_compile_args=["-g"],
         'm4_ifelse(regexlibdir,`NOREGEXLIBDIR',`library_dirs=["../lib/"],',`library_dirs=["../lib/","'regexlibdir`"],')`
         'm4_ifelse(regexlib,`NOREGEXLIB',`libraries=["cbf"],',`libraries=["cbf","'regexlib`"],')`
         include_dirs = ["../include"] )
            
# Build it
setup(name="_pycbf",ext_modules=[e],)'
