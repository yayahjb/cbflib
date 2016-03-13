#
#  pycbf/setup.py generated from m4/setup_py.m4
# 

# Import the things to build python binary extensions

from distutils.core import setup, Extension

# Make our extension module

e = Extension('_pycbf',
              sources = ["pycbf_wrap.c","../src/cbf_simple.c"],
         extra_compile_args=["-g"],
         library_dirs=["../lib/","/Users/yaya/desktop/ncbf/CBFlib_0.9.5.13_build/CBFlib-0.9.5.13/lib"],
         libraries=["cbf","pcreposix"],
         include_dirs = ["../include","/Users/yaya/desktop/ncbf/CBFlib_0.9.5.13_build/CBFlib-0.9.5.13/include"] )
            
# Build it
setup(name="_pycbf",ext_modules=[e],)
