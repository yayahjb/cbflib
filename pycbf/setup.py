

# Import the things to build python binary extensions

from distutils.core import setup, Extension

# Make our extension module

e = Extension('_pycbf',
              sources = ["pycbf_wrap.c","../src/cbf_simple.c"],
         extra_compile_args=["-g"],
         library_dirs=["../lib/"],
         libraries=["cbf"],
         include_dirs = ["../include"] )
            
# Build it
setup(name="_pycbf",ext_modules=[e],)
