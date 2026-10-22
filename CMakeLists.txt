
######################################################################
#  CMakeLists.txt - cmake build file for make to create CBFlib       #
#                                                                    #
# Version 0.9.6 06 November 2018                                     #
#                                                                    #
#                          Paul Ellis and                            #
#         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        #
#                                                                    #
# (C) Copyright 2006 - 2018 Herbert J. Bernstein                     #
#                                                                    #
######################################################################

######################################################################
#                                                                    #
# YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL #
#                                                                    #
# ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  #
# OF THE LGPL                                                        #
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

######################################################################
#                                                                    #
#                    Stanford University Notices                     #
#  for the CBFlib software package that incorporates SLAC software   #
#                 on which copyright is disclaimed                   #
#                                                                    #
# This software                                                      #
# -------------                                                      #
# The term "this software", as used in these Notices, refers to      #
# those portions of the software package CBFlib that were created by #
# employees of the Stanford Linear Accelerator Center, Stanford      #
# University.                                                        #
#                                                                    #
# Stanford disclaimer of copyright                                   #
# --------------------------------                                   #
# Stanford University, owner of the copyright, hereby disclaims its  #
# copyright and all other rights in this software.  Hence, anyone    #
# may freely use it for any purpose without restriction.             #
#                                                                    #
# Acknowledgement of sponsorship                                     #
# ------------------------------                                     #
# This software was produced by the Stanford Linear Accelerator      #
# Center, Stanford University, under Contract DE-AC03-76SFO0515 with #
# the Department of Energy.                                          #
#                                                                    #
# Government disclaimer of liability                                 #
# ----------------------------------                                 #
# Neither the United States nor the United States Department of      #
# Energy, nor any of their employees, makes any warranty, express or #
# implied, or assumes any legal liability or responsibility for the  #
# accuracy, completeness, or usefulness of any data, apparatus,      #
# product, or process disclosed, or represents that its use would    #
# not infringe privately owned rights.                               #
#                                                                    #
# Stanford disclaimer of liability                                   #
# --------------------------------                                   #
# Stanford University makes no representations or warranties,        #
# express or implied, nor assumes any liability for the use of this  #
# software.                                                          #
#                                                                    #
# Maintenance of notices                                             #
# ----------------------                                             #
# In the interest of clarity regarding the origin and status of this #
# software, this and all the preceding Stanford University notices   #
# are to remain affixed to any copy or derivative of this software   #
# made or distributed by the recipient and are to be affixed to any  #
# copy of software made or distributed by the recipient that         #
# contains a copy or derivative of this software.                    #
#                                                                    #
# Based on SLAC Software Notices, Set 4                              #
# OTT.002a, 2004 FEB 03                                              #
######################################################################



######################################################################
#                               NOTICE                               #
# Creative endeavors depend on the lively exchange of ideas. There   #
# are laws and customs which establish rights and responsibilities   #
# for authors and the users of what authors create.  This notice     #
# is not intended to prevent you from using the software and         #
# documents in this package, but to ensure that there are no         #
# misunderstandings about terms and conditions of such use.          #
#                                                                    #
# Please read the following notice carefully.  If you do not         #
# understand any portion of this notice, please seek appropriate     #
# professional legal advice before making use of the software and    #
# documents included in this software package.  In addition to       #
# whatever other steps you may be obliged to take to respect the     #
# intellectual property rights of the various parties involved, if   #
# you do make use of the software and documents in this package,     #
# please give credit where credit is due by citing this package,     #
# its authors and the URL or other source from which you obtained    #
# it, or equivalent primary references in the literature with the    #
# same authors.                                                      #
#                                                                    #
# Some of the software and documents included within this software   #
# package are the intellectual property of various parties, and      #
# placement in this package does not in any way imply that any       #
# such rights have in any way been waived or diminished.             #
#                                                                    #
# With respect to any software or documents for which a copyright    #
# exists, ALL RIGHTS ARE RESERVED TO THE OWNERS OF SUCH COPYRIGHT.   #
#                                                                    #
# Even though the authors of the various documents and software      #
# found here have made a good faith effort to ensure that the        #
# documents are correct and that the software performs according     #
# to its documentation, and we would greatly appreciate hearing of   #
# any problems you may encounter, the programs and documents any     #
# files created by the programs are provided **AS IS** without any   *
# warranty as to correctness, merchantability or fitness for any     #
# particular or general use.                                         #
#                                                                    #
# THE RESPONSIBILITY FOR ANY ADVERSE CONSEQUENCES FROM THE USE OF    #
# PROGRAMS OR DOCUMENTS OR ANY FILE OR FILES CREATED BY USE OF THE   #
# PROGRAMS OR DOCUMENTS LIES SOLELY WITH THE USERS OF THE PROGRAMS   #
# OR DOCUMENTS OR FILE OR FILES AND NOT WITH AUTHORS OF THE          #
# PROGRAMS OR DOCUMENTS.                                             #
######################################################################

######################################################################
#                                                                    #
#                           The IUCr Policy                          #
#      for the Protection and the Promotion of the STAR File and     #
#     CIF Standards for Exchanging and Archiving Electronic Data     #
#                                                                    #
# Overview                                                           #
#                                                                    #
# The Crystallographic Information File (CIF)[1] is a standard for   #
# information interchange promulgated by the International Union of  #
# Crystallography (IUCr). CIF (Hall, Allen & Brown, 1991) is the     #
# recommended method for submitting publications to Acta             #
# Crystallographica Section C and reports of crystal structure       #
# determinations to other sections of Acta Crystallographica         #
# and many other journals. The syntax of a CIF is a subset of the    #
# more general STAR File[2] format. The CIF and STAR File approaches #
# are used increasingly in the structural sciences for data exchange #
# and archiving, and are having a significant influence on these     #
# activities in other fields.                                        #
#                                                                    #
# Statement of intent                                                #
#                                                                    #
# The IUCr's interest in the STAR File is as a general data          #
# interchange standard for science, and its interest in the CIF,     #
# a conformant derivative of the STAR File, is as a concise data     #
# exchange and archival standard for crystallography and structural  #
# science.                                                           #
#                                                                    #
# Protection of the standards                                        #
#                                                                    #
# To protect the STAR File and the CIF as standards for              #
# interchanging and archiving electronic data, the IUCr, on behalf   #
# of the scientific community,                                       #
#                                                                    #
# # holds the copyrights on the standards themselves,                *
#                                                                    #
# # owns the associated trademarks and service marks, and            *
#                                                                    #
# # holds a patent on the STAR File.                                 *
#                                                                    #
# These intellectual property rights relate solely to the            #
# interchange formats, not to the data contained therein, nor to     #
# the software used in the generation, access or manipulation of     #
# the data.                                                          #
#                                                                    #
# Promotion of the standards                                         #
#                                                                    #
# The sole requirement that the IUCr, in its protective role,        #
# imposes on software purporting to process STAR File or CIF data    #
# is that the following conditions be met prior to sale or           #
# distribution.                                                      #
#                                                                    #
# # Software claiming to read files written to either the STAR       *
# File or the CIF standard must be able to extract the pertinent     #
# data from a file conformant to the STAR File syntax, or the CIF    #
# syntax, respectively.                                              #
#                                                                    #
# # Software claiming to write files in either the STAR File, or     *
# the CIF, standard must produce files that are conformant to the    #
# STAR File syntax, or the CIF syntax, respectively.                 #
#                                                                    #
# # Software claiming to read definitions from a specific data       *
# dictionary approved by the IUCr must be able to extract any        #
# pertinent definition which is conformant to the dictionary         #
# definition language (DDL)[3] associated with that dictionary.      #
#                                                                    #
# The IUCr, through its Committee on CIF Standards, will assist      #
# any developer to verify that software meets these conformance      #
# conditions.                                                        #
#                                                                    #
# Glossary of terms                                                  #
#                                                                    #
# [1] CIF:  is a data file conformant to the file syntax defined     #
# at http://www.iucr.org/iucr-top/cif/spec/index.html                #
#                                                                    #
# [2] STAR File:  is a data file conformant to the file syntax       #
# defined at http://www.iucr.org/iucr-top/cif/spec/star/index.html   #
#                                                                    #
# [3] DDL:  is a language used in a data dictionary to define data   #
# items in terms of "attributes". Dictionaries currently approved    #
# by the IUCr, and the DDL versions used to construct these          #
# dictionaries, are listed at                                        #
# http://www.iucr.org/iucr-top/cif/spec/ddl/index.html               #
#                                                                    #
# Last modified: 30 September 2000                                   #
#                                                                    #
# IUCr Policy Copyright (C) 2000 International Union of              #
# Crystallography                                                    #
######################################################################

######################################################################
#  CMakeLists.txt for CBFlib                                         #
#                                                                    #
#  Assumed directory structure                                       #
#    CBFlib_SOURCE_DIR        CBFlib kit containing this file        #
#      doc                    Directory with documentation           #
#      examples               Directory with example programs        #
#      include                Directory with header files            #
#      m4                     Directory with m4 files                #
#      src                    Directory with source files            #
#                                                                    #
#    CBFlib_BINARY_DIR        CBFlib build directory                 #
#                               usually ${CBFlib_SOURCE_DIR}/build   #
#      external_packages      Directory for HDF5, libtiff, etc.      #
#        hdf5-1.8.11                                                 #
#        tiff-3.9.4-rev-6Feb11                                       #
#        regex-20090805                                              #
#        zlib-1.2.8                                                  #
#      data_files             Directory for test files               #
#      bin                    Directory for executable programs      #
#      include                Directory with build-created headers   #
#      src                    Directory with build-created source    #
#                                                                    #
######################################################################

# find_program()'s REQUIRED option was introduced in CMake 3.18.
cmake_minimum_required(VERSION 3.18)
project(CBFlib
  LANGUAGES C CXX
  VERSION 0.9.8)
set(VERSION_INFO "3:0:1")


# Set timestamps of extracted contents to the time of extraction.
# Resolved by explicitly setting DOWNLOAD_EXTRACT_TIMESTAMP in
# externalproject_add() or fetchcontent_declare(), but the option was
# only introduced in CMake 3.24.
cmake_policy(SET CMP0135 NEW)


# Output directories for static and shared libraries as well as
# binaries.  These appear to be required to on Windows and for running
# h5dump elsewhere.
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

option(CBF_ENABLE_FORTRAN "Enable Fortran 90" ON)
if(CBF_ENABLE_FORTRAN)
  include(CheckLanguage)
  check_language(Fortran)
  if(CMAKE_Fortran_COMPILER)
    enable_language(Fortran)
  else()
    message(FATAL_ERROR "Fortran 90 support requested, but no compiler found")
  endif()
endif()

option(CBF_ENABLE_DOC "Build documentation" OFF)
option(CBF_ENABLE_ULP "Enable ULP" ON)

set (CBF_CMAKE_DEBUG "ON")


#
# Java and Python bindings
#
# The Java and Python modules can only be built when BUILD_SHARED_LIBS
# is set, but that is not enforced here.
if(BUILD_SHARED_LIBS)
  option(CBF_ENABLE_JAVA "Enable Java" ON)
  option(CBF_ENABLE_PYTHON "Enable Python" ON)
endif()


#
#  User setable parameters
#

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)


# Check for missing header files.
include(CheckIncludeFile)
check_include_file("unistd.h" HAVE_UNISTD_H)
if(HAVE_UNISTD_H)
  add_compile_definitions("HAVE_UNISTD_H")
endif()


# Check for missing functions: fgetln(3) is in 4.4BSD; realpath(3) is
# in 4.4BSD, POSIX.1-2001; regcomp(3) is in POSIX.1-2001,
# POSIX.1-2008.
include(CheckSymbolExists)
check_symbol_exists(fgetln "stdio.h" HAVE_FGETLN)
if(HAVE_FGETLN)
  add_compile_definitions("HAVE_FGETLN")
endif()

check_symbol_exists(realpath "stdlib.h" HAVE_REALPATH)
if(HAVE_REALPATH)
  add_compile_definitions("HAVE_REALPATH")
endif()

check_symbol_exists(regcomp "regex.h" HAVE_REGEX)
if(HAVE_REGEX)
  add_compile_definitions("HAVE_REGEX")
endif()


#
# Macros
#

#
#  CBF_DEBUG_MESSAGE if CBF_CMAKE_DEBUG issue str
#
macro(CBF_DEBUG_MESSAGE str)
  if(CBF_CMAKE_DEBUG)
    message(STATUS ${str})
  endif(CBF_CMAKE_DEBUG)
endmacro (CBF_DEBUG_MESSAGE)

#
# CBF_REQUIRE_DIRECTORY -- require directory dir
#
macro(CBF_REQUIRE_DIRECTORY dir)
  if (NOT EXISTS "${dir}")
    file(MAKE_DIRECTORY "${dir}")
    CBF_DEBUG_MESSAGE("Created directory ${dir}")
  endif (NOT EXISTS "${dir}")
endmacro(CBF_REQUIRE_DIRECTORY)

#
# Directories
#

#
# Directories on the kit side
#
set(CBF__SRC       "${CBFlib_SOURCE_DIR}/src" )
set(CBF__INCLUDE   "${CBFlib_SOURCE_DIR}/include" )
set(CBF__DOC       "${CBFlib_SOURCE_DIR}/doc" )
set(CBF__EXAMPLES  "${CBFlib_SOURCE_DIR}/examples" )
set(CBF__EXTERNAL_PACKAGES 
                   "${CBFlib_SOURCE_DIR}/external_packages" )
set(CBF__DECTRIS_EXAMPLES
                   "${CBF__EXAMPLES}/dectris_cbf_template_test" )


#
# Directories on the build side
#
set(CBF__BIN       "${CBFlib_BINARY_DIR}/bin" )
set(CBF__LIB       "${CBFlib_BINARY_DIR}/lib" )
set(CBF__BIN_INCLUDE "${CBFlib_BINARY_DIR}/include" )
set(CBF__SHARE     "${CBFlib_BINARY_DIR}/share" )
set(CBF__EXT_PKG   "${CBFlib_BINARY_DIR}/external_packages" )
set(CBF__DATA      "${CBFlib_BINARY_DIR}/data_files" )

CBF_REQUIRE_DIRECTORY(${CBF__BIN})
CBF_REQUIRE_DIRECTORY(${CBF__LIB})
CBF_REQUIRE_DIRECTORY(${CBF__BIN_INCLUDE})
CBF_REQUIRE_DIRECTORY(${CBF__SHARE})
CBF_REQUIRE_DIRECTORY(${CBF__EXT_PKG})
CBF_REQUIRE_DIRECTORY(${CBF__DATA})


# Under the (Linux) libtool convention, the single-component SOVERSION
# is the difference between the current and age components of
# VERSION_INFO.
string(REPLACE ":" ";" _cra "${VERSION_INFO}")
list(GET _cra 0 _current)
list(GET _cra 2 _age)
math(EXPR SOVERSION "${_current} - ${_age}")


set(BIN       "${CBFlib_BINARY_DIR}/bin" CACHE STRING "")
set(EXAMPLES  "${CBFlib_SOURCE_DIR}/examples" CACHE STRING "" )
set(DECTRIS_EXAMPLES "${EXAMPLES}/dectris_cbf_template_test" CACHE STRING "")
set(GRAPHICS  "${CBFlib_SOURCE_DIR}/html_graphics" CACHE STRING "")

if(CBF_DONT_USE_LONG_LONG)
  set(CBF_NOLLFLAG "-DCBF_DONT_USE_LONG_LONG")
else(CBF_DONT_USE_LONG_LONG)
  set(CBF_NOLLFLAG "")
endif(CBF_DONT_USE_LONG_LONG)


#
# Dependencies, external packages
#
# Verify the checksums.  The --binary option appears to be required
# for patching the test data on MSYS2.
include(FetchContent)
find_program(PATCH patch REQUIRED)

fetchcontent_declare(cqrlib
  URL "https://github.com/yayahjb/cqrlib/archive/refs/tags/CQRlib-1.1.4.tar.gz"
  URL_HASH MD5=629a349ed5e8bf6f2a69a2fb1da11c45
  PATCH_COMMAND "${PATCH}"
    -N -i "${CMAKE_CURRENT_SOURCE_DIR}/patches/cqrlib-1.1.4.patch" -p 1 -t)

fetchcontent_declare(data_input
  URL "https://github.com/dials/cbflib/releases/download/CBFlib-${PROJECT_VERSION}/CBFlib_${PROJECT_VERSION}_Data_Files_Input.tar.gz"
  URL_HASH MD5=f98ae4214b3e57acb42437ea4b685c4d
  PATCH_COMMAND "${PATCH}"
    -N -i "${CMAKE_CURRENT_SOURCE_DIR}/patches/cbflib-data-input-${PROJECT_VERSION}.patch" -p 1 -t --binary)

fetchcontent_declare(data_output
  URL "https://github.com/dials/cbflib/releases/download/CBFlib-${PROJECT_VERSION}/CBFlib_${PROJECT_VERSION}_Data_Files_Output.tar.gz"
  URL_HASH MD5=716346077fe2bcbe29e5153f0ecbb8e7
  PATCH_COMMAND "${PATCH}"
    -N -i "${CMAKE_CURRENT_SOURCE_DIR}/patches/cbflib-data-output-${PROJECT_VERSION}.patch" -p 1 -t --binary)

fetchcontent_declare(hdf5
  URL "https://github.com/HDFGroup/hdf5/releases/download/hdf5_1.14.6/hdf5-1.14.6.tar.gz"
  URL_HASH MD5=63426c8e24086634eaf9179a8c5fe9e5)

fetchcontent_declare(pcre2
  URL "https://github.com/PCRE2Project/pcre2/releases/download/pcre2-10.45/pcre2-10.45.tar.gz"
  URL_HASH MD5=88fe01e3f2ec144912ebecf7ef5cd595)

fetchcontent_declare(tiff
  URL "https://download.osgeo.org/libtiff/tiff-4.7.0.tar.gz"
  URL_HASH MD5=3a0fa4a270a4a192b08913f88d0cfbdd)


#
# libm
set(libm "$<$<NOT:$<C_COMPILER_ID:MSVC>>:m>")


#
# CQRlib
set(CBF_WITH_CQRLIB ON CACHE BOOL
  "Link against internally built CQRlib library")
mark_as_advanced(CBF_WITH_CQRLIB)

if(CBF_WITH_CQRLIB)
  fetchcontent_makeavailable(cqrlib)
else()
  find_library(cqrlib_library CQRlib)
  find_path(cqrlib_include_dir cqrlib.h)

  add_library(CQR UNKNOWN IMPORTED)
  set_target_properties(CQR PROPERTIES
    IMPORTED_LOCATION "${cqrlib_library}"
    INTERFACE_INCLUDE_DIRECTORIES "${cqrlib_include_dir}"
    INTERFACE_LINK_LIBRARIES "${libm}")
endif()


#
# INPUT/OUTPUT TEST DATA
#
# If CBF_WITH_DATA_INPUT and/or CBF_WITH_DATA_OUTPUT are set
# externally, the test data in the corresponding directories are
# assumed to be patched.
set(CBF_WITH_DATA_INPUT "" CACHE FILEPATH
  "Path to input test data directory")
mark_as_advanced(CBF_WITH_DATA_INPUT)

if(CBF_WITH_DATA_INPUT)
  set(data_input "${CBF_WITH_DATA_INPUT}")
else()
  fetchcontent_makeavailable(data_input)
  set(data_input "${data_input_SOURCE_DIR}")
endif()

set(CBF_WITH_DATA_OUTPUT "" CACHE FILEPATH
  "Path to reference output test data directory")
mark_as_advanced(CBF_WITH_DATA_OUTPUT)

if(CBF_WITH_DATA_OUTPUT)
  set(data_output "${CBF_WITH_DATA_OUTPUT}")
else()
  fetchcontent_makeavailable(data_output)
  set(data_output "${data_output_SOURCE_DIR}")
endif()


#
# HDF5
#
# See hdf5's root CMakeLists.txt and config/cmake/HDFLibMacros.cmake;
# {LIBAEC,ZLIB}_TGZ_{NAME,ORIGPATH} values taken from
# CMakePresets.json.  Set _h5dump_executable to the path to h5dump and
# _hdf5_target to the real (unaliased) HDF5 target.
set(CBF_WITH_HDF5 ON CACHE BOOL
  "Link against internally built HDF5 library")
mark_as_advanced(CBF_WITH_HDF5)

set(HDF5REGISTER "--register" "manual" CACHE INTERNAL
  "Flags to register HDF5 plugins")
mark_as_advanced(HDF5REGISTER)

if(CBF_WITH_HDF5)
  set(BUILD_TESTING OFF CACHE INTERNAL
    "Build HDF5 unit testing")
  set(HDF5_ALLOW_EXTERNAL_SUPPORT "TGZ" CACHE INTERNAL
    "Allow external library building (NO GIT TGZ)")
  set(HDF5_EXTERNALLY_CONFIGURED ON CACHE INTERNAL
    "HDF5 configured externally")
  set(HDF_PACKAGE_NAMESPACE "hdf5::" CACHE INTERNAL
    "Name for HDF package namespace (can be empty)")
  set(LIBAEC_TGZ_NAME "libaec-1.1.3.tar.gz" CACHE INTERNAL
    "Use SZIP AEC from compressed file")
  set(LIBAEC_TGZ_ORIGPATH
    "https://github.com/MathisRosenhauer/libaec/releases/download/v1.1.3"
    CACHE INTERNAL
    "Use LIBAEC from original location")
  set(ZLIB_TGZ_NAME "zlib-1.3.1.tar.gz" CACHE INTERNAL
    "Use HDF5_ZLib from compressed file")
  set(ZLIB_TGZ_ORIGPATH
    "https://github.com/madler/zlib/releases/download/v1.3.1"
    CACHE INTERNAL
    "Use zlib from original location")
  fetchcontent_makeavailable(hdf5)

  set(_h5dump_executable
    "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump${CMAKE_EXECUTABLE_SUFFIX}")
  if(BUILD_SHARED_LIBS)
    set(_hdf5_target hdf5-shared)
  else()
    set(_hdf5_target hdf5-static)
  endif()
else()
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(hdf5 REQUIRED IMPORTED_TARGET hdf5>=1.10)
  find_program(_h5dump_executable h5dump REQUIRED)
  set(_hdf5_target PkgConfig::hdf5)
endif()


# The h5dump target already exists with CMake 3.30 and HDF 1.14.4 on
# Windows.
if(NOT TARGET h5dump)
  add_executable(h5dump IMPORTED)
  set_target_properties(h5dump PROPERTIES
    IMPORTED_LOCATION "${_h5dump_executable}")
endif()


# CBFlib must be compiled with H5_USE_110_API.  With MSVC, also need
# H5_BUILT_AS_DYNAMIC_LIB if HDF5 is a dynamic library.
# target_compile_definitions cannot be used on an ALIAS target.
add_library(hdf5 ALIAS ${_hdf5_target})
target_compile_definitions(${_hdf5_target}
  INTERFACE H5_USE_110_API)
if(BUILD_SHARED_LIBS)
  target_compile_definitions(${_hdf5_target}
    INTERFACE H5_BUILT_AS_DYNAMIC_LIB)
endif()


#
# PCRE2
set(CBF_WITH_PCRE2 ON CACHE BOOL
  "Link against internally built Perl-compatible regular expressions library")
mark_as_advanced(CBF_WITH_PCRE2)

if(CBF_WITH_PCRE2)
  set(PCRE2_BUILD_PCRE2GREP OFF CACHE INTERNAL "Build pcre2grep")
  set(PCRE2_BUILD_TESTS OFF CACHE INTERNAL "Build the tests")
  fetchcontent_makeavailable(pcre2)

elseif(HAVE_REGEX)
  add_library(pcre2-posix INTERFACE IMPORTED)
  target_compile_definitions(pcre2-posix
    INTERFACE CBF_REGEXLIB_REGEX)

else()
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(pcre2-posix REQUIRED IMPORTED_TARGET libpcre2-posix)
  add_library(pcre2-posix ALIAS PkgConfig::pcre2-posix)
endif()


#
# TIFF
#
# LibTIFF to support tiff2cbf.  Disable tests if building in-tree; the
# tiff-tests cache variable was introduced in LibTIFF 4.5.0.
set(CBF_WITH_LIBTIFF ON CACHE BOOL
  "Link against internally built LibTIFF library")
mark_as_advanced(CBF_WITH_LIBTIFF)

if(CBF_WITH_LIBTIFF)
  # TIFFFieldSetGetSize() was introduced in LibTIFF 4.4.0
  set(tiff-contrib OFF CACHE INTERNAL "Build TIFF contrib")
  set(tiff-docs OFF CACHE INTERNAL "Build TIFF documentation")
  set(tiff-tests OFF CACHE INTERNAL "Build TIFF tests")
  set(tiff-tools OFF CACHE INTERNAL "Build TIFF tools")
  fetchcontent_makeavailable(tiff)

else()
  # find_package(TIFF) appears to miss the dependencies (Lerc, jbig,
  # etc) for a static LibTIFF.  pkg_check_modules(... IMPORTED_TARGET
  # libtiff-4) on the other hand always seems to create a
  # shared-library target; see
  # https://gitlab.kitware.com/cmake/cmake/-/issues/21714.
  #
  # Use pkg-config, with the intent of eventually switching to
  # cmake_pkg_config(), because e.g. Debian does not ship CMake config
  # files in their dev packages.
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(tiff REQUIRED IMPORTED_TARGET libtiff-4>=4.4.0)
  add_library(tiff ALIAS PkgConfig::tiff)
endif()


#
# Source files
#
# Since Bison 2.7 (2012-12-12), use of "%define api.pure full" is
# strongly encouraged.
find_package(BISON 2.7 REQUIRED)
bison_target(cbf_stx "${CBF__SRC}/cbf.stx.y" "cbf_stx.c"
  DEFINES_FILE "cbf_stx.h")

set(
	CBF_C_SOURCES
    "${BISON_cbf_stx_OUTPUTS}"
    ${CBF__SRC}/cbf.c
    "${CBF__SRC}/cbf_airy_disk.c"
    ${CBF__SRC}/cbf_alloc.c
    "${CBF__SRC}/cbf_array2minicbf.c"
    ${CBF__SRC}/cbf_ascii.c
    ${CBF__SRC}/cbf_binary.c
    ${CBF__SRC}/cbf_byte_offset.c
    ${CBF__SRC}/cbf_canonical.c
    ${CBF__SRC}/cbf_codes.c
    ${CBF__SRC}/cbf_compress.c
    ${CBF__SRC}/cbf_context.c
    ${CBF__SRC}/cbf_copy.c
    ${CBF__SRC}/cbf_file.c
    ${CBF__SRC}/cbf_getopt.c
    ${CBF__SRC}/cbf_hdf5.c
    ${CBF__SRC}/cbf_hdf5_filter.c
    ${CBF__SRC}/cbf_lex.c
    ${CBF__SRC}/cbf_minicbf_header.c
    ${CBF__SRC}/cbf_nibble_offset.c
    ${CBF__SRC}/cbf_packed.c
    ${CBF__SRC}/cbf_predictor.c
    ${CBF__SRC}/cbf_read_binary.c
    ${CBF__SRC}/cbf_read_mime.c
    ${CBF__SRC}/cbf_simple.c
    ${CBF__SRC}/cbf_string.c
    ${CBF__SRC}/cbf_tree.c
    ${CBF__SRC}/cbf_uncompressed.c
    ${CBF__SRC}/cbf_write.c
    ${CBF__SRC}/cbf_write_binary.c
    ${CBF__SRC}/cbf_ws.c
	${CBF__SRC}/md5c.c
    ${CBF__SRC}/img.c
)


# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE) 

SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)


# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib" )
ENDIF("${isSystemDir}" STREQUAL "-1")

			
#
# Header files
#
set(
	CBF_HEADERS
	${CBF__INCLUDE}/cbf.h			
    ${CBF__INCLUDE}/cbf_alloc.h
    "${CBF__INCLUDE}/cbf_array2minicbf.h"
    ${CBF__INCLUDE}/cbf_ascii.h
    ${CBF__INCLUDE}/cbf_binary.h		
    ${CBF__INCLUDE}/cbf_byte_offset.h
    ${CBF__INCLUDE}/cbf_canonical.h
    ${CBF__INCLUDE}/cbf_codes.h
    ${CBF__INCLUDE}/cbf_compress.h		
    ${CBF__INCLUDE}/cbf_context.h		
    ${CBF__INCLUDE}/cbf_copy.h			
    ${CBF__INCLUDE}/cbf_file.h			
    ${CBF__INCLUDE}/cbf_getopt.h		
    ${CBF__INCLUDE}/cbf_hdf5.h			
    ${CBF__INCLUDE}/cbf_hdf5_filter.h
    ${CBF__INCLUDE}/cbf_lex.h			
    ${CBF__INCLUDE}/cbf_minicbf_header.h
    ${CBF__INCLUDE}/cbf_nibble_offset.h
    ${CBF__INCLUDE}/cbf_packed.h		
    ${CBF__INCLUDE}/cbf_predictor.h
    ${CBF__INCLUDE}/cbf_read_binary.h
    ${CBF__INCLUDE}/cbf_read_mime.h		
    ${CBF__INCLUDE}/cbf_simple.h		
    ${CBF__INCLUDE}/cbf_string.h		
    ${CBF__INCLUDE}/cbf_tree.h
    ${CBF__INCLUDE}/cbf_uncompressed.h
    ${CBF__INCLUDE}/cbf_write.h
    ${CBF__INCLUDE}/cbf_write_binary.h
    ${CBF__INCLUDE}/cbf_ws.h
    ${CBF__INCLUDE}/global.h
    ${CBF__INCLUDE}/cbff.h			
	${CBF__INCLUDE}/md5.h
    ${CBF__INCLUDE}/img.h
)
if(CBF_ENABLE_ULP)
  list(APPEND CBF_HEADERS
    "${CBF__INCLUDE}/cbf_ulp.h")
endif()


#
# Documentation files
#
set(
	CBF_DOCUMENTS
	${CBF__DOC}/CBFlib.html
			${CBF__DOC}/CBFlib.txt
			${CBF__DOC}/CBFlib_NOTICES.html
			${CBF__DOC}/CBFlib_NOTICES.txt
            ${CBF__DOC}/ChangeLog
			${CBF__DOC}/ChangeLog.html
			${CBF__DOC}/MANIFEST
			${CBF__DOC}/gpl.txt $(DOC)/lgpl.txt
	CACHE STRING ""
)

#
# HTML Graphics files
#
set(
	JPEGS
	${GRAPHICS}/CBFbackground.jpg
			${GRAPHICS}/CBFbig.jpg
			${GRAPHICS}/CBFbutton.jpg
			${GRAPHICS}/cbflibbackground.jpg
            ${GRAPHICS}/cbflibbig.jpg
			${GRAPHICS}/cbflibbutton.jpg
			${GRAPHICS}/cifhome.jpg
            ${GRAPHICS}/iucrhome.jpg
			${GRAPHICS}/noticeButton.jpg
	CACHE STRING ""
)


#
# Build the static and shared CBF libraries
#
# Note that HDF5 is a PUBLIC dependency of CBFlib, because cbf_hdf5.h
# includes hdf5.h.
add_library(cbf ${CBF_C_SOURCES})
if(CBF_ENABLE_ULP)
  target_compile_definitions(cbf
    PUBLIC CBF_USE_ULP)
  target_sources(cbf
    PRIVATE "${CBF__SRC}/cbf_ulp.c")
endif()
if(NOT HAVE_REALPATH)
  target_sources(cbf
    PRIVATE "${CBF__SRC}/realpath.c")
endif()

set_target_properties(cbf PROPERTIES OUTPUT_NAME "cbf")
set_target_properties(cbf PROPERTIES LINKER_LANGUAGE C)
set_target_properties(cbf PROPERTIES SOVERSION "${SOVERSION}")
target_include_directories(cbf
  PUBLIC
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>"
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
    "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")
target_link_libraries(cbf
  PUBLIC hdf5
  PRIVATE pcre2-posix
  PRIVATE ${libm})


#
# Build the static and shared IMG libraries
#
add_library(img "${CBF__SRC}/img.c")
set_target_properties(img PROPERTIES OUTPUT_NAME "img")
set_target_properties(img PROPERTIES LINKER_LANGUAGE C)
target_include_directories(img
  PUBLIC
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
    "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")


#
# Build Fortran sources, libraries, and examples.  The f90cbf library
# and the examples for testing are not installed.
if(CBF_ENABLE_FORTRAN)
  find_program(M4 m4 REQUIRED)
  set(M4FLAGS "-Dfcb_bytes_in_rec=4096" CACHE STRING
    "Flags used by the M4 macro processor during Fortran build")
  mark_as_advanced(M4FLAGS)

  set(f90_sources_m4
    "${CMAKE_CURRENT_SOURCE_DIR}/m4/fcb_exit_binary.m4"
    "${CMAKE_CURRENT_SOURCE_DIR}/m4/fcb_next_binary.m4"
    "${CMAKE_CURRENT_SOURCE_DIR}/m4/fcb_open_cifin.m4"
    "${CMAKE_CURRENT_SOURCE_DIR}/m4/fcb_packed.m4"
    "${CMAKE_CURRENT_SOURCE_DIR}/m4/fcb_read_bits.m4"
    "${CMAKE_CURRENT_SOURCE_DIR}/m4/fcb_read_image.m4"
    "${CMAKE_CURRENT_SOURCE_DIR}/m4/fcb_read_xds_i2.m4"
    "${CMAKE_CURRENT_SOURCE_DIR}/m4/test_fcb_read_image.m4"
    "${CMAKE_CURRENT_SOURCE_DIR}/m4/test_xds_binary.m4")

  foreach(f90srcm4 IN LISTS f90_sources_m4)
    get_filename_component(filename "${f90srcm4}" NAME_WE)
    set(f90bldsrc "${CMAKE_CURRENT_BINARY_DIR}/src/${filename}.f90")
    add_custom_command(
      OUTPUT "${f90bldsrc}"
      COMMAND ${CMAKE_COMMAND} -E make_directory
        "${CMAKE_CURRENT_BINARY_DIR}/src"
      COMMAND ${CMAKE_COMMAND}
        "-Dcommand=${M4};-P;${M4FLAGS};${f90srcm4}"
        "-Doutput-file=${f90bldsrc}"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/m4/fcblib_defines.m4"
              "${f90srcm4}"
      VERBATIM
      WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/m4")
  endforeach()

  add_library(fcb
    "${CMAKE_CURRENT_BINARY_DIR}/src/fcb_exit_binary.f90"
    "${CMAKE_CURRENT_BINARY_DIR}/src/fcb_next_binary.f90"
    "${CMAKE_CURRENT_BINARY_DIR}/src/fcb_open_cifin.f90"
    "${CMAKE_CURRENT_BINARY_DIR}/src/fcb_packed.f90"
    "${CMAKE_CURRENT_BINARY_DIR}/src/fcb_read_bits.f90"
    "${CMAKE_CURRENT_BINARY_DIR}/src/fcb_read_image.f90"
    "${CMAKE_CURRENT_BINARY_DIR}/src/fcb_read_xds_i2.f90"
    "${CBF__SRC}/fcb_atol_wcnt.f90"
    "${CBF__SRC}/fcb_ci_strncmparr.f90"
    "${CBF__SRC}/fcb_nblen_array.f90"
    "${CBF__SRC}/fcb_read_byte.f90"
    "${CBF__SRC}/fcb_read_line.f90"
    "${CBF__SRC}/fcb_skip_whitespace.f90")
  set_target_properties(fcb PROPERTIES OUTPUT_NAME "fcb")
  set_target_properties(fcb PROPERTIES LINKER_LANGUAGE C)
  install(TARGETS fcb DESTINATION lib)

  add_executable(test_fcb_read_image
    "${CMAKE_CURRENT_BINARY_DIR}/src/test_fcb_read_image.f90")
  target_link_libraries(test_fcb_read_image
    fcb)

  add_executable(test_xds_binary
    "${CMAKE_CURRENT_BINARY_DIR}/src/test_xds_binary.f90")
  target_link_libraries(test_xds_binary
    fcb)


  # Use the pre-generated SWIG wrapper, because current SWIG does not
  # support Fortran.
  add_library(f90cbf
    "${CMAKE_CURRENT_SOURCE_DIR}/f90cbf/f90cbf.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/f90cbf/f90cbf_wrap.c")
  target_link_libraries(f90cbf
    cbf)
endif()


#
# Java bindings and examples
if(BUILD_SHARED_LIBS AND CBF_ENABLE_JAVA)
  # In CMake 3.24, COMPONENTS could be set to "JVM" for FindJNI,
  # because AWT is not needed.
  find_package(Java COMPONENTS Development REQUIRED)
  find_package(JNI REQUIRED)
  find_package(SWIG COMPONENTS java REQUIRED)
  include(UseJava)
  include(UseSWIG)


  # Generate ${CMAKE_CURRENT_BINARY_DIR}/jcbf/jcbfJAVA_wrap.c and
  # ${_java_files_generated}.  Cannot use file(GLOB ...) for
  # ${_java_files_generated}, because it would be executed at
  # configuration time, when ${CMAKE_CURRENT_BINARY_DIR}/jcbf may not
  # exist.  get_property(... SWIG_SUPPORT_FILES) only returns
  # "jcbf/cbf.java" and "jcbf/cbfJNI.java"; the contents of
  # CMakeFiles/cbflib.dir/java_class_filelist do not appear to be
  # accessible.
  #
  # CMake 3.19 and earlier may not automatically regenerate
  # ${CMAKE_CURRENT_BINARY_DIR}/jcbf for Makefile Generators unless
  # ${CBF__SRC}/jcbf.i is touched.
  set_property(SOURCE "${CBF__SRC}/jcbf.i" PROPERTY
    INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/include")
  set_property(SOURCE "${CBF__SRC}/jcbf.i" PROPERTY
    USE_SWIG_DEPENDENCIES TRUE)

  swig_add_library(cbf_wrap
    LANGUAGE Java
    OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/jcbf"
    SOURCES "${CBF__SRC}/jcbf.i")
  set_target_properties(cbf_wrap PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    SWIG_COMPILE_OPTIONS "-package;org.iucr.cbflib")
  target_include_directories(cbf_wrap
    PRIVATE "${JAVA_INCLUDE_PATH}"
            "${JAVA_INCLUDE_PATH2}")
  target_link_libraries(cbf_wrap
    PRIVATE cbf)

  set(_java_files_generated
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/CBF_NODETYPE.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_FILE.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_cbf_context.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_cbf_file.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_double.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_int.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_long.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_p__cbf_handle_struct.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_p_cbf_node_struct.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_p_char.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_size_t.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_unsigned_int.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/SWIGTYPE_p_void.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/cbf.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/cbfConstants.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/cbfJNI.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/cbf_handle_struct.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/cbf_node.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/doubleP.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/intP.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/sizetP.java"
    "${CMAKE_CURRENT_BINARY_DIR}/jcbf/uintP.java")

  add_jar(cbflib
    "${_java_files_generated}"
    VERSION "${PROJECT_VERSION}")
  add_dependencies(cbflib
    cbf_wrap)

  add_executable(ctestcbf
    "${CBF__EXAMPLES}/testcbf.c")
  target_link_libraries(ctestcbf
    cbf)

  add_jar(testcbf
    "${CBF__EXAMPLES}/testcbf.java"
    INCLUDE_JARS cbflib)
endif()


#
# Python bindings
if(BUILD_SHARED_LIBS AND CBF_ENABLE_PYTHON)
  # As per Makefile, want links (not lynx) to reduce the diffs against
  # versioned CBFlib.txt.  Need SWIG 4.0.0 or later.  For Python 3,
  # Makefile also adds "# coding=utf-8" to the top of pycbf.py.
  find_program(BROWSER_DUMP_TOOL NAMES links elinks lynx REQUIRED)
  find_package(Python COMPONENTS Development Interpreter REQUIRED)
  find_package(SWIG COMPONENTS python REQUIRED)
  include(UseSWIG)

  add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/pycbf/CBFlib.txt"
    COMMAND ${CMAKE_COMMAND} -E make_directory
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf"
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=${BROWSER_DUMP_TOOL};-dump;CBFlib.html"
      "-Doutput-file=${CMAKE_CURRENT_BINARY_DIR}/pycbf/CBFlib.txt"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/doc/CBFlib.html"
    VERBATIM
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/doc")


  # Generate the SWIG interfaces from CBFlib.txt (so really no need to
  # keep those under version control).
  add_custom_command(
    OUTPUT
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/cbfdetectorwrappers.i"
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/cbfgenericwrappers.i"
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/cbfgoniometerwrappers.i"
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/cbfhandlewrappers.i"
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/cbfpositionerwrappers.i"
    COMMAND Python::Interpreter
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/make_pycbf.py"
      -o "${CMAKE_CURRENT_BINARY_DIR}/pycbf"
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/CBFlib.txt"
    DEPENDS
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/CBFlib.txt"
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/make_pycbf.py"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/pycbf")

  set_property(SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf.i" PROPERTY
    DEPENDS
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/cbfdetectorwrappers.i"
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/cbfgenericwrappers.i"
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/cbfgoniometerwrappers.i"
      "${CMAKE_CURRENT_BINARY_DIR}/pycbf/cbfhandlewrappers.i")
  swig_add_library(pycbf
    SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf.i"
    LANGUAGE Python
    OUTFILE_DIR "${CMAKE_CURRENT_BINARY_DIR}/pycbf"
    OUTPUT_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
  set_target_properties(pycbf PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
  target_compile_definitions(pycbf
    PRIVATE SWIG_PYTHON_STRICT_BYTE_CHAR)
  target_link_libraries(pycbf
    cbf
    Python::Module)

  install(
    TARGETS pycbf
    COMPONENT "Runtime"
    DESTINATION "${Python_SITEARCH}")

  if(CBF_ENABLE_DOC)
    find_program(PDFLATEX pdflatex REQUIRED)
    add_custom_command(
      OUTPUT "pycbf.pdf"
      COMMAND "${PDFLATEX}"
        -interaction batchmode
        -output-directory "${CMAKE_CURRENT_BINARY_DIR}"
        "pycbf.tex"
      COMMAND "${PDFLATEX}"
        -interaction batchmode
        -output-directory "${CMAKE_CURRENT_BINARY_DIR}"
        "pycbf.tex"
      MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf.tex"
      WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/pycbf")
    add_custom_target(pdf ALL
      DEPENDS "pycbf.pdf")
  endif()
endif()


#
#  C and C++ examples
#
add_executable(tiff2cbf
  "${CBF__EXAMPLES}/tif_sprint.c"
  "${CBF__EXAMPLES}/tiff2cbf.c")
target_link_libraries(tiff2cbf
  cbf
  tiff)

add_executable(cbf2nexus
  "${CBF__EXAMPLES}/cbf2nexus.c")
target_link_libraries(cbf2nexus
  cbf)

add_executable(nexus2cbf
  "${CBF__EXAMPLES}/nexus2cbf.c")
target_link_libraries(nexus2cbf
  cbf
  hdf5)

add_executable(minicbf2nexus
  "${CBF__EXAMPLES}/minicbf2nexus.c")
target_link_libraries(minicbf2nexus
  cbf)

add_executable(adscimg2cbf
  "${CBF__EXAMPLES}/adscimg2cbf.c"
  "${CBF__EXAMPLES}/adscimg2cbf_sub.c")
target_link_libraries(adscimg2cbf
  cbf
  "${libm}")

add_executable(cbf2adscimg
  "${CBF__EXAMPLES}/cbf2adscimg.c"
  "${CBF__EXAMPLES}/cbf2adscimg_sub.c")
target_link_libraries(cbf2adscimg
  cbf
  "${libm}")

add_executable(convert_f90_swig_wrap
  "${CBF__EXAMPLES}/convert_f90_swig_wrap.cpp")

add_executable(convert_image
  "${CBF__EXAMPLES}/convert_image.c")
target_link_libraries(convert_image
  cbf)

add_executable(convert_minicbf
  "${CBF__EXAMPLES}/convert_minicbf.c")
target_link_libraries(convert_minicbf
  cbf)

add_executable(makecbf
  "${CBF__EXAMPLES}/makecbf.c")
target_link_libraries(makecbf
  cbf)

add_executable(cbf_standardize_numbers
  "${CBF__EXAMPLES}/cbf_standardize_numbers.c")
target_link_libraries(cbf_standardize_numbers
  cbf
  "${libm}")
if(NOT HAVE_FGETLN)
  target_sources(cbf_standardize_numbers
    PRIVATE "${CBF__SRC}/fgetln.c")
endif()

add_executable(cbf_tail
  "${CBF__EXAMPLES}/cbf_tail.c")
target_link_libraries(cbf_tail
  cbf)

add_executable(cbf_testxfelread
  "${CBF__EXAMPLES}/cbf_testxfelread.c")
target_link_libraries(cbf_testxfelread
  cbf
  "${libm}")

add_executable(changtestcompression
  "${CBF__EXAMPLES}/changtestcompression.c")
target_link_libraries(changtestcompression
  cbf)

add_executable(img2cif
  "${CBF__EXAMPLES}/img2cif.c")
target_link_libraries(img2cif
  cbf)

add_executable(cif2cbf
  "${CBF__EXAMPLES}/cif2cbf.c")
target_link_libraries(cif2cbf
  cbf
  CQR
  "${libm}")

add_executable(cbf_template_t
  "${CBF__DECTRIS_EXAMPLES}/cbf_template_t.c")
target_link_libraries(cbf_template_t
  cbf)

add_executable(testcell
  "${CBF__EXAMPLES}/testcell.C")
target_link_libraries(testcell
  cbf)

add_executable(roi_peaksearch
  "${CBF__EXAMPLES}/dps_peaksearch.c"
  "${CBF__EXAMPLES}/roi_peaksearch.c")
target_link_libraries(roi_peaksearch
  cbf
  "${libm}")

add_executable(sauter_test
  "${CBF__EXAMPLES}/sauter_test.C")
target_link_libraries(sauter_test
  cbf)

add_executable(sequence_match
  "${CBF__EXAMPLES}/sequence_match.c")
target_link_libraries(sequence_match
  cbf)

add_executable(test_cbf_airy_disk
  "${CBF__EXAMPLES}/test_cbf_airy_disk.c")
target_link_libraries(test_cbf_airy_disk
  cbf
  "${libm}")

if(CBF_ENABLE_ULP)
  add_executable(testulp
    "${CBF__EXAMPLES}/testulp.c")
  target_link_libraries(testulp
    cbf)
endif()

add_executable(testhdf5
  "${CBF__EXAMPLES}/testhdf5.c")
target_link_libraries(testhdf5
  cbf)

add_executable(testalloc
  "${CBF__EXAMPLES}/testalloc.c")
target_link_libraries(testalloc
  cbf)

add_executable(testflat
  "${CBF__EXAMPLES}/testflat.c")
target_link_libraries(testflat
  cbf)

add_executable(testflatpacked
  "${CBF__EXAMPLES}/testflatpacked.c")
target_link_libraries(testflatpacked
  cbf)

add_executable(testreals
  "${CBF__EXAMPLES}/testreals.c")
target_link_libraries(testreals
  cbf)


#
# install
#
install (TARGETS makecbf DESTINATION bin)
install (TARGETS img2cif DESTINATION bin)
install (TARGETS cif2cbf DESTINATION bin)
install (TARGETS adscimg2cbf DESTINATION bin)
install (TARGETS cbf2adscimg DESTINATION bin)
install (TARGETS changtestcompression DESTINATION bin)
install (TARGETS convert_image DESTINATION bin)
install (TARGETS convert_minicbf DESTINATION bin)
install (TARGETS minicbf2nexus DESTINATION bin)
install (TARGETS cbf2nexus DESTINATION bin)
install (TARGETS nexus2cbf DESTINATION bin)
install (TARGETS cbf_template_t DESTINATION bin)
install (TARGETS testcell DESTINATION bin)
install (TARGETS sauter_test DESTINATION bin)
install (TARGETS sequence_match DESTINATION bin)
install (TARGETS tiff2cbf DESTINATION bin)
install(TARGETS cbf DESTINATION lib)

install(TARGETS img DESTINATION lib)
install (DIRECTORY ${CBF__INCLUDE}/ DESTINATION include/cbflib FILES_MATCHING PATTERN "*.h")
install (DIRECTORY ${CBF__BIN_INCLUDE}/* DESTINATION include/cbflib FILES_MATCHING PATTERN "*.h")

# message(STATUS "CMAKE_INSTALL_PREFIX/lib ${CMAKE_INSTALL_PREFIX}/lib")


#
# tests  -- to be extended
#
# Fixtures are named like the tests that set them up and generally
# correspond to filenames.  The cleanup tests not only keep the build
# directory clean after ctest, but also verify that any expected
# output files were created for tests without comparison steps.
enable_testing()


# Path to the valgrind executable
find_program(VALGRIND valgrind)
if(VALGRIND)
  set(VALGRIND_COMMAND ${VALGRIND} --error-exitcode=3 --leak-check=full)
else()
  set(VALGRIND_COMMAND "")
endif()


# Set up a wrapper to call 'h5dump' and redirect its output to a file.
file(GENERATE
  OUTPUT "h5dump-$<CONFIG>.cmake"
  INPUT "h5dump.cmake")


#
# Basic (core) tests

#
# makecbf
add_test(NAME makecbf
  COMMAND makecbf
    "${data_input}/example.mar2300"
    "${CBF__DATA}/makecbf.cbf")
set_tests_properties(makecbf PROPERTIES
  FIXTURES_SETUP makecbf
  REQUIRED_FILES "${data_input}/example.mar2300")

add_test(NAME makecbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/makecbf.cbf")
set_tests_properties(makecbf-cleanup PROPERTIES
  FIXTURES_CLEANUP makecbf)


#
# img2cif/cif2cbf
add_test(NAME img2cif-flatpacked-cif
  COMMAND img2cif -c flatpacked -m headers -d digest -e base64
    "${data_input}/example.mar2300"
    "${CBF__DATA}/img2cif_packed.cif")
set_tests_properties(img2cif-flatpacked-cif PROPERTIES
  FIXTURES_SETUP img2cif-flatpacked-cif
  REQUIRED_FILES "${data_input}/example.mar2300")

add_test(NAME img2cif-flatpacked-cif-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/img2cif_packed.cif")
set_tests_properties(img2cif-flatpacked-cif-cleanup PROPERTIES
  FIXTURES_CLEANUP img2cif-flatpacked-cif)

add_test(NAME img2cif-canonical-cif
  COMMAND img2cif -c canonical -m headers -d digest -e base64
    "${data_input}/example.mar2300"
    "${CBF__DATA}/img2cif_canonical.cif")
set_tests_properties(img2cif-canonical-cif PROPERTIES
  FIXTURES_SETUP img2cif-canonical-cif
  REQUIRED_FILES "${data_input}/example.mar2300")

add_test(NAME img2cif-canonical-cif-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/img2cif_canonical.cif")
set_tests_properties(img2cif-canonical-cif-cleanup PROPERTIES
  FIXTURES_CLEANUP img2cif-canonical-cif)

add_test(NAME img2cif-flatpacked-cbf
  COMMAND img2cif -c flatpacked -m headers -d digest -e none
    "${data_input}/example.mar2300"
    "${CBF__DATA}/img2cif_packed.cbf")
set_tests_properties(img2cif-flatpacked-cbf PROPERTIES
  FIXTURES_SETUP img2cif-flatpacked-cbf
  REQUIRED_FILES "${data_input}/example.mar2300")

add_test(NAME img2cif-flatpacked-cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/img2cif_packed.cbf")
set_tests_properties(img2cif-flatpacked-cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP img2cif-flatpacked-cbf)

add_test(NAME img2cif-canonical-cbf
  COMMAND img2cif -c canonical -m headers -d digest -e none
    "${data_input}/example.mar2300"
    "${CBF__DATA}/img2cif_canonical.cbf")
set_tests_properties(img2cif-canonical-cbf PROPERTIES
  FIXTURES_SETUP img2cif-canonical-cbf
  REQUIRED_FILES "${data_input}/example.mar2300")

add_test(NAME img2cif-canonical-cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/img2cif_canonical.cbf")
set_tests_properties(img2cif-canonical-cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP img2cif-canonical-cbf)

add_test(NAME cif2cbf-flatpacked-cbf
  COMMAND cif2cbf -e none -c flatpacked
    "${CBF__DATA}/img2cif_canonical.cif"
    "${CBF__DATA}/cif2cbf_packed.cbf")
set_tests_properties(cif2cbf-flatpacked-cbf PROPERTIES
  FIXTURES_REQUIRED img2cif-canonical-cif
  FIXTURES_SETUP cif2cbf-flatpacked-cbf)

add_test(NAME cif2cbf-flatpacked-cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/cif2cbf_packed.cbf")
set_tests_properties(cif2cbf-flatpacked-cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-flatpacked-cbf)

add_test(NAME cif2cbf-canonical-cbf
  COMMAND cif2cbf -e none -c canonical
    "${CBF__DATA}/img2cif_packed.cif"
    "${CBF__DATA}/cif2cbf_canonical.cbf")
set_tests_properties(cif2cbf-canonical-cbf PROPERTIES
  FIXTURES_REQUIRED img2cif-flatpacked-cif
  FIXTURES_SETUP cif2cbf-canonical-cbf)

add_test(NAME cif2cbf-canonical-cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/cif2cbf_canonical.cbf")
set_tests_properties(cif2cbf-canonical-cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-canonical-cbf)

add_test(NAME makecbf-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/cif2cbf_packed.cbf"
    "${CBF__DATA}/makecbf.cbf")
set_tests_properties(makecbf-cmp PROPERTIES
  FIXTURES_REQUIRED "cif2cbf-flatpacked-cbf;makecbf")

add_test(NAME cif2cbf-img2cif-flatpacked-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/cif2cbf_packed.cbf"
    "${CBF__DATA}/img2cif_packed.cbf")
set_tests_properties(cif2cbf-img2cif-flatpacked-cmp PROPERTIES
  FIXTURES_REQUIRED "cif2cbf-flatpacked-cbf;img2cif-flatpacked-cbf")

add_test(NAME cif2cbf-img2cif-canonical-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/cif2cbf_canonical.cbf"
    "${CBF__DATA}/img2cif_canonical.cbf")
set_tests_properties(cif2cbf-img2cif-canonical-cmp PROPERTIES
  FIXTURES_REQUIRED "cif2cbf-canonical-cbf;img2cif-canonical-cbf")


#
# Extra tests
add_test(NAME cif2cbf-ehcn
  COMMAND cif2cbf -e hex -c none
    "${CBF__DATA}/makecbf.cbf"
    "${CBF__DATA}/cif2cbf_ehcn.cif")
set_tests_properties(cif2cbf-ehcn PROPERTIES
  FIXTURES_REQUIRED makecbf
  FIXTURES_SETUP cif2cbf-ehcn)

add_test(NAME cif2cbf-ehcn-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/cif2cbf_ehcn.cif")
set_tests_properties(cif2cbf-ehcn-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-ehcn)

add_test(NAME cif2cbf-ehcn-encp
  COMMAND cif2cbf -e none -c flatpacked
    "${CBF__DATA}/cif2cbf_ehcn.cif"
    "${CBF__DATA}/cif2cbf_encp.cbf")
set_tests_properties(cif2cbf-ehcn-encp PROPERTIES
  FIXTURES_REQUIRED cif2cbf-ehcn
  FIXTURES_SETUP cif2cbf-ehcn-encp)

add_test(NAME cif2cbf-ehcn-encp-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/cif2cbf_encp.cbf")
set_tests_properties(cif2cbf-ehcn-encp-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-ehcn-encp)

add_test(NAME cif2cbf-ehcn-encp-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/makecbf.cbf"
    "${CBF__DATA}/cif2cbf_encp.cbf")
set_tests_properties(cif2cbf-ehcn-encp-cmp PROPERTIES
  FIXTURES_REQUIRED "makecbf;cif2cbf-ehcn-encp")


#
# 9ins
add_test(NAME 9ins
  COMMAND cif2cbf
    -i "${data_input}/9ins.cif"
    -o "${CBF__DATA}/9ins.cbf")
set_tests_properties(9ins PROPERTIES
  FIXTURES_SETUP 9ins
  REQUIRED_FILES "${data_input}/9ins.cif")

add_test(NAME 9ins-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/9ins.cbf")
set_tests_properties(9ins-cleanup PROPERTIES
  FIXTURES_CLEANUP 9ins)

add_test(NAME 9ins-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_input}/9ins.cif"
    "${CBF__DATA}/9ins.cbf")
set_tests_properties(9ins-cmp PROPERTIES
  FIXTURES_REQUIRED 9ins
  REQUIRED_FILES "${data_input}/9ins.cif")


#
# convert_image-flat
add_test(NAME convert_image-flat
  COMMAND convert_image
    -p "${CBFlib_SOURCE_DIR}/templates/template_mar345_2300x2300.cbf"
    -F "${data_input}/example.mar2300"
    "${CBF__DATA}/converted_flat.cbf")
set_tests_properties(convert_image-flat PROPERTIES
  FIXTURES_SETUP convert_image-flat
  REQUIRED_FILES "${CBFlib_SOURCE_DIR}/templates/template_mar345_2300x2300.cbf;${data_input}/example.mar2300")

add_test(NAME convert_image-flat-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/converted_flat.cbf")
set_tests_properties(convert_image-flat-cleanup PROPERTIES
  FIXTURES_CLEANUP convert_image-flat)

add_test(NAME convert_image-flat-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_output}/converted_flat_orig.cbf"
    "${CBF__DATA}/converted_flat.cbf")
set_tests_properties(convert_image-flat-cmp PROPERTIES
  FIXTURES_REQUIRED convert_image-flat
  REQUIRED_FILES "${data_output}/converted_flat_orig.cbf")


#
# convert_image
add_test(NAME convert_image
  COMMAND convert_image
    -p "${CBFlib_SOURCE_DIR}/templates/template_mar345_2300x2300.cbf"
    "${data_input}/example.mar2300"
    "${CBF__DATA}/converted.cbf")
set_tests_properties(convert_image PROPERTIES
  FIXTURES_SETUP convert_image
  REQUIRED_FILES "${CBFlib_SOURCE_DIR}/templates/template_mar345_2300x2300.cbf;${data_input}/example.mar2300")

add_test(NAME convert_image-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/converted.cbf")
set_tests_properties(convert_image-cleanup PROPERTIES
  FIXTURES_CLEANUP convert_image)

add_test(NAME convert_image-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_output}/converted_orig.cbf"
    "${CBF__DATA}/converted.cbf")
set_tests_properties(convert_image-cmp PROPERTIES
  FIXTURES_REQUIRED convert_image
  REQUIRED_FILES "${data_output}/converted_orig.cbf")


#
# convert_image-adscimg-flat
add_test(NAME convert_image-adscimg-flat
  COMMAND convert_image
    -p "${CBFlib_SOURCE_DIR}/templates/template_adscquantum315_3072x3072.cbf"
    -F -d adscquantum315
    "${data_input}/mb_LP_1_001.img"
    "${CBF__DATA}/adscconverted_flat.cbf")
set_tests_properties(convert_image-adscimg-flat PROPERTIES
  FIXTURES_SETUP convert_image-adscimg-flat
  REQUIRED_FILES "${CBFlib_SOURCE_DIR}/templates/template_adscquantum315_3072x3072.cbf;${data_input}/mb_LP_1_001.img")

add_test(NAME convert_image-adscimg-flat-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/adscconverted_flat.cbf")
set_tests_properties(convert_image-adscimg-flat-cleanup PROPERTIES
  FIXTURES_CLEANUP convert_image-adscimg-flat)

add_test(NAME convert_image-adscimg-flat-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_output}/adscconverted_flat_orig.cbf"
    "${CBF__DATA}/adscconverted_flat.cbf")
set_tests_properties(convert_image-adscimg-flat-cmp PROPERTIES
  FIXTURES_REQUIRED convert_image-adscimg-flat
  REQUIRED_FILES "${data_output}/adscconverted_flat_orig.cbf")


#
# convert_image-adscimg
add_test(NAME convert_image-adscimg
  COMMAND convert_image
    -p "${CBFlib_SOURCE_DIR}/templates/template_adscquantum315_3072x3072.cbf"
    -d adscquantum315
    "${data_input}/mb_LP_1_001.img"
    "${CBF__DATA}/adscconverted.cbf")
set_tests_properties(convert_image-adscimg PROPERTIES
  FIXTURES_SETUP convert_image-adscimg
  REQUIRED_FILES "${CBFlib_SOURCE_DIR}/templates/template_adscquantum315_3072x3072.cbf;${data_input}/mb_LP_1_001.img")

add_test(NAME convert_image-adscimg-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/adscconverted.cbf")
set_tests_properties(convert_image-adscimg-cleanup PROPERTIES
  FIXTURES_CLEANUP convert_image-adscimg)

add_test(NAME convert_image-adscimg-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_output}/adscconverted_orig.cbf"
    "${CBF__DATA}/adscconverted.cbf")
set_tests_properties(convert_image-adscimg-cmp PROPERTIES
  FIXTURES_REQUIRED convert_image-adscimg
  REQUIRED_FILES "${data_output}/adscconverted_orig.cbf")


#
# adscimg2cbf
add_test(NAME adscimg2cbf
  COMMAND adscimg2cbf --no_pad --cbf_packed,flat
    "--output=${CBF__DATA}/mb_LP_1_001.cbf"
    "${data_input}/mb_LP_1_001.img")
set_tests_properties(adscimg2cbf PROPERTIES
  FIXTURES_SETUP adscimg2cbf
  REQUIRED_FILES "${data_input}/mb_LP_1_001.img")

add_test(NAME adscimg2cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/mb_LP_1_001.cbf")
set_tests_properties(adscimg2cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP adscimg2cbf)

add_test(NAME adscimg2cbf-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_output}/mb_LP_1_001_orig.cbf"
    "${CBF__DATA}/mb_LP_1_001.cbf")
set_tests_properties(adscimg2cbf-cmp PROPERTIES
  FIXTURES_REQUIRED adscimg2cbf
  REQUIRED_FILES "${data_output}/mb_LP_1_001_orig.cbf")


#
# cbf2adscimg
add_test(NAME cbf2adscimg
  COMMAND cbf2adscimg "${CBF__DATA}/mb_LP_1_001.cbf")
set_tests_properties(cbf2adscimg PROPERTIES
  FIXTURES_REQUIRED adscimg2cbf
  FIXTURES_SETUP cbf2adscimg)

add_test(NAME cbf2adscimg-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/mb_LP_1_001.img")
set_tests_properties(cbf2adscimg-cleanup PROPERTIES
  FIXTURES_CLEANUP cbf2adscimg)

add_test(NAME cbf2adscimg-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_input}/mb_LP_1_001.img"
    "${CBF__DATA}/mb_LP_1_001.img")
set_tests_properties(cbf2adscimg-cmp PROPERTIES
  FIXTURES_REQUIRED cbf2adscimg
  REQUIRED_FILES "${data_input}/mb_LP_1_001.img")


#
# batch_convert_minicbf
add_test(NAME batch_convert_minicbf
  COMMAND ${CMAKE_COMMAND}
    "-DCBF_CONVERT_MINICBF_PATH=$<TARGET_FILE_DIR:convert_minicbf>"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/examples/batch_convert_minicbf.cmake" --
      "${data_input}"
      "${CBF__DATA}"
      "X4_lots_M1S4_1_*.cbf"
      "${CBFlib_SOURCE_DIR}/templates/template_X4_lots_M1S4.cbf")
set_tests_properties(batch_convert_minicbf PROPERTIES
  FIXTURES_SETUP batch_convert_minicbf
  REQUIRED_FILES "${data_input}/X4_lots_M1S4_1_0001.cbf;${data_input}/X4_lots_M1S4_1_0002.cbf;${data_input}/X4_lots_M1S4_1_0003.cbf;${data_input}/X4_lots_M1S4_1_0004.cbf;${data_input}/X4_lots_M1S4_1_0005.cbf")

add_test(NAME batch_convert_minicbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm
    "${CBF__DATA}/X4_lots_M1S4_1_0001.cbf"
    "${CBF__DATA}/X4_lots_M1S4_1_0002.cbf"
    "${CBF__DATA}/X4_lots_M1S4_1_0003.cbf"
    "${CBF__DATA}/X4_lots_M1S4_1_0004.cbf"
    "${CBF__DATA}/X4_lots_M1S4_1_0005.cbf")
set_tests_properties(batch_convert_minicbf-cleanup PROPERTIES
  FIXTURES_CLEANUP batch_convert_minicbf)


#
# convert_minicbf-v1
add_test(NAME convert_minicbf-v1
  COMMAND convert_minicbf
    -p "${CBFlib_SOURCE_DIR}/templates/template_pilatus6m_2463x2527.cbf"
    -d pilatus6m -v 1
    "${data_input}/insulin_pilatus6m.cbf"
    "${CBF__DATA}/insulin_pilatus6mconverted.cbf")
set_tests_properties(convert_minicbf-v1 PROPERTIES
  FIXTURES_SETUP convert_minicbf-v1
  REQUIRED_FILES "${CBFlib_SOURCE_DIR}/templates/template_pilatus6m_2463x2527.cbf;${data_input}/insulin_pilatus6m.cbf")

add_test(NAME convert_minicbf-v1-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted.cbf")
set_tests_properties(convert_minicbf-v1-cleanup PROPERTIES
  FIXTURES_CLEANUP convert_minicbf-v1)

add_test(NAME convert_minicbf-v1-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_output}/insulin_pilatus6mconverted_orig.cbf"
    "${CBF__DATA}/insulin_pilatus6mconverted.cbf")
set_tests_properties(convert_minicbf-v1-cmp PROPERTIES
  FIXTURES_REQUIRED convert_minicbf-v1
  REQUIRED_FILES "${data_output}/insulin_pilatus6mconverted_orig.cbf")


#
# convert_minicbf-v2
add_test(NAME convert_minicbf-v2
  COMMAND convert_minicbf
    -p "${CBFlib_SOURCE_DIR}/templates/template_pilatus6m_2463x2527.cbf"
    -d pilatus6m
    "${data_input}/insulin_pilatus6m.cbf"
    "${CBF__DATA}/insulin_pilatus6mconverted_v2.cbf")
set_tests_properties(convert_minicbf-v2 PROPERTIES
  FIXTURES_SETUP convert_minicbf-v2
  REQUIRED_FILES "${CBFlib_SOURCE_DIR}/templates/template_pilatus6m_2463x2527.cbf;${data_input}/insulin_pilatus6m.cbf")

add_test(NAME convert_minicbf-v2-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_v2.cbf")
set_tests_properties(convert_minicbf-v2-cleanup PROPERTIES
  FIXTURES_CLEANUP convert_minicbf-v2)

add_test(NAME convert_minicbf-v2-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_output}/insulin_pilatus6mconverted_v2_orig.cbf"
    "${CBF__DATA}/insulin_pilatus6mconverted_v2.cbf")
set_tests_properties(convert_minicbf-v2-cmp PROPERTIES
  FIXTURES_REQUIRED convert_minicbf-v2
  REQUIRED_FILES "${data_output}/insulin_pilatus6mconverted_v2_orig.cbf")


#
# cif2cbf basic roundtrip to HDF5 tests
add_test(NAME cif2cbf-opaque
  COMMAND cif2cbf -5 w -O ${HDF5REGISTER}
    -i "${CBF__DATA}/insulin_pilatus6mconverted.cbf"
    -o "${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5")
set_tests_properties(cif2cbf-opaque PROPERTIES
  FIXTURES_REQUIRED convert_minicbf-v1
  FIXTURES_SETUP cif2cbf-opaque)

add_test(NAME cif2cbf-opaque-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5")
set_tests_properties(cif2cbf-opaque-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-opaque)


#
# Check the default as HDF5 dumps
add_test(NAME insulin-orig-dump
  COMMAND ${CMAKE_COMMAND}
    "-Dinput-file=${data_output}/insulin_pilatus6mconverted_orig.cbf.h5"
    "-Doutput-file=${CBF__DATA}/insulin_pilatus6mconverted_orig.cbf.h5.dump"
    -P "${PROJECT_BINARY_DIR}/h5dump-$<CONFIG>.cmake")
set_tests_properties(insulin-orig-dump PROPERTIES
  FIXTURES_SETUP insulin-orig-dump
  REQUIRED_FILES "${data_output}/insulin_pilatus6mconverted_orig.cbf.h5")

add_test(NAME insulin-orig-dump-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_orig.cbf.h5.dump")
set_tests_properties(insulin-orig-dump-cleanup PROPERTIES
  FIXTURES_CLEANUP insulin-orig-dump)

add_test(NAME cif2cbf-opaque-dump
  COMMAND ${CMAKE_COMMAND}
    "-Dinput-file=${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5"
    "-Doutput-file=${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5.dump"
    -P "${PROJECT_BINARY_DIR}/h5dump-$<CONFIG>.cmake")
set_tests_properties(cif2cbf-opaque-dump PROPERTIES
  FIXTURES_REQUIRED cif2cbf-opaque
  FIXTURES_SETUP cif2cbf-opaque-dump)

add_test(NAME cif2cbf-opaque-dump-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5.dump")
set_tests_properties(cif2cbf-opaque-dump-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-opaque-dump)

add_test(NAME cif2cbf-opaque-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/insulin_pilatus6mconverted_orig.cbf.h5.dump"
    "${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5.dump")
set_tests_properties(cif2cbf-opaque-cmp PROPERTIES
  FIXTURES_REQUIRED "insulin-orig-dump;cif2cbf-opaque-dump")


#
# cif2cbf roundtrip tests
add_test(NAME cif2cbf-encI
  COMMAND cif2cbf -5 w ${HDF5REGISTER} -en -cI
    -i "${CBF__DATA}/insulin_pilatus6mconverted.cbf"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5")
set_tests_properties(cif2cbf-encI PROPERTIES
  FIXTURES_REQUIRED convert_minicbf-v1
  FIXTURES_SETUP cif2cbf-encI)

add_test(NAME cif2cbf-encI-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5")
set_tests_properties(cif2cbf-encI-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encI)

add_test(NAME cif2cbf-encI-cbf
  COMMAND cif2cbf -5 rn ${HDF5REGISTER} -en -cI
    -i "${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5.cbf")
set_tests_properties(cif2cbf-encI-cbf PROPERTIES
  FIXTURES_REQUIRED cif2cbf-encI
  FIXTURES_SETUP cif2cbf-encI-cbf)

add_test(NAME cif2cbf-encI-cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5.cbf")
set_tests_properties(cif2cbf-encI-cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encI-cbf)


#
# cif2cbf-encp
add_test(NAME cif2cbf-encp
  COMMAND cif2cbf -5 w ${HDF5REGISTER} -en -cp
    -i "${CBF__DATA}/insulin_pilatus6mconverted.cbf"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5")
set_tests_properties(cif2cbf-encp PROPERTIES
  FIXTURES_REQUIRED convert_minicbf-v1
  FIXTURES_SETUP cif2cbf-encp)

add_test(NAME cif2cbf-encp-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5")
set_tests_properties(cif2cbf-encp-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encp)

add_test(NAME cif2cbf-encp-cbf
  COMMAND cif2cbf -5 rn ${HDF5REGISTER} -en -cI
    -i "${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5.cbf")
set_tests_properties(cif2cbf-encp-cbf PROPERTIES
  FIXTURES_REQUIRED cif2cbf-encp
  FIXTURES_SETUP cif2cbf-encp-cbf)

add_test(NAME cif2cbf-encp-cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5.cbf")
set_tests_properties(cif2cbf-encp-cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encp-cbf)

add_test(NAME cif2cbf-encp-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5.cbf"
    "${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5.cbf")
set_tests_properties(cif2cbf-encp-cmp PROPERTIES
  FIXTURES_REQUIRED "cif2cbf-encI-cbf;cif2cbf-encp-cbf")


#
# cif2cbf-encb
add_test(NAME cif2cbf-encb
  COMMAND cif2cbf -5 w ${HDF5REGISTER} -en -cb
    -i "${CBF__DATA}/insulin_pilatus6mconverted.cbf"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5")
set_tests_properties(cif2cbf-encb PROPERTIES
  FIXTURES_REQUIRED convert_minicbf-v1
  FIXTURES_SETUP cif2cbf-encb)

add_test(NAME cif2cbf-encb-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5")
set_tests_properties(cif2cbf-encb-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encb)

add_test(NAME cif2cbf-encb-cbf
  COMMAND cif2cbf -5 rn ${HDF5REGISTER} -en -cI
    -i "${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5.cbf")
set_tests_properties(cif2cbf-encb-cbf PROPERTIES
  FIXTURES_REQUIRED cif2cbf-encb
  FIXTURES_SETUP cif2cbf-encb-cbf)

add_test(NAME cif2cbf-encb-cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5.cbf")
set_tests_properties(cif2cbf-encb-cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encb-cbf)

add_test(NAME cif2cbf-encb-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5.cbf"
    "${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5.cbf")
set_tests_properties(cif2cbf-encb-cmp PROPERTIES
  FIXTURES_REQUIRED "cif2cbf-encI-cbf;cif2cbf-encb-cbf")


#
# cif2cbf-encc
add_test(NAME cif2cbf-encc
  COMMAND cif2cbf -5 w ${HDF5REGISTER} -en -cc
    -i "${CBF__DATA}/insulin_pilatus6mconverted.cbf"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5")
set_tests_properties(cif2cbf-encc PROPERTIES
  FIXTURES_REQUIRED convert_minicbf-v1
  FIXTURES_SETUP cif2cbf-encc)

add_test(NAME cif2cbf-encc-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5")
set_tests_properties(cif2cbf-encc-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encc)

add_test(NAME cif2cbf-encc-cbf
  COMMAND cif2cbf -5 rn ${HDF5REGISTER} -en -cI
    -i "${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5.cbf")
set_tests_properties(cif2cbf-encc-cbf PROPERTIES
  FIXTURES_REQUIRED cif2cbf-encc
  FIXTURES_SETUP cif2cbf-encc-cbf)

add_test(NAME cif2cbf-encc-cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5.cbf")
set_tests_properties(cif2cbf-encc-cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encc-cbf)

add_test(NAME cif2cbf-encc-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5.cbf"
    "${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5.cbf")
set_tests_properties(cif2cbf-encc-cmp PROPERTIES
  FIXTURES_REQUIRED "cif2cbf-encI-cbf;cif2cbf-encc-cbf")


#
# cif2cbf-encz
add_test(NAME cif2cbf-encz
  COMMAND cif2cbf -5 w ${HDF5REGISTER} -en -cz
    -i "${CBF__DATA}/insulin_pilatus6mconverted.cbf"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encz.cbf.h5")
set_tests_properties(cif2cbf-encz PROPERTIES
  FIXTURES_REQUIRED convert_minicbf-v1
  FIXTURES_SETUP cif2cbf-encz)

add_test(NAME cif2cbf-encz-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encz.cbf.h5")
set_tests_properties(cif2cbf-encz-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encz)

add_test(NAME cif2cbf-encz-cbf
  COMMAND cif2cbf -5 rn ${HDF5REGISTER} -en -cI
    -i "${CBF__DATA}/insulin_pilatus6mconverted_encz.cbf.h5"
    -o "${CBF__DATA}/insulin_pilatus6mconverted_encz.cbf.h5.cbf")
set_tests_properties(cif2cbf-encz-cbf PROPERTIES
  FIXTURES_REQUIRED cif2cbf-encz
  FIXTURES_SETUP cif2cbf-encz-cbf)

add_test(NAME cif2cbf-encz-cbf-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/insulin_pilatus6mconverted_encz.cbf.h5.cbf")
set_tests_properties(cif2cbf-encz-cbf-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-encz-cbf)

add_test(NAME cif2cbf-encz-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5.cbf"
    "${CBF__DATA}/insulin_pilatus6mconverted_encz.cbf.h5.cbf")
set_tests_properties(cif2cbf-encz-cmp PROPERTIES
  FIXTURES_REQUIRED "cif2cbf-encI-cbf;cif2cbf-encz-cbf")


#
# tiff2cbf-XRD1621
add_test(NAME tiff2cbf-XRD1621
  COMMAND tiff2cbf -cn
    "${data_input}/XRD1621.tif"
    "${CBF__DATA}/XRD1621.cbf")
set_tests_properties(tiff2cbf-XRD1621 PROPERTIES
  FIXTURES_SETUP tiff2cbf-XRD1621
  REQUIRED_FILES "${data_input}/XRD1621.tif")

add_test(NAME tiff2cbf-XRD1621-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/XRD1621.cbf")
set_tests_properties(tiff2cbf-XRD1621-cleanup PROPERTIES
  FIXTURES_CLEANUP tiff2cbf-XRD1621)

add_test(NAME tiff2cbf-XRD1621-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_output}/XRD1621_orig.cbf"
    "${CBF__DATA}/XRD1621.cbf")
set_tests_properties(tiff2cbf-XRD1621-cmp PROPERTIES
  FIXTURES_REQUIRED tiff2cbf-XRD1621
  REQUIRED_FILES "${data_output}/XRD1621_orig.cbf")


#
# cif2cbf-XRD1621
add_test(NAME cif2cbf-XRD1621
  COMMAND cif2cbf -I 4 -C 100. -L 0. -e n -c b
    -i "${data_output}/XRD1621_orig.cbf"
    -o "${CBF__DATA}/XRD1621_I4encbC100.cbf")
set_tests_properties(cif2cbf-XRD1621 PROPERTIES
  FIXTURES_SETUP cif2cbf-XRD1621
  REQUIRED_FILES "${data_output}/XRD1621_orig.cbf")

add_test(NAME cif2cbf-XRD1621-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/XRD1621_I4encbC100.cbf")
set_tests_properties(cif2cbf-XRD1621-cleanup PROPERTIES
  FIXTURES_CLEANUP cif2cbf-XRD1621)

add_test(NAME cif2cbf-XRD1621-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${data_output}/XRD1621_I4encbC100_orig.cbf"
    "${CBF__DATA}/XRD1621_I4encbC100.cbf")
set_tests_properties(cif2cbf-XRD1621-cmp PROPERTIES
  FIXTURES_REQUIRED cif2cbf-XRD1621
  REQUIRED_FILES "${data_output}/XRD1621_I4encbC100_orig.cbf")


#
# airy_disk
add_test(NAME airy_disk
  COMMAND test_cbf_airy_disk)


#
# cbf_testxfelread
#
# cbf_testxfelread reads idx-s00-20131106040304531.cbf and writes
# idx-s00-20131106040304531_flat.cbf, both in the current working
# directory.
add_test(NAME xfelread-setup
  COMMAND ${CMAKE_COMMAND} -E copy
    "${CMAKE_CURRENT_SOURCE_DIR}/idx-s00-20131106040304531.cbf"
    "${CBF__DATA}")
set_tests_properties(xfelread-setup PROPERTIES
  FIXTURES_SETUP xfelread-setup)

add_test(NAME xfelread-setup-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/idx-s00-20131106040304531.cbf")
set_tests_properties(xfelread-setup-cleanup PROPERTIES
  FIXTURES_CLEANUP xfelread-setup)

add_test(NAME xfelread
  COMMAND cbf_testxfelread
  WORKING_DIRECTORY "${CBF__DATA}")
set_tests_properties(xfelread PROPERTIES
  FIXTURES_REQUIRED xfelread-setup
  FIXTURES_SETUP xfelread)

add_test(NAME xfelread-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/idx-s00-20131106040304531_flat.cbf")
set_tests_properties(xfelread-cleanup PROPERTIES
  FIXTURES_CLEANUP xfelread)


#
# Unit tests
#
# testalloc
if(VALGRIND)
  add_test(NAME testalloc
    COMMAND ${VALGRIND_COMMAND} $<TARGET_FILE:testalloc>)
else()
  add_test(NAME testalloc
    COMMAND testalloc)
endif()


#
# testhdf5
add_test(NAME testhdf5
  COMMAND testhdf5
  WORKING_DIRECTORY "${CBF__DATA}")
set_tests_properties(testhdf5 PROPERTIES
  FIXTURES_SETUP testhdf5)

add_test(NAME testhdf5-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testfile.h5")
set_tests_properties(testhdf5-cleanup PROPERTIES
  FIXTURES_CLEANUP testhdf5)


#
# testulp
if(CBF_ENABLE_ULP)
  add_test(NAME testulp
    COMMAND testulp)
endif()


#
# flat
#
# testflat reads testflatin.cbf and writes testflatout.cbf, both in
# the current working directory.
add_test(NAME flat-setup
  COMMAND ${CMAKE_COMMAND} -E copy
    "${data_input}/testflatin.cbf"
    "${CBF__DATA}/testflatin.cbf")
set_tests_properties(flat-setup PROPERTIES
  FIXTURES_SETUP flat-setup
  REQUIRED_FILES "${data_input}/testflatin.cbf")

add_test(NAME flat-setup-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testflatin.cbf")
set_tests_properties(flat-setup-cleanup PROPERTIES
  FIXTURES_CLEANUP flat-setup)

add_test(NAME flat
  COMMAND testflat
  WORKING_DIRECTORY "${CBF__DATA}")
set_tests_properties(flat PROPERTIES
  FIXTURES_REQUIRED flat-setup
  FIXTURES_SETUP flat)

add_test(NAME flat-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testflatout.cbf")
set_tests_properties(flat-cleanup PROPERTIES
  FIXTURES_CLEANUP flat)

add_test(NAME flat-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/testflatin.cbf"
    "${CBF__DATA}/testflatout.cbf")
set_tests_properties(flat-cmp PROPERTIES
  FIXTURES_REQUIRED "flat-setup;flat")


#
# flatpacked
#
# testflatpacked reads testflatpackedin.cbf and writes
# testflatpackedout.cbf, both in the current working directory.
add_test(NAME flatpacked-setup
  COMMAND ${CMAKE_COMMAND} -E copy
    "${data_input}/testflatpackedin.cbf"
    "${CBF__DATA}/testflatpackedin.cbf")
set_tests_properties(flatpacked-setup PROPERTIES
  FIXTURES_SETUP flatpacked-setup
  REQUIRED_FILES "${data_input}/testflatpackedin.cbf")

add_test(NAME flatpacked-setup-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testflatpackedin.cbf")
set_tests_properties(flatpacked-setup-cleanup PROPERTIES
  FIXTURES_CLEANUP flatpacked-setup)

add_test(NAME flatpacked
  COMMAND testflatpacked
  WORKING_DIRECTORY "${CBF__DATA}")
set_tests_properties(flatpacked PROPERTIES
  FIXTURES_REQUIRED flatpacked-setup
  FIXTURES_SETUP flatpacked)

add_test(NAME flatpacked-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testflatpackedout.cbf")
set_tests_properties(flatpacked-cleanup PROPERTIES
  FIXTURES_CLEANUP flatpacked)

add_test(NAME flatpacked-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/testflatpackedin.cbf"
    "${CBF__DATA}/testflatpackedout.cbf")
set_tests_properties(flatpacked-cmp PROPERTIES
  FIXTURES_REQUIRED "flatpacked-setup;flatpacked")


#
# reals
#
# testreals reads testrealin.cbf and writes testrealout.cbf, both in
# the current working directory.
add_test(NAME reals-setup
  COMMAND ${CMAKE_COMMAND} -E copy
    "${data_input}/testrealin.cbf"
    "${CBF__DATA}/testrealin.cbf")
set_tests_properties(reals-setup PROPERTIES
  FIXTURES_SETUP reals-setup
  REQUIRED_FILES "${data_input}/testrealin.cbf")

add_test(NAME reals-setup-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testrealin.cbf")
set_tests_properties(reals-setup-cleanup PROPERTIES
  FIXTURES_CLEANUP reals-setup)

add_test(NAME reals
  COMMAND testreals
  WORKING_DIRECTORY "${CBF__DATA}")
set_tests_properties(reals PROPERTIES
  FIXTURES_REQUIRED reals-setup
  FIXTURES_SETUP reals)

add_test(NAME reals-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testrealout.cbf")
set_tests_properties(reals-cleanup PROPERTIES
  FIXTURES_CLEANUP reals)

add_test(NAME reals-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/testrealin.cbf"
    "${CBF__DATA}/testrealout.cbf")
set_tests_properties(reals-cmp PROPERTIES
  FIXTURES_REQUIRED "reals-setup;reals")


#
# sauter
#
# sauter_test reads adscconverted_flat.cbf in the current working
# directory.  If convert_image-adscimg-flat passes, the file should
# exist; if convert_image-adscimg-flat-cmp also passes, it is
# identical to ${data_output}/adscconverted_flat_orig.cbf.
add_test(NAME sauter
  COMMAND sauter_test
  WORKING_DIRECTORY "${CBF__DATA}")
set_tests_properties(sauter PROPERTIES
  FIXTURES_REQUIRED "convert_image-adscimg-flat;convert_image-adscimg-flat-cmp")



#
# changtestcompression
add_test(NAME changtestcompression
  COMMAND changtestcompression
  WORKING_DIRECTORY "${CBF__DATA}")
set_tests_properties(changtestcompression PROPERTIES
  FIXTURES_SETUP changtestcompression)

add_test(NAME changtestcompression-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/CTC.cbf")
set_tests_properties(changtestcompression-cleanup PROPERTIES
  FIXTURES_CLEANUP changtestcompression)


#
# testcell
add_test(NAME testcell
  COMMAND ${CMAKE_COMMAND}
    "-Dcommand=$<TARGET_FILE:testcell>"
    "-Dinput-file=${CMAKE_CURRENT_SOURCE_DIR}/testcell.dat"
    "-Doutput-file=${CBF__DATA}/testcell.prt"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake")
set_tests_properties(testcell PROPERTIES
  FIXTURES_SETUP testcell
  REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/testcell.dat")

add_test(NAME testcell-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testcell.prt")
set_tests_properties(testcell-cleanup PROPERTIES
  FIXTURES_CLEANUP testcell)

add_test(NAME testcell-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
    "${data_output}/testcell_orig.prt"
    "${CBF__DATA}/testcell.prt")
set_tests_properties(testcell-cmp PROPERTIES
  FIXTURES_REQUIRED testcell
  REQUIRED_FILES "${data_output}/testcell_orig.prt")


#
# minicbf2nexus
#
# Avoid file(GLOB ...) in the source tree.  Dump the content of
# generated and reference HDF5 files and use cbf_tail to strip off
# some file names.
set(_minicbf2nexus_files
  "${data_input}/X4_lots_M1S4_1_0001.cbf"
  "${data_input}/X4_lots_M1S4_1_0002.cbf"
  "${data_input}/X4_lots_M1S4_1_0003.cbf"
  "${data_input}/X4_lots_M1S4_1_0004.cbf"
  "${data_input}/X4_lots_M1S4_1_0005.cbf")
add_test(NAME minicbf2nexus
  COMMAND minicbf2nexus -c zlib
    -C "${CBFlib_SOURCE_DIR}/minicbf_test/config" ${HDF5REGISTER}
    -o "${CBF__DATA}/minicbf.h5"
    ${_minicbf2nexus_files})
set_tests_properties(minicbf2nexus PROPERTIES
  FIXTURES_SETUP minicbf2nexus
  REQUIRED_FILES ${_minicbf2nexus_files})

add_test(NAME minicbf2nexus-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/minicbf.h5")
set_tests_properties(minicbf2nexus-cleanup PROPERTIES
  FIXTURES_CLEANUP minicbf2nexus)

add_test(NAME minicbf2nexus-orig
  COMMAND ${CMAKE_COMMAND}
    "-Dinput-file=${data_output}/minicbf_orig.h5"
    "-Doutput-file=${CBF__DATA}/minicbf_orig.dump"
    -P "${PROJECT_BINARY_DIR}/h5dump-$<CONFIG>.cmake")
set_tests_properties(minicbf2nexus-orig PROPERTIES
  FIXTURES_SETUP minicbf2nexus-orig
  REQUIRED_FILES "${data_output}/minicbf_orig.h5")

add_test(NAME minicbf2nexus-orig-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/minicbf_orig.dump")
set_tests_properties(minicbf2nexus-orig-cleanup PROPERTIES
  FIXTURES_CLEANUP minicbf2nexus-orig)

add_test(NAME minicbf2nexus-dump
  COMMAND ${CMAKE_COMMAND}
    "-Dinput-file=${CBF__DATA}/minicbf.h5"
    "-Doutput-file=${CBF__DATA}/minicbf.dump"
    -P "${PROJECT_BINARY_DIR}/h5dump-$<CONFIG>.cmake")
set_tests_properties(minicbf2nexus-dump PROPERTIES
  FIXTURES_REQUIRED minicbf2nexus
  FIXTURES_SETUP minicbf2nexus-dump)

add_test(NAME minicbf2nexus-dump-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/minicbf.dump")
set_tests_properties(minicbf2nexus-dump-cleanup PROPERTIES
  FIXTURES_CLEANUP minicbf2nexus-dump)

add_test(NAME minicbf2nexus-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/minicbf_orig.dump"
    "${CBF__DATA}/minicbf.dump")
set_tests_properties(minicbf2nexus-cmp PROPERTIES
  FIXTURES_REQUIRED "minicbf2nexus-orig;minicbf2nexus-dump")


#
# cbf2nexus/nexus2cbf
#
# Roundtrip cbf2nexus and nexus2cbf using i19 data.  Dump the content
# of generated HDF5 files and use cbf_tail to strip off some file
# names.
add_test(NAME cbf2nexus-1
  COMMAND cbf2nexus -c zlib --list
    -o "${CBF__DATA}/i19-1.h5"
    "${data_input}/1191_00005.cbf")
set_tests_properties(cbf2nexus-1 PROPERTIES
  FIXTURES_SETUP cbf2nexus-1
  REQUIRED_FILES "${data_input}/1191_00005.cbf")

add_test(NAME cbf2nexus-1-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/i19-1.h5")
set_tests_properties(cbf2nexus-1-cleanup PROPERTIES
  FIXTURES_CLEANUP cbf2nexus-1)

add_test(NAME nexus2cbf-1
  COMMAND nexus2cbf
    -o "${CBF__DATA}/i19-1.cbf"
    "${CBF__DATA}/i19-1.h5")
set_tests_properties(nexus2cbf-1 PROPERTIES
  FIXTURES_REQUIRED cbf2nexus-1
  FIXTURES_SETUP nexus2cbf-1)

add_test(NAME nexus2cbf-1-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/i19-1.cbf")
set_tests_properties(nexus2cbf-1-cleanup PROPERTIES
  FIXTURES_CLEANUP nexus2cbf-1)

add_test(NAME cbf2nexus-2
  COMMAND cbf2nexus -c zlib --list
    -o "${CBF__DATA}/i19-2.h5"
    "${CBF__DATA}/i19-1.cbf")
set_tests_properties(cbf2nexus-2 PROPERTIES
  FIXTURES_REQUIRED nexus2cbf-1
  FIXTURES_SETUP cbf2nexus-2)

add_test(NAME cbf2nexus-2-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/i19-2.h5")
set_tests_properties(cbf2nexus-2-cleanup PROPERTIES
  FIXTURES_CLEANUP cbf2nexus-2)

add_test(NAME nexus2cbf-2
  COMMAND nexus2cbf
    -o "${CBF__DATA}/i19-2.cbf"
    "${CBF__DATA}/i19-2.h5")
set_tests_properties(nexus2cbf-2 PROPERTIES
  FIXTURES_REQUIRED cbf2nexus-2
  FIXTURES_SETUP nexus2cbf-2)

add_test(NAME nexus2cbf-2-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/i19-2.cbf")
set_tests_properties(nexus2cbf-2-cleanup PROPERTIES
  FIXTURES_CLEANUP nexus2cbf-2)

add_test(NAME nexus2cbf-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/i19-1.cbf"
    "${CBF__DATA}/i19-2.cbf")
set_tests_properties(nexus2cbf-cmp PROPERTIES
  FIXTURES_REQUIRED "nexus2cbf-1;nexus2cbf-2")

add_test(NAME h5dump-1
  COMMAND ${CMAKE_COMMAND}
    "-Dinput-file=${CBF__DATA}/i19-1.h5"
    "-Doutput-file=${CBF__DATA}/i19-1.dump"
    -P "${PROJECT_BINARY_DIR}/h5dump-$<CONFIG>.cmake")
set_tests_properties(h5dump-1 PROPERTIES
  FIXTURES_REQUIRED cbf2nexus-1
  FIXTURES_SETUP h5dump-1)

add_test(NAME h5dump-1-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/i19-1.dump")
set_tests_properties(h5dump-1-cleanup PROPERTIES
  FIXTURES_CLEANUP h5dump-1)

add_test(NAME h5dump-2
  COMMAND ${CMAKE_COMMAND}
    "-Dinput-file=${CBF__DATA}/i19-2.h5"
    "-Doutput-file=${CBF__DATA}/i19-2.dump"
    -P "${PROJECT_BINARY_DIR}/h5dump-$<CONFIG>.cmake")
set_tests_properties(h5dump-2 PROPERTIES
  FIXTURES_REQUIRED cbf2nexus-2
  FIXTURES_SETUP h5dump-2)

add_test(NAME h5dump-2-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/i19-2.dump")
set_tests_properties(h5dump-2-cleanup PROPERTIES
  FIXTURES_CLEANUP h5dump-2)

add_test(NAME h5dump-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBF__DATA}/i19-1.dump"
    "${CBF__DATA}/i19-2.dump")
set_tests_properties(h5dump-cmp PROPERTIES
  FIXTURES_REQUIRED "h5dump-1;h5dump-2")


#
# dectristests
#
# cbf_template_t reads cbf_6M_template.cbf and writes
# cbf_template_t.out, both in the current working directory.
add_test(NAME dectris-setup
  COMMAND ${CMAKE_COMMAND} -E copy
    "${CBFlib_SOURCE_DIR}/templates/cbf_6M_template.cbf"
    "${CBF__DATA}/cbf_6M_template.cbf")
set_tests_properties(dectris-setup PROPERTIES
  REQUIRED_FILES "${CBFlib_SOURCE_DIR}/templates/cbf_6M_template.cbf"
  FIXTURES_SETUP dectris-setup)

add_test(NAME dectris-setup-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/cbf_6M_template.cbf")
set_tests_properties(dectris-setup-cleanup PROPERTIES
  FIXTURES_CLEANUP dectris-setup)

add_test(NAME dectris
  COMMAND cbf_template_t
  WORKING_DIRECTORY "${CBF__DATA}")
set_tests_properties(dectris PROPERTIES
  FIXTURES_REQUIRED dectris-setup
  FIXTURES_SETUP dectris)

add_test(NAME dectris-cleanup
  COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/cbf_template_t.out")
set_tests_properties(dectris-cleanup PROPERTIES
  FIXTURES_CLEANUP dectris)

add_test(NAME dectris-cmp
  COMMAND ${CMAKE_COMMAND} -E compare_files
    "${CBFlib_SOURCE_DIR}/templates/cbf_test_orig.out"
    "${CBF__DATA}/cbf_template_t.out")
set_tests_properties(dectris-cmp PROPERTIES
  FIXTURES_REQUIRED dectris
  REQUIRED_FILES "${CBFlib_SOURCE_DIR}/templates/cbf_test_orig.out")


#
# Fortran tests
#
# test_fcb_read_image and test_xds_binary expect the name of the input
# file on stdin and write to stdout.  The input file cannot have any
# directory components.
if(CBF_ENABLE_FORTRAN)
  #
  # xds_binary-flat
  add_test(NAME xds_binary-flat
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:test_xds_binary>"
      "-Dinput=testflatin.cbf"
      "-Doutput-file=${CBF__DATA}/test_xds_bin_testflatout.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake"
    WORKING_DIRECTORY "${data_input}")
  set_tests_properties(xds_binary-flat PROPERTIES
    FIXTURES_SETUP xds_binary-flat
    REQUIRED_FILES "${data_input}/testflatin.cbf")

  add_test(NAME xds_binary-flat-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/test_xds_bin_testflatout.out")
  set_tests_properties(xds_binary-flat-cleanup PROPERTIES
    FIXTURES_CLEANUP xds_binary-flat)

  add_test(NAME xds_binary-flat-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/test_xds_bin_testflatout_orig.out"
      "${CBF__DATA}/test_xds_bin_testflatout.out")
  set_tests_properties(xds_binary-flat-cmp PROPERTIES
    FIXTURES_REQUIRED xds_binary-flat
    REQUIRED_FILES "${data_output}/test_xds_bin_testflatout_orig.out")


  #
  # xds_binary-flatpacked
  add_test(NAME xds_binary-flatpacked
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:test_xds_binary>"
      "-Dinput=testflatpackedin.cbf"
      "-Doutput-file=${CBF__DATA}/test_xds_bin_testflatpackedout.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake"
    WORKING_DIRECTORY "${data_input}")
  set_tests_properties(xds_binary-flatpacked PROPERTIES
    FIXTURES_SETUP xds_binary-flatpacked
    REQUIRED_FILES "${data_input}/testflatpackedin.cbf")

  add_test(NAME xds_binary-flatpacked-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/test_xds_bin_testflatpackedout.out")
  set_tests_properties(xds_binary-flatpacked-cleanup PROPERTIES
    FIXTURES_CLEANUP xds_binary-flatpacked)

  add_test(NAME xds_binary-flatpacked-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/test_xds_bin_testflatpackedout_orig.out"
      "${CBF__DATA}/test_xds_bin_testflatpackedout.out")
  set_tests_properties(xds_binary-flatpacked-cmp PROPERTIES
    FIXTURES_REQUIRED xds_binary-flatpacked
    REQUIRED_FILES "${data_output}/test_xds_bin_testflatpackedout_orig.out")


  #
  # fcb_read_image-flat
  add_test(NAME fcb_read_image-flat
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:test_fcb_read_image>"
      "-Dinput=testflatin.cbf"
      "-Doutput-file=${CBF__DATA}/test_fcb_read_testflatout.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake"
    WORKING_DIRECTORY "${data_input}")
  set_tests_properties(fcb_read_image-flat PROPERTIES
    FIXTURES_SETUP fcb_read_image-flat
    REQUIRED_FILES "${data_input}/testflatin.cbf")

  add_test(NAME fcb_read_image-flat-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/test_fcb_read_testflatout.out")
  set_tests_properties(fcb_read_image-flat-cleanup PROPERTIES
    FIXTURES_CLEANUP fcb_read_image-flat)

  add_test(NAME fcb_read_image-flat-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/test_fcb_read_testflatout_orig.out"
      "${CBF__DATA}/test_fcb_read_testflatout.out")
  set_tests_properties(fcb_read_image-flat-cmp PROPERTIES
    FIXTURES_REQUIRED fcb_read_image-flat
    REQUIRED_FILES "${data_output}/test_fcb_read_testflatout_orig.out")


  #
  # fcb_read_image-flatpacked
  add_test(NAME fcb_read_image-flatpacked
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:test_fcb_read_image>"
      "-Dinput=testflatpackedin.cbf"
      "-Doutput-file=${CBF__DATA}/test_fcb_read_testflatpackedout.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake"
    WORKING_DIRECTORY "${data_input}")
  set_tests_properties(fcb_read_image-flatpacked PROPERTIES
    FIXTURES_SETUP fcb_read_image-flatpacked
    REQUIRED_FILES "${data_input}/testflatpackedin.cbf")

  add_test(NAME fcb_read_image-flatpacked-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/test_fcb_read_testflatpackedout.out")
  set_tests_properties(fcb_read_image-flatpacked-cleanup PROPERTIES
    FIXTURES_CLEANUP fcb_read_image-flatpacked)

  add_test(NAME fcb_read_image-flatpacked-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/test_fcb_read_testflatpackedout_orig.out"
      "${CBF__DATA}/test_fcb_read_testflatpackedout.out")
  set_tests_properties(fcb_read_image-flatpacked-cmp PROPERTIES
    FIXTURES_REQUIRED fcb_read_image-flatpacked
    REQUIRED_FILES "${data_output}/test_fcb_read_testflatpackedout_orig.out")
endif()


#
# Java tests
if(BUILD_SHARED_LIBS AND CBF_ENABLE_JAVA)
  add_test(NAME java-setup-c
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:ctestcbf>;${CMAKE_CURRENT_SOURCE_DIR}/examples/template_pilatus6m_2463x2527.cbf"
      "-Doutput-file=${CBF__DATA}/testcbfc.txt"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake")
  set_tests_properties(java-setup-c PROPERTIES
    FIXTURES_SETUP java-setup-c
    REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/examples/template_pilatus6m_2463x2527.cbf")

  add_test(NAME java-cleanup-c
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testcbfc.txt")
  set_tests_properties(java-cleanup-c PROPERTIES
    FIXTURES_CLEANUP java-setup-c)


  # Tweak PATH to find CBFlib: this appears to be necessary on
  # Windows, where java.library.path may not be honored for dependent
  # libraries.  Linux still needs java.library_path.
  #
  # CMake 3.22 has path_list_append() for manipulating path lists;
  # could use $<JOIN:${class_path},${glue}> with that.
  set(class_path
    "$<TARGET_PROPERTY:cbflib,JAR_FILE>"
    "$<TARGET_PROPERTY:testcbf,JAR_FILE>")

  set(library_path
    "$<TARGET_FILE_DIR:cbf_wrap>"
    "$<TARGET_FILE_DIR:cbf>"
    "$ENV{PATH}")

  if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set(glue "\;")
  else()
    set(glue ":")
  endif()

  add_test(NAME java-setup-java
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=${Java_JAVA_EXECUTABLE};-Djava.library.path=$<TARGET_FILE_DIR:cbf_wrap>;-cp;$<JOIN:${class_path},${glue}>;testcbf;${CMAKE_CURRENT_SOURCE_DIR}/examples/template_pilatus6m_2463x2527.cbf"
      "-Doutput-file=${CBF__DATA}/testcbfj.txt"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake")
  set_tests_properties(java-setup-java PROPERTIES
    ENVIRONMENT "PATH=$<JOIN:${library_path},${glue}>"
    FIXTURES_SETUP java-setup-java
    REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/examples/template_pilatus6m_2463x2527.cbf")

  add_test(NAME java-cleanup-java
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/testcbfj.txt")
  set_tests_properties(java-cleanup-java PROPERTIES
    FIXTURES_CLEANUP java-setup-java)

  add_test(NAME java-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files
      "${CBF__DATA}/testcbfc.txt"
      "${CBF__DATA}/testcbfj.txt")
  set_tests_properties(java-cmp PROPERTIES
    FIXTURES_REQUIRED "java-setup-c;java-setup-java")
endif()


#
# Python tests
if(BUILD_SHARED_LIBS AND CBF_ENABLE_PYTHON)
  add_test(NAME pycbf-test1
    COMMAND Python::Interpreter
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_test1.py"
      "${CBF__DATA}/img2cif_packed.cif"
      "${CBF__DATA}/pycbf_test1.raw")
  set_tests_properties(pycbf-test1 PROPERTIES
    ENVIRONMENT "PYTHONPATH=$<TARGET_FILE_DIR:pycbf>"
    FIXTURES_REQUIRED "img2cif-flatpacked-cif"
    FIXTURES_SETUP pycbf-test1
    REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_test1.py")

  add_test(NAME pycbf-test1-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/pycbf_test1.raw")
  set_tests_properties(pycbf-test1-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-test1)

  add_test(NAME pycbf-test1-standardize-numbers
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:cbf_standardize_numbers>;-;4"
      "-Dinput-file=${CBF__DATA}/pycbf_test1.raw"
      "-Doutput-file=${CBF__DATA}/pycbf_test1.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake")
  set_tests_properties(pycbf-test1-standardize-numbers PROPERTIES
    FIXTURES_REQUIRED pycbf-test1
    FIXTURES_SETUP pycbf-test1-standardize-numbers)

  add_test(NAME pycbf-test1-standardize-numbers-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/pycbf_test1.out")
  set_tests_properties(pycbf-test1-standardize-numbers-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-test1-standardize-numbers)

  add_test(NAME pycbf-test1-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/pycbf_test1_orig.out"
      "${CBF__DATA}/pycbf_test1.out")
  set_tests_properties(pycbf-test1-cmp PROPERTIES
    FIXTURES_REQUIRED pycbf-test1-standardize-numbers
    REQUIRED_FILES "${data_output}/pycbf_test1_orig.out")


  add_test(NAME pycbf-test2
    COMMAND Python::Interpreter
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_test2.py"
      "${CBF__DATA}/adscconverted.cbf"
      "${CBF__DATA}/pycbf_test2.raw")
  set_tests_properties(pycbf-test2 PROPERTIES
    ENVIRONMENT "PYTHONPATH=$<TARGET_FILE_DIR:pycbf>"
    FIXTURES_REQUIRED "convert_image-adscimg"
    FIXTURES_SETUP pycbf-test2
    REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_test2.py")

  add_test(NAME pycbf-test2-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/pycbf_test2.raw")
  set_tests_properties(pycbf-test2-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-test2)

  add_test(NAME pycbf-test2-standardize-numbers
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:cbf_standardize_numbers>;-;4"
      "-Dinput-file=${CBF__DATA}/pycbf_test2.raw"
      "-Doutput-file=${CBF__DATA}/pycbf_test2.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake")
  set_tests_properties(pycbf-test2-standardize-numbers PROPERTIES
    FIXTURES_REQUIRED pycbf-test2
    FIXTURES_SETUP pycbf-test2-standardize-numbers)

  add_test(NAME pycbf-test2-standardize-numbers-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/pycbf_test2.out")
  set_tests_properties(pycbf-test2-standardize-numbers-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-test2-standardize-numbers)

  add_test(NAME pycbf-test2-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/pycbf_test2_orig.out"
      "${CBF__DATA}/pycbf_test2.out")
  set_tests_properties(pycbf-test2-cmp PROPERTIES
    FIXTURES_REQUIRED pycbf-test2-standardize-numbers
    REQUIRED_FILES "${data_output}/pycbf_test2_orig.out")


  add_test(NAME pycbf-test3
    COMMAND Python::Interpreter
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_test3.py")
  set_tests_properties(pycbf-test3 PROPERTIES
    ENVIRONMENT "PYTHONPATH=$<TARGET_FILE_DIR:pycbf>"
    REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_test3.py")


  add_test(NAME pycbf-test4
    COMMAND Python::Interpreter
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_test4.py"
      "${CBF__DATA}/img2cif_packed.cif"
      "${CBF__DATA}/pycbf_test4.raw"
      "${CBF__DATA}/newtest1.cbf")
  set_tests_properties(pycbf-test4 PROPERTIES
    ENVIRONMENT "PYTHONPATH=$<TARGET_FILE_DIR:pycbf>"
    FIXTURES_REQUIRED "img2cif-flatpacked-cif"
    FIXTURES_SETUP pycbf-test4
    REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_test4.py")

  add_test(NAME pycbf-test4-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm
      "${CBF__DATA}/pycbf_test4.raw"
      "${CBF__DATA}/newtest1.cbf")
  set_tests_properties(pycbf-test4-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-test4)

  add_test(NAME pycbf-test4-standardize-numbers
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:cbf_standardize_numbers>;-;4"
      "-Dinput-file=${CBF__DATA}/pycbf_test4.raw"
      "-Doutput-file=${CBF__DATA}/pycbf_test4.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake")
  set_tests_properties(pycbf-test4-standardize-numbers PROPERTIES
    FIXTURES_REQUIRED pycbf-test4
    FIXTURES_SETUP pycbf-test4-standardize-numbers)

  add_test(NAME pycbf-test4-standardize-numbers-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/pycbf_test4.out")
  set_tests_properties(pycbf-test4-standardize-numbers-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-test4-standardize-numbers)

  add_test(NAME pycbf-test4-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/pycbf_test4_orig.out"
      "${CBF__DATA}/pycbf_test4.out")
  set_tests_properties(pycbf-test4-cmp PROPERTIES
    FIXTURES_REQUIRED pycbf-test4-standardize-numbers
    REQUIRED_FILES "${data_output}/pycbf_test4_orig.out")


  add_test(NAME pycbf-fel1
    COMMAND Python::Interpreter
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_testfelaxes.py"
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/fel_test1.cbf"
      "${CBF__DATA}/fel_test1.raw")
  set_tests_properties(pycbf-fel1 PROPERTIES
    ENVIRONMENT "PYTHONPATH=$<TARGET_FILE_DIR:pycbf>"
    FIXTURES_SETUP pycbf-fel1
    REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/fel_test1.cbf")

  add_test(NAME pycbf-fel1-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/fel_test1.raw")
  set_tests_properties(pycbf-fel1-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-fel1)

  add_test(NAME pycbf-fel1-standardize-numbers
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:cbf_standardize_numbers>;-;4"
      "-Dinput-file=${CBF__DATA}/fel_test1.raw"
      "-Doutput-file=${CBF__DATA}/fel_test1.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake")
  set_tests_properties(pycbf-fel1-standardize-numbers PROPERTIES
    FIXTURES_REQUIRED pycbf-fel1
    FIXTURES_SETUP pycbf-fel1-standardize-numbers)

  add_test(NAME pycbf-fel1-standardize-numbers-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/fel_test1.out")
  set_tests_properties(pycbf-fel1-standardize-numbers-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-fel1-standardize-numbers)

  add_test(NAME pycbf-fel1-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/fel_test1_orig.out"
      "${CBF__DATA}/fel_test1.out")
  set_tests_properties(pycbf-fel1-cmp PROPERTIES
    FIXTURES_REQUIRED pycbf-fel1-standardize-numbers
    REQUIRED_FILES "${data_output}/fel_test1_orig.out")


  add_test(NAME pycbf-fel2
    COMMAND Python::Interpreter
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_testfelaxes.py"
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/fel_test2.cbf"
      "${CBF__DATA}/fel_test2.raw")
  set_tests_properties(pycbf-fel2 PROPERTIES
    ENVIRONMENT "PYTHONPATH=$<TARGET_FILE_DIR:pycbf>"
    FIXTURES_SETUP pycbf-fel2
    REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/fel_test2.cbf")

  add_test(NAME pycbf-fel2-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/fel_test2.raw")
  set_tests_properties(pycbf-fel2-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-fel2)

  add_test(NAME pycbf-fel2-standardize-numbers
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:cbf_standardize_numbers>;-;4"
      "-Dinput-file=${CBF__DATA}/fel_test2.raw"
      "-Doutput-file=${CBF__DATA}/fel_test2.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake")
  set_tests_properties(pycbf-fel2-standardize-numbers PROPERTIES
    FIXTURES_REQUIRED pycbf-fel2
    FIXTURES_SETUP pycbf-fel2-standardize-numbers)

  add_test(NAME pycbf-fel2-standardize-numbers-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/fel_test2.out")
  set_tests_properties(pycbf-fel2-standardize-numbers-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-fel2-standardize-numbers)

  add_test(NAME pycbf-fel2-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/fel_test2_orig.out"
      "${CBF__DATA}/fel_test2.out")
  set_tests_properties(pycbf-fel2-cmp PROPERTIES
    FIXTURES_REQUIRED pycbf-fel2-standardize-numbers
    REQUIRED_FILES "${data_output}/fel_test2_orig.out")


  add_test(NAME pycbf-fel3
    COMMAND Python::Interpreter
      "${CMAKE_CURRENT_SOURCE_DIR}/pycbf/pycbf_testfelaxes.py"
      "${data_input}/hit-20140306005258847.cbf"
      "${CBF__DATA}/fel_test3.raw")
  set_tests_properties(pycbf-fel3 PROPERTIES
    ENVIRONMENT "PYTHONPATH=$<TARGET_FILE_DIR:pycbf>"
    FIXTURES_SETUP pycbf-fel3
    REQUIRED_FILES "${data_input}/hit-20140306005258847.cbf")

  add_test(NAME pycbf-fel3-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/fel_test3.raw")
  set_tests_properties(pycbf-fel3-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-fel3)

  add_test(NAME pycbf-fel3-standardize-numbers
    COMMAND ${CMAKE_COMMAND}
      "-Dcommand=$<TARGET_FILE:cbf_standardize_numbers>;-;4"
      "-Dinput-file=${CBF__DATA}/fel_test3.raw"
      "-Doutput-file=${CBF__DATA}/fel_test3.out"
      -P "${CMAKE_CURRENT_SOURCE_DIR}/redirect.cmake")
  set_tests_properties(pycbf-fel3-standardize-numbers PROPERTIES
    FIXTURES_REQUIRED pycbf-fel3
    FIXTURES_SETUP pycbf-fel3-standardize-numbers)

  add_test(NAME pycbf-fel3-standardize-numbers-cleanup
    COMMAND ${CMAKE_COMMAND} -E rm "${CBF__DATA}/fel_test3.out")
  set_tests_properties(pycbf-fel3-standardize-numbers-cleanup PROPERTIES
    FIXTURES_CLEANUP pycbf-fel3-standardize-numbers)

  add_test(NAME pycbf-fel3-cmp
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol
      "${data_output}/fel_test3_orig.out"
      "${CBF__DATA}/fel_test3.out")
  set_tests_properties(pycbf-fel3-cmp PROPERTIES
    FIXTURES_REQUIRED pycbf-fel3-standardize-numbers
    REQUIRED_FILES "${data_output}/fel_test3_orig.out")
endif()
