
######################################################################
#  CMakeLists.txt - cmake build file for make to create CBFlib       #
#                                                                    #
# Version 0.9.5 27 April 2014                                        #
#                                                                    #
#                          Paul Ellis and                            #
#         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        #
#                                                                    #
# (C) Copyright 2006 - 2011 Herbert J. Bernstein                     #
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
#      data_files             Directory for test files               #
#      bin                    Directory for executable programs      #
#      include                Directory with build-created headers   #
#      src                    Directory with build-created source    #
#                                                                    #
######################################################################


cmake_minimum_required (VERSION 2.8.7)

set(CBF_USE_FORTRAN_ENV $ENV{CBF_USE_FORTRAN})
set(CBF_F90FLAGS_ENV $ENV{F90FLAGS})
set(CBF_USE_ULP $ENV{CBF_USE_ULP})
set(CBF_M4FLAGS_ENV $ENV{M4FLAGS})
set(CBF_HDF5REGISTER_MANUAL_ENV $ENV{CBF_HDF5REGISTER_MANUAL})

if (CBF_USE_FORTRAN_ENV STREQUAL "NO")
  project (CBFlib C CXX)
  set(CBF_HDF5_ENABLE_FORTRAN "no")
else (CBF_USE_FORTRAN_ENV STREQUAL "NO")
  project (CBFlib C CXX Fortran)
  set(CBF_HDF5_ENABLE_FORTRAN "yes")
endif (CBF_USE_FORTRAN_ENV STREQUAL "NO")
enable_testing()

set (CBF_CMAKE_DEBUG "ON")

if (CBF_USE_ULP STREQUAL "YES")
  set(CMAKE_C_FLAGS "-Wall -g -fno-strict-aliasing -DCBF_USE_ULP")
else (CBF_USE_ULP STREQUAL "YES")
  set(CMAKE_C_FLAGS "-Wall -g -fno-strict-aliasing")
endif (CBF_USE_ULP STREQUAL "YES")

#
#  User setable parameters
#

if (CBF_M4FLAGS_ENV)
  set(CBF_M4FLAGS ${CBF_M4FLAGS_ENV})
else (CBF_M4FLAGS_ENV)
  set(CBF_M4FLAGS "-Dfcb_bytes_in_rec=4096")
endif (CBF_M4FLAGS_ENV)

if (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))

if (CBF_F90FLAGS_ENV)
  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${CBF_F90FLAGS_ENV}")
  set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG}   ${CBF_F90FLAGS_ENV}")
else (CBF_F90FLAGS_ENV)
  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -fno-range-check")
  set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG}   -fno-range-check")
endif (CBF_F90FLAGS_ENV)

endif (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

#
#  Favor static libraries
#
IF(WIN32)
    SET(CMAKE_FIND_LIBRARY_SUFFIXES .lib .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
ELSE(WIN32)
    SET(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
ENDIF(WIN32)


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
# CBF_LOAD_TARBALL -- load TARBALL into WORK_DIR as UNPACKED_DIRECTORY
#                     Use an absolute path for UNPACKED_DIRECTORY
#
macro(CBF_LOAD_TARBALL WORK_DIR UNPACKED_DIRECTORY TARBALL)
if (NOT EXISTS ${UNPACKED_DIRECTORY})
execute_process(
  COMMAND ${WGET_EXECUTABLE} ${TARBALL}
  WORKING_DIRECTORY ${WORK_DIR}
)
execute_process(
  COMMAND ${CMAKE_COMMAND} -E tar xzvf ${UNPACKED_DIRECTORY}.tar.gz
  WORKING_DIRECTORY ${WORK_DIR}
)
execute_process(
  COMMAND ${CMAKE_COMMAND} -E remove ${UNPACKED_DIRECTORY}.tar.gz
  WORKING_DIRECTORY ${WORK_DIR}
)
endif (NOT EXISTS ${UNPACKED_DIRECTORY})
endmacro(CBF_LOAD_TARBALL)


#
# Directories
#

#
# Directories on the kit side
#
set(CBF__SRC       "${CBFlib_SOURCE_DIR}/src" )
set(CBF__INCLUDE   "${CBFlib_SOURCE_DIR}/include" )
set(CBF__M4        "${CBFlib_SOURCE_DIR}/m4" )
set(CBF__DOC       "${CBFlib_SOURCE_DIR}/doc" )
set(CBF__EXAMPLES  "${CBFlib_SOURCE_DIR}/examples" )
set(CBF__EXTERNAL_PACKAGES 
                   "${CBFlib_SOURCE_DIR}/external_packages" )
set(CBF__DECTRIS_EXAMPLES
                   "${CBF__EXAMPLES}/dectris_cbf_template_test" )


#
# Directories on the build side
#
set(CBF__BLDSRC    "${CBFlib_BINARY_DIR}/src" )
set(CBF__BLDEXMP   "${CBFlib_BINARY_DIR}/src" )
set(CBF__BIN       "${CBFlib_BINARY_DIR}/bin" )
set(CBF__LIB       "${CBFlib_BINARY_DIR}/lib" )
set(CBF__BIN_INCLUDE "${CBFlib_BINARY_DIR}/include" )
set(CBF__SHARE     "${CBFlib_BINARY_DIR}/share" )
set(CBF__EXT_PKG   "${CBFlib_BINARY_DIR}/external_packages" )
set(CBF__DATA      "${CBFlib_BINARY_DIR}/data_files" )

set(HDF5_PLUGIN_PATH ${CBF__LIB})


CBF_REQUIRE_DIRECTORY(${CBF__BLDSRC})
CBF_REQUIRE_DIRECTORY(${CBF__BLDEXMP})
CBF_REQUIRE_DIRECTORY(${CBF__BIN})
CBF_REQUIRE_DIRECTORY(${CBF__LIB})
CBF_REQUIRE_DIRECTORY(${CBF__BIN_INCLUDE})
CBF_REQUIRE_DIRECTORY(${CBF__SHARE})
CBF_REQUIRE_DIRECTORY(${CBF__EXT_PKG})
CBF_REQUIRE_DIRECTORY(${CBF__DATA})

# Version string and API version string -- obtained from cbf.h

# Extract the version components

file (READ ${CBFlib_SOURCE_DIR}/include/cbf.h CBFlib_cbf_h_contents)
string (REGEX REPLACE ".*#define[ \t]+CBF_VERS_MAJOR[ \t]+([0-9]*).*$"
    "\\1" CBF_VERS_MAJOR ${CBFlib_cbf_h_contents})
CBF_DEBUG_MESSAGE( "MAJOR VERSION: ${CBF_VERS_MAJOR}")
string (REGEX REPLACE ".*#define[ \t]+CBF_VERS_MINOR[ \t]+([0-9]*).*$"
    "\\1" CBF_VERS_MINOR ${CBFlib_cbf_h_contents})
CBF_DEBUG_MESSAGE( "MINOR VERSION: ${CBF_VERS_MINOR}")
string (REGEX REPLACE ".*#define[ \t]+CBF_VERS_RELEASE[ \t]+([0-9.]*).*$"
    "\\1" CBF_VERS_RELEASE ${CBFlib_cbf_h_contents})
CBF_DEBUG_MESSAGE( "RELEASE: ${CBF_VERS_RELEASE}")
string (REGEX REPLACE ".*#define[ \t]+CBF_VERS_SUBRELEASE[ \t]+\"([0-9A-Za-z.]*)\".*$"
    "\\1" CBF_VERS_SUBRELEASE ${CBFlib_cbf_h_contents})
CBF_DEBUG_MESSAGE( "SUBRELEASE: ${CBF_VERS_SUBRELEASE}")

# Extract the API version components

string (REGEX REPLACE ".*#define[ \t]+CBF_APIVERS_CUR[ \t]+([0-9]*).*$"
    "\\1" CBF_APIVERS_CUR ${CBFlib_cbf_h_contents})
CBF_DEBUG_MESSAGE( "API VERSION: ${CBF_APIVERS_CUR}")
string (REGEX REPLACE ".*#define[ \t]+CBF_APIVERS_REV[ \t]+([0-9]*).*$"
    "\\1" CBF_APIVERS_REV ${CBFlib_cbf_h_contents})
CBF_DEBUG_MESSAGE( "API REVISION: ${CBF_APIVERS_REV}")
string (REGEX REPLACE ".*#define[ \t]+CBF_APIVERS_AGE[ \t]+([0-9]*).*$"
    "\\1" CBF_APIVERS_AGE ${CBFlib_cbf_h_contents})
CBF_DEBUG_MESSAGE( "API AGE: ${CBF_APIVERS_AGE}")


if(CBF_VERS_SUBRELEASE)
  set (CBF_VERSION "${CBF_VERS_MAJOR}.${CBF_VERS_MINOR}.${CBF_VERS_RELEASE}-${CBF_VERS_SUBRELEASE}")
else(CBF_VERS_SUBRELEASE)
  set (CBF_VERSION "${CBF_VERS_MAJOR}.${CBF_VERS_MINOR}.${CBF_VERS_RELEASE}")
endif(CBF_VERS_SUBRELEASE)
if(CBF_CMAKE_DEBUG)
  message (STATUS "VERSION:  ${CBF_VERSION}")
endif(CBF_CMAKE_DEBUG)

set (CBF_APIVERSION "${CBF_APIVERS_CUR}.${CBF_APIVERS_REV}.${CBF_APIVERS_AGE}")
CBF_DEBUG_MESSAGE( "APIVERSION:  ${CBF_APIVERSION}")




set(JCBF      "${CBFlib_SOURCE_DIR}/jcbf" CACHE STRING "")
set(JAVADIR   "${CBFlib_SOURCE_DIR}/java" CACHE STRING "")
set(BIN       "${CBFlib_BINARY_DIR}/bin" CACHE STRING "")
set(PYCBF     "${CBFlib_SOURCE_DIR}/pycbf" CACHE STRING "")
set(EXAMPLES  "${CBFlib_SOURCE_DIR}/examples" CACHE STRING "" )
set(DECTRIS_EXAMPLES "${EXAMPLES}/dectris_cbf_template_test" CACHE STRING "")
set(MINICBF_TEST "${CBFlib_SOURCE_DIR}/minicbf_test" CACHE STRING "")
set(GRAPHICS  "${CBFlib_SOURCE_DIR}/html_graphics" CACHE STRING "")


#
# Definition to get a version of tifflib to support tiff2cbf
#
set(CBF_TIFF        "tiff-4.0.3-rev-29Sep13")
set(TIFF_INSTALL_DIR "${CMAKE_BINARY_DIR}/${CBF_TIFF}")

#
# Definitions to get a version of HDF5
#
set(CBF_HDF5        "hdf5-1.8.11_rev_20Sep13")
set(CBF_HDF5URL  "http://downloads.sf.net/cbflib/${CBF_HDF5}.tar.gz")
set(CBF_HDF5REGISTER_ARG "--register")
if (CBF_HDF5REGISTER_MANUAL_ENV STREQUAL "YES")
  set(CBF_HDF5REGISTER_VAL "manual")
else (CBF_HDF5REGISTER_MANUAL_ENV STREQUAL "YES")
  set(CBF_HDF5REGISTER_VAL "plugin")
endif (CBF_HDF5REGISTER_MANUAL_ENV STREQUAL "YES")  
set(HDF5_EXTERNALLY_CONFIGURED  1)
set(HDF5_INSTALL_DIR "${CMAKE_BINARY_DIR}/${CBF_HDF5}")
set(HDF5_BINARY_DIR "${CBF_HDF5}/bin")

if(CBF_DONT_USE_LONG_LONG)
  set(CBF_NOLLFLAG "-DCBF_DONT_USE_LONG_LONG")
else(CBF_DONT_USE_LONG_LONG)
  set(CBF_NOLLFLAG "")
endif(CBF_DONT_USE_LONG_LONG)

#
# URLs from which to retrieve needed external package snapshots
#
# We give two REGEX packages, the old 1993 regex release for
#    compatability with older CBFlib release Makefiles
#    and a recent PCRE for use in the future.  Only the PCRE
#    version is used for cmake builds
#
set(CBF_REGEX    "regex-20090805")
set(CBF_PCRE     "pcre-8.33")
set(CBF_REGEXURL "http://downloads.sf.net/cbflib/${CBF_REGEX}.tar.gz")
set(CBF_PCREURL  "http://downloads.sf.net/cbflib/${CBF_PCRE}.tar.gz")
set(CBF_TIFFURL  "http://downloads.sf.net/cbflib/${CBF_TIFF}.tar.gz")
set(CBF_HDF5URL  "http://downloads.sf.net/cbflib/${CBF_HDF5}.tar.gz")


#
# Get wget
#
find_program(WGET_EXECUTABLE wget)
CBF_DEBUG_MESSAGE( "WGET found at ${WGET_EXECUTABLE}")

#
#  ZLIB
#
find_package(ZLIB)

include(ExternalProject)

ExternalProject_Add(
	${CBF_TIFF}
	URL "${CBF_TIFFURL}"
	SOURCE_DIR ${CMAKE_BINARY_DIR}/external_packages/${CBF_TIFF}
	CMAKE_ARGS
		-DCMAKE_BUILD_TYPE:STRING=RELEASE DEBUG
		-DCMAKE_INSTALL_PREFIX:PATH=${TIFF_INSTALL_DIR}
		-DCMAKE_INSTALL_RPATH:PATH=${TIFF_INSTALL_DIR}/lib
	INSTALL_DIR ${CMAKE_BINARY_DIR}/${CBF_TIFF}
)

set(TIFF_LIBRARY_PATH ${TIFF_INSTALL_DIR}/lib/libtiff${CMAKE_SHARED_LIBRARY_SUFFIX})

ExternalProject_add(
	${CBF_HDF5}
	URL "${CBF_HDF5URL}"
	SOURCE_DIR ${CMAKE_BINARY_DIR}/external_packages/${CBF_HDF5}
    CMAKE_ARGS
		-DCMAKE_BUILD_TYPE:STRING=RELEASE DEBUG
		-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=On
		-DHDF5_BUILD_CPP_LIB:BOOL=Off
		-DBUILD_SHARED_LIBS:BOOL=On
		-DHDF5_BUILD_TOOLS:BOOL=On
        -DHDF5_ENABLE_FORTRAN=${CBF_HDF5_ENABLE_FORTRAN}
		-DCMAKE_INSTALL_PREFIX:PATH=${HDF5_INSTALL_DIR}
		-DCMAKE_INSTALL_RPATH:PATH=${HDF5_INSTALL_DIR}/lib
	INSTALL_DIR ${CMAKE_BINARY_DIR}/${CBF_HDF5}
)

set(HDF5_LIBRARY_PATH ${HDF5_INSTALL_DIR}/lib/libhdf5${CMAKE_SHARED_LIBRARY_SUFFIX})

#
# CBF_REGEX or CBF_PCRE
#
# Need to get:
# - an include directory
# - a library to link against
# - an (optional) dependency to build
#
# Try for PCRE first
find_library(CBF_REGEXLIB pcre)
find_path(CBF_REGEXLIB_INCLUDE_DIR NAMES pcreposix.h)
message(STATUS "CBF_REGEXLIB_INCLUDE_DIR for pcreposix.h = '${CBF_REGEXLIB_INCLUDE_DIR}'")
message(STATUS "CBF_REGEXLIB = '${CBF_REGEXLIB}'")
if (NOT CBF_REGEXLIB_INCLUDE_DIR OR NOT CBF_REGEXLIB)
	# PCRE not found, try for REGEX
	message(STATUS "'pcre' library not found")
	find_library(CBF_REGEXLIB regex)
	find_path(CBF_REGEXLIB_INCLUDE_DIR NAMES regex.h)
	message(STATUS "CBF_REGEXLIB_INCLUDE_DIR for regex.h = '${CBF_REGEXLIB_INCLUDE_DIR}'")
	message(STATUS "CBF_REGEXLIB = '${CBF_REGEXLIB}'")
	if (NOT CBF_REGEXLIB_INCLUDE_DIR OR NOT CBF_REGEXLIB)
		# Neither PCRE not REGEX found, load and build PCRE
		message(STATUS "'regex' library not found")
		ExternalProject_add(
			PCRE
			URL ${CBF_PCREURL}
			SOURCE_DIR ${CMAKE_BINARY_DIR}/external_packages/${CBF_PCRE}
			CMAKE_ARGS
				-DCMAKE_BUILD_TYPE:STRING=RELEASE DEBUG
				-DCMAKE_INSTALL_PREFIX:PATH=${CBFlib_BINARY_DIR}/${CBF_PCRE}
			INSTALL_DIR ${CBFlib_BINARY_DIR}/${CBF_PCRE}
		)
		set(CBF_RE "PCRE")
		set(CBF_REGEXLIB_INCLUDE_DIR ${CMAKE_BINARY_DIR}/${CBF_PCRE}/include)
		set(CBF_REGEXLIB_LIBRARY_PATH ${CBFlib_BINARY_DIR}/${CBF_PCRE}/lib/libpcre.a)
		add_definitions(-DCBF_REGEXLIB_PCRE)
	else ()
		set(CBF_RE "")
		set(CBF_REGEXLIB_LIBRARY_PATH "${CBF_REGEXLIB}/libregex.so")
		add_definitions(-DCBF_REGEXLIB_REGEX)
	endif ()
else ()
	set(CBF_RE "")
		set(CBF_REGEXLIB_LIBRARY_PATH "${CBF_REGEXLIB}/libpcre.so")
	add_definitions(-DCBF_REGEXLIB_PCRE)
endif ()

#
# Data Directories
#
set(CBF_DATADIRI  "${CBF__DATA}/CBFlib_${CBF_VERSION}_Data_Files_Input" )
set(CBF_DATADIRO  "${CBF__DATA}/CBFlib_${CBF_VERSION}_Data_Files_Output" )
set(CBF_DATADIRS  "${CBF__DATA}/CBFlib_${CBF_VERSION}_Data_Files_Output_Sigs_Only")
#
# URLs from which to retrieve the data directories
#
set(CBF_DATAURLBASE	"http://downloads.sf.net/cbflib")
set(CBF_DATAURLI  "${CBF_DATAURLBASE}/CBFlib_${CBF_VERSION}_Data_Files_Input.tar.gz")
set(CBF_DATAURLO  "${CBF_DATAURLBASE}/CBFlib_${CBF_VERSION}_Data_Files_Output.tar.gz")
set(CBF_DATAURLS  "${CBF_DATAURLBASE}/CBFlib_${CBF_VERSION}_Data_Files_Output_Sigs_Only.tar.gz")
#
# Load and unpack the Data Files
#
CBF_LOAD_TARBALL(${CBF__DATA} ${CBF_DATADIRI} ${CBF_DATAURLI})
CBF_LOAD_TARBALL(${CBF__DATA} ${CBF_DATADIRO} ${CBF_DATAURLO})
CBF_LOAD_TARBALL(${CBF__DATA} ${CBF_DATADIRS} ${CBF_DATAURLS})
#
# Verify the checksums
#
file(GLOB CBF_DATADIRI_FILES "${CBF_DATADIRI}/*")
file(GLOB CBF_DATADIRO_FILES "${CBF_DATADIRO}/*")
foreach(loop_file ${CBF_DATADIRI_FILES} )
  if (NOT "${loop_file}" MATCHES "[*.]md5")
    file(MD5 "${loop_file}" loop_file_md5)
    file(STRINGS "${loop_file}.md5" loop_file_md5_orig LIMIT_COUNT 1)
    if (NOT ("${loop_file_md5}" STREQUAL "${loop_file_md5_orig}"))
      message(WARNINH "loop_file: ${loop_file}:|${loop_file_md5}|${loop_file_md5_orig}|")
    endif (NOT ("${loop_file_md5}" STREQUAL "${loop_file_md5_orig}"))
  endif (NOT "${loop_file}" MATCHES "[*.]md5")
endforeach(loop_file)
foreach(loop_file ${CBF_DATADIRO_FILES} )
  if (NOT "${loop_file}" MATCHES "[*.]md5")
    file(MD5 "${loop_file}" loop_file_md5)
    file(STRINGS "${loop_file}.md5" loop_file_md5_orig LIMIT_COUNT 1)
    if (NOT ("${loop_file_md5}" STREQUAL "${loop_file_md5_orig}"))
      message(WARNING "loop_file: ${loop_file}:|${loop_file_md5}|${loop_file_md5_orig}|")
    endif (NOT ("${loop_file_md5}" STREQUAL "${loop_file_md5_orig}"))
  endif (NOT "${loop_file}" MATCHES "[*.]md5")
endforeach(loop_file)


#
# Source files
#
set(
	CBF_C_SOURCES
    ${CBF__SRC}/cbf.c
    ${CBF__SRC}/cbf_alloc.c
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
    ${CBF__SRC}/cbf_nibble_offset.c
    ${CBF__SRC}/cbf_packed.c
    ${CBF__SRC}/cbf_predictor.c
    ${CBF__SRC}/cbf_read_binary.c
    ${CBF__SRC}/cbf_read_mime.c
    ${CBF__SRC}/cbf_simple.c
    ${CBF__SRC}/cbf_string.c
    ${CBF__SRC}/cbf_stx.c
    ${CBF__SRC}/cbf_tree.c
    ${CBF__SRC}/cbf_uncompressed.c
    ${CBF__SRC}/cbf_ulp.c
    ${CBF__SRC}/cbf_write.c
    ${CBF__SRC}/cbf_write_binary.c
    ${CBF__SRC}/cbf_ws.c
	${CBF__SRC}/md5c.c
    ${CBF__SRC}/img.c
)

if (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))

set(
	CBF_F90_BUILT_SOURCES 
			${CBF__BLDSRC}/fcb_exit_binary.f90
			${CBF__BLDSRC}/fcb_next_binary.f90
			${CBF__BLDSRC}/fcb_open_cifin.f90
			${CBF__BLDSRC}/fcb_packed.f90
			${CBF__BLDSRC}/fcb_read_bits.f90
			${CBF__BLDSRC}/fcb_read_image.f90
	${CBF__BLDSRC}/fcb_read_xds_i2.f90
)
            
set(
	CBF_F90_SOURCES
	${CBF__SRC}/fcb_atol_wcnt.f90
			${CBF__SRC}/fcb_ci_strncmparr.f90
			${CBF__SRC}/fcb_nblen_array.f90
			${CBF__SRC}/fcb_read_byte.f90
			${CBF__SRC}/fcb_read_line.f90
	${CBF__SRC}/fcb_skip_whitespace.f90
)

endif (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))

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
    ${CBF__INCLUDE}/cbf_nibble_offset.h
    ${CBF__INCLUDE}/cbf_packed.h		
    ${CBF__INCLUDE}/cbf_predictor.h
    ${CBF__INCLUDE}/cbf_read_binary.h
    ${CBF__INCLUDE}/cbf_read_mime.h		
    ${CBF__INCLUDE}/cbf_simple.h		
    ${CBF__INCLUDE}/cbf_string.h		
    ${CBF__INCLUDE}/cbf_stx.h
    ${CBF__INCLUDE}/cbf_tree.h
    ${CBF__INCLUDE}/cbf_uncompressed.h
    ${CBF__INCLUDE}/cbf_ulp.h
    ${CBF__INCLUDE}/cbf_write.h
    ${CBF__INCLUDE}/cbf_write_binary.h
    ${CBF__INCLUDE}/cbf_ws.h
    ${CBF__INCLUDE}/global.h
    ${CBF__INCLUDE}/cbff.h			
	${CBF__INCLUDE}/md5.h
    ${CBF__INCLUDE}/img.h
)


if (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))

#
# m4 FCB library macro files
#
set(
	CBF_M4_FCB_DEFINES
	${CBF__M4}/fcblib_defines.m4
)

set(
	CBF_M4_FCB_FILES 
			${CBF__M4}/fcb_exit_binary.m4
			${CBF__M4}/fcb_next_binary.m4
			${CBF__M4}/fcb_open_cifin.m4
			${CBF__M4}/fcb_packed.m4
			${CBF__M4}/fcb_read_bits.m4
			${CBF__M4}/fcb_read_image.m4
	${CBF__M4}/fcb_read_xds_i2.m4
)
#
# m4 F90 examples macro files
#
set(
	CBF_M4_F90_EXAMPLES
            ${CBF__M4}/test_fcb_read_image.m4
	${CBF__M4}/test_xds_binary.m4
)

endif (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))


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

# Set up the necessary includes

include_directories(
	BEFORE SYSTEM
                      ${CBFlib_SOURCE_DIR}/include 
	${CBFlib_BINARY_DIR}/${CBF_TIFF}/include
	${CBFlib_BINARY_DIR}/${CBF_HDF5}/include
                      ${CBF_REGEXLIB_INCLUDE_DIR}
                      )

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/bin")

#
#  Build the static and shared CBF libraries
#

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
add_library(cbf_static STATIC ${CBF_C_SOURCES})
add_dependencies(cbf_static ${CBF_RE} ${CBF_TIFF} ${CBF_HDF5})
set_target_properties(cbf_static PROPERTIES OUTPUT_NAME "cbf")
set_target_properties(cbf_static PROPERTIES LINKER_LANGUAGE C)
set_target_properties(cbf_static PROPERTIES SOVERSION "${CBF_APIVERSION}")
set(CBF_STATIC_LIBRARY_PATH ${CBFlib_BINARY_DIR}/lib/libcbf.a)


set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
add_library(cbf_shared SHARED  ${CBF_C_SOURCES})
add_dependencies(cbf_shared ${CBF_RE} ${CBF_TIFF} ${CBF_HDF5})
set_target_properties(cbf_shared PROPERTIES OUTPUT_NAME "cbf")
set_target_properties(cbf_shared PROPERTIES LINKER_LANGUAGE C)
set_target_properties(cbf_shared PROPERTIES SOVERSION "${CBF_APIVERSION}")
target_link_libraries(cbf_shared ${HDF5_LIBRARY_PATH})
set(CBF_SHARED_LIBRARY_PATH ${CBFlib_BINARY_DIR}/lib/libcbf.so)

#
# Build the static and shared IMG libraries
#

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
add_library(img_static STATIC ${CBF__SRC}/img.c)
set_target_properties(img_static PROPERTIES OUTPUT_NAME "img")
set_target_properties(img_static PROPERTIES LINKER_LANGUAGE C)
set(IMG_STATIC_LIBRARY_PATH ${CBFlib_BINARY_DIR}/lib/libimg.a)


set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
add_library(img_shared SHARED  ${CBF__SRC}/img.c)
set_target_properties(img_shared PROPERTIES OUTPUT_NAME "img")
set_target_properties(img_shared PROPERTIES LINKER_LANGUAGE C)
target_link_libraries(img_shared ${HDF5_LIBRARY_PATH})
set(IMG_SHARED_LIBRARY_PATH ${CBFlib_BINARY_DIR}/lib/libimg.so)


if (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))

#
# Build the f90 library sources
#
find_program(M4 m4)
foreach(f90src IN LISTS CBF_F90_BUILT_SOURCES)
  get_filename_component(filename "${f90src}" NAME_WE )
  set(f90bldsrc "${CBF__BLDSRC}/${filename}.f90")
  set(f90srcm4 "${CBF__M4}/${filename}.m4")
	add_custom_command(
		OUTPUT "${f90bldsrc}"
    WORKING_DIRECTORY "${CBF__M4}"
    COMMAND ${M4} -P "${CBF_M4FLAGS}" "${f90srcm4}" > "${f90bldsrc}"
    DEPENDS ${CBF_M4_FCB_DEFINES} ${f90srcm4}
		COMMENT "Generating ${f90bldsrc}"
	)
endforeach(f90src)


#
# Build the fcb libraries
#
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
add_library(fcb_static STATIC ${CBF_F90_BUILT_SOURCES};${CBF_F90_SOURCES})
set_target_properties(fcb_static PROPERTIES OUTPUT_NAME "fcb")
set_target_properties(fcb_static PROPERTIES LINKER_LANGUAGE C)
set(FCB_STATIC_LIBRARY_PATH ${CBFlib_BINARY_DIR}/lib/libfcb.a)


set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
add_library(fcb_shared SHARED  ${CBF_F90_BUILT_SOURCES};${CBF_F90_SOURCES})
set_target_properties(fcb_shared PROPERTIES OUTPUT_NAME "fcb")
set_target_properties(fcb_shared PROPERTIES LINKER_LANGUAGE C)
target_link_libraries(fcb_shared ${HDF5_LIBRARY_PATH})
set(FCB_SHARED_LIBRARY_PATH ${CBFlib_BINARY_DIR}/lib/libfcb.so)

endif (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))


#
#  C and C++ examples
#

# Note: to add a target with multiple sources/dependencies/libraries you must pass a list
# separated by semicolons for the appropriate parameter.
# Note: the math library should be linked in by appending ';m' to the library list here
macro(add_target target source dependencies libraries)
	add_executable(${target} ${source})
	add_dependencies(${target} ${dependencies})
	target_link_libraries(${target} ${libraries})
endmacro()

add_target(tiff2cbf "${CBF__EXAMPLES}/tiff2cbf.c" "cbf_static;${CBF_TIFF}" "${CBF_STATIC_LIBRARY_PATH};${TIFF_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(cbf2nexus "${CBF__EXAMPLES}/cbf2nexus.c" "cbf_static;${CBF_HDF5}" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(nexus2cbf "${CBF__EXAMPLES}/nexus2cbf.c" "cbf_static;${CBF_HDF5}" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(minicbf2nexus "${CBF__EXAMPLES}/minicbf2nexus.c" "cbf_static;${CBF_HDF5}" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(adscimg2cbf "${CBF__EXAMPLES}/adscimg2cbf.c;${CBF__EXAMPLES}/adscimg2cbf_sub.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};m")
add_target(cbf2adscimg "${CBF__EXAMPLES}/cbf2adscimg.c;${CBF__EXAMPLES}/cbf2adscimg_sub.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};m")
add_target(convert_image "${CBF__EXAMPLES}/convert_image.c" "cbf_static;img_static" "${CBF_STATIC_LIBRARY_PATH};${IMG_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(convert_minicbf "${CBF__EXAMPLES}/convert_minicbf.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(makecbf "${CBF__EXAMPLES}/makecbf.c" "cbf_static;img_static" "${CBF_STATIC_LIBRARY_PATH};${IMG_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(cbf_tail "${CBF__EXAMPLES}/cbf_tail.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH}")
add_target(changtestcompression "${CBF__EXAMPLES}/changtestcompression.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(img2cif "${CBF__EXAMPLES}/img2cif.c" "cbf_static;img_static" "${CBF_STATIC_LIBRARY_PATH};${IMG_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(cif2cbf "${CBF__EXAMPLES}/cif2cbf.c" "cbf_static;${CBF_HDF5}" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH};m")
add_target(cbf_template_t "${CBF__DECTRIS_EXAMPLES}/cbf_template_t.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(testcell "${CBF__EXAMPLES}/testcell.C" "cbf_static" "${CBF_STATIC_LIBRARY_PATH}")
add_target(sauter_test "${CBF__EXAMPLES}/sauter_test.C" "cbf_static" "${CBF_STATIC_LIBRARY_PATH}")
add_target(sequence_match "${CBF__EXAMPLES}/sequence_match.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(testulp "${CBF__EXAMPLES}/testulp.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(testhdf5 "${CBF__EXAMPLES}/testhdf5.c" "cbf_static;${CBF_HDF5}" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(testtree "${CBF__EXAMPLES}/testtree.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};;${HDF5_LIBRARY_PATH}")
add_target(testalloc "${CBF__EXAMPLES}/testalloc.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(testflat "${CBF__EXAMPLES}/testflat.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};;${HDF5_LIBRARY_PATH}")
add_target(testflatpacked "${CBF__EXAMPLES}/testflatpacked.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};;${HDF5_LIBRARY_PATH}")
add_target(testreals "${CBF__EXAMPLES}/testreals.c" "cbf_static" "${CBF_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")

if (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))

#
#  F90 examples
#
add_custom_command(OUTPUT "${CBF__BLDEXMP}/test_fcb_read_image.f90"
    WORKING_DIRECTORY "${CBF__M4}"
    COMMAND ${M4} -P "${CBF_M4FLAGS}" "${CBF__M4}/test_fcb_read_image.m4" > "${CBF__BLDEXMP}/test_fcb_read_image.f90"
    DEPENDS ${CBF_M4_FCB_DEFINES} "${CBF__M4}/test_fcb_read_image.m4"
    COMMENT "Generating ${test_fcb_read_image.f90}")
add_custom_command(OUTPUT "${CBF__BLDEXMP}/test_xds_binary.f90"
    WORKING_DIRECTORY "${CBF__M4}"
    COMMAND ${M4} -P "${CBF_M4FLAGS}" "${CBF__M4}/test_xds_binary.m4" > "${CBF__BLDEXMP}/test_xds_binary.f90"
    DEPENDS ${CBF_M4_FCB_DEFINES} "${CBF__M4}/test_xds_binary.m4"
    COMMENT "Generating ${test_xds_binary.f90}")

add_target(test_fcb_read_image "${CBF__BLDEXMP}/test_fcb_read_image.f90" "fcb_static" "${FCB_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")
add_target(test_xds_binary "${CBF__BLDEXMP}/test_xds_binary.f90" "fcb_static" "${FCB_STATIC_LIBRARY_PATH};${HDF5_LIBRARY_PATH}")

endif (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))



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
install (TARGETS cbf_static DESTINATION lib)
install (TARGETS cbf_shared DESTINATION lib)

if (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))
  install (TARGETS fcb_static DESTINATION lib)
  install (TARGETS fcb_shared DESTINATION lib)
endif (NOT (CBF_USE_FORTRAN_ENV STREQUAL "NO"))

install (TARGETS img_static DESTINATION lib)
install (TARGETS img_shared DESTINATION lib)
install (DIRECTORY ${CBF__INCLUDE}/ DESTINATION include/cbflib FILES_MATCHING PATTERN "*.h")
install (DIRECTORY ${CBF__BIN_INCLUDE}/* DESTINATION include/cbflib FILES_MATCHING PATTERN "*.h")

# message(STATUS "CMAKE_INSTALL_PREFIX/lib ${CMAKE_INSTALL_PREFIX}/lib")

# path to the valgrind executable
set(VALGRIND_COMMAND "")
find_program(VALGRIND valgrind)
if (VALGRIND)
	CBF_DEBUG_MESSAGE("valgrind found at ${VALGRIND}")
	set(VALGRIND_COMMAND ${VALGRIND} --error-exitcode=3 --leak-check=full)
else ()
	CBF_DEBUG_MESSAGE("valgrind could not be not found")
endif ()

#
# tests  -- to be extended
#

#
# CBF_SET_TEST_ENVIRONMENT
#
macro(CBF_SET_TEST_ENVIRONMENT TESTNAME)
  IF(WIN32)
    set (xrpathname "PATH")
    set (xrpath "${HDF5_INSTALL_DIR}/lib;$ENV{PATH}")
    string(REPLACE ";" "\\;" xrpath "${xrpath}")
  ELSE(WIN32)
    IF(APPLE)
      set(xrpathname "DYLD_LIBRARY_PATH")
    ELSE(APPLE)
      set(xrpathname "LD_LIBRARY_PATH")
    ENDIF(APPLE)
    set (xrpath "${HDF5_INSTALL_DIR}/lib:$ENV{${xrpathname}}")
  ENDIF(WIN32)
  set_property(TEST ${TESTNAME} PROPERTY ENVIRONMENT "${xrpathname}=${xrpath}")
  message(status "${xrpathname}=${xrpath}")
endmacro(CBF_SET_TEST_ENVIRONMENT)

macro(CBF_APPEND_TEST_ENVIRONMENT TESTNAME)
  IF(WIN32)
    set (xrpathname "PATH")
    set (xrpath "${HDF5_INSTALL_DIR}/lib;$ENV{PATH})
    string(REPLACE ";" "\\;" xrpath "${xrpath}")
  ELSE(WIN32)
    IF(APPLE)
      set( xrpathname "DYLD_LIBRARY_PATH" )
    ELSE(APPLE)
      set( xrpathname "LD_LIBRARY_PATH" )
    ENDIF(APPLE)
    set (xrpath "${HDF5_INSTALL_DIR}/lib;$ENV{${xrpathname})
  ENDIF(WIN32)
  set_property(TEST ${TESTNAME} APPEND PROPERTY ENVIRONMENT "$xrpathname=${xrpath}")
endmacro(CBF_APPEND_TEST_ENVIRONMENT TESTNAME)



set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} ${HDF5_INSTALL_DIR}/lib)

# set up a wrapper to call 'h5dump' and redirect its output to a file
configure_file(${CBFlib_SOURCE_DIR}/h5dump.cmake ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump.cmake @ONLY)

# core tests

add_test(makecbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/makecbf ${CBF_DATADIRI}/example.mar2300 ${CBF__DATA}/makecbf.cbf)
CBF_SET_TEST_ENVIRONMENT(makecbf)
# $(BIN)/img2cif -c flatpacked -m headers -d digest -e base64 example.mar2300 img2cif_packed.cif
add_test(img2cif_packed.cif ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/img2cif -c flatpacked -m headers -d digest -e base64 ${CBF_DATADIRI}/example.mar2300 ${CBF__DATA}/img2cif_packed.cif)
set_property(TEST img2cif_packed.cif PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
# $(BIN)/img2cif -c canonical -m headers -d digest -e base64 example.mar2300 img2cif_canonical.cif
add_test(img2cif_canonical.cif ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/img2cif -c canonical -m headers -d digest -e base64 ${CBF_DATADIRI}/example.mar2300 ${CBF__DATA}/img2cif_canonical.cif)
set_property(TEST img2cif_canonical.cif PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
# $(BIN)/img2cif -c flatpacked -m headers -d digest -e none example.mar2300 img2cif_packed.cbf
add_test(img2cif_packed.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/img2cif -c flatpacked -m headers -d digest -e none ${CBF_DATADIRI}/example.mar2300 ${CBF__DATA}/img2cif_packed.cbf)
set_property(TEST img2cif_packed.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
# $(BIN)/img2cif -c canonical -m headers -d digest -e none example.mar2300 img2cif_canonical.cbf
add_test(img2cif_canonical.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/img2cif -c canonical -m headers -d digest -e none ${CBF_DATADIRI}/example.mar2300 ${CBF__DATA}/img2cif_canonical.cbf)
set_property(TEST img2cif_canonical.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
# $(BIN)/cif2cbf -e none -c flatpacked img2cif_canonical.cif cif2cbf_packed.cbf
add_test(cif2cbf_packed.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -e none -c flatpacked ${CBF__DATA}/img2cif_canonical.cif ${CBF__DATA}/cif2cbf_packed.cbf)
set_property(TEST cif2cbf_packed.cbf APPEND PROPERTY DEPENDS img2cif_canonical.cif)
set_property(TEST cif2cbf_packed.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
# $(BIN)/cif2cbf -e none -c canonical img2cif_packed.cif cif2cbf_canonical.cbf
add_test(cif2cbf_canonical.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -e none -c canonical ${CBF__DATA}/img2cif_packed.cif ${CBF__DATA}/cif2cbf_canonical.cbf)
set_property(TEST cif2cbf_canonical.cbf APPEND PROPERTY DEPENDS img2cif_packed.cif)
set_property(TEST cif2cbf_canonical.cbf  PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
# -cmp cif2cbf_packed.cbf makecbf.cbf
add_test(cmp-cif2cbf_packed.cbf-makecbf.cbf ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/cif2cbf_packed.cbf ${CBF__DATA}/makecbf.cbf)
set_property(TEST cmp-cif2cbf_packed.cbf-makecbf.cbf APPEND PROPERTY DEPENDS cif2cbf_packed.cbf)
set_property(TEST cmp-cif2cbf_packed.cbf-makecbf.cbf APPEND PROPERTY DEPENDS makecbf)
set_property(TEST cmp-cif2cbf_packed.cbf-makecbf.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
# -cmp cif2cbf_packed.cbf img2cif_packed.cbf
add_test(cmp-cif2cbf_packed.cbf-img2cif_packed.cbf ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/cif2cbf_packed.cbf ${CBF__DATA}/img2cif_packed.cbf)
set_property(TEST cmp-cif2cbf_packed.cbf-img2cif_packed.cbf APPEND PROPERTY DEPENDS cif2cbf_packed.cbf)
set_property(TEST cmp-cif2cbf_packed.cbf-img2cif_packed.cbf APPEND PROPERTY DEPENDS img2cif_packed.cbf)
set_property(TEST cmp-cif2cbf_packed.cbf-img2cif_packed.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
# -cmp cif2cbf_canonical.cbf img2cif_canonical.cbf
add_test(cmp-cif2cbf_canonical.cbf-img2cif_canonical.cbf ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/cif2cbf_canonical.cbf ${CBF__DATA}/img2cif_canonical.cbf)
set_property(TEST cmp-cif2cbf_canonical.cbf-img2cif_canonical.cbf APPEND PROPERTY DEPENDS cif2cbf_canonical.cbf)
set_property(TEST cmp-cif2cbf_canonical.cbf-img2cif_canonical.cbf APPEND PROPERTY DEPENDS img2cif_canonical.cbf)
set_property(TEST cmp-cif2cbf_canonical.cbf-img2cif_canonical.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# $(BIN)/cif2cbf -e hex -c none makecbf.cbf cif2cbf_ehcn.cif
add_test(cif2cbf_ehcn.cif ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -e hex -c none ${CBF__DATA}/makecbf.cbf ${CBF__DATA}/cif2cbf_ehcn.cif)
set_property(TEST cif2cbf_ehcn.cif APPEND PROPERTY DEPENDS makecbf)
set_property(TEST cif2cbf_ehcn.cif PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
# $(BIN)/cif2cbf -e none -c flatpacked cif2cbf_ehcn.cif cif2cbf_encp.cbf
add_test(cif2cbf_encp.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -e none -c flatpacked ${CBF__DATA}/cif2cbf_ehcn.cif ${CBF__DATA}/cif2cbf_encp.cbf)
set_property(TEST cif2cbf_encp.cbf APPEND PROPERTY DEPENDS cif2cbf_ehcn.cif)
set_property(TEST cif2cbf_encp.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
#-cmp makecbf.cbf cif2cbf_encp.cbf
add_test(cmp-makecbf.cbf-cif2cbf_encp.cbf ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/makecbf.cbf ${CBF__DATA}/cif2cbf_encp.cbf)
set_property(TEST cmp-makecbf.cbf-cif2cbf_encp.cbf APPEND PROPERTY DEPENDS makecbf)
set_property(TEST cmp-makecbf.cbf-cif2cbf_encp.cbf APPEND PROPERTY DEPENDS cif2cbf_encp.cbf)
set_property(TEST cmp-makecbf.cbf-cif2cbf_encp.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# $(BIN)/cif2cbf -i 9ins.cif -o 9ins.cbf
add_test(9ins.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -i ${CBF_DATADIRI}/9ins.cif -o ${CBF__DATA}/9ins.cbf)
set_property(TEST 9ins.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
# -cmp 9ins.cif 9ins.cbf
add_test(cmp-9ins.cif-9ins.cbf ${CMAKE_COMMAND} -E compare_files ${CBF_DATADIRI}/9ins.cif ${CBF__DATA}/9ins.cbf)
set_property(TEST cmp-9ins.cif-9ins.cbf APPEND PROPERTY DEPENDS 9ins.cbf)
set_property(TEST cmp-9ins.cif-9ins.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# $(BIN)/convert_image -F example.mar2300 converted_flat.cbf
add_test(
	converted_flat.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/convert_image
	-F -p ${CBFlib_SOURCE_DIR}/templates/template_mar345_2300x2300.cbf
	${CBF_DATADIRI}/example.mar2300
	${CBF__DATA}/converted_flat.cbf
)
set_property(TEST converted_flat.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
# -cmp converted_flat_orig.cbf converted_flat.cbf
add_test(cmp-converted_flat_orig.cbf-converted_flat.cbf ${CMAKE_COMMAND} -E compare_files ${CBF_DATADIRO}/converted_flat_orig.cbf ${CBF__DATA}/converted_flat.cbf)
set_property(TEST cmp-converted_flat_orig.cbf-converted_flat.cbf APPEND PROPERTY DEPENDS converted_flat.cbf)
set_property(TEST cmp-converted_flat_orig.cbf-converted_flat.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# $(BIN)/convert_image example.mar2300 converted.cbf
add_test(converted.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/convert_image -p ${CBFlib_SOURCE_DIR}/templates/template_mar345_2300x2300.cbf ${CBF_DATADIRI}/example.mar2300 ${CBF__DATA}/converted.cbf)
set_property(TEST converted.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# -cmp converted_orig.cbf converted.cbf
add_test(cmp-converted_orig.cbf-converted.cbf ${CMAKE_COMMAND} -E compare_files ${CBF_DATADIRO}/converted_orig.cbf ${CBF__DATA}/converted.cbf)
set_property(TEST cmp-converted_orig.cbf-converted.cbf APPEND PROPERTY DEPENDS converted.cbf)
set_property(TEST cmp-converted_orig.cbf-converted.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# copy a file into another directory so that test input data is not modified
add_test(mb_LP_1_001.img ${CMAKE_COMMAND} -E copy ${CBF_DATADIRI}/mb_LP_1_001.img ${CBF__DATA}/mb_LP_1_001.img)

# $(BIN)/convert_image -F -d adscquantum315 mb_LP_1_001.img adscconverted_flat.cbf
add_test(
	adscconverted_flat.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/convert_image
	-F -p ${CBFlib_SOURCE_DIR}/templates/template_adscquantum315_3072x3072.cbf
	-d adscquantum315
	${CBF__DATA}/mb_LP_1_001.img
	${CBF__DATA}/adscconverted_flat.cbf
)
set_property(TEST adscconverted_flat.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
set_property(TEST adscconverted_flat.cbf APPEND PROPERTY DEPENDS mb_LP_1_001.img)
# -cmp adscconverted_flat_orig.cbf adscconverted_flat.cbf
add_test(cmp-adscconverted_flat_orig.cbf-adscconverted_flat.cbf ${CMAKE_COMMAND} -E compare_files ${CBF_DATADIRO}/adscconverted_flat_orig.cbf ${CBF__DATA}/adscconverted_flat.cbf)
set_property(TEST cmp-adscconverted_flat_orig.cbf-adscconverted_flat.cbf APPEND PROPERTY DEPENDS adscconverted_flat.cbf)
set_property(TEST cmp-adscconverted_flat_orig.cbf-adscconverted_flat.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# $(BIN)/convert_image -d adscquantum315 mb_LP_1_001.img adscconverted.cbf
add_test(
	adscconverted.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/convert_image
	-p ${CBFlib_SOURCE_DIR}/templates/template_adscquantum315_3072x3072.cbf
	-d adscquantum315
	${CBF__DATA}/mb_LP_1_001.img
	${CBF__DATA}/adscconverted.cbf
)
set_property(TEST adscconverted.cbf APPEND PROPERTY DEPENDS mb_LP_1_001.img)
set_property(TEST adscconverted.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
# -cmp adscconverted_orig.cbf adscconverted.cbf
add_test(cmp-adscconverted_orig.cbf-adscconverted.cbf ${CMAKE_COMMAND} -E compare_files ${CBF_DATADIRO}/adscconverted_orig.cbf ${CBF__DATA}/adscconverted.cbf)
set_property(TEST cmp-adscconverted_orig.cbf-adscconverted.cbf APPEND PROPERTY DEPENDS adscconverted.cbf)
set_property(TEST cmp-adscconverted_orig.cbf-adscconverted.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# $(BIN)/adscimg2cbf --no_pad --cbf_packed,flat mb_LP_1_001.img
add_test(mb_LP_1_001.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/adscimg2cbf --no_pad --cbf_packed,flat ${CBF__DATA}/mb_LP_1_001.img)
set_property(TEST mb_LP_1_001.cbf APPEND PROPERTY DEPENDS mb_LP_1_001.img)
set_property(TEST mb_LP_1_001.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
# -cmp mb_LP_1_001_orig.cbf mb_LP_1_001.cbf
add_test(cmp-mb_LP_1_001_orig.cbf-mb_LP_1_001.cbf ${CMAKE_COMMAND} -E compare_files ${CBF_DATADIRO}/mb_LP_1_001_orig.cbf ${CBF__DATA}/mb_LP_1_001.cbf)
set_property(TEST cmp-mb_LP_1_001_orig.cbf-mb_LP_1_001.cbf APPEND PROPERTY DEPENDS mb_LP_1_001.cbf)
set_property(TEST cmp-mb_LP_1_001_orig.cbf-mb_LP_1_001.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# cp mb_LP_1_001.cbf nmb_LP_1_001.cbf
add_test(nmb_LP_1_001.cbf ${CMAKE_COMMAND} -E copy ${CBF__DATA}/mb_LP_1_001.cbf ${CBF__DATA}/nmb_LP_1_001.cbf)
set_property(TEST nmb_LP_1_001.cbf APPEND PROPERTY DEPENDS mb_LP_1_001.cbf)
set_property(TEST nmb_LP_1_001.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
# $(BIN)/cbf2adscimg nmb_LP_1_001.cbf
add_test(nmb_LP_1_001.img ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cbf2adscimg ${CBF__DATA}/nmb_LP_1_001.cbf)
set_property(TEST nmb_LP_1_001.img APPEND PROPERTY DEPENDS nmb_LP_1_001.cbf)
set_property(TEST nmb_LP_1_001.img PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
# -cmp mb_LP_1_001.img nmb_LP_1_001.img
add_test(cmp-mb_LP_1_001.img-nmb_LP_1_001.img ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/mb_LP_1_001.img ${CBF__DATA}/nmb_LP_1_001.img)
set_property(TEST cmp-mb_LP_1_001.img-nmb_LP_1_001.img APPEND PROPERTY DEPENDS mb_LP_1_001.img)
set_property(TEST cmp-mb_LP_1_001.img-nmb_LP_1_001.img APPEND PROPERTY DEPENDS nmb_LP_1_001.img)

# $(BIN)/convert_minicbf -d pilatus6m -v 1 insulin_pilatus6m.cbf insulin_pilatus6mconverted.cbf
add_test(
	insulin_pilatus6mconverted.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/convert_minicbf
	-p ${CBFlib_SOURCE_DIR}/templates/template_pilatus6m_2463x2527.cbf
	-d pilatus6m -v 1
	${CBF_DATADIRI}/insulin_pilatus6m.cbf
	${CBF__DATA}/insulin_pilatus6mconverted.cbf
)
set_property(TEST insulin_pilatus6mconverted.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# -cmp insulin_pilatus6mconverted_rev_orig.cbf insulin_pilatus6mconverted.cbf
add_test(
	cmp-insulin_pilatus6mconverted_rev_orig.cbf-insulin_pilatus6mconverted.cbf
	${CMAKE_COMMAND} -E compare_files
	${CBF_DATADIRO}/insulin_pilatus6mconverted_rev_orig.cbf
	${CBF__DATA}/insulin_pilatus6mconverted.cbf
)
set_property(TEST cmp-insulin_pilatus6mconverted_rev_orig.cbf-insulin_pilatus6mconverted.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf)
set_property(TEST cmp-insulin_pilatus6mconverted_rev_orig.cbf-insulin_pilatus6mconverted.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# $(BIN)/convert_minicbf -d pilatus6m insulin_pilatus6m.cbf insulin_pilatus6mconverted_v2.cbf
add_test(
	insulin_pilatus6mconverted_v2.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/convert_minicbf
	-p ${CBFlib_SOURCE_DIR}/templates/template_pilatus6m_2463x2527.cbf
	-d pilatus6m
	${CBF_DATADIRI}/insulin_pilatus6m.cbf
	${CBF__DATA}/insulin_pilatus6mconverted_v2.cbf
)
set_property(TEST insulin_pilatus6mconverted_v2.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
# -cmp insulin_pilatus6mconverted_v2_orig.cbf insulin_pilatus6mconverted_v2.cbf
add_test(
	cmp-insulin_pilatus6mconverted_v2_orig.cbf-insulin_pilatus6mconverted_v2.cbf
	${CMAKE_COMMAND} -E compare_files
	${CBF_DATADIRO}/insulin_pilatus6mconverted_v2_orig.cbf
	${CBF__DATA}/insulin_pilatus6mconverted_v2.cbf
)
set_property(TEST cmp-insulin_pilatus6mconverted_v2_orig.cbf-insulin_pilatus6mconverted_v2.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 
set_property(TEST cmp-insulin_pilatus6mconverted_v2_orig.cbf-insulin_pilatus6mconverted_v2.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_v2.cbf)

# cif2cbf basic round-trip to hdf5 tests

# $(BIN)/cif2cbf -5 w -O $(HDF5REGISTER) -i insulin_pilatus6mconverted.cbf -o insulin_pilatus6mconverted.cbf.h5
add_test(
	insulin_pilatus6mconverted.cbf.h5
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 w -O
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted.cbf
	-o ${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5
)
set_property(TEST insulin_pilatus6mconverted.cbf.h5 APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted.cbf.h5 APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf)
set_property(TEST insulin_pilatus6mconverted.cbf.h5 PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib") 

# $(BIN)/cif2cbf -5 rn $(HDF5REGISTER) -en -cp -i insulin_pilatus6mconverted.cbf.h5 -o insulin_pilatus6mconverted.cbf.h5.cbf
add_test(
	insulin_pilatus6mconverted.cbf.h5.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 rn -en -cp
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5
	-o ${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5.cbf
)
set_property(TEST insulin_pilatus6mconverted.cbf.h5.cbf APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf.h5)
set_property(TEST insulin_pilatus6mconverted.cbf.h5.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")
# -cmp insulin_pilatus6mconverted.cbf.h5.cbf insulin_pilatus6mconverted_orig.cbf.h5.cbf
add_test(
	cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted.cbf.h5.cbf
	${CMAKE_COMMAND} -E compare_files
	${CBF_DATADIRO}/insulin_pilatus6mconverted_orig.cbf.h5.cbf
	${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5.cbf
)
set_property(TEST cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf.h5.cbf)

# $(BIN)/h5dump insulin_pilatus6mconverted_orig.cbf.h5 | $(ALLBUTONE) > insulin_pilatus6mconverted_orig.cbf.h5.dump
add_test(
	insulin_pilatus6mconverted_orig.cbf.h5.dump
	${CMAKE_COMMAND}
	-Dinput=${CBF_DATADIRO}/insulin_pilatus6mconverted_orig.cbf.h5
	-Doutput=${CBF__DATA}/insulin_pilatus6mconverted_orig.cbf.h5.dump
	-P ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump.cmake
)
# $(BIN)/h5dump insulin_pilatus6mconverted.cbf.h5 | $(ALLBUTONE)  > insulin_pilatus6mconverted.cbf.h5.dump
add_test(
	insulin_pilatus6mconverted.cbf.h5.dump
	${CMAKE_COMMAND}
	-Dinput=${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5
	-Doutput=${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5.dump
	-P ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump.cmake
)
set_property(TEST insulin_pilatus6mconverted.cbf.h5.dump APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf.h5)
# $(DIFF) insulin_pilatus6mconverted_orig.cbf.h5.dump insulin_pilatus6mconverted.cbf.h5.dump
add_test(
	cmp-insulin_pilatus6mconverted_orig.cbf.h5.dump-insulin_pilatus6mconverted.cbf.h5.dump
	${CMAKE_COMMAND} -E compare_files
	${CBF__DATA}/insulin_pilatus6mconverted_orig.cbf.h5.dump
	${CBF__DATA}/insulin_pilatus6mconverted.cbf.h5.dump
)
set_property(TEST cmp-insulin_pilatus6mconverted_orig.cbf.h5.dump-insulin_pilatus6mconverted.cbf.h5.dump APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_orig.cbf.h5.dump)
set_property(TEST cmp-insulin_pilatus6mconverted_orig.cbf.h5.dump-insulin_pilatus6mconverted.cbf.h5.dump APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf.h5.dump)

# cif2cbf round-trip tests

# convert to hdf5 with '-en -cI' options, then back to cbf with '-en -cp' options to check the file against a reference file

# $(BIN)/cif2cbf -5 w $(HDF5REGISTER) -en -cI -i insulin_pilatus6mconverted.cbf -o insulin_pilatus6mconverted_encI.cbf.h5
add_test(
	insulin_pilatus6mconverted_encI.cbf.h5
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 w -en -cI
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted.cbf
	-o ${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5
)
set_property(TEST insulin_pilatus6mconverted_encI.cbf.h5 APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted_encI.cbf.h5 APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf)
# $(BIN)/cif2cbf -5 rn $(HDF5REGISTER) -en -cp -i insulin_pilatus6mconverted_encI.cbf.h5 -o insulin_pilatus6mconverted_encI.cbf.h5.cbf
add_test(
	insulin_pilatus6mconverted_encI.cbf.h5.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 rn -en -cp
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5
	-o ${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5.cbf
)
set_property(TEST insulin_pilatus6mconverted_encI.cbf.h5.cbf APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted_encI.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_encI.cbf.h5)
# -cmp insulin_pilatus6mconverted_encI.cbf.h5.cbf insulin_pilatus6mconverted_orig.cbf.h5.cbf
add_test(
	cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted_encI.cbf.h5.cbf
	${CMAKE_COMMAND} -E compare_files
	${CBF_DATADIRO}/insulin_pilatus6mconverted_orig.cbf.h5.cbf
	${CBF__DATA}/insulin_pilatus6mconverted_encI.cbf.h5.cbf
)
set_property(TEST cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted_encI.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_encI.cbf.h5.cbf)

# convert to hdf5 with '-en -cp' options, then back to cbf with '-en -cp' options to check the file against a reference file

# $(BIN)/cif2cbf -5 w $(HDF5REGISTER) -en -cp -i insulin_pilatus6mconverted.cbf -o insulin_pilatus6mconverted_encp.cbf.h5
add_test(
	insulin_pilatus6mconverted_encp.cbf.h5
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 w -en -cp
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted.cbf
	-o ${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5
)
set_property(TEST insulin_pilatus6mconverted_encp.cbf.h5 APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted_encp.cbf.h5 APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf)
# $(BIN)/cif2cbf -5 rn $(HDF5REGISTER) -en -cp -i insulin_pilatus6mconverted_encp.cbf.h5 -o insulin_pilatus6mconverted_encp.cbf.h5.cbf
add_test(
	insulin_pilatus6mconverted_encp.cbf.h5.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 rn -en -cp
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5
	-o ${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5.cbf
)
set_property(TEST insulin_pilatus6mconverted_encp.cbf.h5.cbf APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted_encp.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_encp.cbf.h5)
# -cmp insulin_pilatus6mconverted_encp.cbf.h5.cbf insulin_pilatus6mconverted_orig.cbf.h5.cbf
add_test(
	cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted_encp.cbf.h5.cbf
	${CMAKE_COMMAND} -E compare_files
	${CBF_DATADIRO}/insulin_pilatus6mconverted_orig.cbf.h5.cbf
	${CBF__DATA}/insulin_pilatus6mconverted_encp.cbf.h5.cbf
)
set_property(TEST cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted_encp.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_encp.cbf.h5.cbf)

# convert to hdf5 with '-en -cb' options, then back to cbf with '-en -cp' options to check the file against a reference file

# $(BIN)/cif2cbf -5 w $(HDF5REGISTER) -en -cb -i insulin_pilatus6mconverted.cbf -o insulin_pilatus6mconverted_encb.cbf.h5
add_test(
	insulin_pilatus6mconverted_encb.cbf.h5
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 w -en -cb
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted.cbf
	-o ${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5
)
set_property(TEST insulin_pilatus6mconverted_encb.cbf.h5 APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted_encb.cbf.h5 APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf)
# $(BIN)/cif2cbf -5 rn $(HDF5REGISTER) -en -cp -i insulin_pilatus6mconverted_encb.cbf.h5 -o insulin_pilatus6mconverted_encb.cbf.h5.cbf
add_test(
	insulin_pilatus6mconverted_encb.cbf.h5.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 rn -en -cp
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5
	-o ${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5.cbf
)
set_property(TEST insulin_pilatus6mconverted_encb.cbf.h5.cbf APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted_encb.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_encb.cbf.h5)
# -cmp insulin_pilatus6mconverted_encb.cbf.h5.cbf insulin_pilatus6mconverted_orig.cbf.h5.cbf
add_test(
	cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted_encb.cbf.h5.cbf
	${CMAKE_COMMAND} -E compare_files
	${CBF_DATADIRO}/insulin_pilatus6mconverted_orig.cbf.h5.cbf
	${CBF__DATA}/insulin_pilatus6mconverted_encb.cbf.h5.cbf
)
set_property(TEST cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted_encb.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_encb.cbf.h5.cbf)

# convert to hdf5 with '-en -cc' options, then back to cbf with '-en -cp' options to check the file against a reference file

# $(BIN)/cif2cbf -5 w $(HDF5REGISTER) -en -cc -i insulin_pilatus6mconverted.cbf -o insulin_pilatus6mconverted_encc.cbf.h5
add_test(
	insulin_pilatus6mconverted_encc.cbf.h5
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 w -en -cc
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted.cbf
	-o ${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5
)
set_property(TEST insulin_pilatus6mconverted_encc.cbf.h5 APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted_encc.cbf.h5 APPEND PROPERTY DEPENDS insulin_pilatus6mconverted.cbf)
# $(BIN)/cif2cbf -5 rn $(HDF5REGISTER) -en -cp -i insulin_pilatus6mconverted_encc.cbf.h5 -o insulin_pilatus6mconverted_encc.cbf.h5.cbf
add_test(
	insulin_pilatus6mconverted_encc.cbf.h5.cbf
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cif2cbf -5 rn -en -cp
	${CBF_HDF5REGISTER_ARG} ${CBF_HDF5REGISTER_VAL}
	-i ${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5
	-o ${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5.cbf
)
set_property(TEST insulin_pilatus6mconverted_encc.cbf.h5.cbf APPEND PROPERTY ENVIRONMENT "HDF5_PLUGIN_PATH=${HDF5_PLUGIN_PATH}")
set_property(TEST insulin_pilatus6mconverted_encc.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_encc.cbf.h5)
# -cmp insulin_pilatus6mconverted_encc.cbf.h5.cbf insulin_pilatus6mconverted_orig.cbf.h5.cbf
add_test(
	cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted_encc.cbf.h5.cbf
	${CMAKE_COMMAND} -E compare_files
	${CBF_DATADIRO}/insulin_pilatus6mconverted_orig.cbf.h5.cbf
	${CBF__DATA}/insulin_pilatus6mconverted_encc.cbf.h5.cbf
)
set_property(TEST cmp-insulin_pilatus6mconverted_orig.cbf.h5.cbf-insulin_pilatus6mconverted_encc.cbf.h5.cbf APPEND PROPERTY DEPENDS insulin_pilatus6mconverted_encc.cbf.h5.cbf)

# conversion tests for tiff2cbf

# $(BIN)/tiff2cbf XRD1621.tif XRD1621.cbf
add_test(XRD1621.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/tiff2cbf ${CBF_DATADIRI}/XRD1621.tif ${CBF__DATA}/XRD1621.cbf)
set_property(TEST XRD1621.cbf APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/${CBF_TIFF}/lib")
# $(DIFF) XRD1621_orig.cbf XRD1621.cbf
add_test(
	cmp-XRD1621_orig.cbf-XRD1621.cbf
	${CMAKE_COMMAND} -E compare_files
	${CBF_DATADIRO}/XRD1621_orig.cbf
	${CBF__DATA}/XRD1621.cbf
)
set_property(TEST cmp-XRD1621_orig.cbf-XRD1621.cbf APPEND PROPERTY DEPENDS XRD1621.cbf)
# $(BIN)/cif2cbf -I 4 -C 100. -L 0. -e n -c b -i XRD1621.cbf -o XRD1621_I4encbC100.cbf
# $(DIFF) XRD1621_I4encbC100_orig.cbf XRD1621_I4encbC100.cbf

# unit tests

add_test(testalloc ${VALGRIND_COMMAND} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testalloc)
add_test(testhdf5 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testhdf5)
add_test(testtree ${VALGRIND_COMMAND} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testtree)
add_test(testulp ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testulp)

add_test(testflatin.cbf ${CMAKE_COMMAND} -E copy ${CBF_DATADIRI}/testflatin.cbf ${CBF__DATA}/testflatin.cbf)
add_test(NAME testflat COMMAND ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testflat WORKING_DIRECTORY ${CBF__DATA})
set_property(TEST testflat APPEND PROPERTY DEPENDS testflatin.cbf)
add_test(cmp-testflatin.cbf-testflatout.cbf ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/testflatin.cbf ${CBF__DATA}/testflatout.cbf)
set_property(TEST cmp-testflatin.cbf-testflatout.cbf APPEND PROPERTY DEPENDS testflat)

add_test(testflatpackedin.cbf ${CMAKE_COMMAND} -E copy ${CBF_DATADIRI}/testflatpackedin.cbf ${CBF__DATA}/testflatpackedin.cbf)
add_test(NAME testflatpacked COMMAND ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testflatpacked WORKING_DIRECTORY ${CBF__DATA})
set_property(TEST testflatpacked APPEND PROPERTY DEPENDS testflatpackedin.cbf)
add_test(cmp-testflatpackedin.cbf-testflatpackedout.cbf ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/testflatpackedin.cbf ${CBF__DATA}/testflatpackedout.cbf)
set_property(TEST cmp-testflatpackedin.cbf-testflatpackedout.cbf APPEND PROPERTY DEPENDS testflatpacked)

add_test(testrealin.cbf ${CMAKE_COMMAND} -E copy ${CBF_DATADIRI}/testrealin.cbf ${CBF__DATA}/testrealin.cbf)
add_test(NAME testreals COMMAND ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testreals WORKING_DIRECTORY ${CBF__DATA})
set_property(TEST testreals APPEND PROPERTY DEPENDS testrealin.cbf)
add_test(cmp-testrealin.cbf-testrealout.cbf ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/testrealin.cbf ${CBF__DATA}/testrealout.cbf)
set_property(TEST cmp-testrealin.cbf-testrealout.cbf APPEND PROPERTY DEPENDS testreals)

# $(BIN)/sauter_test
add_test(NAME sauter_test COMMAND ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/sauter_test WORKING_DIRECTORY ${CBF__DATA})
set_property(TEST sauter_test APPEND PROPERTY DEPENDS adscconverted_flat.cbf)
# $(BIN)/changtestcompression
add_test(changtestcompression ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/changtestcompression)

# set up a test script for 'testcell':
configure_file(${CBF__EXAMPLES}/testcell.cmake ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testcell.cmake)
# $(BIN)/testcell < testcell.dat > testcell.prt
add_test(testcell ${CMAKE_COMMAND} -P ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testcell.cmake)
# -cmp testcell_orig.prt testcell.prt
add_test(cmp-testcell_orig.prt-testcell.prt ${CMAKE_COMMAND} -E compare_files ${CBF_DATADIRO}/testcell_orig.prt ${CBF__DATA}/testcell.prt)
set_property(TEST cmp-testcell_orig.prt-testcell.prt APPEND PROPERTY DEPENDS testcell)

# basic minicbf2nexus tests
  
# generate a hfd5 file from minicbf data
add_test(
	minicbf.h5
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/minicbf2nexus
	-c zlib
	-C ${CBFlib_SOURCE_DIR}/minicbf_test/config
	--register manual
	-o ${CBF__DATA}/minicbf.h5
	${CBF_DATADIRI}/X4_lots_M1S4_1_0001.cbf
	${CBF_DATADIRI}/X4_lots_M1S4_1_0002.cbf
	${CBF_DATADIRI}/X4_lots_M1S4_1_0003.cbf
	${CBF_DATADIRI}/X4_lots_M1S4_1_0004.cbf
	${CBF_DATADIRI}/X4_lots_M1S4_1_0005.cbf
)
  
# dump the content of generated and reference hdf5 files & use 'cbf_tail' to strip off some file names
add_test(
	minicbf_original.dump
	${CMAKE_COMMAND}
	-Dinput=${CBF_DATADIRO}/minicbf_original.h5
	-Doutput=${CBF__DATA}/minicbf_original.dump
	-P ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump.cmake
)
add_test(
	minicbf.dump
	${CMAKE_COMMAND}
	-Dinput=${CBF__DATA}/minicbf.h5
	-Doutput=${CBF__DATA}/minicbf.dump
	-P ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump.cmake
)
set_property(TEST minicbf.dump APPEND PROPERTY DEPENDS minicbf.h5)

# compare dumped hdf5 file content
add_test(cmp-minicbf_original.dump-minicbf.dump ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/minicbf_original.dump ${CBF__DATA}/minicbf.dump)
set_property(TEST cmp-minicbf_original.dump-minicbf.dump APPEND PROPERTY DEPENDS minicbf_original.dump)
set_property(TEST cmp-minicbf_original.dump-minicbf.dump APPEND PROPERTY DEPENDS minicbf.dump)

# round-trip cbf2nexus & nexus2cbf tests using i19 data

# generate the data
add_test(i19-1.h5 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cbf2nexus -c zlib --list -o ${CBF__DATA}/i19-1.h5 ${CBF_DATADIRI}/1191_00005.cbf)
set_property(TEST i19-1.h5 PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")

add_test(i19-1.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/nexus2cbf -o ${CBF__DATA}/i19-1.cbf ${CBF__DATA}/i19-1.h5)
set_property(TEST i19-1.cbf APPEND PROPERTY DEPENDS i19-1.h5)
set_property(TEST i19-1.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")

add_test(i19-2.h5 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cbf2nexus -c zlib --list -o ${CBF__DATA}/i19-2.h5 ${CBF__DATA}/i19-1.cbf)
set_property(TEST i19-2.h5 APPEND PROPERTY DEPENDS i19-1.cbf)
set_property(TEST i19-2.h5 PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")

add_test(i19-2.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/nexus2cbf -o ${CBF__DATA}/i19-2.cbf ${CBF__DATA}/i19-2.h5)
set_property(TEST i19-2.cbf APPEND PROPERTY DEPENDS i19-2.h5)
set_property(TEST i19-1.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")


# compare the cbf files from i19 data
add_test(cmp-i19-1.cbf-i19-2.cbf ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/i19-1.cbf ${CBF__DATA}/i19-2.cbf)
set_property(TEST cmp-i19-1.cbf-i19-2.cbf APPEND PROPERTY DEPENDS i19-1.cbf)
set_property(TEST cmp-i19-1.cbf-i19-2.cbf APPEND PROPERTY DEPENDS i19-2.cbf)

# dump the content of generated hdf5 files & use 'cbf_tail' to strip off some file names
add_test(
	i19-1.dump
	${CMAKE_COMMAND}
	-Dinput=${CBF__DATA}/i19-1.h5
	-Doutput=${CBF__DATA}/i19-1.dump
	-P ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump.cmake
)
set_property(TEST i19-1.dump APPEND PROPERTY DEPENDS i19-1.h5)
add_test(
	i19-2.dump
	${CMAKE_COMMAND}
	-Dinput=${CBF__DATA}/i19-2.h5
	-Doutput=${CBF__DATA}/i19-2.dump
	-P ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump.cmake
)
set_property(TEST i19-2.dump APPEND PROPERTY DEPENDS i19-2.h5)

# compare dumped hdf5 file content
add_test(cmp-i19-1.dump-i19-2.dump ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/i19-1.dump ${CBF__DATA}/i19-2.dump)
set_property(TEST cmp-i19-1.dump-i19-2.dump APPEND PROPERTY DEPENDS i19-1.dump)
set_property(TEST cmp-i19-1.dump-i19-2.dump APPEND PROPERTY DEPENDS i19-2.dump)

# round-trip cbf2nexus & nexus2cbf tests using i03 data

# generate the data
add_test(i03-1.h5 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cbf2nexus -c zlib --list -o ${CBF__DATA}/i03-1.h5 ${CBF_DATADIRI}/thaumatin_die_M1S5_1_0005_2.cbf)
set_property(TEST i03-1.h5 PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")

add_test(i03-1.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/nexus2cbf -o ${CBF__DATA}/i03-1.cbf ${CBF__DATA}/i03-1.h5)
set_property(TEST i03-1.cbf APPEND PROPERTY DEPENDS i03-1.h5)
set_property(TEST i03-1.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")

add_test(i03-2.h5 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/cbf2nexus -c zlib --list -o ${CBF__DATA}/i03-2.h5 ${CBF__DATA}/i03-1.cbf)
set_property(TEST i03-2.h5 APPEND PROPERTY DEPENDS i03-1.cbf)
set_property(TEST i03-2.h5 PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")

add_test(i03-2.cbf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/nexus2cbf -o ${CBF__DATA}/i03-2.cbf ${CBF__DATA}/i03-2.h5)
set_property(TEST i03-2.cbf APPEND PROPERTY DEPENDS i03-2.h5)
set_property(TEST i03-2.cbf PROPERTY ENVIRONMENT "DYLD_LIBRARY_PATH=${HDF5_INSTALL_DIR}/lib")


# compare the cbf files from i03 data
add_test(cmp-i03-1.cbf-i03-2.cbf ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/i03-1.cbf ${CBF__DATA}/i03-2.cbf)
set_property(TEST cmp-i03-1.cbf-i03-2.cbf APPEND PROPERTY DEPENDS i03-1.cbf)
set_property(TEST cmp-i03-1.cbf-i03-2.cbf APPEND PROPERTY DEPENDS i03-2.cbf)

# dump the content of generated hdf5 files & use 'cbf_tail' to strip off some file names
add_test(
	i03-1.dump
	${CMAKE_COMMAND}
	-Dinput=${CBF__DATA}/i03-1.h5
	-Doutput=${CBF__DATA}/i03-1.dump
	-P ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump.cmake
)
set_property(TEST i03-1.dump APPEND PROPERTY DEPENDS i03-1.h5)
add_test(
	i03-2.dump
	${CMAKE_COMMAND}
	-Dinput=${CBF__DATA}/i03-2.h5
	-Doutput=${CBF__DATA}/i03-2.dump
	-P ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/h5dump.cmake
)
set_property(TEST i03-2.dump APPEND PROPERTY DEPENDS i03-2.h5)

# compare dumped hdf5 file content
add_test(cmp-i03-1.dump-i03-2.dump ${CMAKE_COMMAND} -E compare_files ${CBF__DATA}/i03-1.dump ${CBF__DATA}/i03-2.dump)
set_property(TEST cmp-i03-1.dump-i03-2.dump APPEND PROPERTY DEPENDS i03-1.dump)
set_property(TEST cmp-i03-1.dump-i03-2.dump APPEND PROPERTY DEPENDS i03-2.dump)

