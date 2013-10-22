
######################################################################
#  CMakeLists.txt - cmake build file for make to create CBFlib       #
#                                                                    #
# Version 0.9.3 23 September 2013                                    #
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

project (CBFlib C CXX Fortran)
enable_testing()
set (CBF_CMAKE_DEBUG "ON")

#
#  User setable parameters
#
set(CBF_M4FLAGS_ENV $ENV{M4FLAGS})
if (CBF_M4FLAGS_ENV)
  set(CBF_M4FLAGS ${CBF_M4FLAGS_ENV})
else (CBF_M4FLAGS_ENV)
  set(CBF_M4FLAGS "-Dfcb_bytes_in_rec=4096")
endif (CBF_M4FLAGS_ENV)

set(CBF_F90FLAGS_ENV $ENV{F90FLAGS})
if (CBF_F90FLAGS_ENV)
  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${CBF_F90FLAGS_ENV}")
  set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG}   ${CBF_F90FLAGS_ENV}")
else (CBF_F90FLAGS_ENV)
  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -fno-range-check")
  set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG}   -fno-range-check")
endif (CBF_F90FLAGS_ENV)

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
# CBF_ADD_C_EXAMPLE -- set up a C example program
#
macro(CBF_ADD_C_EXAMPLE PGMNAME EXDIR EXTRASRCS EXTRALIBS)
  add_executable(${PGMNAME} "${EXDIR}/${PGMNAME}.c" "${EXTRASRCS}")
  target_link_libraries(${PGMNAME} cbf_static ${CBF_HDF5_LIBRARY_PATH} ${EXTRALIBS})
endmacro(CBF_ADD_C_EXAMPLE)

macro(CBF_ADD_CXX_EXAMPLE PGMNAME EXDIR EXTRASRCS EXTRALIBS)
  add_executable(${PGMNAME} "${EXDIR}/${PGMNAME}.C" "${EXTRASRCS}")
  target_link_libraries(${PGMNAME} cbf_static ${CBF_HDF5_LIBRARY_PATH} ${EXTRALIBS})
endmacro(CBF_ADD_CXX_EXAMPLE)


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
set(CBF__SOLIB     "${CBFlib_BINARY_DIR}/solib" )
set(CBF__BIN_INCLUDE "${CBFlib_BINARY_DIR}/include" )
set(CBF__SHARE     "${CBFlib_BINARY_DIR}/share" )
set(CBF__EXT_PKG   "${CBFlib_BINARY_DIR}/external_packages" )
set(CBF__DATA      "${CBFlib_BINARY_DIR}/data_files" )


CBF_REQUIRE_DIRECTORY(${CBF__BLDSRC})
CBF_REQUIRE_DIRECTORY(${CBF__BLDEXMP})
CBF_REQUIRE_DIRECTORY(${CBF__BIN})
CBF_REQUIRE_DIRECTORY(${CBF__LIB})
CBF_REQUIRE_DIRECTORY(${CBF__SOLIB})
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
string (REGEX REPLACE ".*#define[ \t]+CBF_VERS_RELEASE[ \t]+([0-9]*).*$"
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
set(CBF_TIFFPREFIX  ${CBFlib_BINARY_DIR})

#
# Definitions to get a version of HDF5
#
set(CBF_HDF5        "hdf5-1.8.11_rev_20Sep13")
set(CBF_HDF5URL  "http://downloads.sf.net/cbflib/${CBF_HDF5}.tar.gz")
set(CBF_HDF5PREFIX  "${CBFlib_BINARY_DIR}")
set(CBF_HDF5LIBS    "${CBF__LIB}/libhdf5.a -lz -ldl")
set(CBF_HDF5SOLIBS  "-L${CBF__LIB} -lhdf5 -lz")
set(CBF_HDF5REGISTER "--register plugin")
set(HDF5_EXTERNALLY_CONFIGURED  1)
set(HDF5_INSTALL_BIN_DIR "${CBF__BIN}")
set(HDF5_INSTALL_LIB_DIR "${CBF__LIB}")
set(HDF5_INSTALL_INCLUDE_DIR "${CBF__BIN_INCLUDE}")
set(HDF5_INSTALL_DATA_DIR "${CBF__SHARE}")

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



#Load external packages in build directory at cmake time
add_subdirectory(${CBF__EXTERNAL_PACKAGES}  "${CBF__EXT_PKG}")

find_file(CBF_EXT_PKG_BLD_RUNNING cbf_ext_pkg_build_running "${CBF__EXT_PKG}")
if (${CBF_EXT_PKG_BLD_RUNNING} STREQUAL "CBF_EXT_PKG_BLD_RUNNING-NOTFOUND")
  file(WRITE "${CBF__EXT_PKG}/cbf_ext_pkg_build_running" "cbf_ext_pkg_build_running")
  CBF_DEBUG_MESSAGE("CMAKE_COMMAND:        ${CMAKE_COMMAND}")
  CBF_DEBUG_MESSAGE("CBFlib_SOURCE_DIR:    ${CBFlib_SOURCE_DIR} ")
  execute_process(
    COMMAND ${CMAKE_COMMAND} ${CBFlib_SOURCE_DIR}
    RESULT_VARIABLE CBF_BUILD_RESULT)
  CBF_DEBUG_MESSAGE("CBF_BUILD_RESULT: ${CBF_BUILD_RESULT}")
  execute_process(
    COMMAND ${CMAKE_COMMAND} --build ${CBFlib_BINARY_DIR} --target HDF5
    RESULT_VARIABLE CBF_BUILD_RESULT)
  CBF_DEBUG_MESSAGE("CBF_BUILD_RESULT: ${CBF_BUILD_RESULT}")
  execute_process(
    COMMAND ${CMAKE_COMMAND} --build ${CBFlib_BINARY_DIR} --target TIFF
    RESULT_VARIABLE CBF_BUILD_RESULT)
  CBF_DEBUG_MESSAGE("CBF_BUILD_RESULT: ${CBF_BUILD_RESULT}")
  file(REMOVE "${CBF__EXT_PKG}/cbf_ext_pkg_build_running")
endif (${CBF_EXT_PKG_BLD_RUNNING} STREQUAL "CBF_EXT_PKG_BLD_RUNNING-NOTFOUND")


file(GLOB CBF_HDF5_LIBRARY_PATH "${CMAKE_CURRENT_BINARY_DIR}/lib/*hdf5*")
CBF_DEBUG_MESSAGE( "CBF_HDF5_LIBRARY_PATH:  ${CBF_HDF5_LIBRARY_PATH}")
file(GLOB CBF_TIFF_LIBRARY_PATH "${CMAKE_CURRENT_BINARY_DIR}/lib/*tif*")
CBF_DEBUG_MESSAGE( "CBF_TIFF_LIBRARY_PATH: ${CBF_TIFF_LIBRARY_PATH}")

if ("|${CBF_HDF5_LIBRARY_PATH}|" STREQUAL "||" OR "|${CBF_TIFF_LIBRARY_PATH}|" STREQUAL "||")
  MESSAGE(STATUS "Must build external packages in ${CBF__EXT_PKG}")
  MESSAGE(STATUS "Then rerun cmake")
  MESSAGE(WARNING "Finishing run to make Makefiles")
endif ("|${CBF_HDF5_LIBRARY_PATH}|" STREQUAL "||" OR "|${CBF_TIFF_LIBRARY_PATH}|" STREQUAL "||")


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
set(CBF_C_SOURCES ${CBF__SRC}/cbf.c
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
			${CBF__SRC}/md5c.c)


set(CBF_F90_BUILT_SOURCES 
			${CBF__BLDSRC}/fcb_exit_binary.f90
			${CBF__BLDSRC}/fcb_next_binary.f90
			${CBF__BLDSRC}/fcb_open_cifin.f90
			${CBF__BLDSRC}/fcb_packed.f90
			${CBF__BLDSRC}/fcb_read_bits.f90
			${CBF__BLDSRC}/fcb_read_image.f90
			${CBF__BLDSRC}/fcb_read_xds_i2.f90)
            
set(CBF_F90_SOURCES ${CBF__SRC}/fcb_atol_wcnt.f90
			${CBF__SRC}/fcb_ci_strncmparr.f90
			${CBF__SRC}/fcb_nblen_array.f90
			${CBF__SRC}/fcb_read_byte.f90
			${CBF__SRC}/fcb_read_line.f90
			${CBF__SRC}/fcb_skip_whitespace.f90)

			
#
# Header files
#
set(CBF_HEADERS ${CBF__INCLUDE}/cbf.h			
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
			${CBF__INCLUDE}/md5.h)


#
# m4 FCB library macro files
#
set(CBF_M4_FCB_DEFINES
            ${CBF__M4}/fcblib_defines.m4)
set(CBF_M4_FCB_FILES 
			${CBF__M4}/fcb_exit_binary.m4
			${CBF__M4}/fcb_next_binary.m4
			${CBF__M4}/fcb_open_cifin.m4
			${CBF__M4}/fcb_packed.m4
			${CBF__M4}/fcb_read_bits.m4
			${CBF__M4}/fcb_read_image.m4
			${CBF__M4}/fcb_read_xds_i2.m4)
#
# m4 F90 examples macro files
#
set(CBF_M4_F90_EXAMPLES
            ${CBF__M4}/test_fcb_read_image.m4
			${CBF__M4}/test_xds_binary.m4)


#
# Documentation files
#
set(CBF_DOCUMENTS ${CBF__DOC}/CBFlib.html
			${CBF__DOC}/CBFlib.txt
			${CBF__DOC}/CBFlib_NOTICES.html
			${CBF__DOC}/CBFlib_NOTICES.txt
            ${CBF__DOC}/ChangeLog
			${CBF__DOC}/ChangeLog.html
			${CBF__DOC}/MANIFEST
			${CBF__DOC}/gpl.txt $(DOC)/lgpl.txt
            CACHE STRING "")

#
# HTML Graphics files
#
set(JPEGS   ${GRAPHICS}/CBFbackground.jpg
			${GRAPHICS}/CBFbig.jpg
			${GRAPHICS}/CBFbutton.jpg
			${GRAPHICS}/cbflibbackground.jpg
            ${GRAPHICS}/cbflibbig.jpg
			${GRAPHICS}/cbflibbutton.jpg
			${GRAPHICS}/cifhome.jpg
            ${GRAPHICS}/iucrhome.jpg
			${GRAPHICS}/noticeButton.jpg
            CACHE STRING "")

# Set up the necessary includes

include_directories ( BEFORE SYSTEM ${CBFlib_BINARY_DIR}/include 
                      ${CBFlib_SOURCE_DIR}/include 
                      ${CBFlib_SOURCE_DIR}/src
                      ${CBF_REGEXLIB_INCLUDE_DIR}
                      )

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/bin")

#
#  Build the static and shared CBF libraries
#

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
add_library(cbf_static STATIC ${CBF_C_SOURCES})
set_target_properties(cbf_static PROPERTIES OUTPUT_NAME "cbf")
set_target_properties(cbf_static PROPERTIES LINKER_LANGUAGE C)
set_target_properties(cbf_static PROPERTIES SOVERSION "${CBF_APIVERSION}")


set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/solib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/solib")
add_library(cbf_shared SHARED  ${CBF_C_SOURCES})
set_target_properties(cbf_shared PROPERTIES OUTPUT_NAME "cbf")
set_target_properties(cbf_shared PROPERTIES LINKER_LANGUAGE C)
set_target_properties(cbf_shared PROPERTIES SOVERSION "${CBF_APIVERSION}")
target_link_libraries(cbf_shared ${CBF_HDF5_LIBRARY_PATH})

#
# Build the static and shared IMG libraries
#

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
add_library(img_static STATIC ${CBF__EXAMPLES}/img.c)
set_target_properties(img_static PROPERTIES OUTPUT_NAME "img")
set_target_properties(img_static PROPERTIES LINKER_LANGUAGE C)


set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/solib")
add_library(img_shared SHARED  ${CBF__EXAMPLES}/img.c)
set_target_properties(img_shared PROPERTIES OUTPUT_NAME "img")
set_target_properties(img_shared PROPERTIES LINKER_LANGUAGE C)
target_link_libraries(img_shared ${CBF_HDF5_LIBRARY_PATH})


#
# Build the f90 library sources
#
find_program(M4 m4)
foreach(f90src IN LISTS CBF_F90_BUILT_SOURCES)
  get_filename_component(filename "${f90src}" NAME_WE )
  set(f90bldsrc "${CBF__BLDSRC}/${filename}.f90")
  set(f90srcm4 "${CBF__M4}/${filename}.m4")
  add_custom_command(OUTPUT "${f90bldsrc}"
    WORKING_DIRECTORY "${CBF__M4}"
    COMMAND ${M4} -P "${CBF_M4FLAGS}" "${f90srcm4}" > "${f90bldsrc}"
    DEPENDS ${CBF_M4_FCB_DEFINES} ${f90srcm4}
    COMMENT "Generating ${f90bldsrc}")
endforeach(f90src)


#
# Build the fcb libraries
#
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/lib")
add_library(fcb_static STATIC ${CBF_F90_BUILT_SOURCES};${CBF_F90_SOURCES})
set_target_properties(fcb_static PROPERTIES OUTPUT_NAME "fcb")
set_target_properties(fcb_static PROPERTIES LINKER_LANGUAGE C)


set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CBFlib_BINARY_DIR}/solib")
add_library(fcb_shared SHARED  ${CBF_F90_BUILT_SOURCES};${CBF_F90_SOURCES})
set_target_properties(fcb_shared PROPERTIES OUTPUT_NAME "fcb")
set_target_properties(fcb_shared PROPERTIES LINKER_LANGUAGE C)
target_link_libraries(fcb_shared ${CBF_HDF5_LIBRARY_PATH})


#
#  C and C++ examples
#
CBF_ADD_C_EXAMPLE(convert_image "${CBF__EXAMPLES}" "" "img_static")
CBF_ADD_C_EXAMPLE(convert_minicbf "${CBF__EXAMPLES}" "" "")
CBF_ADD_C_EXAMPLE(makecbf "${CBF__EXAMPLES}" "" "img_static")
CBF_ADD_C_EXAMPLE(adscimg2cbf "${CBF__EXAMPLES}" "${CBF__EXAMPLES}/adscimg2cbf_sub.c" "")
CBF_ADD_C_EXAMPLE(cbf2adscimg "${CBF__EXAMPLES}" "${CBF__EXAMPLES}/cbf2adscimg_sub.c" "")
CBF_ADD_C_EXAMPLE(changtestcompression "${CBF__EXAMPLES}" "" "")
CBF_ADD_C_EXAMPLE(img2cif "${CBF__EXAMPLES}" "" "img_static")
CBF_ADD_C_EXAMPLE(cif2cbf "${CBF__EXAMPLES}" "" "")
CBF_ADD_C_EXAMPLE(cbf2nexus "${CBF__EXAMPLES}" "" "")
CBF_ADD_C_EXAMPLE(minicbf2nexus "${CBF__EXAMPLES}" "" "")
CBF_ADD_C_EXAMPLE(cbf_template_t "${CBF__DECTRIS_EXAMPLES}" "" "")
CBF_ADD_CXX_EXAMPLE(testcell "${CBF__EXAMPLES}" "" "")
CBF_ADD_CXX_EXAMPLE(sauter_test "${CBF__EXAMPLES}" "" "")
CBF_ADD_C_EXAMPLE(sequence_match "${CBF__EXAMPLES}" "" "")
CBF_ADD_C_EXAMPLE(tiff2cbf "${CBF__EXAMPLES}" "${CBF__EXT_PKG}/${CBF_TIFF}/libtiff/tif_sprint.c" "${CBF_TIFF_LIBRARY_PATH}")
CBF_ADD_C_EXAMPLE(testulp "${CBF__EXAMPLES}" "" "")
CBF_ADD_C_EXAMPLE(testhdf5 "${CBF__EXAMPLES}" "" "")
CBF_ADD_C_EXAMPLE(testalloc "${CBF__EXAMPLES}" "" "")



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

add_executable(test_fcb_read_image "${CBF__BLDEXMP}/test_fcb_read_image.f90")
add_executable(test_xds_binary     "${CBF__BLDEXMP}/test_xds_binary.f90")
target_link_libraries(test_fcb_read_image fcb_static)
target_link_libraries(test_xds_binary     fcb_static)



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
install (TARGETS cbf_template_t DESTINATION bin)
install (TARGETS testcell DESTINATION bin)
install (TARGETS sauter_test DESTINATION bin)
install (TARGETS sequence_match DESTINATION bin)
install (TARGETS tiff2cbf DESTINATION bin)
install (TARGETS cbf_static DESTINATION lib)
install (TARGETS cbf_shared DESTINATION lib)
install (TARGETS fcb_static DESTINATION lib)
install (TARGETS fcb_shared DESTINATION lib)
install (TARGETS img_static DESTINATION lib)
install (TARGETS img_shared DESTINATION lib)
install (DIRECTORY ${CBF__INCLUDE}/ DESTINATION include/cbflib FILES_MATCHING PATTERN "*.h")
install (DIRECTORY ${CBF__BIN_INCLUDE}/* DESTINATION include/cbflib FILES_MATCHING PATTERN "*.h")

message(STATUS "CMAKE_INSTALL_PREFIX/lib ${CMAKE_INSTALL_PREFIX}/lib")


#
# tests  -- to be extended
#
add_test(NAME cbftest_makecbf
  COMMAND ${CMAKE_COMMAND}
    -DCMD1=$<TARGET_FILE:makecbf> -DARG11=${CBF_DATADIRI}/example.mar2300 -DARG12=makecbf.cbf
    -P ${CMAKE_CURRENT_SOURCE_DIR}/runcommands.cmake)
add_test(cbftest_testalloc ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testalloc)
add_test(cbftest_testhdf5 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testhdf5)
add_test(cbftest_testulp ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/testulp)

  
  
