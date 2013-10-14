######################################################################
#  CMakeLists.txt - cmake build file for make to create              #
#                   external_packages in CBFlib                      #
#                                                                    #
# Version 0.9.3 28 September 2013                                    #
#                                                                    #
#                          Paul Ellis and                            #
#         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        #
#                                                                    #
# (C) Copyright 2006 - 2013 Herbert J. Bernstein                     #
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
#      external_packages      Directory for external packages        #
#      include                Directory with header files            #
#      m4                     Directory with m4 files                #
#      src                    Directory with source files            #
#                                                                    #
#    CBFlib_EXTP_BINARY_DIR        CBFlib build directory                 #
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


cmake_minimum_required (VERSION 2.8)

project (CBFlib_EXTP C CXX Fortran)

#
#  User setable parameters
#
#  NONE

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
# Defined in parent CMakeLists.txt


#
#  ZLIB
#
find_package(ZLIB)

#
# CBF_HDF5  
#
CBF_LOAD_TARBALL(${CBF__EXT_PKG} ${CBF__EXT_PKG}/${CBF_HDF5} ${CBF_HDF5URL})
include(ExternalProject)
ExternalProject_add(HDF5
    SOURCE_DIR ${CBF__EXT_PKG}/${CBF_HDF5}
    BINARY_DIR ${CBF__EXT_PKG}/${CBF_HDF5}-build
    CMAKE_ARGS
      -DCMAKE_BUILD_TYPE:STRING=RELEASE DEBUG
      -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=On
      -DHDF5_BUILD_CPP_LIB:BOOL=Off
      -DBUILD_SHARED_LIBS:BOOL=On
      -DHDF5_BUILD_TOOLS:BOOL=Off
      -DCMAKE_INSTALL_PREFIX:PATH=${CBFlib_BINARY_DIR}
      INSTALL_DIR ${CBFlib_BINARY_DIR}/
)
SET (HDF5_INCLUDE_DIR ${CBFlib_BINARY_DIR}/include)
SET (HDF5_LIBRARY_DIR ${CBFlib_BINARY_DIR}/lib)
     
install(DIRECTORY ${CBF__EXT_PKG}/${CBF_HDF5}-build/include/ DESTINATION ${CBFlib_BINARY_DIR}/include FILES_MATCHING PATTERN "*.h")
install(DIRECTORY ${CBF__EXT_PKG}/${CBF_HDF5}-build/lib/ DESTINATION ${CBFlib_BINARY_DIR}/lib FILES_MATCHING PATTERN "*hdf5*")
install(DIRECTORY ${CBF__EXT_PKG}/${CBF_HDF5}-build/bin/ DESTINATION ${CBFlib_BINARY_DIR}/bin FILES_MATCHING PATTERN "*hdf5*") 

#
# CBF_TIFF  
#
CBF_LOAD_TARBALL(${CBF__EXT_PKG} ${CBF__EXT_PKG}/${CBF_TIFF} ${CBF_TIFFURL})
ExternalProject_add(TIFF
    SOURCE_DIR ${CBF__EXT_PKG}/${CBF_TIFF}
    BINARY_DIR ${CBF__EXT_PKG}/${CBF_TIFF}-build
    CMAKE_ARGS
      -DCMAKE_BUILD_TYPE:STRING=RELEASE DEBUG
      -DCMAKE_INSTALL_PREFIX:PATH=${CBFlib_BINARY_DIR}
      INSTALL_DIR ${CBFlib_BINARY_DIR}/
)
SET (TIFF_INCLUDE_DIR ${CBFlib_BINARY_DIR}/include)
SET (TIFF_LIBRARY_DIR ${CBFlib_BINARY_DIR}/lib)

install(DIRECTORY ${CBF__EXT_PKG}/${CBF_TIFF}-build/include/ DESTINATION ${CBFlib_BINARY_DIR}/include FILES_MATCHING PATTERN "*.h")
install(DIRECTORY ${CBF__EXT_PKG}/${CBF_TIFF}-build/lib/ DESTINATION ${CBFlib_BINARY_DIR}/lib FILES_MATCHING PATTERN "*tif*")
install(DIRECTORY ${CBF__EXT_PKG}/${CBF_TIFF}-build/bin/ DESTINATION ${CBFlib_BINARY_DIR}/bin FILES_MATCHING PATTERN "*tif*")



#
# CBF_REGEX or CBF_PCRE
#
# Try for PCRE first
find_library(CBF_REGEXLIB pcre)
find_path(CBF_REGEXLIB_INCLUDE_DIR NAMES pcreposix.h)
message(STATUS "CBF_REGEXLIB_INCLUDE_DIR for pcreposix.h ${CBF_REGEXLIB_INCLUDE_DIR}")
if (${CBF_REGEXLIB_INCLUDE_DIR} STREQUAL "CBF_REGEXLIB_INCLUDE_DIR-NOTFOUND" 
    OR ${CBF_REGEXLIB} STREQUAL "CBF_REGEXLIB-NOTFOUND")
  # PCRE not found, try for REGEX
  find_library(CBF_REGEXLIB regex)
  find_path(CBF_REGEXLIB_INCLUDE_DIR NAMES regex.h)
  message(STATUS "CBF_REGEXLIB_INCLUDE_DIR for regex.h ${CBF_REGEXLIB_INCLUDE_DIR}")
  if (${CBF_REGEXLIB_INCLUDE_DIR} STREQUAL "CBF_REGEXLIB_INCLUDE_DIR-NOTFOUND" 
    OR ${CBF_REGEXLIB} STREQUAL "CBF_REGEXLIB-NOTFOUND")
    # Neither PCRE not REGEX found, load and build PCRE
    CBF_LOAD_TARBALL(${CBF__EXT_PKG} ${CBF__EXT_PKG}/${CBF_PCRE} ${CBF_PCREURL})
    ExternalProject_add(PCRE
    SOURCE_DIR ${CBF__EXT_PKG}/${CBF_PCRE}
    BINARY_DIR ${CBF__EXT_PKG}/${CBF_PCRE}-build
    CMAKE_ARGS
      -DCMAKE_BUILD_TYPE:STRING=RELEASE DEBUG
      -DCMAKE_INSTALL_PREFIX:PATH=${CBFlib_BINARY_DIR}
      INSTALL_DIR ${CBFlib_BINARY_DIR}/
    )
    SET (CBF_REGEXLIB_INCLUDE_DIR ${CBFlib_BINARY_DIR}/include)
    SET (CBF_REGEXLIB_LIBRARY_DIR ${CBFlib_BINARY_DIR}/lib)
    install(DIRECTORY ${CBF__EXT_PKG}/${CBF_PCRE}-build/include/ DESTINATION ${CBFlib_BINARY_DIR}/include FILES_MATCHING PATTERN "*.h")
    install(DIRECTORY ${CBF__EXT_PKG}/${CBF_PCRE}-build/lib/ DESTINATION ${CBFlib_BINARY_DIR}/lib FILES_MATCHING PATTERN "*pcre*")
    install(DIRECTORY ${CBF__EXT_PKG}/${CBF_PCRE}-build/bin/ DESTINATION ${CBFlib_BINARY_DIR}/bin FILES_MATCHING PATTERN "*pcre*") 
    add_definitions(-DCBF_REGEXLIB_PCRE)
 
  else (${CBF_REGEXLIB_INCLUDE_DIR} STREQUAL "CBF_REGEXLIB_INCLUDE_DIR-NOTFOUND" 
    OR ${CBF_REGEXLIB} STREQUAL "CBF_REGEXLIB-NOTFOUND")
    add_definitions(-DCBF_REGEXLIB_REGEX)
  endif (${CBF_REGEXLIB_INCLUDE_DIR} STREQUAL "CBF_REGEXLIB_INCLUDE_DIR-NOTFOUND" 
    OR ${CBF_REGEXLIB} STREQUAL "CBF_REGEXLIB-NOTFOUND")
else (${CBF_REGEXLIB_INCLUDE_DIR} STREQUAL "CBF_REGEXLIB_INCLUDE_DIR-NOTFOUND" 
    OR ${CBF_REGEXLIB} STREQUAL "CBF_REGEXLIB-NOTFOUND")
  add_definitions(-DCBF_REGEXLIB_PCRE)
endif (${CBF_REGEXLIB_INCLUDE_DIR} STREQUAL "CBF_REGEXLIB_INCLUDE_DIR-NOTFOUND" 
    OR ${CBF_REGEXLIB} STREQUAL "CBF_REGEXLIB-NOTFOUND")
CBF_DEBUG_MESSAGE("CBF_REGEXLIB: ${CBF_REGEXLIB}")
CBF_DEBUG_MESSAGE("CBF_REGEXLIB_INCLUDE_DIR: ${CBF_REGEXLIB_INCLUDE_DIR}")

