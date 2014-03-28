#! /bin/bash

# by J. Sloan of Diamond Light Source
# **********************************************************************
# *                                                                    *
# * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
# *                                                                    *
# * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
# * OF THE LGPL                                                        *
# *                                                                    *
# **********************************************************************
# *                                                                    *
# * This program is free software; you can redistribute it and/or      *
# * modify it under the terms of the GNU General Public License as     *
# * published by the Free Software Foundation; either version 2 of     *
# * (the License, or (at your option) any later version.               *
# *                                                                    *
# * This program is distributed in the hope that it will be useful,    *
# * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
# * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
# * GNU General Public License for more details.                       *
# *                                                                    *
# * You should have received a copy of the GNU General Public License  *
# * along with this program; if not, write to the Free Software        *
# * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           *
# * 02111-1307  USA                                                    *
# *                                                                    *
# **********************************************************************
# *                                                                    *
# * This library is free software; you can redistribute it and/or      *
# * modify it under the terms of the GNU Lesser General Public         *
# * License as published by the Free Software Foundation; either       *
# * version 2.1 of the License, or (at your option) any later version. *
# *                                                                    *
# * This library is distributed in the hope that it will be useful,    *
# * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
# * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
# * Lesser General Public License for more details.                    *
# *                                                                    *
# * You should have received a copy of the GNU Lesser General Public   *
# * License along with this library; if not, write to the Free         *
# * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    *
# * MA  02110-1301  USA                                                *
# *                                                                    *
# **********************************************************************

# This script controls documentaion extraction, pre-processing & construction.
# It should be called as './make-docs.sh [0|1]', where the argument defines
# the output mode and defaults to 0. An output mode of 0 is a 'compatibility
# mode' to generate output to be copied & pasted into CBFlib.html. An output
# mode of 1 is a 'readability mode' which adds some extra information and a
# few formatting improvements that I commonly use in HTML.

# This should be re-written in python to provide some more complex pre-processing.

if (( $# < 1 ))
then
	mode=0
else
	mode=$1
fi

echo "Running 'doxygen'..."
doxygen doc/src/config &>/dev/null
echo "Extracting data..."
xsltproc -o doc/doc.xml doc/src/doxygen2xml.xsl doc/src/data.xml
echo "processing escape sequences..."
sed -i 's/\\\*/\*/g;s/\\\//\//g' doc/doc.xml
echo "Indexing..."
xsltproc -o doc/doc.xml doc/src/index.xsl doc/doc.xml
echo "Generating HTML..."
xsltproc --stringparam mode $mode -o doc/doc.html doc/src/xml2html.xsl doc/src/data.xml
echo "Done."

