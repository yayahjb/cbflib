<!--
by J. Sloan of Diamond Light Source
 **********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
 **********************************************************************
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * (the License, or (at your option) any later version.               *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           *
 * 02111-1307  USA                                                    *
 *                                                                    *
 **********************************************************************
 *                                                                    *
 * This library is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU Lesser General Public         *
 * License as published by the Free Software Foundation; either       *
 * version 2.1 of the License, or (at your option) any later version. *
 *                                                                    *
 * This library is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
 * Lesser General Public License for more details.                    *
 *                                                                    *
 * You should have received a copy of the GNU Lesser General Public   *
 * License along with this library; if not, write to the Free         *
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    *
 * MA  02110-1301  USA                                                *
 *                                                                    *
 **********************************************************************
-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="html" version="4.0" encoding="iso-8859-1" indent="yes"/>
	<!--
	mode:
	0 compatibility mode for use when generating documenation to be pasted into 'doc/CBFlib.html'.
	1 readability mode which includes some more information and better formatting of ToC-type lists.
	-->
	<xsl:variable name="mode" select="1"/>
	<xsl:include href="function.xsl"/>
	
	<!-- Try to simplify the output a little bit -->
	<xsl:strip-space elements="*"/>

	<!-- identity template -->
	<xsl:template match="node()|@*">
		<xsl:copy>
			<xsl:apply-templates select="node()|@*"/>
		</xsl:copy>
	</xsl:template>

	<!-- remove empty paragraphs -->
	<xsl:template match="p[not(*) and not(text()[normalize-space()])]"/>

	<!--
	Generate a (very basic) table of contents from eveything in the current group.
	The current grop does not need to correspond to the entire section of the documentation,
	it can be a subset of a section like 'cbf_H5G*' functions, allowing for fine-grained
	lists of cross-links without preventing the entire section from being listed.
	-->
	<xsl:template name="mini-ToC-link">
		<li>
			<xsl:if test="$mode &gt;= 1"><xsl:attribute name="class"><xsl:text>mini-ToC</xsl:text></xsl:attribute></xsl:if>
			<a>
				<xsl:attribute name="href">
					<xsl:text>#</xsl:text>
					<xsl:value-of select="index"/>
				</xsl:attribute>
				<xsl:choose>
					<xsl:when test="$mode &gt;= 1">
						<code><xsl:value-of select="name"/></code>
						<span class="index" style="float:right;"><xsl:value-of select="index"/></span>
					</xsl:when>
					<xsl:otherwise>
						<xsl:value-of select="index"/>
						<xsl:text> </xsl:text>
						<xsl:value-of select="name"/>
					</xsl:otherwise>
				</xsl:choose>
			</a>
		</li>
	</xsl:template>

	<xsl:template match="/">
		<html>
			<head>
				<title>CBFlib Documentation</title>
				<link href="style.css" rel="stylesheet"/>
			</head>
			<body>
				<div id="2.6">
					<h2>2.6 HDF5 abstraction layer and convenience functions</h2>
					<!-- Desciption of section -->
					<xsl:copy-of select="document('2.6.xml')"/>
					<!-- List of what the section contains -->
					<p>This section describes functions available for working with:</p>
					<ul class="mini-ToC">
						<li>
							<strong>Attributes:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='H5A']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
						<li>
							<strong>Datasets</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='H5D']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
						<li>
							<strong>Files:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='H5F']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
						<li>
							<strong>Groups:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='H5G']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
						<li>
							<strong>Identifiers:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='H5I']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
						<li>
							<strong>Objects:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='H5O']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
						<li>
							<strong>Dataspaces:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='H5S']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
						<li>
							<strong>Datatypes:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='H5T']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
					</ul>
					<!-- High-level desciption of major parts/ideas -->
					<xsl:copy-of select="document('2.6-dataset-rank.xml')"/>
					<xsl:copy-of select="document('2.6-hdf5-datatypes.xml')"/>
					<xsl:copy-of select="document('2.6-comparing-data.xml')"/>
					<!-- Function documentation -->
					<xsl:for-each select="/functions/function[ToC-group/text()='H5A']"><hr/><xsl:call-template name="function"/></xsl:for-each>
					<xsl:for-each select="/functions/function[ToC-group/text()='H5D']"><hr/><xsl:call-template name="function"/></xsl:for-each>
					<xsl:for-each select="/functions/function[ToC-group/text()='H5F']"><hr/><xsl:call-template name="function"/></xsl:for-each>
					<xsl:for-each select="/functions/function[ToC-group/text()='H5G']"><hr/><xsl:call-template name="function"/></xsl:for-each>
					<xsl:for-each select="/functions/function[ToC-group/text()='H5I']"><hr/><xsl:call-template name="function"/></xsl:for-each>
					<xsl:for-each select="/functions/function[ToC-group/text()='H5O']"><hr/><xsl:call-template name="function"/></xsl:for-each>
					<xsl:for-each select="/functions/function[ToC-group/text()='H5S']"><hr/><xsl:call-template name="function"/></xsl:for-each>
					<xsl:for-each select="/functions/function[ToC-group/text()='H5T']"><hr/><xsl:call-template name="function"/></xsl:for-each>
				</div>
				<hr/>
				<div id="2.7">
					<h2>2.7 High-level NeXus-related functions</h2>
					<!-- Desciption of section -->
					<xsl:copy-of select="document('2.7.xml')"/>
					<!-- List of what the section contains -->
					<p>This section describes functions for working with:</p>
					<ul class="mini-ToC">
						<li>
							<strong>CBF's HDF5 handles:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='H5Handle']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
						<li>
							<strong>miniCBF configuration settings:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='minicbf-config']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
					</ul>
					<!-- High-level desciption of major parts/ideas -->
					<xsl:copy-of select="document('2.7-minicbf-config.xml')"/>
					<!-- Function documentation -->
					<xsl:for-each select="/functions/function[ToC-group/text()='H5Handle']"><hr/><xsl:call-template name="function"/></xsl:for-each>
					<xsl:for-each select="/functions/function[ToC-group/text()='minicbf-config']"><hr/><xsl:call-template name="function"/></xsl:for-each>
				</div>
				<hr/>
				<div id="2.8">
					<h2>2.8 Miscellaneous utility functions</h2>
					<!-- Desciption of section -->
					<xsl:copy-of select="document('2.8.xml')"/>
					<!-- List of what the section contains -->
					<p>This section describes functions for working with:</p>
					<ul class="mini-ToC">
						<li>
							<strong>CBF's getopt functions:</strong>
							<ul><xsl:for-each select="/functions/function[ToC-group/text()='getopt']"><xsl:call-template name="mini-ToC-link"/></xsl:for-each></ul>
						</li>
					</ul>
					<!-- High-level desciption of major parts/ideas -->
					<xsl:copy-of select="document('2.8-getopt.xml')"/>
					<!-- Function documentation -->
					<xsl:for-each select="/functions/function[ToC-group/text()='getopt']"><hr/><xsl:call-template name="function"/></xsl:for-each>
				</div>
			</body>
		</html>
	</xsl:template>

</xsl:stylesheet>

