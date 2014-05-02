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
	<xsl:output method="xml" version="1.0" encoding="iso-8859-1" indent="yes"/>

	<!-- Try to simplify the output a little bit -->
	<xsl:strip-space elements="*"/>

	<!-- Note: no params matched on 'sectiondef' or 'memberdef' here -->
	<xsl:template match="/">
		<functions>
			<xsl:for-each select="document('../../doxygen/xml/group__section__HDF5__H5A.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">H5A</xsl:with-param>
					<xsl:with-param name="index-group">2.6</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__section__HDF5__H5D.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">H5D</xsl:with-param>
					<xsl:with-param name="index-group">2.6</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__section__HDF5__H5F.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">H5F</xsl:with-param>
					<xsl:with-param name="index-group">2.6</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__section__HDF5__H5G.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">H5G</xsl:with-param>
					<xsl:with-param name="index-group">2.6</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__section__HDF5__H5I.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">H5I</xsl:with-param>
					<xsl:with-param name="index-group">2.6</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__section__HDF5__H5O.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">H5O</xsl:with-param>
					<xsl:with-param name="index-group">2.6</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__section__HDF5__H5S.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">H5S</xsl:with-param>
					<xsl:with-param name="index-group">2.6</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__section__HDF5__H5T.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">H5T</xsl:with-param>
					<xsl:with-param name="index-group">2.6</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__section__H5Handle.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">H5Handle</xsl:with-param>
					<xsl:with-param name="index-group">2.7</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__section__minicbf__config.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">minicbf-config</xsl:with-param>
					<xsl:with-param name="index-group">2.7</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
			<xsl:for-each select="document('../../doxygen/xml/group__getopt.xml')/doxygen/compounddef/sectiondef[@kind='func']/memberdef[@kind='function']">
				<xsl:call-template name="extract-function" select=".">
					<xsl:with-param name="ToC-group">getopt</xsl:with-param>
					<xsl:with-param name="index-group">2.8</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
		</functions>
	</xsl:template>

	<!-- suppress output from some elements -->
	<xsl:template match="parameterlist"/>
	<xsl:template match="simplesect[@kind='return' or @kind='see']"/>

	<!-- Convert some elements to appropriate html markup. -->
	<xsl:template match="itemizedlist"><ul><xsl:apply-templates select="node()"/></ul></xsl:template>
	<xsl:template match="orderedlist"><ol><xsl:apply-templates select="node()"/></ol></xsl:template>
	<xsl:template match="listitem"><li><xsl:apply-templates select="node()/node()"/></li></xsl:template>
	<xsl:template match="preformatted"><pre><xsl:apply-templates select="node()"/></pre></xsl:template>
	<xsl:template match="computeroutput"><code><xsl:apply-templates select="node()"/></code></xsl:template>
	<xsl:template match="bold"><strong><xsl:apply-templates select="node()"/></strong></xsl:template>
	<xsl:template match="emphasis"><em><xsl:apply-templates select="node()"/></em></xsl:template>
	<xsl:template match="linebreak"><br/></xsl:template>
	<xsl:template match="para">
		<xsl:apply-templates select="computeroutput[preformatted]"/>
		<xsl:apply-templates select="itemizedlist"/>
		<xsl:apply-templates select="orderedlist"/>
		<p>
			<xsl:apply-templates select="node()[not(self::computeroutput[preformatted]) and not(self::itemizedlist) and not(self::orderedlist)]|@*"/>
		</p>
	</xsl:template>

	<!-- extract a filename from a full unix path -->
	<xsl:template name="extract-filename">
		<xsl:param name="str"/>
		<xsl:choose>
			<xsl:when test="contains($str,'/')">
				<xsl:call-template name="extract-filename">
					<xsl:with-param name="str" select="substring-after($str,'/')"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$str"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- Note: both params matched on 'sectiondef' or 'memberdef' here -->
	<xsl:template name="extract-function">
		<xsl:param name="ToC-group"/>
		<xsl:param name="index-group"/>
		<function>
			<xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
			<ToC-group><xsl:value-of select="$ToC-group"/></ToC-group>
			<index-group><xsl:value-of select="$index-group"/></index-group>
			<type><xsl:value-of select="type"/></type>
			<name><xsl:value-of select="name"/></name>
			<signature>
				<xsl:value-of select="definition"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="argsstring"/>
			</signature>
			<location>
				<xsl:call-template name="extract-filename">
					<xsl:with-param name="str" select="location/@file"/>
				</xsl:call-template>
			</location>
			<return><xsl:value-of select="detaileddescription/*/simplesect[@kind='return']"/></return>
			<params>
				<xsl:for-each select="param">
					<param>
						<xsl:variable name="name" select="string(declname)"/>
						<type><xsl:value-of select="type"/></type>
						<name><xsl:value-of select="$name"/></name>
						<desc>
							<xsl:choose>
								<xsl:when test="briefdescription">
									<xsl:apply-templates select="briefdescription"/>
								</xsl:when>
								<xsl:otherwise>
									<xsl:apply-templates select="../detaileddescription/*/parameterlist/*/parameternamelist[parametername=$name]/../parameterdescription/para"/>
								</xsl:otherwise>
							</xsl:choose>
						</desc>
					</param>
				</xsl:for-each>
			</params>
			<brief>
				<xsl:apply-templates select="briefdescription/*"/>
			</brief>
			<detail>
				<xsl:apply-templates select="detaileddescription/*"/>
			</detail>
			<seealso>
				<xsl:for-each select="detaileddescription/*/simplesect[@kind='see']//ref[@kindref='member']">
					<ref id="{@refid}"><xsl:value-of select="."/></ref>
				</xsl:for-each>
			</seealso>
		</function>
	</xsl:template>

</xsl:stylesheet>
