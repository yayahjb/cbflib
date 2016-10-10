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

	<!--
	Generate HTML documentation for a single function.
	
	The 'mode' variable controls exactly what is generated:
	- 0: generate markup that should be trivially copyable into CBFlib.html
	- 1: generate markup containing more information, ie the type of argument in tables of arguments.
	-->
	<xsl:template name="function">
		<div class="function">
			<xsl:attribute name="id">
				<xsl:value-of select="index"/>
			</xsl:attribute>
			<h4>
				<xsl:value-of select="index"/>
				<xsl:text> </xsl:text>
				<xsl:choose>
					<xsl:when test="$mode &gt;= 1"><code><xsl:value-of select="name"/></code></xsl:when>
					<xsl:otherwise><xsl:value-of select="name"/></xsl:otherwise>
				</xsl:choose>
			</h4>
			<p><xsl:value-of select="brief"/></p>
			<p><strong>PROTOTYPE</strong></p>
			<p><code>#include "<xsl:value-of select="location"/>"</code><br/>
			<code><xsl:value-of select="signature"/></code></p>
			<xsl:if test="count(detail/node()) &gt; 0">
				<p><strong>DESCRIPTION</strong></p>
				<xsl:apply-templates select="detail/node()"/>
			</xsl:if>
			<p><strong>ARGUMENTS</strong></p>
			<xsl:choose>
 				<xsl:when test="params/param">
					<!--
					Extract a list of parameters for the current function and put them in a table with sufficiently
					detailed class attributes to make styling really simple. Type could be added later, but I don't
					know if the SWIG generator will work correctly with it.
					-->
					<table class="params">
						<xsl:if test="$mode &gt;= 1">
							<tr style="background:none;">
								<th style="border-top-left-radius:5px;">Type</th>
								<th>Name</th>
								<th style="border-top-right-radius:5px;">Description</th>
							</tr>
						</xsl:if>
						<xsl:for-each select="params/param">
							<tr>
								<xsl:if test="$mode &gt;= 1">
									<td class="type"><xsl:value-of select="type"/></td>
								</xsl:if>
								<td class="name"><xsl:value-of select="name"/></td>
								<td class="desc"><xsl:apply-templates select="desc/node()"/></td>
							</tr>
						</xsl:for-each>
					</table>
 				</xsl:when>
 				<xsl:otherwise>
 					<p>This function takes no arguments.</p>
 				</xsl:otherwise>
 			</xsl:choose>
			<xsl:if test="count(return/node()) &gt; 0">
				<p><strong>RETURN VALUE</strong></p>
				<p><xsl:apply-templates select="return/node()"/></p>
			</xsl:if>
			<xsl:if test="count(seealso/ref) &gt; 0">
				<!--
				If I have an explicit list of things that should be referenced then use it.
				I can add a working link if I have a valid reference to the documentation for the item,
				otherwise I need to add only the text part of the reference.
				-->
				<p><strong>SEE ALSO</strong></p>
				<ul class="see-also">
					<xsl:variable name="self" select="@id"/>
					<xsl:for-each select="seealso/ref">
						<xsl:variable name="id" select="@id"/>
						<xsl:variable name="other" select="/functions/function[@id=$id]"/>
						<xsl:if test="$id != $self">
							<xsl:choose>
								<xsl:when test="$other">
									<li>
										<a>
											<xsl:attribute name="href">
												<xsl:text>#</xsl:text>
												<xsl:value-of select="$other/index"/>
											</xsl:attribute>
											<xsl:value-of select="$other/index"/>
											<xsl:text> </xsl:text>
											<xsl:choose>
												<xsl:when test="$mode &gt;= 1"><code><xsl:value-of select="$other/name"/></code></xsl:when>
												<xsl:otherwise><xsl:value-of select="$other/name"/></xsl:otherwise>
											</xsl:choose>
										</a>
									</li>
								</xsl:when>
								<xsl:otherwise>
									<li><xsl:value-of select="."/></li>
								</xsl:otherwise>
							</xsl:choose>
						</xsl:if>
					</xsl:for-each>
				</ul>
			</xsl:if>
		</div>
	</xsl:template>
</xsl:stylesheet>

