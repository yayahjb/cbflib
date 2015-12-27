/**********************************************************************
 * cbf_airy_disk.c -- functions for airy-disk point spread            *
 *                                                                    *
 * ccbf_airy_disk.c version 1                                         *
 *                                                                    *
 * Kaden Badalian and Herbert J. Bernstein                            *
 * CBFlib Version 0.9.5.4 21 June 2015                                *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2015 Herbert J. Bernstein                            *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
 **********************************************************************/

/*************************** GPL NOTICES ******************************
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
 **********************************************************************/

/************************* LGPL NOTICES *******************************
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
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 *                    Stanford University Notices                     *
 *  for the CBFlib software package that incorporates SLAC software   *
 *                 on which copyright is disclaimed                   *
 *                                                                    *
 * This software                                                      *
 * -------------                                                      *
 * The term "this software", as used in these Notices, refers to      *
 * those portions of the software package CBFlib that were created by *
 * employees of the Stanford Linear Accelerator Center, Stanford      *
 * University.                                                        *
 *                                                                    *
 * Stanford disclaimer of copyright                                   *
 * --------------------------------                                   *
 * Stanford University, owner of the copyright, hereby disclaims its  *
 * copyright and all other rights in this software.  Hence, anyone    *
 * may freely use it for any purpose without restriction.             *
 *                                                                    *
 * Acknowledgement of sponsorship                                     *
 * ------------------------------                                     *
 * This software was produced by the Stanford Linear Accelerator      *
 * Center, Stanford University, under Contract DE-AC03-76SFO0515 with *
 * the Department of Energy.                                          *
 *                                                                    *
 * Government disclaimer of liability                                 *
 * ----------------------------------                                 *
 * Neither the United States nor the United States Department of      *
 * Energy, nor any of their employees, makes any warranty, express or *
 * implied, or assumes any legal liability or responsibility for the  *
 * accuracy, completeness, or usefulness of any data, apparatus,      *
 * product, or process disclosed, or represents that its use would    *
 * not infringe privately owned rights.                               *
 *                                                                    *
 * Stanford disclaimer of liability                                   *
 * --------------------------------                                   *
 * Stanford University makes no representations or warranties,        *
 * express or implied, nor assumes any liability for the use of this  *
 * software.                                                          *
 *                                                                    *
 * Maintenance of notices                                             *
 * ----------------------                                             *
 * In the interest of clarity regarding the origin and status of this *
 * software, this and all the preceding Stanford University notices   *
 * are to remain affixed to any copy or derivative of this software   *
 * made or distributed by the recipient and are to be affixed to any  *
 * copy of software made or distributed by the recipient that         *
 * contains a copy or derivative of this software.                    *
 *                                                                    *
 * Based on SLAC Software Notices, Set 4                              *
 * OTT.002a, 2004 FEB 03                                              *
 **********************************************************************/



/**********************************************************************
 *                               NOTICE                               *
 * Creative endeavors depend on the lively exchange of ideas. There   *
 * are laws and customs which establish rights and responsibilities   *
 * for authors and the users of what authors create.  This notice     *
 * is not intended to prevent you from using the software and         *
 * documents in this package, but to ensure that there are no         *
 * misunderstandings about terms and conditions of such use.          *
 *                                                                    *
 * Please read the following notice carefully.  If you do not         *
 * understand any portion of this notice, please seek appropriate     *
 * professional legal advice before making use of the software and    *
 * documents included in this software package.  In addition to       *
 * whatever other steps you may be obliged to take to respect the     *
 * intellectual property rights of the various parties involved, if   *
 * you do make use of the software and documents in this package,     *
 * please give credit where credit is due by citing this package,     *
 * its authors and the URL or other source from which you obtained    *
 * it, or equivalent primary references in the literature with the    *
 * same authors.                                                      *
 *                                                                    *
 * Some of the software and documents included within this software   *
 * package are the intellectual property of various parties, and      *
 * placement in this package does not in any way imply that any       *
 * such rights have in any way been waived or diminished.             *
 *                                                                    *
 * With respect to any software or documents for which a copyright    *
 * exists, ALL RIGHTS ARE RESERVED TO THE OWNERS OF SUCH COPYRIGHT.   *
 *                                                                    *
 * Even though the authors of the various documents and software      *
 * found here have made a good faith effort to ensure that the        *
 * documents are correct and that the software performs according     *
 * to its documentation, and we would greatly appreciate hearing of   *
 * any problems you may encounter, the programs and documents any     *
 * files created by the programs are provided **AS IS** without any   *
 * warranty as to correctness, merchantability or fitness for any     *
 * particular or general use.                                         *
 *                                                                    *
 * THE RESPONSIBILITY FOR ANY ADVERSE CONSEQUENCES FROM THE USE OF    *
 * PROGRAMS OR DOCUMENTS OR ANY FILE OR FILES CREATED BY USE OF THE   *
 * PROGRAMS OR DOCUMENTS LIES SOLELY WITH THE USERS OF THE PROGRAMS   *
 * OR DOCUMENTS OR FILE OR FILES AND NOT WITH AUTHORS OF THE          *
 * PROGRAMS OR DOCUMENTS.                                             *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 *                           The IUCr Policy                          *
 *      for the Protection and the Promotion of the STAR File and     *
 *     CIF Standards for Exchanging and Archiving Electronic Data     *
 *                                                                    *
 * Overview                                                           *
 *                                                                    *
 * The Crystallographic Information File (CIF)[1] is a standard for   *
 * information interchange promulgated by the International Union of  *
 * Crystallography (IUCr). CIF (Hall, Allen & Brown, 1991) is the     *
 * recommended method for submitting publications to Acta             *
 * Crystallographica Section C and reports of crystal structure       *
 * determinations to other sections of Acta Crystallographica         *
 * and many other journals. The syntax of a CIF is a subset of the    *
 * more general STAR File[2] format. The CIF and STAR File approaches *
 * are used increasingly in the structural sciences for data exchange *
 * and archiving, and are having a significant influence on these     *
 * activities in other fields.                                        *
 *                                                                    *
 * Statement of intent                                                *
 *                                                                    *
 * The IUCr's interest in the STAR File is as a general data          *
 * interchange standard for science, and its interest in the CIF,     *
 * a conformant derivative of the STAR File, is as a concise data     *
 * exchange and archival standard for crystallography and structural  *
 * science.                                                           *
 *                                                                    *
 * Protection of the standards                                        *
 *                                                                    *
 * To protect the STAR File and the CIF as standards for              *
 * interchanging and archiving electronic data, the IUCr, on behalf   *
 * of the scientific community,                                       *
 *                                                                    *
 * * holds the copyrights on the standards themselves,                *
 *                                                                    *
 * * owns the associated trademarks and service marks, and            *
 *                                                                    *
 * * holds a patent on the STAR File.                                 *
 *                                                                    *
 * These intellectual property rights relate solely to the            *
 * interchange formats, not to the data contained therein, nor to     *
 * the software used in the generation, access or manipulation of     *
 * the data.                                                          *
 *                                                                    *
 * Promotion of the standards                                         *
 *                                                                    *
 * The sole requirement that the IUCr, in its protective role,        *
 * imposes on software purporting to process STAR File or CIF data    *
 * is that the following conditions be met prior to sale or           *
 * distribution.                                                      *
 *                                                                    *
 * * Software claiming to read files written to either the STAR       *
 * File or the CIF standard must be able to extract the pertinent     *
 * data from a file conformant to the STAR File syntax, or the CIF    *
 * syntax, respectively.                                              *
 *                                                                    *
 * * Software claiming to write files in either the STAR File, or     *
 * the CIF, standard must produce files that are conformant to the    *
 * STAR File syntax, or the CIF syntax, respectively.                 *
 *                                                                    *
 * * Software claiming to read definitions from a specific data       *
 * dictionary approved by the IUCr must be able to extract any        *
 * pertinent definition which is conformant to the dictionary         *
 * definition language (DDL)[3] associated with that dictionary.      *
 *                                                                    *
 * The IUCr, through its Committee on CIF Standards, will assist      *
 * any developer to verify that software meets these conformance      *
 * conditions.                                                        *
 *                                                                    *
 * Glossary of terms                                                  *
 *                                                                    *
 * [1] CIF:  is a data file conformant to the file syntax defined     *
 * at http://www.iucr.org/iucr-top/cif/spec/index.html                *
 *                                                                    *
 * [2] STAR File:  is a data file conformant to the file syntax       *
 * defined at http://www.iucr.org/iucr-top/cif/spec/star/index.html   *
 *                                                                    *
 * [3] DDL:  is a language used in a data dictionary to define data   *
 * items in terms of "attributes". Dictionaries currently approved    *
 * by the IUCr, and the DDL versions used to construct these          *
 * dictionaries, are listed at                                        *
 * http://www.iucr.org/iucr-top/cif/spec/ddl/index.html               *
 *                                                                    *
 * Last modified: 30 September 2000                                   *
 *                                                                    *
 * IUCr Policy Copyright (C) 2000 International Union of              *
 * Crystallography                                                    *
 **********************************************************************/

#ifdef __cplusplus

extern "C" {
    
#endif
    
#define CBF_AIRY_DISK_C
#include "cbf_airy_disk.h"
    
#ifndef CBF_ARGUMENT
#define CBF_ARGUMENT 4
#endif
    
    const double cbf_airy_unit_disk_partvol_1D[20] =
    {   1.426420856518437e-4, 0.001836262121580318,
        0.00835666373463087,  0.02443516191841331, 0.05536596434951432,
        0.1057021433691522,   0.1778607627549379,  0.2710544112372565,
        0.3809159889912883,   0.5,                 0.6190840110087124,
        0.728945588762744,    0.8221392372450625,  0.8942978566308483,
        0.9446340356504864,   0.9755648380815874,  0.9916433362653698,
        0.9981637378784199,   0.9998573579143488,  1.0};
    
    const double cbf_airy_unit_disk_partvol_2D[20][20] =
    {/*  0*/{0.00346705051,0.00687093888,0.01015137498,0.01325363,0.01613087439,
        0.01874602235,0.0210729779,0.02309722219,0.02481572954,0.0262362476,
        0.02737602025,0.028260067814,0.02891916459,0.029387667305,0.029701349023,
        0.029895382025,0.030002591544,0.0300520725245,0.0300682266117,0.03007023969885},
     /*  1*/{0.00687093888,0.01361651492,0.02011707249,0.02626400701,0.03196440993,
            0.03714473227,0.04175325815,0.04576126834,0.04916286917,0.05197355744,
            0.05422767792,0.055975001591,0.057276703239,0.058201044041,0.058819066573,
            0.059200587596,0.059410730873,0.059507183065,0.059538286112,0.05954200547876},
     /*  2*/{0.01015137498,0.02011707249,0.02971988508,0.03879900525,0.04721691669,
            0.05486482696,0.06166633187,0.06757913356,0.07259477623,0.0767365049,
            0.08005548073,0.082625692379,0.084537978127,0.085893613208,0.086797919453,
            0.087354321149,0.0876592067765,0.0877978679698,0.0878416833722,0.0878465959026},
     /*  3*/{0.01325363,0.02626400701,0.03879900525,0.05064804234,0.06163112817,
            0.07160599402,0.08047289452,0.08817684888,0.0947072748,0.10009515475,
            0.104408043237,0.107743362885,0.110220536166,0.111972551582,0.113137566164,
            0.113851102023,0.1142393096179,0.1144136536852,0.11446724090183,0.114472794345986},
     /*  4*/{0.01613087439,0.03196440993,0.04721691669,0.06163112817,0.07498729427,
            0.08711191373,0.09788360577,0.10723583456,0.11515643076,0.12168408346,
            0.126902183716,0.130930572083,0.133915863969,0.136021090055,0.137415392204,
            0.138264460281,0.1387222899152,0.1389246969786,0.13898485561242,0.138990629865132},
     /*  5*/{0.01874602235,0.03714473227,0.05486482696,0.07160599402,0.08711191373,
            0.10118048454,0.11367068402,0.12450572989,0.13367247743,0.14121725945,
            0.14723861975,0.151877592345,0.155306321045,0.157715888108,0.159304223433,
            0.160264900067,0.1607774967045,0.1610000371,0.16106381668609,0.161069618583945},
     /*  6*/{0.0210729779,0.04175325815,0.06166633187,0.08047289452,0.09788360577,
            0.11367068402,0.1276756668,0.13981295657,0.15006908182,0.158497911592,
            0.165212340699,0.170373192337,0.174176246495,0.176838386239,0.17858385588,
            0.1796315488013,0.1801840986497,0.1804193515579,0.18048456232083,0.180490364218685},
    /*  7*/{0.02309722219,0.04576126834,0.06757913356,0.08817684888,0.10723583456,
            0.12450572989,0.13981295657,0.15306459433,0.164247494023,0.173422897088,
            0.180717141216,0.186309287366,0.190416682607,0.193279565331,0.195145820046,
            0.1962569023884,0.1968357927859,0.19707761724866,0.197143126330647,0.197148928228502},
    /*  8*/{0.02481572954,0.04916286917,0.07259477623,0.0947072748,0.11515643076,
            0.13367247743,0.15006908182,0.164247494023,0.176195501882,0.185981489282,
            0.193744236426,0.199679379223,0.204023640027,0.207038041256,0.2089913123677,
            0.210144603992,0.2107384436316,0.21098262459778,0.211048143691914,0.211053945589769},
    /*  9*/{0.0262362476,0.05197355744,0.0767365049,0.10009515475,0.12168408346,
            0.14121725945,0.158497911592,0.173422897088,0.185981489282,0.196248911791,
            0.204375311535,0.210571162955,0.215090304768,0.218211915893,0.2202227339766,
            0.2214007134883,0.22200112478044,0.222245696747985,0.222311215842119,0.222317017739974},
    /* 10*/{0.02737602025,0.05422767792,0.08005548073,0.104408043237,0.126902183716,
            0.14723861975,0.165212340699,0.180717141216,0.193744236426,0.204375311535,
            0.212770748181,0.219154088005,0.223794015021,0.2269852484838,0.2290297321883,
            0.2302193902632,0.23082150461232,0.231066076579865,0.231131595673999,0.231137397571854},
    /* 11*/{0.028260067814,0.055975001591,0.082625692379,0.107743362885,0.130930572083,
            0.151877592345,0.170373192337,0.186309287366,0.199679379223,0.210571162955,
            0.219154088005,0.225662995349,0.2303791835435,0.2336103704434,0.2356710086544,
            0.2368642879497,0.237466520025906,0.237711091993451,0.237776611087585,0.23778241298544},
    /* 12*/{0.02891916459,0.057276703239,0.084537978127,0.110220536166,0.133915863969,
            0.155306321045,0.174176246495,0.190416682607,0.204023640027,0.215090304768,
            0.223794015021,0.2303791835435,0.2351375811481,0.238387513302,0.24045340997332,
            0.241647079150478,0.242249311226684,0.242493883194229,0.242559402288363,0.242565204186218},
    /* 13*/{0.029387667305,0.058201044041,0.085893613208,0.111972551582,0.136021090055,
            0.157715888108,0.176838386239,0.193279565331,0.207038041256,0.218211915893,
            0.2269852484838,0.2336103704434,0.238387513302,0.24164333763812,0.243709867436562,
            0.24490353661372,0.245505768689926,0.245750340657471,0.245815859751605,0.24582166164946},
    /* 14*/{0.029701349023,0.058819066573,0.086797919453,0.113137566164,0.137415392204,
            0.159304223433,0.17858385588,0.195145820046,0.2089913123677,0.2202227339766,
            0.2290297321883,0.2356710086544,0.24045340997332,0.243709867436562,0.245776397235004,
            0.246970066412162,0.247572298488368,0.247816870455913,0.247882389550047,0.247888191447902},
    /* 15*/{0.029895382025,0.059200587596,0.087354321149,0.113851102023,0.138264460281,
            0.160264900067,0.1796315488013,0.1962569023884,0.210144603992,0.2214007134883,
            0.2302193902632,0.2368642879497,0.241647079150478,0.24490353661372,0.246970066412162,
            0.24816373558932,0.248765967665526,0.249010539633071,0.249076058727205,0.24908186062506},
    /* 16*/{0.030002591544,0.059410730873,0.0876592067765,0.1142393096179,0.1387222899152,
            0.1607774967045,0.1801840986497,0.1968357927859,0.2107384436316,0.22200112478044,
            0.23082150461232,0.237466520025906,0.242249311226684,0.245505768689926,0.247572298488368,
            0.248765967665526,0.249368199741732,0.249612771709277,0.249678290803411,0.249684092701266},
    /* 17*/{0.0300520725245,0.059507183065,0.0877978679698,0.1144136536852,0.1389246969786,
            0.1610000371,0.1804193515579,0.19707761724866,0.21098262459778,0.222245696747985,
            0.231066076579865,0.237711091993451,0.242493883194229,0.245750340657471,0.247816870455913,
            0.249010539633071,0.249612771709277,0.249857343676822,0.249922862770956,0.249928664668811},
    /* 18*/{0.0300682266117,0.059538286112,0.0878416833722001,0.11446724090183,0.13898485561242,
            0.16106381668609,0.18048456232083,0.197143126330647,0.211048143691914,0.222311215842119,
            0.231131595673999,0.237776611087585,0.242559402288363,0.245815859751605,0.247882389550047,
            0.249076058727205,0.249678290803411,0.249922862770956,0.249988381865091,0.249994183762946},
    /* 19*/{0.03007023969885,0.05954200547876,0.0878465959026001,0.114472794345986,0.138990629865132,
            0.161069618583945,0.180490364218685,0.197148928228502,0.211053945589769,0.222317017739974,
            0.231137397571854,0.23778241298544,0.242565204186218,0.24582166164946,0.247888191447902,
            0.24908186062506,0.249684092701266,0.249928664668811,0.249994183762946,0.2499999856608}};

    
    /* compute the value of a normalized Airy disk to the first
     minimum, with that minimum at r=1, a volume of 1
     and a fwhm of 0.8434376803.  This is a series approximation good
     to 8 decimal places.  The value is forced to zero outside
     the unit disk */
    
    int cbf_airy_unit_disk(double x, double y, double *value) {
        
        double r,r_2,r_3,r_4,r_5,r_6,r_7,r_8,r_9,r_10,r_11,r_12,r_13,r_14,r_15,r_16;
        double val;
        
        if (!value) return CBF_ARGUMENT;
        
        r_2 = x*x+y*y;
        r = sqrt(r_2);
        r_3 = r*r_2;
        r_4 = r_2*r_2;
        r_5 = r*r_4;
        r_6 = r_3*r_3;
        r_7 = r_3*r_4;
        r_8 = r_4*r_4;
        r_9 = r_4*r_5;
        r_10 = r_5*r_5;
        r_11 = r_5*r_6;
        r_12 = r_6*r_6;
        r_13 = r_6*r_7;
        r_14 = r_7*r_7;
        r_15 = r_7*r_8;
        r_16 = r_8*r_8;
        val = (r>1.)?0.:
        (-0.0145718909798132   *r_16
         +0.1128200913854083   *r_15
         -0.3251669103199633   *r_14
         +0.3415426671579541   *r_13
         +0.03830169403530381  *r_12
         +0.3036850720561378   *r_11
         -1.622663925343156    *r_10
         +0.1035599755468041   *r_9
         +3.658218851892756    *r_8
         +0.01440953571188533  *r_7
         -6.723054067684573    *r_6
         +7.828561594404555e-4 *r_5
         +7.84111524870691     *r_4
         +1.398176146475285e-5 *r_3
         -5.12432349713814     *r_2
         +5.077977353218757e-8 *r
         +1.395330318373548);
        
        *value = val;
        return 0;
    }
    
    /* Compute an airy disk for a specified fwhm and
       volume. */
    
    int cbf_airy_disk(double x, double y,
                      double cenx, double ceny,
                      double volume, double fwhm, double * value) {
        
        double scale, xoff, yoff;
        double val;
        int error;
        
        if (!value || fwhm <= 0. || volume <= 0.) return CBF_ARGUMENT;
        
        scale = fwhm/CBF_AIRY_UNIT_DISK_FWHM;
        xoff = (x-cenx)/scale;
        yoff = (y-ceny)/scale;
        
        error = cbf_airy_unit_disk(xoff,yoff,&val);
        if (error) return error;
        
        *value = val*volume/(scale*scale);
        
        return 0;
        
    }
    
    int cbf_airy_simpsons_rule_step(double xlo, double ylo,
                                    double xhi, double yhi,
                                    double * value){
        double value1,value2,value3,value4,value5,value6,
        value7,value8,value9,valueAvg,ymid,xmid;
        int error = 0;
        ymid = (ylo+yhi)/2.;
        xmid = (xlo+xhi)/2.;
        error |= cbf_airy_unit_disk(xlo,ylo,&value1);
        error |= cbf_airy_unit_disk(xlo,ymid,&value2);
        error |= cbf_airy_unit_disk(xlo,yhi,&value3);
        error |= cbf_airy_unit_disk(xmid,ylo,&value4);
        error |= cbf_airy_unit_disk(xmid,ymid,&value5);
        error |= cbf_airy_unit_disk(xmid,yhi,&value6);
        error |= cbf_airy_unit_disk(xhi,ylo,&value7);
        error |= cbf_airy_unit_disk(xhi,ymid,&value8);
        error |= cbf_airy_unit_disk(xhi,yhi,&value9);
        
        valueAvg = (value1+4.*value2+value3
                    +4.*value4+16.*value5+4.*value6
                    +value7+4.*value8+value9)/36.;
        if (value) *value = valueAvg*(xhi-xlo)*(yhi-ylo);
        return error;
    }
    
    int cbf_airy_unit_disk_volume(double xlo, double ylo,
                                  double xhi, double yhi,
                                  int steps, double * volume) {
        
        int stepx,stepy;
        double xstep, ystep;
        double xlos,ylos,xhis,yhis;
        double vol;
        int error=0;
        
        if (!volume) return CBF_ARGUMENT;
        
        xstep=(xhi-xlo)/((double)steps);
        ystep=(yhi-ylo)/((double)steps);
        
        *volume = 0.;
        
        for (stepx = 0; stepx < steps; stepx++) {
            xlos = xlo + xstep*((double)stepx);
            xhis = xlos + xstep;
            for (stepy = 0; stepy < steps; stepy++) {
                ylos = ylo + ystep*((double)stepy);
                yhis = ylos + ystep;
                error|=cbf_airy_simpsons_rule_step(xlos,ylos,xhis,yhis,&vol);
                *volume += vol;
            }
        }
        
        return 0;
        
    }
    
    static int cbf_airy_bin_big(double cutpoint) {
        if (cutpoint < -0.9) return 0;
        if (cutpoint >= 1.) return 20;
        return (int)((cutpoint+1.)*10.);
    }
    
    static double cbf_airy_bin_big_diff(double cutpoint,int bin) {
        return cutpoint+1. - ((double)(bin))/10.;
    }
    
    static int cbf_airy_bin_small(double cutpoint) {
        if (cutpoint < -0.95) return -20;
        if (cutpoint >= 1.) return 20;
        return -20+(int)((cutpoint+1.)*20.);
    }
    
    static double cbf_airy_bin_small_diff(double cutpoint,int bin) {
        return cutpoint+1. - ((double)(bin+20))/20.;
    }
    
    static double partvol_1D(int ii) {
        if (ii <= 0 ) return 0.;
        if (ii > 20) return 1.;
        return cbf_airy_unit_disk_partvol_1D[ii-1];
    }
    
    static double partvol_2D(int ii,int jj) {
        if (jj < -20) jj = -20;
        if (jj > 20) jj = 20;
        if (ii < -20) ii = -20;
        if (ii > 20) ii = 20;
        if (ii >= 0 && jj >= 0) {
            
            if (ii == 0) {
                if (jj == 0) return (0.25);
                return(0.25 + cbf_airy_unit_disk_partvol_2D[19][jj-1]);
            }
            if (jj == 0) {
                return(0.25 + cbf_airy_unit_disk_partvol_2D[ii-1][19]);
            }
            return(.25 + cbf_airy_unit_disk_partvol_2D[ii-1][jj-1]
                   + cbf_airy_unit_disk_partvol_2D[ii-1][19]
                   + cbf_airy_unit_disk_partvol_2D[19][jj-1]);
            
        } else if (ii < 0 && jj >= 0) {
            if (jj == 0) {
                return(0.25
                       -cbf_airy_unit_disk_partvol_2D[-ii-1][19]);
            }
            return((.25-cbf_airy_unit_disk_partvol_2D[-ii-1][19])
                   + (cbf_airy_unit_disk_partvol_2D[19][jj-1]
                      - cbf_airy_unit_disk_partvol_2D[-ii-1][jj-1]));
        } else if (ii >= 0 && jj < 0) {
            if (ii == 0) {
                return(0.25
                       -cbf_airy_unit_disk_partvol_2D[19][-jj-1]);
            }
            return((.25-cbf_airy_unit_disk_partvol_2D[19][-jj-1])
                   + (cbf_airy_unit_disk_partvol_2D[ii-1][19]
                   - cbf_airy_unit_disk_partvol_2D[ii-1][-jj-1]));
        } else return (.25+cbf_airy_unit_disk_partvol_2D[-ii-1][-jj-1]
                     -cbf_airy_unit_disk_partvol_2D[19][-jj-1]
                     -cbf_airy_unit_disk_partvol_2D[-ii-1][19]);
        
    }
    
    static double partvol_2D_real(int ii, int jj, double xdif, double ydif) {
        double vol00, vol01, vol10, vol11;
        double volout;
        vol00 = partvol_2D(ii,jj);
        vol01 = partvol_2D(ii+1,jj);
        vol10 = partvol_2D(ii,jj+1);
        vol11 = partvol_2D(ii+1,jj+1);
        volout = vol00
        +(vol01-vol00)*xdif*20.
        +(vol10-vol00)*ydif*20.
        +(vol11+vol00-vol01-vol10)*xdif*ydif*400;
        return volout;
    }
    
    
    int cbf_airy_disk_volume(double xlo, double ylo,
                             double xhi, double yhi,
                             double cenx, double ceny,
                             double volumein, double fwhm,
                             double * volumeout) {
        double scale, xofflo, yofflo, xoffhi, yoffhi;
        int icutxlo, icutxhi, icutylo, icutyhi;
        double diffxlo=0., diffxhi=0., diffylo=0., diffyhi=0.;
        
        if (!volumeout || fwhm <= 0.) {
            return CBF_ARGUMENT;
        }
        
        if (fabs(volumein) < 1.e-38) {
            *volumeout = 0.;
            return 0;
        }
                
        scale = fwhm/CBF_AIRY_UNIT_DISK_FWHM;
        xofflo = (xlo-cenx)/scale;
        yofflo = (ylo-ceny)/scale;
        xoffhi = (xhi-cenx)/scale;
        yoffhi = (yhi-ceny)/scale;
        if (xofflo <= -1. && yofflo <= -1.
            && xoffhi >= 1. && yoffhi >= 1. ) {
            /* return the full volumein */
            *volumeout = volumein;
            return 0;
        } else if ( xofflo <= -1. && xoffhi >= 1. ) {
            /* we need to form a slab in the y-direction */
            icutylo = cbf_airy_bin_big(yofflo);
            diffylo = cbf_airy_bin_big_diff(yofflo,icutylo);
            if (diffylo > 0.1) diffylo = 0.1;
            icutyhi = cbf_airy_bin_big(yoffhi);
            diffyhi = cbf_airy_bin_big_diff(yoffhi,icutyhi);
            if (diffyhi > 0.1) diffyhi = 0.1;
            *volumeout = volumein*
            ((partvol_1D(icutyhi)+diffyhi*10.*(partvol_1D(icutyhi+1)
                          -partvol_1D(icutyhi)))
            -(partvol_1D(icutylo)+diffylo*10.*(partvol_1D(icutylo+1)
                          -partvol_1D(icutylo))));
            return 0;
            
        } else if ( yofflo <= -1. && yoffhi >= 1. ) {
            /* we need to form a slab in the x-direction */
            icutxlo = cbf_airy_bin_big(xofflo);
            diffxlo = cbf_airy_bin_big_diff(xofflo,icutxlo);
            diffxlo = xofflo-1.-((double)icutxlo)/10.;
            if (diffxlo > 0.1) diffxlo = 0.1;
            icutxhi = cbf_airy_bin_big(xoffhi);
            diffxlo = cbf_airy_bin_big_diff(xoffhi,icutxhi);
            if (diffxhi > 0.1) diffxhi = 0.1;
            *volumeout = volumein*
            ((partvol_1D(icutxhi)+diffxhi*10.*(partvol_1D(icutxhi+1)
                           -partvol_1D(icutxhi)))
             -(partvol_1D(icutxlo)+diffxlo*10.*(partvol_1D(icutxlo+1)
                            -partvol_1D(icutxlo))));
            return 0;

        } else {
            double v1,v2,v3,v4;
            icutxlo = cbf_airy_bin_small(xofflo);
            diffxlo = cbf_airy_bin_small_diff(xofflo,icutxlo);
            if (diffxlo > 0.05) diffxlo = 0.05;
            icutxhi = cbf_airy_bin_small(xoffhi);
            diffxhi = cbf_airy_bin_small_diff(xoffhi,icutxhi);
            if (diffxhi > 0.05) diffxhi = 0.05;
            icutylo = cbf_airy_bin_small(yofflo);
            diffylo = cbf_airy_bin_small_diff(yofflo,icutylo);
            if (diffylo > 0.05) diffylo = 0.05;
            icutyhi = cbf_airy_bin_small(yoffhi);
            diffyhi = cbf_airy_bin_small_diff(yoffhi,icutyhi);
            if (diffyhi > 0.05) diffyhi = 0.05;
            
            v1 = partvol_2D_real(icutxlo,icutylo,diffxlo,diffylo);
            v2 = partvol_2D_real(icutxhi,icutylo,diffxhi,diffylo);
            v3 = partvol_2D_real(icutxlo,icutyhi,diffxlo,diffyhi);
            v4 = partvol_2D_real(icutxhi,icutyhi,diffxhi,diffyhi);
            
            *volumeout = volumein*(v1+v4-v2-v3);
            return 0;
            
        }
        
    }
        
    
#ifdef __cplusplus
    
}

#endif

