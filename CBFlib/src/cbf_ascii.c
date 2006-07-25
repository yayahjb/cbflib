/**********************************************************************
 * cbf_ascii -- write plain ASCII values                              *
 *                                                                    *
 * Version 0.4 15 November 1998                                       *
 *                                                                    *
 *             Paul Ellis (ellis@ssrl.slac.stanford.edu)              *
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
 *                        The IUCr Policy                             *
 *                                on                                  *
 *     the Use of the Crystallographic Information File (CIF)         *
 *                                                                    *
 * The Crystallographic Information File (Hall, Allen & Brown,        *
 * 1991) is, as of January 1992, the recommended method for           *
 * submitting publications to Acta Crystallographica Section C. The   *
 * International Union of Crystallography holds the Copyright on      *
 * the CIF, and has applied for Patents on the STAR File syntax       *
 * which is the basis for the CIF format.                             *
 *                                                                    *
 * It is a principal objective of the IUCr to promote the use of      *
 * CIF for the exchange and storage of scientific data. The IUCr's    *
 * sponsorship of the CIF development was motivated by its            *
 * responsibility to its scientific journals, which set the           *
 * standards in crystallographic publishing. The IUCr intends that    *
 * CIFs will be used increasingly for electronic submission of        *
 * manuscripts to these journals in future. The IUCr recognises       *
 * that, if the CIF and the STAR File are to be adopted as a means    *
 * for universal data exchange, the syntax of these files must be     *
 * strictly and uniformly adhered to. Even small deviations from      *
 * the syntax would ultimately cause the demise of the universal      *
 * file concept. Through its Copyrights and Patents the IUCr has      *
 * taken the steps needed to ensure strict conformance with this      *
 * syntax.                                                            *
 *                                                                    *
 * The IUCr policy on the use of the CIF and STAR File processes is   *
 * as follows:                                                        *
 * _________________________________________________________________  *
 *                                                                    *
 *  * 1 CIFs and STAR Files may be generated, stored or transmitted,  *
 *    without permission or charge, provided their purpose is not     *
 *    specifically for profit or commercial gain, and provided that   *
 *    the published syntax is strictly adhered to.                    *
 *  * 2 Computer software may be developed for use with CIFs or STAR  *
 *    files, without permission or charge, provided it is distributed *
 *    in the public domain. This condition also applies to software   *
 *    for which a charge is made, provided that its primary function  *
 *    is for use with files that satisfy condition 1 and that it is   *
 *    distributed as a minor component of a larger package of         *
 *    software.                                                       *
 *  * 3 Permission will be granted for the use of CIFs and STAR Files *
 *    for specific commercial purposes (such as databases or network  *
 *    exchange processes), and for the distribution of commercial     *
 *    CIF/STAR software, on written application to the IUCr Executive *
 *    Secretary, 2 Abbey Square, Chester CH1 2HU, England. The        *
 *    nature, terms and duration of the licences granted will be      *
 *    determined by the IUCr Executive and Finance Committees.        *
 *                                                                    *
 * _________________________________________________________________  *
 *                                                                    *
 * In summary, the IUCr wishes to promote the use of the STAR File    *
 * concepts as a standard universal data file. It will insist on      *
 * strict compliance with the published syntax for all                *
 * applications. To assist with this compliance, the IUCr provides    *
 * public domain software for checking the logical integrity of a     *
 * CIF, and for validating the data name definitions contained        *
 * within a CIF. Detailed information on this software, and the       *
 * associated dictionaries, may be obtained from the IUCr Office at   *
 * 5 Abbey Square, Chester CH1 2HU, England.                          *
 **********************************************************************/

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_ascii.h"
#include "cbf_tree.h"
#include "cbf_file.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>


  /* Write an ascii value */

int cbf_write_ascii (const char *string, cbf_file *file)
{
  static const char missing [] = { CBF_TOKEN_WORD, '?', '\0' };

  int end;

  unsigned int column;

  const char *c;

  char delim;


    /* Check the arguments */

  if (!string)

    string = missing;
 
  else

    if (*string != CBF_TOKEN_WORD     &&
        *string != CBF_TOKEN_SQSTRING &&
        *string != CBF_TOKEN_DQSTRING &&
        *string != CBF_TOKEN_SCSTRING && 
        *string != CBF_TOKEN_NULL)

      return CBF_ARGUMENT;


    /* Get the current column */

  cbf_failnez (cbf_get_filecoordinates (file, NULL, &column))
  

    /* Do we need to start a new line? */

  if (column)

    if (*string == CBF_TOKEN_SCSTRING)

      cbf_failnez (cbf_write_character (file, '\n'))

    else
    {
      if (*string == CBF_TOKEN_WORD ||
          *string == CBF_TOKEN_NULL )

        end = column + 3;

      else

        end = column + 1;

      for (c = string + 1; *c && end <= CBF_LINELENGTH; c++)

        if (*c == '\t')

          end = (end & ~0x07) + 8;

        else

          end = end + 1;

      if (end > CBF_LINELENGTH)

        cbf_failnez (cbf_write_character (file, '\n'))
    }


    /* Write the value */

  switch (*string)
  {
      /* Simple word? */
      
    case  CBF_TOKEN_WORD:
    case  CBF_TOKEN_NULL:
    
      cbf_failnez (cbf_write_character (file, ' '))
      
      cbf_failnez (cbf_write_string (file, string + 1))
      
      break;


      /* Single line? */

    case CBF_TOKEN_SQSTRING:
    case CBF_TOKEN_DQSTRING:

      if (*string == CBF_TOKEN_SQSTRING)

        delim = '\'';

      else

        delim = '"';

      cbf_failnez (cbf_write_character (file, ' '))
      
      cbf_failnez (cbf_write_character (file, delim))
      
      cbf_failnez (cbf_write_string (file, string + 1))
      
      cbf_failnez (cbf_write_character (file, delim))
      
      break;


      /* Multiple lines? */

    case CBF_TOKEN_SCSTRING:

      cbf_failnez (cbf_write_character (file, ';'))

      end = 1;

      for (c = string + 1; *c; c++)
      {
        if (*c == ';' && end == 0)

          cbf_failnez (cbf_write_character (file, '\\'))

        cbf_failnez (cbf_write_character (file, *c))

        if (*c == '\n')

          end = 0;

        else

          end = 1;
      }
      
      cbf_failnez (cbf_write_string (file, "\n;\n"))

      end = 0;

      break;
  }


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif

