/**********************************************************************
 * cbf_context -- handle cbf contexts                                 *
 *                                                                    *
 * Version 0.6 13 January 1999                                        *
 *                                                                    *
 *            Paul Ellis (ellis@ssrl.slac.stanford.edu) and           *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
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
 *                          The IUCr Policy                           *
 *                                 on                                 *
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
#include "cbf_alloc.h"
#include "cbf_context.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>


  /* Create and initialise a context */

int cbf_make_context (cbf_context **context)
{
    /* Allocate the memory */

  cbf_failnez (cbf_alloc ((void **) context, NULL, sizeof (cbf_context), 1))


    /* Initialise */
    
  (*context)->temporary = NULL;

  (*context)->connections = 1;


    /* Success */

  return 0;
}


  /* Free a context */

int cbf_free_context (cbf_context **context)
{
  int errorcode;

  errorcode = 0;

  if (context)

    if (*context)
    {
      if ((*context)->temporary)

        errorcode = cbf_free_file (&(*context)->temporary);

      errorcode |= cbf_free ((void **) context, NULL);
    }


    /* Success? */

  return errorcode;
}


  /* Add a context connection */

int cbf_add_contextconnection (cbf_context **context)
{
    /* Does the context pointer exist? */

  if (!context)

    return CBF_ARGUMENT;


    /* Does the context exist? */

  if (*context)
  {
    (*context)->connections++;

    return 0;
  }


    /* Create a new context */

  return cbf_make_context (context);
}


  /* Remove a context connection */

int cbf_delete_contextconnection (cbf_context **context)
{
    /* Does the context pointer exist? */

  if (!context)

    return CBF_ARGUMENT;


    /* Does the context exist? */

  if (!*context)

    return CBF_ARGUMENT;


    /* Remove a connection */

  (*context)->connections--;


    /* Delete the context? */

  if ((*context)->connections == 0)

    return cbf_free_context (context);


    /* Success */

  return 0;
}


  /* Open a temporary file connection */

int cbf_open_temporary (cbf_context *context, cbf_file **temporary)
{
  FILE *stream;

  int errorcode;

  
    /* Check the arguments */

  if (!context || !temporary)

    return CBF_ARGUMENT;


    /* Does a temporary file already exist? */

  if (context->temporary)
  {
    cbf_failnez (cbf_add_fileconnection (&context->temporary, NULL))

    *temporary = context->temporary;

    return 0;
  }


    /* Create the temporary file */

  stream = tmpfile ();

  if (!stream)

    return CBF_FILEOPEN;

  errorcode = cbf_make_file (&context->temporary, stream);
  
  if (errorcode)
  {
    if (fclose (stream))

      errorcode |= CBF_FILECLOSE;

    return errorcode;
  }


    /* Open a connection */
    
  return cbf_open_temporary (context, temporary);
}


  /* Close a temporary file connection */

int cbf_close_temporary (cbf_context *context, cbf_file **temporary)
{
    /* Check the arguments */

  if (!context || !temporary)

    return CBF_ARGUMENT;

  if (!*temporary)

    return CBF_ARGUMENT;


    /* Check that the temporary file matches */

  if (context->temporary != *temporary)

    return CBF_NOTFOUND;
    

    /* Delete the connection */

  cbf_failnez (cbf_delete_fileconnection (&context->temporary))

  *temporary = NULL;


    /* Is there only one connection left? */

  if (context->temporary)

    if (cbf_file_connections (context->temporary) == 1)

      cbf_failnez (cbf_free_file (&context->temporary))


    /* Success */

  return 0;
}


  /* Copy a string */

const char *cbf_copy_string (cbf_context *context, const char *string, 
                                                         char type)
{
  char *new_string;

  if (string)

    if (type)
    {
      if (cbf_alloc ((void **) &new_string, NULL, 
                      sizeof (char), strlen (string) + 2) == 0)
      {
        *new_string = type;
        
        strcpy (new_string + 1, string);

        return new_string;
      }
    }
    else

      if (cbf_alloc ((void **) &new_string, NULL, \
                      sizeof (char), strlen (string) + 1) == 0)
      {
        strcpy (new_string, string);

        return new_string;
      }

 
    /* Fail */

  return NULL;
}


  /* Free a string */

void cbf_free_string (cbf_context *context, const char *string)
{
  cbf_free ((void **) &string, NULL);
}


#ifdef __cplusplus

}

#endif
