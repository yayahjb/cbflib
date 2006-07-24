
#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_compress.h"
#include "cbf_lex.h"
#include "cbf_file.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>


  /* Return a copy of the text */

int cbf_return_text (int code, YYSTYPE *val, int offset, cbf_file *file, char type)
{
  val->text = cbf_copy_string (NULL, file->text + offset, type);

  if (!val->text)
  {
    val->errorcode = CBF_ALLOC;

    return ERROR;
  }

  return code;
}


  /* Get the next token */

int cbf_lex (YYSTYPE *val, cbf_file *file)
{
  int data, loop, item, column, comment, word, string, ascii, length, 
      c, count, reprocess, errorcode;

  long int id;

  unsigned long size;

  file->text_used = 0;

  c = file->last_read;
  
  column = comment = reprocess = (c == '.' || c == '#');

  data = loop = item = comment = word = string = !reprocess;

  do
  {
    length = file->text_used;

    if (reprocess)

      reprocess = 0;

    else
    
      c = cbf_read_character (file);
    

      /* Discard spaces ([[:space:]]+) */

    if (length == 0)

      if (isspace (c))
      
        continue;
            
        
       /* DATA ([Dd][Aa][Tt][Aa][_][^[:space:]]*) */
    
    if (data)
    {
      if (length < 5)
    
        data = toupper (c) == "DATA_" [length];
      
      else
      {
        data = !isspace (c) && c != EOF;
      
        if (!data)

          return cbf_return_text (DATA, val, 5, file, 0);
      }
    }
   
   
       /* LOOP ([Ll][Oo][Oo][Pp][_]) */
     
    if (loop)
    {
      loop = toupper (c) == "LOOP_" [length];
      
      if (loop && length == 4)

        return LOOP;
    }

   
       /* ITEM ([_][^[:space:]\.]+) */
     
    if (item)
    {
      if (length == 0)
    
        item = c == '_';
      
      else
      {
        item = !isspace (c) && c != '.' && c != EOF;

        if (length >= 2 && !item)
        {
          if (c == '.')

            return cbf_return_text (CATEGORY, val, 1, file, 0);

          return cbf_return_text (ITEM, val, 1, file, 0);
        }
      }
    }

   
      /* COLUMN (\.[^[:space:]]+) */
     
    if (column)
    {
      column = !isspace (c) && c != EOF;

      if (!column)

        return cbf_return_text (COLUMN, val, 1, file, 0);
    }

  
      /* STRING ([\'][^'\n]*[\'\n])|(([\"][^"\n]*[\"\n])) */
     
    if (string)
    {
      if (length == 0)
    
        string = c == '\'' || c == '"';
      
      else
      {
        string = c != file->text [0] && c != '\n' && c != EOF;
      
        if (!string)
        {
          if (file->text [0] == '\'')
          
            return cbf_return_text (STRING, val, 1, file, '\301');

          return cbf_return_text (STRING, val, 1, file, '\302');
        }
      }
    }


       /* COMMENT ([#][^\n]*) */
     
    if (comment)
    {
      if (length == 0)
    
        comment = c == '#';
      
      else
      {
        comment = c != '\n' && c != EOF;
      
        if (!comment)

          return cbf_return_text (COMMENT, val, 1, file, 0);
      }
    }


       /* WORD ([^[:space:]]+) */
     
    if (!data && !loop && !item && !comment && !string && !column)
    {
      word = !isspace (c) && c != EOF;
      
      if (length && !word)
      {
          /* Missing value? */
          
        if (length == 1 && file->text [0] == '?')
        {
          val->text = NULL;

          return WORD;
        }

        memmove (file->text + 1, file->text, length + 1);

        return cbf_return_text (WORD, val, 1, file, '\300');
      }
    }


      /* semicolon-delimited STRING (^;[^\n]*[\n])([^;][^\n]*[\n])+)(;) */
      
    if (file->column == 1)
    
      if (c == ';')
      {
        do
        {
          errorcode = cbf_save_character (file, c);

          if (errorcode)
          {
            val->errorcode = errorcode;
            
            return ERROR;
          }

          c = cbf_read_character (file);

          ascii = isgraph (c) || isspace (c);
        }
        while ((c != ';' || file->column != 1) && ascii);

        if (ascii)
        {
          file->text [file->text_used - 1] = '\0';


            /* Convert "\n\\;" -> "\n;" */

          for (count = 0; file->text [count]; count++)

            if (strncmp (file->text + count, "\n\\;", 3) == 0)

              memmove (file->text + count + 1, file->text + count + 2,
                                               file->text_used - count - 2);

          return cbf_return_text (STRING, val, 1, file, '\303');
        }


          /* BINARY ((^;)[[:graph:][:space:]]*...
                      ...[[:graph:][:space:]]*(^;))

             Byte
                 
                0     Ctrl-Z     26
                1     Ctrl-D      4
                2               213
              3-6     id
             7-14     size                */

        while (c == 26 || c == 4)
        
          c = cbf_read_character (file);


          /* OK? */

        if (c != 213)
        
          return cbf_return_text (UNKNOWN, val, 0, file, 0);


          /* id */

        id = 0;

        for (count = 0; count < 64; count += 8)
        {
          c = cbf_get_character (file);

          if (c == EOF)

            return cbf_return_text (UNKNOWN, val, 0, file, 0);

          id |= c << count;
        }


          /* Size */

        size = 0;

        for (count = 0; count < 64; count += 8)
        {
          c = cbf_get_character (file);

          if (c == EOF)

            return cbf_return_text (UNKNOWN, val, 0, file, 0);

          size |= c << count;
        }


          /* Position */

        data = ftell (file->stream);


          /* ... end */

        if (fseek (file->stream, size, SEEK_CUR))

          return cbf_return_text (UNKNOWN, val, 0, file, 0);

        do
        {
          c = cbf_read_character (file);

          if (c == EOF)

            return cbf_return_text (UNKNOWN, val, 0, file, 0);
        }
        while (c != ';' || file->column != 1);


          /* Add a connection */

        errorcode =  cbf_add_fileconnection (&file, NULL);

        if (errorcode)
        {
          val->errorcode = errorcode;
            
          return ERROR;
        }
        

          /* Code the id, file, position and size */

        sprintf (file->text, "%x %p %x %lx", id, file, data, size);

        errorcode = cbf_return_text (BINARY, val, 0, file, '\304');

        if (errorcode == ERROR)

          val->errorcode |= cbf_delete_fileconnection (&file);

        return errorcode;
      }


      /* Add the character to the text */

    errorcode = cbf_save_character (file, c);

    if (errorcode)
    {
      val->errorcode = errorcode;
            
      return ERROR;
    }
  }
  while (c != EOF);
  
  return 0;
}

#ifdef __cplusplus

}

#endif

