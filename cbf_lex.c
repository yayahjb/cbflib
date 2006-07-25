
#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_compress.h"
#include "cbf_lex.h"
#include "cbf_file.h"
#include "cbf_part.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

extern int didchat;


  /* Return a copy of the text */

int
cbf_return_text (int code, YYSTYPE *val, int offset, 
                   cbf_file *file, char type)
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
      c, count, reprocess, errorcode, firstfew, ismime;
      
  char linbuf[2];
  long int id;
  long start, posdata, initstr, size;
  
  struct part *mppart;
  cbf_file *texttemp;
  FILE *textstream;

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
            return cbf_return_text (STRING, val, 1, file, CBF_TOKEN_SQSTRING);
          return cbf_return_text (STRING, val, 1, file, CBF_TOKEN_DQSTRING);
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
        if (length == 1 && 
          (file->text [0] == '?' || file->text [0] == '.')){
          memmove (file->text + 1, file->text, length + 1);
          return cbf_return_text (WORD, val, 1, file, CBF_TOKEN_NULL);
        }
        memmove (file->text + 1, file->text, length + 1);
        return cbf_return_text (WORD, val, 1, file, CBF_TOKEN_WORD);
      }
    }


      /* semicolon-delimited STRING (^;[^\n]*[\n])([^;][^\n]*[\n])+)(;) */
      
    if (file->column == 1)
      if (c == ';')
      {
        ismime = 0;
                
        /* Save the position */
        posdata = file->fpos;
        
        /* Attempt to process as a MIME part to a temporary file */
        mppart = part_init(file->stream);
        mppart->ciftext = 1;
        part_ungets(
          "Content-Type: multipart/mixed;\n  boundary=CIF-BINARY-FORMAT-SECTION--\n\n",
        mppart);

        textstream = tmpfile ();
        if (!textstream) {
          val->errorcode = CBF_FILEOPEN;
          return ERROR;
        }
        mppart->xferfile = textstream;
        start = 0;
        errorcode = cbf_make_file (&texttemp, textstream);
        if (errorcode) {
            if (fclose (textstream))
               errorcode |= CBF_FILECLOSE;
            val->errorcode = errorcode;
            return ERROR;
        }
        didchat = 0;
        handleMessage(mppart, "text/plain", 0, 0);
        file->fpos += mppart->fpos;
        if (mppart->cnt > 0){
           fseek (file->stream, -(mppart->cnt), SEEK_CUR);
           file->fpos -= (long) mppart->cnt;
        }

        if (mppart->gotfile  && !(mppart->binflag) ) {
          size = mppart->size;
          if (size) {
          do {
            c = cbf_read_character (file);
            if (c == EOF)
              return cbf_return_text (UNKNOWN, val, 0, file, 0);
          }
          while (c != ';' || file->column != 1);
          id = mppart->id;
          sprintf (file->text, 
             "%x %p %lx %lx", id, texttemp, 0, size);
          errorcode = cbf_add_fileconnection(&texttemp, NULL);
          if (errorcode) {
            val->errorcode |= cbf_delete_fileconnection (&file);
            return errorcode;
          }
          errorcode = cbf_return_text 
             (BINARY, val, 0, file, CBF_TOKEN_BIN_TMP);
          if (errorcode == ERROR)
            val->errorcode |= cbf_delete_fileconnection (&file);
          return errorcode;
          } else {
          if (fseek (file->stream, posdata, SEEK_SET))
            return CBF_FILEREAD;
          errorcode = CBF_FORMAT;
          if (fclose (textstream))
            errorcode |= CBF_FILECLOSE;
          return ERROR;
          }
        } else {
          if (fseek (file->stream, posdata, SEEK_SET))
            return CBF_FILEREAD;
          file->fpos = posdata;
        }
        
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
          return cbf_return_text (STRING, val, 1, file, CBF_TOKEN_SCSTRING);
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
        posdata = file->fpos;

          /* ... end */

        if (fseek (file->stream, size, SEEK_CUR))
          return cbf_return_text (UNKNOWN, val, 0, file, 0);
        file->fpos += size;
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
        sprintf (file->text, "%x %p %lx %lx", id, file, posdata, size);
        errorcode = cbf_return_text (BINARY, val, 0, file, CBF_TOKEN_BIN);
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

/* version of chat from mpack */

void chat(char *s)
{
  didchat = 1;
  fprintf(stderr, "%s\n",s);
}

#ifdef __cplusplus

}

#endif

