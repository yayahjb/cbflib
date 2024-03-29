
  /* image object v. 1.1 */

#ifndef IMG_H
#define IMG_H

#ifdef __cplusplus

extern "C" {
        
#endif

#include <stdio.h>

    
#define img_BAD_ARGUMENT    0x0001
#define img_BAD_ALLOC       0x0002
#define img_BAD_OPEN        0x0004
#define img_BAD_READ        0x0008
#define img_BAD_FORMAT      0x0010
#define img_BAD_FIELD       0x0020
#define img_BAD_WRITE       0x0040
    
    /* Tag */
    
    typedef struct
    {
        char                 *tag;
        char                 *data;
    }
    img_tag;
    
    
    /* Image */
    
    typedef struct
    {
        int                   tags;
        img_tag              *tag;
        
        int                   size [2];  /* size[0] = columns, size[1] = rows */
        int					rowmajor;  /* set to 1 for row major, 0 for column major */
        int                  *image;
    }
    img_object;
    
    typedef img_object *img_handle;
    
    
    /* Functions */
    
#define img_pixel(img,x,y)                                              \
          (((img)->rowmajor)?                                           \
          (((img)->image) [((img)->size) [0] * (int) (y) + (int) (x)]) :\
          (((img)->image) [((img)->size) [1] * (int) (x) + (int) (y)]))
#define img_pixelptr(img,x,y)                                              \
             (((img)->rowmajor)?                                           \
            &(((img)->image) [((img)->size) [0] * (int) (y) + (int) (x)]) :\
            &(((img)->image) [((img)->size) [1] * (int) (x) + (int) (y)]))
#define img_columns(img)    ((img)->size [0])
#define img_rows(img)       ((img)->size [1])
    
    int         img_swap_i4 (int i4);
    float       img_float_i4 (int i4,
                              int VAX);
    int         img_read_i4 (FILE *file,
                             int *i4);
    int         img_read_smvheader (img_handle img,
                                    FILE *file);
    int         img_read_smvdata (img_handle img,
                                  FILE *file);
    int         img_read_smv (img_handle img,
                              const char *name);
    int         img_write_smv (img_object   *img,
                       const char   *name,
                       unsigned int  bits);
    int         img_read_mar300header (img_handle img,
                                       FILE *file,
                                       int *org_data);
    int         img_read_mar300data (img_handle img,
                                     FILE *file,
                                     int *org_data);
    int         img_read_mar300 (img_handle img,
                                 const char *name);
    int         img_read_mar345header (img_handle img,
                                       FILE *file,
                                       int *org_data);
    int         img_read_mar345data (img_handle img,
                                     FILE *file,
                                     int *org_data);
    int         img_read_mar345 (img_handle img,
                                 const char *name);
    int         img_get_tags (img_handle img);
    int         img_set_tags (img_handle img, int tags);
    int         img_delete_fieldnumber (img_handle img, int x);
    
    img_handle  img_make_handle    ( void );
    int         img_read           (img_handle img,
                                    const char *name);
    int         img_free_handle    (img_handle img);
    int         img_delete_field   (img_handle img,
                                    const char *tag);
    const char *img_get_field      (img_handle img,
                                    const char *tag);
    int         img_get_next_field (img_handle img,
                                    const char **tag,
                                    const char **data,
                                    int *index);
    int         img_set_field      (img_handle img,
                                    const char *tag,
                                    const char *data);
    double      img_get_number     (img_handle img,
                                    const char *tag);
    int         img_set_number     (img_handle img,
                                    const char *tag,
                                    const char *format,
                                    double data);
    int         img_get_pixel      (img_handle img,
                                    int x,
                                    int y);
    int         img_set_pixel      (img_handle img,
                                    int x,
                                    int y,
                                    int data);
    int         img_set_dimensions (img_handle img,
                                    int columns,
                                    int rows);
    int         img_get_dimension  (img_handle img,
                                    int dimension);
    
    void *      img_memcpy(void * dst, 
                                    const void * src, 
                                    const size_t n);
    
#ifdef __cplusplus
    
}

#endif

#endif /* IMG_H */

