
  /* image object v. 1.0 */

#ifndef IMG_H
#define IMG_H

#ifdef __cplusplus

extern "C" {

#endif

#define IMG_ARGUMENT    0x0001
#define IMG_ALLOC       0x0002
#define IMG_OPEN        0x0004
#define IMG_READ        0x0008
#define IMG_FORMAT      0x0010
#define IMG_FIELD       0x0020

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

  int                   size [2];
  int                  *image;
}
img_object;

typedef img_object *img_handle;

  /* Functions */

#define img_pixel(img,x,y) (((img)->image) [((img)->size) [1] * (int) (x) + (int) (y)])
#define img_columns(img)    ((img)->size [0])
#define img_rows(img)       ((img)->size [1])

img_handle  img_make_handle    ();
int         img_read           (img_handle img, const char *name);
int         img_free_handle    (img_handle img);
int         img_delete_field   (img_handle img, const char *tag);
const char *img_get_field      (img_handle img, const char *tag);
int         img_set_field      (img_handle img, const char *tag, const char *data);
double      img_get_number     (img_handle img, const char *tag);
int         img_set_number     (img_handle img, const char *tag, const char *format,
                                                                  double data);
int         img_get_pixel      (img_handle img, int x, int y);
int         img_set_pixel      (img_handle img, int x, int y, int data);
int         img_set_dimensions (img_handle img, int columns, int rows);
int         img_get_dimension  (img_handle img, int dimension);


#ifdef __cplusplus

}

#endif

#endif /* IMG_H */

