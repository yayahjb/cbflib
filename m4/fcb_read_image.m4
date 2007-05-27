m4_dnl  fcb_read_image.m4 -- m4 macro file to generate I2, I4, 
m4_dnl                   and 3D_I2 and 3D_I4 versions
m4_dnl                   of FCB_READ_IMAGE

m4_include(`fcblib_defines.m4')
m4_define(`fcb_macro_FCB_READ_IMAGE',
`!  FCB_READ_IMAGE_$1.F90
!
!  Derived from f90 test code by W. Kabsch
!  H. J. Bernstein, 24 March 2007
!
!  Reads the next binary image from a CIF files that
!  has already been opened.  Leaves the file open and
!  positioned just after the binary and still within
!  the text field
!
!  This version is for
!
!  m4_ifelse($1,`I2',`2-D INTEGER*2 arrays',
   $1,`I4',`2-D INTEGER*4 arrays',
   $1,`3D_I2',`3-D INTEGER*2 arrays',
   $1,`3D_I4',`3-D INTEGER*4 arrays',`2-D INTEGER*2 arrays')
m4_define(`fcb_3d_flag',`2D')m4_dnl
m4_ifelse($1,`3D_I2',`m4_define(`fcb_3d_flag',`3D')',
          $1,`3D_I4',`m4_define(`fcb_3d_flag',`3D')')m4_dnl
!
!  with function
!
!  FCB_READ_IMAGE_$1

      INTEGER FUNCTION FCB_READ_IMAGE_$1(ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2, m4_ifelse(fcb_3d_flag,`3D',
        `DIM3, ') &
        PADDING,TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,             &
        REC_IN_FILE,BUFFER)
      
!-----------------------------------------------------------------------
m4_ifelse($1,`I2',
`! Reads a 16-bit integer twos complement 2D image',
$1,`I4',
`! Reads a 32-bit integer twos complement 2D image',
$1,`3D_I2',
`! Reads a 16-bit integer twos complement 3D image',
$1,`3D_I4',
`! Reads a 32-bit integer twos complement 3D image')
!
! compressed by a BYTE_OFFSET algorithm by W. Kabsch based
! on a proposal by A. Hammersley or
! compressed by a PACKED algorithm by J. P. Abrahams as
! used in CCP4, with modifications by P. Ellis and
! H. J. Bernstein.
!
! The BYTE-OFFSET algorithm is a slightly simplified version of
! that described in Andy Hammersley''`s web page
! (http://www.esrf.fr/computing/Forum/imgCIF/cbf_definition.html)
!
!-----------------------------------------------------------------------
! ARRAY    - Image                                              (RESULT)
! NELEM    - The number of elements to be read                   (GIVEN)
! NELEM_READ
!          - The number of elements actually read               (RESULT)
! ELSIGN   - Flag for signed (1) OR unsigned (0) data            (GIVEN)
! COMPRESSION
!          - The actual compression of the image                (RESULT)
! DIM1     - The fastest dimension of ARRAY                      (GIVEN)
m4_ifelse(`fcb_3d_flag',`3D',
`! DIM2     - The next slower dimension                           (GIVEN)
! DIM3      - The slowest dimension                              (GIVEN)',
`! DIM2     - The slowest dimension                               (GIVEN)')
! TAPIN    - Fortran device unit number assigned to image file   (GIVEN)
! FCB_BYTES_IN_REC
!          - The number of bytes in each bufferload to read      (GIVEN)
! BYTE_IN_FILE
!          - The position in the file of the next byte to read   (GIVEN,
!                                                                RESULT)
! REC_IN_FILE
!          - The record number from 1 of the block in BUFFER     (GIVEN,
!                                                                RESULT)
! BUFFER   - Buffer of bytes read from the file                  (GIVEN,
!                                                                RESULT)
! PADDING  - Pad bytes after the binary                         (RESULT)
!
! Returns (as function value)                                   (RESULT)
!             CBF_FORMAT (=1): 
!                cannot handle this CBF format (not implemented)
!             0: No error
!-----------------------------------------------------------------------
      IMPLICIT                       NONE

      INTEGER(8),   INTENT(IN):: DIM1,DIM2`'m4_ifelse(fcb_3d_flag,`3D',
        `,DIM3') 
      INTEGER(m4_ifelse($1,`I2',2,$1,`3D_I2',2,4)),  INTENT(OUT):: ARRAY(DIM1,DIM2`'m4_ifelse(fcb_3d_flag,`3D',
        `,DIM3'))
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN
      INTEGER,     INTENT(OUT):: COMPRESSION
      INTEGER(8),  INTENT(OUT):: PADDING
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)



! --  Definition of CBF_FORMAT
      fcb_errcode_CBF_FORMAT
! --  External functions called
      fcb_interface_FCB_READ_BYTE
      fcb_interface_FCB_NEXT_BINARY
      fcb_interface_FCB_CI_STRNCMPARR
      fcb_interface_FCB_DECOMPRESS_PACKED_$1
! --  Local variables
      INTEGER             STEP,FIRST2,LAST2,FIRST4,LAST4,II,I,J`'m4_ifelse(fcb_3d_flag,
      `3D',`,K')
      INTEGER(4)          DIFF,PIXVALUE,MARKER,IADR
      INTEGER(2)          SHORTINT
      INTEGER(1)          LAST_CHAR,ONEBYTE,                          &
                          TWOBYTES(2),FOURBYTES(4),ENDIANORDER(4),    &
                          MARKBYTES(4)
      INTEGER  ENCODING

      fcb_param_ENC_LIST_ALL
      INTEGER  SIZE
      INTEGER  ID

      fcb_param_CBF_LIST_ALL

      INTEGER  BITS,VORZEICHEN,REELL
      CHARACTER(len=24)   DIGEST
      CHARACTER(len=14)   BYTEORDER
      !Possible parameter values for BYTEORDER are:
	 !"LITTLE_ENDIAN"     supported
	 !"BIG_ENDIAN"    not supported
      INTEGER(8)  DIMOVER
      INTEGER(8)  LDIM1       !Number of "fast" pixels of the image
      INTEGER(8)  LDIM2       !Number of "slow" pixels of the image
      INTEGER(8)  LDIM3
      INTEGER(4)  PREV_ELEMENT

      DATA             ENDIANORDER/Z''`12''`,Z''`34''`,Z''`56''`,Z''`78''`/
      DATA             MARKBYTES/Z''`0C''`,Z''`1A''`,Z''`04''`,Z''`D5''`/

!-----------------------------------------------------------------------
! --  Determine endian architecture of this machine
!-----------------------------------------------------------------------
! Definition: If the lowest memory address of multi-byte data is
!             considered the starting address of the data, the least
!             significant byte (LSB) is at the lowest memory address
!             for a ''`little_endian''` cpu architecture.
!
! Example:    The 32 bit hex value Z''`12345678''` is stored as follows:
!             ENDIAN ORDER   BYTE0   BYTE1 BYTE2 BYTE3
!              Big Endian    12       34    56    78(LSB)
!            Little Endian   78(LSB)  56    34    12
!-----------------------------------------------------------------------
      PIXVALUE=TRANSFER(ENDIANORDER,PIXVALUE)
      STEP=0
      IF (PIXVALUE .EQ. Z''`78563412''`) THEN !Little Endian machine
	 STEP=1
	 FIRST2=1;LAST2=2
	 FIRST4=1;LAST4=4
      ENDIF
      IF (PIXVALUE .EQ. Z''`12345678''`) THEN ! Big Endian machine
	 STEP=-1
	 FIRST2=2;LAST2=1
	 FIRST4=4;LAST4=1
      ENDIF
      IF (STEP.EQ.0)GO TO 110

!-----------------------------------------------------------------------
! --   Skip to the next binary and parse the MIME header
!-----------------------------------------------------------------------
      IF (FCB_NEXT_BINARY(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,BYTE_IN_FILE,&
	REC_IN_FILE,BUFFER,ENCODING,SIZE,ID,DIGEST,COMPRESSION,BITS,        &
	VORZEICHEN,REELL,BYTEORDER,DIMOVER,LDIM1,LDIM2,LDIM3,PADDING).NE.0) &
	 GO TO 130
      IF ((DIM1.NE.LDIM1).OR.(DIM2.NE.LDIM2)`'m4_ifelse(`fcb_3d_flag',
      `3D',`.OR.(DIM3.NE.LDIM3)'))GO TO 130

!-----------------------------------------------------------------------
! --  Advance to start of binary image data
!-----------------------------------------------------------------------
! In CBF the binary data begins immediately after the first occurence
! of the following 4 bytes (MARKBYTES) in the image file
!             Octet    Hex  Decimal             Purpose
!               1       0C   12       (ctrl-L) End the current page
!               2       1A   26       (ctrl-Z) Stop listings in MS-DOS
!               3       04   04       (Ctrl-D) Stop listings in UNIX
!               4       D5   213      Binary section begins
!               5..5+n-1              Binary data (n octets)
!-----------------------------------------------------------------------
      MARKER=TRANSFER(MARKBYTES,MARKER)
      FOURBYTES=0
      DO
	 DO I=1,3
	    FOURBYTES(I)=FOURBYTES(I+1)
	 ENDDO
	 BYTE_IN_FILE=BYTE_IN_FILE+1
	 IF (FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,               &
	    REC_IN_FILE,BYTE_IN_FILE,FOURBYTES(4)).NE.0) GO TO 140
	 PIXVALUE=TRANSFER(FOURBYTES,PIXVALUE)
	 IF (PIXVALUE.EQ.MARKER)EXIT
      ENDDO
      ! *** DEBUG *** PRINT *, "fwa-1 address of IMAGE at: "
      ! *** DEBUG *** PRINT *, "BYTE_IN_FILE: ", BYTE_IN_FILE
      ! *** DEBUG *** PRINT *, "REC_IN_FILE: ", REC_IN_FILE

!-----------------------------------------------------------------------
! --  Read data image of 32 bit two''`s complement integers, compressed
! --  by the BYTE-OFFSET algorithm.
! --  After the expansion the original pixel values are coded by 16 bit
! --  in a special way suitable for XDS (see INTEGER*2 FUNCTION CNT2PIX).
!-----------------------------------------------------------------------
      FCB_READ_IMAGE_$1=CBF_FORMAT  !Cannot handle this CBF format
      IF ((BYTEORDER.EQ."LITTLE_ENDIAN").AND.(ENCODING.EQ.ENC_NONE).AND.&
	  (IAND(COMPRESSION,CBF_COMPRESSION_MASK).EQ.CBF_BYTE_OFFSET))THEN
	 PIXVALUE=0
	 NELEM_READ=0
	 m4_ifelse(fcb_3d_flag,
      `3D',`DO K = 1, DIM3')
     DO J = 1, DIM2
     DO I = 1, DIM1
	    BYTE_IN_FILE=BYTE_IN_FILE+1
	    IF (FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,            &
	       REC_IN_FILE,BYTE_IN_FILE,ONEBYTE).NE.0) GO TO 140
	    DIFF=ONEBYTE
	    IF (DIFF.EQ.-128)THEN
	       DO II=FIRST2,LAST2,STEP
		  BYTE_IN_FILE=BYTE_IN_FILE+1
		  IF (FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,      &
		     REC_IN_FILE,BYTE_IN_FILE,TWOBYTES(II)).NE.0)GO TO 140
	       ENDDO
	       SHORTINT=TRANSFER(TWOBYTES,SHORTINT)
	       DIFF=SHORTINT
	       IF (DIFF.EQ.-32768)THEN
		  DO II=FIRST4,LAST4,STEP
		     BYTE_IN_FILE=BYTE_IN_FILE+1
		     IF (FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,   &
			REC_IN_FILE,BYTE_IN_FILE,FOURBYTES(II)).NE.0)    &
			GO TO 140
		  ENDDO
		  DIFF=TRANSFER(FOURBYTES,DIFF)
	       ENDIF
	    ENDIF
	    PIXVALUE=PIXVALUE+DIFF
	    ARRAY(I,J`'m4_ifelse(fcb_3d_flag,`3D',`,K')) = PIXVALUE
	    NELEM_READ=NELEM_READ+1
	 ENDDO
	 ENDDO`'m4_ifelse(fcb_3d_flag,`3D',`
	 ENDDO')
	 
	 FCB_READ_IMAGE_$1=0  !No error
	 ELSE
      IF ((BYTEORDER.EQ."LITTLE_ENDIAN").AND.(ENCODING.EQ.ENC_NONE).AND.    &
	  ((IAND(COMPRESSION,CBF_COMPRESSION_MASK).EQ.CBF_PACKED) .OR.          &
	   (IAND(COMPRESSION,CBF_COMPRESSION_MASK).EQ.CBF_PACKED_V2)))THEN
	  FCB_READ_IMAGE_$1=FCB_DECOMPRESS_PACKED_$1 (ARRAY,NELEM,NELEM_READ,   &
        VORZEICHEN, COMPRESSION, DIM1, DIM2,`'m4_ifelse(fcb_3d_flag,`3D',`DIM3,')  &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,        &
        REC_IN_FILE,BUFFER)
      IF (NELEM_READ.NE.NELEM) PRINT *, "EARLY TERMINATION AT ",NELEM_READ
      PREV_ELEMENT = 0
      END IF
    END IF

!-----------------------------------------------------------------------
100   RETURN
110   FCB_READ_IMAGE_$1=-1 !Cannot determine endian architecture of this machine
      RETURN
130   FCB_READ_IMAGE_$1=-3 !Wrong image format
      GO TO 100
140   FCB_READ_IMAGE_$1=-4 !Cannot read image
      GO TO 100
      END FUNCTION FCB_READ_IMAGE_$1')
      
      fcb_macro_FCB_READ_IMAGE(`I2')

      fcb_macro_FCB_READ_IMAGE(`I4')

      fcb_macro_FCB_READ_IMAGE(`3D_I2')

      fcb_macro_FCB_READ_IMAGE(`3D_I4')
      
