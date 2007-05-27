m4_define(`fcb_not_first',`')m4_dnl
m4_define(`fcb_is_last',`is_last')m4_dnl
m4_dnl Error codes:
m4_define(`fcb_errcode_CBF_FORMAT',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FORMAT            = Z''`00000001''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !      1')m4_dnl
m4_define(`fcb_errcode_CBF_ALLOC',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_ALLOC             = Z''`00000002''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !      2')m4_dnl
m4_define(`fcb_errcode_CBF_ARGUMENT',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_ARGUMENT          = Z''`00000004''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !      4')m4_dnl
m4_define(`fcb_errcode_CBF_ASCII',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_ASCII             = Z''`00000008''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !      8')m4_dnl
m4_define(`fcb_errcode_CBF_BINARY',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_BINARY            = Z''`00000010''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !     16')m4_dnl
m4_define(`fcb_errcode_CBF_BITCOUNT',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_BITCOUNT          = Z''`00000020''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !     32')m4_dnl
m4_define(`fcb_errcode_CBF_ENDOFDATA',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_ENDOFDATA         = Z''`00000040''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !     64')m4_dnl
m4_define(`fcb_errcode_CBF_FILECLOSE',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FILECLOSE         = Z''`00000080''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !    128')m4_dnl
m4_define(`fcb_errcode_CBF_FILEOPEN',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FILEOPEN          = Z''`00000100''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !    256')m4_dnl
m4_define(`fcb_errcode_CBF_FILEREAD',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FILEREAD          = Z''`00000200''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !    512')m4_dnl
m4_define(`fcb_errcode_CBF_FILESEEK',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FILESEEK          = Z''`00000400''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !   1024')m4_dnl
m4_define(`fcb_errcode_CBF_FILETELL',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FILETELL          = Z''`00000800''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !   2048')m4_dnl
m4_define(`fcb_errcode_CBF_FILEWRITE',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FILEWRITE         = Z''`00001000''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !   4096')m4_dnl
m4_define(`fcb_errcode_CBF_IDENTICAL',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_IDENTICAL         = Z''`00002000''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !   8192')m4_dnl
m4_define(`fcb_errcode_CBF_NOTFOUND',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_NOTFOUND          = Z''`00004000''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !  16384')m4_dnl
m4_define(`fcb_errcode_CBF_OVERFLOW',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_OVERFLOW          = Z''`00008000''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !  32768')m4_dnl
m4_define(`fcb_errcode_CBF_UNDEFINED',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_UNDEFINED         = Z''`00010000''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !  65536')m4_dnl
m4_define(`fcb_errcode_CBF_NOTIMPLEMENTED',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_NOTIMPLEMENTED    = Z''`00020000''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` ! 131072')m4_dnl
m4_define(`fcb_errcode_LIST_ALL',
m4_define(`fcb_is_last',`')m4_dnl
!
      !     Definitions of CBF error code parameters
      !
      fcb_errcode_CBF_FORMAT
m4_define(`fcb_not_first',`not_first')m4_dnl
        fcb_errcode_CBF_ALLOC
        fcb_errcode_CBF_ARGUMENT
        fcb_errcode_CBF_ASCII
        fcb_errcode_CBF_BINARY
        fcb_errcode_CBF_BITCOUNT
        fcb_errcode_CBF_ENDOFDATA
        fcb_errcode_CBF_FILECLOSE
        fcb_errcode_CBF_FILEOPEN
        fcb_errcode_CBF_FILEREAD
        fcb_errcode_CBF_FILESEEK
        fcb_errcode_CBF_FILETELL
        fcb_errcode_CBF_FILEWRITE
        fcb_errcode_CBF_IDENTICAL
        fcb_errcode_CBF_NOTFOUND
        fcb_errcode_CBF_OVERFLOW
        fcb_errcode_CBF_UNDEFINED
m4_define(`fcb_is_last',`is_last')m4_dnl
        fcb_errcode_CBF_NOTIMPLEMENTED
m4_define(`fcb_not_first',`')m4_dnl
)m4_dnl
m4_dnl
m4_dnl Possible parameter values for ENCODING are:
m4_define(`fcb_param_ENC_NONE',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')ENC_NONE         = Z''`0001''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Use BINARY encoding')m4_dnl
m4_define(`fcb_param_ENC_BASE64',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')ENC_BASE64       = Z''`0002''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Use BASE64 encoding')m4_dnl
m4_define(`fcb_param_ENC_BASE32k',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')ENC_BASE32K      = Z''`0004''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Use X-BASE32K encoding')m4_dnl
m4_define(`fcb_param_ENC_QP',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')ENC_QP           = Z''`0008''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Use QUOTED-PRINTABLE encoding')m4_dnl
m4_define(`fcb_param_ENC_BASE10',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')ENC_BASE10       = Z''`0010''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Use BASE10 encoding')m4_dnl
m4_define(`fcb_param_ENC_BASE16',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')ENC_BASE16       = Z''`0020''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Use BASE16 encoding')m4_dnl
m4_define(`fcb_param_ENC_BASE8',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')ENC_BASE8        = Z''`0040''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Use BASE8  encoding')m4_dnl
m4_define(`fcb_param_ENC_LIST_ALL',
m4_define(`fcb_is_last',`')m4_dnl
!
      !     Definitions of CBF encodings parameters
      !
      fcb_param_ENC_NONE
m4_define(`fcb_not_first',`not_first')m4_dnl
        fcb_param_ENC_BASE64
        fcb_param_ENC_BASE32k
        fcb_param_ENC_QP
        fcb_param_ENC_BASE10
        fcb_param_ENC_BASE16
m4_define(`fcb_is_last',`is_last')m4_dnl
        fcb_param_ENC_BASE8
m4_define(`fcb_not_first',`')m4_dnl
)m4_dnl
m4_dnl
m4_dnl Possible parameter values for COMPRESSION are:
m4_dnl
m4_define(`fcb_param_CBF_INTEGER',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_INTEGER      = Z''`0010''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Uncompressed integer')m4_dnl
m4_define(`fcb_param_CBF_FLOAT',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FLOAT        = Z''`0020''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Uncompressed IEEE floating point')m4_dnl
m4_define(`fcb_param_CBF_CANONICAL',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_CANONICAL    = Z''`0050''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Canonical compression')m4_dnl
m4_define(`fcb_param_CBF_PACKED',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_PACKED       = Z''`0060''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Packed compression')m4_dnl
m4_define(`fcb_param_CBF_PACKED_V2',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_PACKED_V2    = Z''`0090''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Packed compression')m4_dnl
m4_define(`fcb_param_CBF_BYTE_OFFSET',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_BYTE_OFFSET  = Z''`0070''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Byte Offset Compression')m4_dnl
m4_define(`fcb_param_CBF_PREDICTOR',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_PREDICTOR    = Z''`0080''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Predictor_Huffman Compression')m4_dnl
m4_define(`fcb_param_CBF_NONE',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_NONE         = Z''`0040''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !No compression flag')m4_dnl
m4_define(`fcb_param_CBF_COMPRESSION_MASK',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_COMPRESSION_MASK =     &
                           Z''`00FF''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Mask to sep compressiontype from flags')m4_dnl
m4_define(`fcb_param_CBF_FLAG_MASK',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FLAG_MASK    = Z''`0F00''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Mask to sep flags from compression type')m4_dnl
m4_define(`fcb_param_CBF_UNCORRELATED_SECTIONS',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_UNCORRELATED_SECTIONS =&
                           Z''`0100''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Flag for uncorrelated sections')m4_dnl
m4_define(`fcb_param_CBF_FLAT_IMAGE',
`m4_ifelse(fcb_not_first,`',``INTEGER,PARAMETER:: &
        '',``'')CBF_FLAT_IMAGE   = Z''`0200''`m4_ifelse(fcb_is_last,`is_last',`   ',``, &'')'` !Flag for flat (linear) images')m4_dnl
m4_define(`fcb_param_CBF_LIST_ALL',
m4_define(`fcb_is_last',`')m4_dnl
!
      !     Definitions of CBF compression parameters
      !
      fcb_param_CBF_INTEGER
m4_define(`fcb_not_first',`not_first')m4_dnl
        fcb_param_CBF_FLOAT
        fcb_param_CBF_CANONICAL
        fcb_param_CBF_PACKED
        fcb_param_CBF_PACKED_V2
        fcb_param_CBF_BYTE_OFFSET
        fcb_param_CBF_PREDICTOR
        fcb_param_CBF_NONE
        fcb_param_CBF_COMPRESSION_MASK
        fcb_param_CBF_FLAG_MASK
        fcb_param_CBF_UNCORRELATED_SECTIONS
m4_define(`fcb_is_last',`is_last')m4_dnl
        fcb_param_CBF_FLAT_IMAGE
m4_define(`fcb_not_first',`')m4_dnl
)m4_dnl
m4_dnl
m4_dnl fcblib function interfaces
m4_dnl
m4_define(`fcb_interface_FCB_ATOL_WCNT',
      `INTERFACE
      INTEGER(8) FUNCTION FCB_ATOL_WCNT(ARRAY,N,CNT)
!-----------------------------------------------------------------------
!     Converts bytes in ARRAY to an INTEGER(8), consuming CNT bytes
!-----------------------------------------------------------------------
      INTEGER,  INTENT(OUT):: CNT
      INTEGER,   INTENT(IN):: N
      INTEGER(1),INTENT(IN):: ARRAY(N)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_CI_STRNCMPARR',
      `INTERFACE
      INTEGER FUNCTION FCB_CI_STRNCMPARR(STRING, ARRAY, N, LIMIT)
!-----------------------------------------------------------------------
! Compares up to LIMIT characters of STRING and ARRAY case insensitive
!-----------------------------------------------------------------------
      CHARACTER(LEN=*),INTENT(IN):: STRING
      INTEGER,         INTENT(IN):: N,LIMIT
      INTEGER(1),      INTENT(IN):: ARRAY(N)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_DECOMPRESS_PACKED_I2',
      `INTERFACE
      INTEGER FUNCTION FCB_DECOMPRESS_PACKED_I2 (ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2,      &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,  &
        REC_IN_FILE,BUFFER)
      INTEGER(8),   INTENT(IN):: DIM1,DIM2 
      INTEGER(2),  INTENT(OUT):: ARRAY(DIM1,DIM2)
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN, COMPRESSION
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_DECOMPRESS_PACKED_I4',
      `INTERFACE
      INTEGER FUNCTION FCB_DECOMPRESS_PACKED_I4 (ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2,  &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,                   &
        REC_IN_FILE,BUFFER)
      INTEGER(8),   INTENT(IN):: DIM1,DIM2 
      INTEGER(4),  INTENT(OUT):: ARRAY(DIM1,DIM2)
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN, COMPRESSION
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_DECOMPRESS_PACKED_3D_I2',
      `INTERFACE
      INTEGER FUNCTION FCB_DECOMPRESS_PACKED_3D_I2 (ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2, DIM3,  &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,                   &
        REC_IN_FILE,BUFFER)
      INTEGER(8),   INTENT(IN):: DIM1,DIM2,DIM3 
      INTEGER(2),  INTENT(OUT):: ARRAY(DIM1,DIM2,DIM3)
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN, COMPRESSION
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_DECOMPRESS_PACKED_3D_I4',
      `INTERFACE
      INTEGER FUNCTION FCB_DECOMPRESS_PACKED_3D_I4 (ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2, DIM3,  &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,                   &
        REC_IN_FILE,BUFFER)
      INTEGER(8),   INTENT(IN):: DIM1,DIM2,DIM3 
      INTEGER(4),  INTENT(OUT):: ARRAY(DIM1,DIM2,DIM3)
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN, COMPRESSION
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_EXIT_BINARY',
      `INTERFACE
      INTEGER FUNCTION FCB_EXIT_BINARY(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,&
                                      BYTE_IN_FILE,REC_IN_FILE,BUFFER,  &
                                      PADDING )
!-----------------------------------------------------------------------
!     Skip to end of binary section that was just read
!-----------------------------------------------------------------------
      INTEGER,   INTENT(IN)   :: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: BYTE_IN_FILE,REC_IN_FILE
      INTEGER(1),INTENT(INOUT):: LAST_CHAR,BUFFER(FCB_BYTES_IN_REC)
      INTEGER(8),INTENT(IN)   :: PADDING
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_NBLEN_ARRAY',
      `INTERFACE
      INTEGER FUNCTION FCB_NBLEN_ARRAY(ARRAY, ARRAYLEN)
!-----------------------------------------------------------------------
!     Returns the non-blank length of an array
!-----------------------------------------------------------------------
      INTEGER,    INTENT(IN):: ARRAYLEN
      INTEGER(1), INTENT(IN):: ARRAY(ARRAYLEN)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_NEXT_BINARY',
      `INTERFACE
      INTEGER FUNCTION FCB_NEXT_BINARY(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,&
                                      BYTE_IN_FILE,REC_IN_FILE,BUFFER,  &
                                      ENCODING,SIZE,ID,DIGEST,          &
                                      COMPRESSION,BITS,VORZEICHEN,REELL,&
                                      BYTEORDER,DIMOVER,DIM1,DIM2,DIM3, &
                                      PADDING )
!-----------------------------------------------------------------------
!     Skip to the next binary and parse MIME header.
!-----------------------------------------------------------------------
      INTEGER,   INTENT(IN)   :: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: BYTE_IN_FILE,REC_IN_FILE
      INTEGER(1),INTENT(INOUT):: LAST_CHAR,BUFFER(FCB_BYTES_IN_REC)
      INTEGER,   INTENT(OUT)  :: ENCODING,SIZE,ID,COMPRESSION,BITS,  &
                                 VORZEICHEN,REELL
      CHARACTER(*), INTENT(OUT):: BYTEORDER,DIGEST
      INTEGER(8),      INTENT(OUT):: DIMOVER
      INTEGER(8),      INTENT(OUT):: DIM1
      INTEGER(8),      INTENT(OUT):: DIM2
      INTEGER(8),      INTENT(OUT):: DIM3
      INTEGER(8),      INTENT(OUT):: PADDING

      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_OPEN_CIFIN',
      `INTERFACE
      INTEGER FUNCTION FCB_OPEN_CIFIN(FILNAM,TAPIN,LAST_CHAR,                &
      FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,BUFFER)
!-----------------------------------------------------------------------
! --  Open CBF file named FILNAM and connect to unit number TAPIN
!-----------------------------------------------------------------------
!     We have chosen to use the direct access method to read the file
!     with explicit buffer handling. This approach is general but
!     clumpsy. Rather than putting the buffer and its control variables
!     into COMMON these are passed as local arguments to make the routines
!     inherently ''`threadsafe''` in a parallel programming environment.
!     Note also, that a reading error could occur for the last record
!     if it did not fill a full block. This could be avoided if the
!     images were padded with a sufficient number of additional bytes
!     (arbitrary values) after the end of the valid binary data.
!
!     The more natural method would use byte stream I/O which is,
!     unfortunately, only an extension of Fortran 90 that has been
!     implemented in some compilers (like the Intel ifort) but
!     not in all (like the SGI IRIX f90).
!     For BSD style opens, there is a special variant on the direct
!     access open with a recl of 1 to give byte-by-byte access.
!-----------------------------------------------------------------------
! FILNAM   - Name of the file countaining the image              (GIVEN)
! TAPIN    - Fortran device unit number assigned to image file   (GIVEN)
! LAST_CHAR - 
!            Last character read                                (RESULT)
! FCB_BYTES_IN_REC -
!            Number of bytes in a record                         (GIVEN)
! BYTE_IN_FILE -
!            Byte (counting from 1) of the byte to read         (RESULT)
! REC_IN_FILE -
!            Record (counting from 1) of next record to read    (RESULT)
! BUFFER -   Array of length FCB_BYTES_IN_REC                    (GIVEN)
!-----------------------------------------------------------------------
      IMPLICIT                   NONE
      CHARACTER(len=*),INTENT(IN) :: FILNAM
      INTEGER,         INTENT(IN) :: TAPIN,FCB_BYTES_IN_REC
      INTEGER(1),      INTENT(OUT):: LAST_CHAR
      INTEGER,         INTENT(OUT):: BYTE_IN_FILE,REC_IN_FILE
      INTEGER(1),    INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl

m4_dnl
m4_define(`fcb_interface_FCB_READ_BITS',
      `INTERFACE
      INTEGER FUNCTION FCB_READ_BITS(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE,             &
				     BITCOUNT,IINT,LINT)
!-----------------------------------------------------------------------
! Get integer value starting at BYTE_IN_FILE from file TAPIN
! continuing through BITCOUNT bits, with sign extension.  
! (first byte is BYTE_IN_FILE=1)
!-----------------------------------------------------------------------
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      INTEGER,   INTENT(INOUT):: BCOUNT
      INTEGER(1),INTENT(INOUT):: BBYTE
      INTEGER,      INTENT(IN):: BITCOUNT
      INTEGER,      INTENT(IN):: LINT
      INTEGER(4),  INTENT(OUT):: IINT(LINT)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_READ_BYTE',
      `INTERFACE
      INTEGER FUNCTION FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
				     REC_IN_FILE,BYTE_IN_FILE,IBYTE)
!-----------------------------------------------------------------------
! Get byte number BYTE_IN_FILE from file  (first byte is BYTE_IN_FILE=1)
!-----------------------------------------------------------------------
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      INTEGER(1),  INTENT(OUT):: IBYTE
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_READ_LINE',
      `INTERFACE
      INTEGER FUNCTION FCB_READ_LINE(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,  &
			 BYTE_IN_FILE,REC_IN_FILE,BUFFER,LINE,N,LINELEN)
!-----------------------------------------------------------------------
!     Reads successive bytes into byte array LINE(N), stopping at N,
!     error or first CR(Z''`0D''`) or LF(Z''`0A''`), discarding a LF after a CR.
!-----------------------------------------------------------------------
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC,N
      INTEGER,   INTENT(INOUT):: BYTE_IN_FILE,REC_IN_FILE
      INTEGER,     INTENT(OUT):: LINELEN
      INTEGER(1),INTENT(INOUT):: LAST_CHAR,BUFFER(FCB_BYTES_IN_REC)
      INTEGER(1),  INTENT(OUT):: LINE(N)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_READ_IMAGE_I2',
      `INTERFACE
      INTEGER FUNCTION FCB_READ_IMAGE_I2(ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2, PADDING,                &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,                     &
        REC_IN_FILE,BUFFER)
      
!-----------------------------------------------------------------------
! Reads a 16-bit integer twos complement 2D image
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
! DIM2     - The slowest dimension                               (GIVEN)
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
      INTEGER(8),   INTENT(IN):: DIM1,DIM2
      INTEGER(2),  INTENT(OUT):: ARRAY(DIM1,DIM2)
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN
      INTEGER,     INTENT(OUT):: COMPRESSION
      INTEGER(8),  INTENT(OUT):: PADDING
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_READ_IMAGE_I4',
      `INTERFACE
      INTEGER FUNCTION FCB_READ_IMAGE_I4(ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2, PADDING,                &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,                     &
        REC_IN_FILE,BUFFER)
      
!-----------------------------------------------------------------------
! Reads a 32-bit integer twos complement 2D image
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
! DIM2     - The slowest dimension                               (GIVEN)
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
      INTEGER(8),   INTENT(IN):: DIM1,DIM2
      INTEGER(4),  INTENT(OUT):: ARRAY(DIM1,DIM2)
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN
      INTEGER,     INTENT(OUT):: COMPRESSION
      INTEGER(8),  INTENT(OUT):: PADDING
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_READ_IMAGE_3D_I2',
      `INTERFACE
      INTEGER FUNCTION FCB_READ_IMAGE_3D_I2(ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2, DIM3, PADDING,        &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,                   &
        REC_IN_FILE,BUFFER)
      
!-----------------------------------------------------------------------
! Reads a 16-bit integer twos complement 3D image
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
! DIM2     - The slowest dimension                               (GIVEN)
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
      INTEGER(8),   INTENT(IN):: DIM1,DIM2,DIM3
      INTEGER(2),  INTENT(OUT):: ARRAY(DIM1,DIM2,DIM3)
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN
      INTEGER,     INTENT(OUT):: COMPRESSION
      INTEGER(8),  INTENT(OUT):: PADDING
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_READ_IMAGE_3D_I4',
      `INTERFACE
      INTEGER FUNCTION FCB_READ_IMAGE_3D_I4(ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2, DIM3, PADDING,        &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,                   &
        REC_IN_FILE,BUFFER)
      
!-----------------------------------------------------------------------
! Reads a 32-bit integer twos complement 3D image
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
! DIM2     - The slowest dimension                               (GIVEN)
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
      INTEGER(8),   INTENT(IN):: DIM1,DIM2,DIM3
      INTEGER(4),  INTENT(OUT):: ARRAY(DIM1,DIM2,DIM3)
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN
      INTEGER,     INTENT(OUT):: COMPRESSION
      INTEGER(8),  INTENT(OUT):: PADDING
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_READ_INTEGER',
      `INTERFACE
      INTEGER FUNCTION FCB_READ_INTEGER(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE,                &
				     VALSIGN,BITCOUNT,IINT,LINT)
!-----------------------------------------------------------------------
! Get integer value starting at BYTE_IN_FILE from file TAPIN
! continuing through BITCOUNT bits, with optional sign extension.  
! (first byte is BYTE_IN_FILE=1)
!-----------------------------------------------------------------------
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      INTEGER,   INTENT(INOUT):: BCOUNT
      INTEGER(1),INTENT(INOUT):: BBYTE
      INTEGER,      INTENT(IN):: VALSIGN,BITCOUNT
      INTEGER,      INTENT(IN):: LINT
      INTEGER(4),  INTENT(OUT):: IINT(LINT)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_READ_XDS_I2',
      `INTERFACE
      INTEGER FUNCTION FCB_READ_XDS_I2(FILNAM,TAPIN,NX,NY,IFRAME,JFRAME)
!-----------------------------------------------------------------------
! Reads a 32 bit integer two''`s complement image compressed by a
! BYTE-OFFSET algorithm. W. Kabsch,  Version 9-2006
!
! REVISED 1-2007, H. J. Bernstein to conform to CBFlib_0.7.7
! (http://www.bernstein-plus-sons.com/software/CBF)
!
! The BYTE-OFFSET algorithm is a slightly simplified version of
! that described in Andy Hammersley''`s web page
! (http://www.esrf.fr/computing/Forum/imgCIF/cbf_definition.html)
!
!-----------------------------------------------------------------------
! FILNAM   - Name of the file countaining the image              (GIVEN)
! TAPIN    - Fortran device unit number assigned to image file   (GIVEN)
!   NX     - Number of "fast" pixels of the image                (GIVEN)
!   NY     - Number of "slow" pixels of the image                (GIVEN)
! IFRAME   - 16 bit coded image as needed by XDS                (RESULT)
! JFRAME   - 32 bit scratch array                               (RESULT)
! Returns (as function value)                                   (RESULT)
!             1: cannot handle this CBF format (not implemented)
!             0: No error
!            -1: Cannot determine endian architecture of this machine
!            -2: Cannot open image file
!            -3: Wrong image format
!            -4: Cannot read image
!-----------------------------------------------------------------------
      IMPLICIT                       NONE
      CHARACTER(len=*),INTENT(IN) :: FILNAM
      INTEGER,         INTENT(IN) :: TAPIN,NX,NY
      INTEGER(2),      INTENT(OUT):: IFRAME(NX*NY)
      INTEGER(4),      INTENT(OUT):: JFRAME(NX,NY)
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_SKIP_WHITESPACE',
      `INTERFACE
      INTEGER FUNCTION FCB_SKIP_WHITESPACE(TAPIN,LAST_CHAR,             &
		       FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,BUFFER,&
		       LINE,N,LINELEN,ICUR,FRESH_LINE)
!-----------------------------------------------------------------------
!     Skips forward on the current LINE of size N with data in
!     LINE(1:LINELEN) from the current position ICUR moving over
!     whitespace and comments, reading new lines into LINE if
!     needed. The flag FRESH_LINE indicates that a fresh line
!     should be read on entry.
!-----------------------------------------------------------------------
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC,N
      INTEGER,   INTENT(INOUT):: BYTE_IN_FILE,REC_IN_FILE,LINELEN,ICUR, &
				 FRESH_LINE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC),LINE(N),      &
				 LAST_CHAR
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_UPDATE_JPA_POINTERS_I2',
      `INTERFACE
      INTEGER FUNCTION FCB_UPDATE_JPA_POINTERS_I2(TRAIL_INDEX_ARRAY,&
        NDIM1, NDIM2, ARRAY, DIM1, DIM2, AVERAGE, COMPRESSION)      
      INTEGER(8),INTENT(INOUT):: TRAIL_INDEX_ARRAY(4), NDIM1, NDIM2 
      INTEGER(8),   INTENT(IN):: DIM1,DIM2
      INTEGER(2),   INTENT(IN):: ARRAY(DIM1,DIM2)
      INTEGER(4),  INTENT(OUT):: AVERAGE
      INTEGER,      INTENT(IN):: COMPRESSION
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_UPDATE_JPA_POINTERS_I4',
      `INTERFACE
      INTEGER FUNCTION FCB_UPDATE_JPA_POINTERS_I4(TRAIL_INDEX_ARRAY,&
        NDIM1, NDIM2, ARRAY, DIM1, DIM2, AVERAGE, COMPRESSION)
      INTEGER(8),INTENT(INOUT):: TRAIL_INDEX_ARRAY(4), NDIM1, NDIM2 
      INTEGER(8),   INTENT(IN):: DIM1,DIM2
      INTEGER(4),   INTENT(IN):: ARRAY(DIM1,DIM2)
      INTEGER(4),  INTENT(OUT):: AVERAGE
      INTEGER,      INTENT(IN):: COMPRESSION
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_UPDATE_JPA_POINTERS_3D_I2',
      `INTERFACE
      INTEGER FUNCTION FCB_UPDATE_JPA_POINTERS_3D_I2(TRAIL_INDEX_ARRAY,&
        NDIM1, NDIM2, NDIM3, ARRAY, DIM1, DIM2, DIM3, AVERAGE, COMPRESSION)
      INTEGER(8),INTENT(INOUT):: TRAIL_INDEX_ARRAY(8), NDIM1, NDIM2, NDIM3
      INTEGER(8),   INTENT(IN):: DIM1,DIM2,DIM3
      INTEGER(2),   INTENT(IN):: ARRAY(DIM1,DIM2,DIM3)
      INTEGER(4),  INTENT(OUT):: AVERAGE
      INTEGER,      INTENT(IN):: COMPRESSION
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_define(`fcb_interface_FCB_UPDATE_JPA_POINTERS_3D_I4',
      `INTERFACE
      INTEGER FUNCTION FCB_UPDATE_JPA_POINTERS_3D_I4(TRAIL_INDEX_ARRAY,&
        NDIM1, NDIM2, NDIM3, ARRAY, DIM1, DIM2, DIM3, AVERAGE, COMPRESSION)
      
      IMPLICIT                   NONE
      INTEGER(8),INTENT(INOUT):: TRAIL_INDEX_ARRAY(8), NDIM1, NDIM2, NDIM3
      INTEGER(8),   INTENT(IN):: DIM1,DIM2,DIM3
      INTEGER(4),   INTENT(IN):: ARRAY(DIM1,DIM2,DIM3)
      INTEGER(4),  INTENT(OUT):: AVERAGE
      INTEGER,      INTENT(IN):: COMPRESSION
      END FUNCTION
!-----------------------------------------------------------------------
      END INTERFACE')m4_dnl
m4_dnl
m4_dnl macro version of cbf file open to be used both in the library
m4_dnl and in applications that wish to avoid the long argument list
m4_dnl
m4_dnl The macro should be called with 3 arguments
m4_dnl     $1 should give any additional parameters to OPEN
m4_dnl        such as ERR=120 or IOSTAT=IOS
m4_dnl     $2 should give any additional parameters for the
m4_dnl        first read such as ERR=140 or IOSTAT=IOS
m4_dnl     $3 should give any the action to take if the file
m4_dnl        does not begin with "###CBF: "
m4_define(`fcb_macro_FCB_OPEN_CIFIN',
``!-----------------------------------------------------------------------
! --  Open CBF file named FILNAM and connect to unit number TAPIN
!-----------------------------------------------------------------------
!     We have chosen to use the direct access method to read the file
!     with explicit buffer handling. This approach is general but
!     clumpsy. Rather than putting the buffer and its control variables
!     into COMMON these are passed as local arguments to make the routines
!     inherently '''``threadsafe'''`` in a parallel programming environment.
!     Note also, that a reading error could occur for the last record
!     if it did not fill a full block. This could be avoided if the
!     images were padded with a sufficient number of additional bytes
!     (arbitrary values) after the end of the valid binary data.
!
!     The more natural method would use byte stream I/O which is,
!     unfortunately, only an extension of Fortran 90 that has been
!     implemented in some compilers (like the Intel ifort) but
!     not in all (like the SGI IRIX f90).
!     For BSD style opens, there is a special variant on the direct
!     access open with a recl of 1 to give byte-by-byte access.
!-----------------------------------------------------------------------
      INQUIRE(IOLENGTH=FCB_RECORD_SIZE)BUFFER
      OPEN(UNIT=TAPIN,FILE=TRIM(FILNAM),STATUS='''``OLD'''``,ACTION='''``READ'''``,     &
        ACCESS='''``DIRECT'''``,FORM='''``UNFORMATTED'''``,RECL=FCB_RECORD_SIZE,        &
        $1)
      ! *** DEBUG *** PRINT *, "RECL: ", FCB_RECORD_SIZE
      DO BYTE_IN_FILE = 1, FCB_BYTES_IN_REC
        BUFFER(BYTE_IN_FILE) = 0
      END DO
      READ(TAPIN,REC=1,$2)BUFFER     !Read the first record' 
m4_ifelse($3,`',`',`      IF (FCB_CI_STRNCMPARR("###CBF: ",BUFFER,FCB_BYTES_IN_REC,8).NE.0) &
       '$3` !Check for presence of the CBF-format keyword')`
      REC_IN_FILE=1
      BYTE_IN_FILE=0
      LAST_CHAR=0
'')m4_dnl
m4_define(`fcb_interface_LIST_ALL',`
!
      !     Definitions of fcblib interfaces
      !
      fcb_interface_FCB_ATOL_WCNT
      fcb_interface_FCB_CI_STRNCMPARR
      fcb_interface_FCB_DECOMPRESS_PACKED_I2
      fcb_interface_FCB_DECOMPRESS_PACKED_I4
      fcb_interface_FCB_DECOMPRESS_PACKED_3D_I2
      fcb_interface_FCB_DECOMPRESS_PACKED_3D_I4
      fcb_interface_FCB_EXIT_BINARY
      fcb_interface_FCB_NBLEN_ARRAY
      fcb_interface_FCB_NEXT_BINARY
      fcb_interface_FCB_OPEN_CIFIN
      fcb_interface_FCB_READ_BITS
      fcb_interface_FCB_READ_BYTE
      fcb_interface_FCB_READ_INTEGER
      fcb_interface_FCB_READ_LINE
      fcb_interface_FCB_READ_XDS_I2
      fcb_interface_FCB_SKIP_WHITESPACE
      fcb_interface_FCB_UPDATE_JPA_POINTERS_I2
      fcb_interface_FCB_UPDATE_JPA_POINTERS_I4
      fcb_interface_FCB_UPDATE_JPA_POINTERS_3D_I2
      fcb_interface_FCB_UPDATE_JPA_POINTERS_3D_I4
')