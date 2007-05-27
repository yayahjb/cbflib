m4_include(`fcblib_defines.m4')m4_dnl
`      INTEGER FUNCTION FCB_OPEN_CIFIN(FILNAM,TAPIN,LAST_CHAR,                &
      FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,BUFFER)
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
      INTEGER                        FCB_RECORD_SIZE
'      
      fcb_errcode_CBF_FORMAT
      fcb_errcode_CBF_FILEOPEN
      fcb_errcode_CBF_FILEREAD
      fcb_interface_FCB_CI_STRNCMPARR

      
fcb_macro_FCB_OPEN_CIFIN(`IOSTAT=FCB_OPEN_CIFIN',
      `IOSTAT=FCB_OPEN_CIFIN',
      `THEN
        FCB_OPEN_CIFIN = CBF_FILEREAD
      ENDIF')`
      
      RETURN
      END FUNCTION FCB_OPEN_CIFIN'
