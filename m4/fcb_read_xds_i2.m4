m4_include(`fcblib_defines.m4')
`      INTEGER FUNCTION FCB_READ_XDS_I2(FILNAM,TAPIN,NX,NY,IFRAME,JFRAME)
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
! Returns (as function value)                                   (RESULT)
!             CBF_FORMAT (=1): 
!                cannot handle this CBF format (not implemented)
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
      INTEGER(8)       NELEM,NELEM_READ
! --  Definition of CBF_FORMAT'
      fcb_errcode_CBF_FORMAT
`! --  External functions called'
      fcb_interface_FCB_READ_BYTE
      fcb_interface_FCB_NEXT_BINARY
      fcb_interface_FCB_CI_STRNCMPARR
      fcb_interface_FCB_DECOMPRESS_PACKED_I4
      INTEGER(2)       CNT2PIX
`! --  Local variables
      INTEGER,PARAMETER:: FCB_BYTES_IN_REC='m4_ifelse(`fcb_bytes_in_rec',`',4096,`fcb_bytes_in_rec')`
      INTEGER             FCB_RECORD_SIZE,BYTE_IN_FILE,REC_IN_FILE,     &
			  STEP,FIRST2,LAST2,FIRST4,LAST4,I,J,IOS
      INTEGER(4)          DIFF,PIXVALUE,MARKER,IADR
      INTEGER(2)          SHORTINT
      INTEGER(1)          BUFFER(FCB_BYTES_IN_REC),LAST_CHAR,ONEBYTE,   &
			  TWOBYTES(2),FOURBYTES(4),ENDIANORDER(4),      &
			  MARKBYTES(4)
      INTEGER  ENCODING
'
      fcb_param_ENC_LIST_ALL
`      INTEGER  SIZE
      INTEGER  ID
      INTEGER  COMPRESSION
'
      fcb_param_CBF_LIST_ALL
`
      INTEGER  BITS,VORZEICHEN,REELL
      CHARACTER(len=24)   DIGEST
      CHARACTER(len=14)   BYTEORDER
      !Possible parameter values for BYTEORDER are:
	 !"LITTLE_ENDIAN"     supported
	 !"BIG_ENDIAN"    not supported
      INTEGER(8)  DIMOVER
      INTEGER(8)  DIM1       !Number of "fast" pixels of the image
      INTEGER(8)  DIM2       !Number of "slow" pixels of the image
      INTEGER(8)  DIM3
      INTEGER(8)  PADDING
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

'fcb_macro_FCB_OPEN_CIFIN(`ERR=120',`IOSTAT=IOS',`GO TO 130') m4_dnl
`
      IF (IOS.GT.0) GO TO 140
!-----------------------------------------------------------------------
! --   Skip to the next binary and parse the MIME header
!-----------------------------------------------------------------------
      IF (FCB_NEXT_BINARY(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,BYTE_IN_FILE,&
	REC_IN_FILE,BUFFER,ENCODING,SIZE,ID,DIGEST,COMPRESSION,BITS,    &
	VORZEICHEN,REELL,BYTEORDER,DIMOVER,DIM1,DIM2,DIM3,PADDING).NE.0)&
	 GO TO 130
      IF ((DIM1.NE.NX).OR.(DIM2.NE.NY))GO TO 130

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
      FCB_READ_XDS_I2=CBF_FORMAT  !Cannot handle this CBF format
      IF ((BYTEORDER.EQ."LITTLE_ENDIAN").AND.(ENCODING.EQ.ENC_NONE).AND.&
	  (IAND(COMPRESSION,CBF_COMPRESSION_MASK).EQ.CBF_BYTE_OFFSET))THEN
	 PIXVALUE=0
	 DO IADR=1,NX*NY
	    BYTE_IN_FILE=BYTE_IN_FILE+1
	    IF (FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,            &
	       REC_IN_FILE,BYTE_IN_FILE,ONEBYTE).NE.0) GO TO 140
	    DIFF=ONEBYTE
	    IF (DIFF.EQ.-128)THEN
	       DO I=FIRST2,LAST2,STEP
		  BYTE_IN_FILE=BYTE_IN_FILE+1
		  IF (FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,      &
		     REC_IN_FILE,BYTE_IN_FILE,TWOBYTES(I)).NE.0)GO TO 140
	       ENDDO
	       SHORTINT=TRANSFER(TWOBYTES,SHORTINT)
	       DIFF=SHORTINT
	       IF (DIFF.EQ.-32768)THEN
		  DO I=FIRST4,LAST4,STEP
		     BYTE_IN_FILE=BYTE_IN_FILE+1
		     IF (FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,   &
			REC_IN_FILE,BYTE_IN_FILE,FOURBYTES(I)).NE.0)    &
			GO TO 140
		  ENDDO
		  DIFF=TRANSFER(FOURBYTES,DIFF)
	       ENDIF
	    ENDIF
	    PIXVALUE=PIXVALUE+DIFF
	    IFRAME(IADR)=CNT2PIX(PIXVALUE) ! xds-specific 16 bit coding
	 ENDDO
	 FCB_READ_XDS_I2=0  !No error
	 ELSE
      IF ((BYTEORDER.EQ."LITTLE_ENDIAN").AND.(ENCODING.EQ.ENC_NONE).AND.&
	  ((IAND(COMPRESSION,CBF_COMPRESSION_MASK).EQ.CBF_PACKED) .OR.       &
	   (IAND(COMPRESSION,CBF_COMPRESSION_MASK).EQ.CBF_PACKED_V2)))THEN
	  NELEM = NX*NY
	  FCB_READ_XDS_I2=FCB_DECOMPRESS_PACKED_I4 (JFRAME,NELEM,NELEM_READ, &
        VORZEICHEN, COMPRESSION, DIM1, DIM2,  &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,        &
        REC_IN_FILE,BUFFER)
      IF (NELEM_READ.NE.NELEM) PRINT *, "EARLY TERMINATION AT ",NELEM_READ
      PREV_ELEMENT = 0
      DO J = 1,NY
      DO I = 1,NX
      IF (JFRAME(I,J).NE.PREV_ELEMENT) THEN
        PREV_ELEMENT = JFRAME(I,J)
        PRINT *,"ARRAY(",I+(J-1)*NX,") =",JFRAME(I,J)
      ENDIF
      IFRAME(I+(J-1)*NX) = CNT2PIX(JFRAME(I,J))
      END DO
      END DO
      END IF
    END IF

!-----------------------------------------------------------------------
100   CLOSE(TAPIN)
      RETURN
110   FCB_READ_XDS_I2=-1 !Cannot determine endian architecture of this machine
      RETURN
120   FCB_READ_XDS_I2=-2 !Cannot open image file
      RETURN
130   FCB_READ_XDS_I2=-3 !Wrong image format
      GO TO 100
140   FCB_READ_XDS_I2=-4 !Cannot read image
      GO TO 100
      END FUNCTION FCB_READ_XDS_I2

      INTEGER(2) FUNCTION CNT2PIX(I32)
      IMPLICIT              NONE
      INTEGER(4),INTENT(IN)::I32
      INTEGER(4),PARAMETER ::RATIO=32            !compression ratio
      INTEGER(4),PARAMETER ::OFLOW=RATIO*32768   !largest  32 bit INTEGER
      INTEGER(4),PARAMETER ::UFLOW=1-32768/RATIO !smallest 32 bit INTEGER
      REAL(4)                R
! I16=CNT2PIX(I32) codes an integer I32 in the range UFLOW<=I32<=OFLOW
! by a 16 bit number I16.
! J32=PIX2CNT(I16) retrieves an approximation to the original value
! with a maximum absolute error of RATIO/2.
      R=MIN(I32,OFLOW)
      R=MAX(I32,UFLOW)
      IF (I32.GT.32767)R=-R/RATIO
      CNT2PIX=NINT(R)
      RETURN
      END FUNCTION CNT2PIX'
