m4_include(`fcblib_defines.m4')m4_dnl
`      INTEGER FUNCTION FCB_NEXT_BINARY(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,&
                                      BYTE_IN_FILE,REC_IN_FILE,BUFFER,  &
                                      ENCODING,SIZE,ID,DIGEST,          &
                                      COMPRESSION,BITS,VORZEICHEN,REELL,&
                                      BYTEORDER,DIMOVER,DIM1,DIM2,DIM3, &
                                      PADDING )
!-----------------------------------------------------------------------
!     Skip to the next binary and parse MIME header.
!-----------------------------------------------------------------------
      IMPLICIT                   NONE
      INTEGER,   INTENT(IN)   :: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: BYTE_IN_FILE,REC_IN_FILE
      INTEGER(1),INTENT(INOUT):: LAST_CHAR,BUFFER(FCB_BYTES_IN_REC)
      INTEGER,   INTENT(OUT)  :: ENCODING
      INTEGER, INTENT(OUT)        :: SIZE    !Binary size
      INTEGER, INTENT(OUT)        :: ID      !Binary ID
      CHARACTER(len=*),INTENT(OUT):: DIGEST  !Message digest
      INTEGER,         INTENT(OUT):: COMPRESSION
      INTEGER,         INTENT(OUT):: BITS,VORZEICHEN,REELL
      CHARACTER(len=*),INTENT(OUT):: BYTEORDER
      ! Possible parameter values for BYTEORDER are:
         ! "LITTLE_ENDIAN"     supported
         ! "BIG_ENDIAN"    not supported
      INTEGER(8),      INTENT(OUT):: DIMOVER
      INTEGER(8),      INTENT(OUT):: DIM1
      INTEGER(8),      INTENT(OUT):: DIM2
      INTEGER(8),      INTENT(OUT):: DIM3
      INTEGER(8),      INTENT(OUT):: PADDING
'      
      fcb_param_ENC_LIST_ALL
      fcb_param_CBF_LIST_ALL
`
      
      !External functions called'
      fcb_interface_FCB_SKIP_WHITESPACE
      fcb_interface_FCB_CI_STRNCMPARR
      fcb_interface_FCB_NBLEN_ARRAY
      fcb_interface_FCB_READ_LINE
      fcb_interface_FCB_ATOL_WCNT
      
`!-----------------------------------------------------------------------
'      fcb_errcode_CBF_FORMAT
`
      INTEGER,PARAMETER           ::  LINESIZE=2048
      INTEGER CONTINUATION
      INTEGER(1) LINE(LINESIZE)     ! BUFFER FOR THE NEXT LINE
      INTEGER LINELEN               ! VALID CHARACTERS IN LINE
      INTEGER IC                    ! CHARACTER WITHIN LINE
      INTEGER STATE                 ! SELECTION FROM VALUE (0, ...)
      INTEGER ITEM                  ! 1 FOR MIME ITEM FOUND, 0 OTHERWISE
      INTEGER LINE_COUNT            ! NUMBER OF LINES INTO HEADER
      INTEGER FRESH_LINE
      INTEGER QUOTE
      INTEGER TEXT_BITS
      INTEGER COUNT
      INTEGER FAILURE
      INTEGER I,INTEXT,J

      INTEGER VALUELEN(12)

      CHARACTER*29 BOUNDARY
      CHARACTER*32 VALUE(12)
      DATA BOUNDARY/"--CIF-BINARY-FORMAT-SECTION--"/
      DATA VALUE/                           &
        "Content-Type:",                    & !  /* State 0  */
        "Content-Transfer-Encoding:",       & !  /* State 1  */
        "Content-MD5:",                     & !  /* State 2  */
        "X-Binary-Size:",                   & !  /* State 3  */
        "X-Binary-ID:",                     & !  /* State 4  */
        "X-Binary-Element-Type:",           & !  /* State 5  */
        "X-Binary-Element-Byte-Order:",     & !  /* State 6  */
        "X-Binary-Size-Fastest-Dimension:", & !  /* State 7  */
        "X-Binary-Size-Second-Dimension:",  & !  /* State 8  */
        "X-Binary-Size-Third-Dimension:",   & !  /* State 9  */
        "X-Binary-Size-Padding:",           & !  /* State 10 */
        "X-Binary-Number-of-Elements:"      & !  /* State 11 */
      /
!-----------------------------------------------------------------------
      DO I = 1,12
        VALUELEN(I)=LEN(TRIM(VALUE(I)))
      END DO

      FAILURE = 0

! --  Repeat : Skip lines until the start of a text field is reached and
! --  then loop until a mime boundary or end of the text field is reached
      INTEXT=0
      DO
         IF (FCB_READ_LINE(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,BYTE_IN_FILE,&
            REC_IN_FILE,BUFFER,LINE,LINESIZE,LINELEN).NE.0 ) THEN
            FCB_NEXT_BINARY=1
            RETURN
         END IF
         ! *** DEBUG *** PRINT *," LINELEN, LINE: ", LINELEN, LINE(1:LINELEN)
         IF (LINELEN.GT.0)THEN
            IF (INTEXT.EQ.0)THEN
               IF (LINE(1).EQ.IACHAR(''`;''`))INTEXT=1 !start of a text field
               ! *** DEBUG *** PRINT *, "FOUND START OF TEXT FIELD"
            ELSE
               IF (LINE(1).EQ.IACHAR(''`;''`))THEN
                  IF (LINELEN.EQ.1.OR.LINE(2).EQ.32.OR.LINE(2).EQ.9)    &
                     INTEXT=0     !end of the text field is reached
               ENDIF
               IF (FCB_CI_STRNCMPARR(BOUNDARY,LINE,LINELEN,29).EQ.0)EXIT
            ENDIF
         ENDIF
      ENDDO
      ! *** DEBUG *** PRINT *, "MIME BOUNDARY --CIF-BINARY-FORMAT-SECTION-- FOUND"

!-----------------------------------------------------------------------
      STATE = -1
      LINE_COUNT = 0
      FRESH_LINE = 0
      ENCODING = 0
      SIZE = 0
      ID = 0
      DIGEST = ""
      COMPRESSION = CBF_NONE
      BITS = 0
      VORZEICHEN = -1
      REELL = -1
      BYTEORDER="LITTLE_ENDIAN"
      DIMOVER = 0
      DIM1 = 0
      DIM2 = 0
      DIM3 = 0
      PADDING = 0

      DO
        IF (FRESH_LINE.EQ.0) THEN
          IF (FCB_READ_LINE(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,           &
            BYTE_IN_FILE,REC_IN_FILE,BUFFER,LINE,LINESIZE,LINELEN).NE.0)&
            THEN
            FCB_NEXT_BINARY=1
            RETURN
          ENDIF
          IF (LINELEN.GT.0 .AND. LINE(1).EQ.IACHAR(''`;''`) .AND.         &
            (LINELEN.EQ.1 .OR. LINE(2).EQ.IACHAR(''` ''`) .OR.            &
            LINE(2).EQ.Z''`09''`) )  GO TO 100
          IF (FCB_CI_STRNCMPARR(BOUNDARY,LINE,LINELEN,29).EQ.0)       &
            GO TO 100
        END IF

        FRESH_LINE = 0
        LINE_COUNT = LINE_COUNT+1
        CONTINUATION = 0
        IF (LINELEN.GT.0.AND.                                          &
          (LINE(1).EQ.IACHAR(''` ''`).OR.LINE(1).EQ.Z''`09''`))CONTINUATION=1
        ITEM = 0
        IF (CONTINUATION .EQ. 0 )  THEN
          DO IC = 1, LINELEN
            IF ((LINE(IC).EQ.IACHAR(''`:''`)).AND.(IC.GT.1))ITEM=1
            IF ((ITEM.NE.0).OR.(LINE(IC).LE.32.OR.LINE(IC).GE.127)) EXIT
          END DO
        END IF

        ! Check for the end of the header
        IF (LINE_COUNT.GT.1.AND.FCB_NBLEN_ARRAY(LINE,LINELEN).EQ.0) THEN
          FCB_NEXT_BINARY = 0
          RETURN
        END IF

        ! Check for valid header-ness of line
        IF (ITEM.EQ.0.AND.(LINE_COUNT.EQ.1.OR.CONTINUATION.EQ.0)) GO TO 110

        ! Look for the entries we are interested in
        IC = 1
        IF (ITEM.NE.0) THEN
          DO STATE = 11,0,-1
            J=STATE+1
            I=VALUELEN(J)
            IF (FCB_CI_STRNCMPARR(VALUE(J)(1:I),LINE,LINELEN,I).EQ.0)THEN
              IC = I+1
              EXIT
            END IF
          END DO
        END IF

        ! Skip past comments and whitespace
          IF (FCB_SKIP_WHITESPACE(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,      &
            BYTE_IN_FILE,REC_IN_FILE,BUFFER,LINE,LINESIZE,LINELEN,IC,   &
            FRESH_LINE).NE.0) GO TO 100

        SELECT CASE (STATE) ! Get the value

        CASE (0)  !"Content-Type:"
          I=LINELEN-IC+1
          IF (FCB_CI_STRNCMPARR("application/",LINE(IC:LINELEN),I,12).NE.0.AND.&
            FCB_CI_STRNCMPARR("image/",      LINE(IC:LINELEN),I, 6).NE.0.AND.&
            FCB_CI_STRNCMPARR("text/",       LINE(IC:LINELEN),I, 5).NE.0.AND.&
            FCB_CI_STRNCMPARR("audio/",      LINE(IC:LINELEN),I, 6).NE.0.AND.&
            FCB_CI_STRNCMPARR("video/",      LINE(IC:LINELEN),I, 6).NE.0)    &
            GO TO 110
          DO
            IF (IC.GT.LINELEN)EXIT
            ! Skip to the end of the section (a semicolon)
            DO
              IF (IC.GT.LINELEN)EXIT
              IF (LINE(IC).EQ.Z''`22''`) THEN    ! double quote
                IC = IC+1
                DO
                  IF (IC.GT.LINELEN)EXIT
                  IF (LINE(IC).EQ.Z''`22''`) THEN  !double quote
                    IC = IC+1
                    EXIT
                  ELSE
                    IF (LINE(IC).EQ.Z''`5C''`) THEN !backslash
                      IC = IC+1
                      END IF
                    IF (IC .LE. LINELEN) IC = IC+1
                  END IF
                END DO
              ELSE
                IF (LINE(IC).EQ.IACHAR(''`(''`)) THEN
                  FCB_NEXT_BINARY = FCB_SKIP_WHITESPACE(TAPIN,      &
                    LAST_CHAR,FCB_BYTES_IN_REC,BYTE_IN_FILE,        &
                    REC_IN_FILE,BUFFER,LINE,LINESIZE,LINELEN,IC,    &
                    FRESH_LINE)
                  IF (FCB_NEXT_BINARY.NE.0) RETURN
                ELSE
                  IF (LINE(IC).EQ.IACHAR(''`;''`)) THEN
                    IC = IC+1
                    EXIT
                  ELSE
                    IC = IC+1
                  END IF
                END IF
              END IF
            END DO
          FCB_NEXT_BINARY = FCB_SKIP_WHITESPACE(TAPIN,LAST_CHAR,        &
             FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,BUFFER,          &
             LINE,LINESIZE,LINELEN,IC,FRESH_LINE)
          IF (FCB_NEXT_BINARY.NE.0) RETURN
          IF (FCB_CI_STRNCMPARR("conversions",                          &
             LINE(IC:LINELEN), LINELEN-IC+1, 11) .EQ.0 ) THEN
             IC = IC+11
             FCB_NEXT_BINARY = FCB_SKIP_WHITESPACE(TAPIN,LAST_CHAR,     &
                FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,BUFFER,       &
                LINE,LINESIZE,LINELEN,IC,FRESH_LINE)
             IF (FCB_NEXT_BINARY.NE.0) RETURN
             IF (LINE(IC).EQ.IACHAR(''`=''`)) THEN
                IC = IC+1
                FCB_NEXT_BINARY = FCB_SKIP_WHITESPACE(TAPIN,LAST_CHAR,  &
                   FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,BUFFER,     &
                   LINE,LINESIZE,LINELEN,IC,FRESH_LINE)
                IF (FCB_NEXT_BINARY.NE.0) RETURN
                QUOTE = 0
                IF (LINE(IC).EQ.Z''`22''`) QUOTE=1  ! double quote
                COMPRESSION = CBF_NONE
                IF (FCB_CI_STRNCMPARR("x-CBF_PACKED",                   &
                   LINE(IC+QUOTE:LINELEN),LINELEN-IC-QUOTE+1,12).EQ.0)  &
                   COMPRESSION=CBF_PACKED
                IF (FCB_CI_STRNCMPARR("x-CBF_PACKED_V2",                &
                   LINE(IC+QUOTE:LINELEN),LINELEN-IC-QUOTE+1,15).EQ.0)  &
                   COMPRESSION=CBF_PACKED_V2
                IF (FCB_CI_STRNCMPARR("x-CBF_CANONICAL",                &
                   LINE(IC+QUOTE:LINELEN),LINELEN-IC-QUOTE+1,15).EQ.0)  &
                   COMPRESSION=CBF_CANONICAL
                IF (FCB_CI_STRNCMPARR("x-cbf_byte_offset",              &
                   LINE(IC+QUOTE:LINELEN),LINELEN-IC-QUOTE+1,17).EQ.0)  &
                   COMPRESSION=CBF_BYTE_OFFSET
                IF (FCB_CI_STRNCMPARR("x-cbf_predictor",                &
                   LINE(IC+QUOTE:LINELEN),LINELEN-IC-QUOTE+1,15).EQ.0)  &
                   COMPRESSION=CBF_PREDICTOR
                IF ( (COMPRESSION .EQ. CBF_PACKED) .OR.                 &
                     (COMPRESSION .EQ. CBF_PACKED_V2) ) THEN
                  DO
                    IF (IC.GT.LINELEN)EXIT
                    ! Skip to the end of the section (a semicolon)
                    DO
                      IF (IC.GT.LINELEN)EXIT
                      IF (LINE(IC).EQ.Z''`22''`) THEN    ! double quote
                        IC = IC+1
                        DO
                          IF (IC.GT.LINELEN)EXIT
                          IF (LINE(IC).EQ.Z''`22''`) THEN  !double quote
                            IC = IC+1
                            EXIT
                          ELSE
                            IF (LINE(IC).EQ.Z''`5C''`) THEN !backslash
                              IC = IC+1
                              END IF
                            IF (IC .LE. LINELEN) IC = IC+1
                          END IF
                        END DO
                      ELSE
                        IF (LINE(IC).EQ.IACHAR(''`(''`)) THEN
                          FCB_NEXT_BINARY = FCB_SKIP_WHITESPACE(TAPIN,      &
                            LAST_CHAR,FCB_BYTES_IN_REC,BYTE_IN_FILE,        &
                            REC_IN_FILE,BUFFER,LINE,LINESIZE,LINELEN,IC,    &
                            FRESH_LINE)
                          IF (FCB_NEXT_BINARY.NE.0) RETURN
                        ELSE
                          IF (LINE(IC).EQ.IACHAR(''`;''`)) THEN
                            IC = IC+1
                            EXIT
                          ELSE
                            IC = IC+1
                          END IF
                        END IF
                      END IF
                      FCB_NEXT_BINARY = FCB_SKIP_WHITESPACE(TAPIN,      &
                        LAST_CHAR,FCB_BYTES_IN_REC,BYTE_IN_FILE,        &
                        REC_IN_FILE,BUFFER,LINE,LINESIZE,LINELEN,IC,    &
                        FRESH_LINE)
                      IF (FCB_NEXT_BINARY.NE.0) RETURN
                      QUOTE = 0
                      IF (LINE(IC).EQ.Z''`22''`) QUOTE=1  ! double quote
                      IF (FCB_CI_STRNCMPARR("uncorrelated_sections",    &
                        LINE(IC+QUOTE:LINELEN),LINELEN-IC-QUOTE+1,21).EQ.0)  &
                        COMPRESSION = IOR(COMPRESSION,CBF_UNCORRELATED_SECTIONS)
                      IF (FCB_CI_STRNCMPARR("flat",    &
                        LINE(IC+QUOTE:LINELEN),LINELEN-IC-QUOTE+1,4).EQ.0)  &
                        COMPRESSION = IOR(COMPRESSION,CBF_FLAT_IMAGE)
                    END DO
                  END DO
                END IF
             END IF
          END IF
        END DO
        STATE = -1
      ! *** DEBUG *** PRINT *, "COMPRESSION: ", COMPRESSION

      CASE (1)  ! Binary encoding
         FAILURE = 1
         QUOTE = 0;
         IF (LINE(IC) .EQ. Z''`22''`) QUOTE = 1  !double quote
         IF (FCB_CI_STRNCMPARR("Quoted-Printable",                      &
            LINE(IC+QUOTE:LINELEN), LINELEN-IC-QUOTE+1, 16) .EQ. 0)THEN
            IF (IC+16.EQ.LINELEN+1 .OR.                                 &
               FCB_NBLEN_ARRAY(LINE(IC+16),1).EQ.0 .OR.                 &
               LINE(IC+16).EQ.IACHAR(''`(''`).OR.                           &
               (QUOTE.EQ.1.AND.LINE(IC+16).EQ.Z''`22''`)) THEN  !double quote
               FAILURE = 0
               ENCODING = ENC_QP
            END IF
         END IF
         IF (FCB_CI_STRNCMPARR("Base64",                                &
            LINE(IC+QUOTE:LINELEN), LINELEN-IC-QUOTE+1, 6) .EQ. 0)THEN
            IF (IC+6.EQ.LINELEN+1 .OR.                                  &
               FCB_NBLEN_ARRAY(LINE(IC+6),1).EQ.0 .OR.                  &
               LINE(IC+6).EQ.IACHAR(''`(''`).OR.                            &
               (QUOTE.EQ.1.AND.LINE(IC+6).EQ.Z''`22''`)) THEN  ! double quote
               FAILURE = 0
               ENCODING = ENC_BASE64
            END IF
         END IF
         IF (FCB_CI_STRNCMPARR("X-Base32k",                             &
            LINE(IC+QUOTE:LINELEN), LINELEN-IC-QUOTE+1, 9) .EQ. 0)THEN
            IF (IC+9.EQ.LINELEN+1 .OR.                                  &
               FCB_NBLEN_ARRAY(LINE(IC+9),1).EQ.0 .OR.                  &
               LINE(IC+9).EQ.IACHAR(''`(''`).OR.                            &
               (QUOTE.EQ.1.AND.LINE(IC+9).EQ.Z''`22''`)) THEN  ! double quote
               FAILURE = 0
               ENCODING = ENC_BASE32K
            END IF
         END IF
         IF (FCB_CI_STRNCMPARR("X-Base8",                               &
            LINE(IC+QUOTE:LINELEN), LINELEN-IC-QUOTE+1, 7) .EQ. 0)THEN
            IF (IC+7.EQ.LINELEN+1 .OR.                                  &
               FCB_NBLEN_ARRAY(LINE(IC+7),1).EQ.0 .OR.                  &
               LINE(IC+7).EQ.IACHAR(''`(''`).OR.                            &
               (QUOTE.EQ.1.AND.LINE(IC+7).EQ.Z''`22''`)) THEN  ! double quote
               FAILURE = 0
               ENCODING = ENC_BASE8
            END IF
         END IF
         IF (FCB_CI_STRNCMPARR("X-Base10",                              &
            LINE(IC+QUOTE:LINELEN), LINELEN-IC-QUOTE+1, 8) .EQ. 0)THEN
            IF (IC+8.EQ.LINELEN+1 .OR.                                  &
               FCB_NBLEN_ARRAY(LINE(IC+8),1).EQ.0 .OR.                  &
               LINE(IC+8).EQ.IACHAR(''`(''`).OR.                            &
               (QUOTE.EQ.1.AND.LINE(IC+8).EQ.Z''`22''`)) THEN  ! double quote
               FAILURE = 0
               ENCODING = ENC_BASE10
            END IF
         END IF
         IF (FCB_CI_STRNCMPARR("X-Base16",                              &
            LINE(IC+QUOTE:LINELEN), LINELEN-IC-QUOTE+1, 8) .EQ. 0)THEN
            IF (IC+8.EQ.LINELEN+1 .OR.                                  &
               FCB_NBLEN_ARRAY(LINE(IC+8),1).EQ.0 .OR.                  &
               LINE(IC+8).EQ.IACHAR(''`(''`).OR.                            &
               (QUOTE.EQ.1.AND.LINE(IC+8).EQ.Z''`22''`)) THEN   !double quote
               FAILURE = 0
               ENCODING = ENC_BASE16
            END IF
         END IF
         IF (FCB_CI_STRNCMPARR("7bit",                                  &
            LINE(IC+QUOTE:LINELEN), LINELEN-IC-QUOTE+1, 4) .EQ. 0 .OR.  &
            FCB_CI_STRNCMPARR("8bit",                                   &
            LINE(IC+QUOTE:LINELEN), LINELEN-IC-QUOTE+1, 4) .EQ. 0 ) THEN
            IF (IC+4.EQ.LINELEN+1 .OR.                                  &
               FCB_NBLEN_ARRAY(LINE(IC+4),1).EQ.0 .OR.                  &
               LINE(IC+4).EQ.IACHAR(''`(''`).OR.                            &
               (QUOTE.EQ.1.AND.LINE(IC+4).EQ.Z''`22''`)) THEN  !double quote
               FAILURE = 0
               ENCODING = ENC_NONE
            END IF
         END IF
         IF (FCB_CI_STRNCMPARR("Binary",                                &
            LINE(IC+QUOTE:LINELEN), LINELEN-IC-QUOTE+1, 6) .EQ. 0) THEN
            IF (IC+6.EQ.LINELEN+1 .OR.                                  &
               FCB_NBLEN_ARRAY(LINE(IC+6),1).EQ.0 .OR.                  &
               LINE(IC+6).EQ.IACHAR(''`(''`).OR.                            &
               (QUOTE.EQ.1.AND.LINE(IC+6).EQ.Z''`22''`)) THEN  ! double quote
               FAILURE = 0
               ENCODING = ENC_NONE
            END IF
         END IF
         IF (FAILURE.NE.0)GO TO 110
      ! *** DEBUG *** PRINT *, "ENCODING: ", ENCODING

      CASE (2)  ! Message digest
         IF (LINELEN.GE.IC+23) THEN
            DO I = IC,IC+23
               DIGEST(I-IC+1:I-IC+1)=ACHAR(LINE(I))
            END DO
         ELSE
            DO I = IC,LINELEN
               DIGEST(I-IC+1:I-IC+1)=ACHAR(LINE(I))
            END DO
            DIGEST(LINELEN-IC+1:24)=''` ''`
         END IF
      ! *** DEBUG *** PRINT *, "DIGEST: ", DIGEST

      CASE (3)  ! Binary size
         SIZE = FCB_ATOL_WCNT(LINE(IC:LINELEN),LINELEN-IC+1,COUNT)
      ! *** DEBUG *** PRINT *, "SIZE: ", SIZE

      CASE (4)  ! Binary ID */
         ID = FCB_ATOL_WCNT(LINE(IC:LINELEN),LINELEN-IC+1,COUNT)
      ! *** DEBUG *** PRINT *, "ID: ", ID

      CASE (5)
      ! Binary element type (signed/unsigned ?-bit integer)
      !                or   (signed ?-bit real/complex IEEE)
         FAILURE = 3
         QUOTE = 0
         DO
            IF (IC.GT.LINELEN)EXIT
            FCB_NEXT_BINARY = FCB_SKIP_WHITESPACE(TAPIN,LAST_CHAR,      &
               FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,BUFFER,        &
               LINE,LINESIZE,LINELEN,IC,FRESH_LINE)
            IF (FCB_NEXT_BINARY.NE.0) RETURN
            IF (LINE(IC) .EQ. Z''`22''`) THEN     ! double quote
               IF (QUOTE.NE.0) EXIT
               IC = IC+1
               QUOTE = QUOTE+1
            END IF
            IF (FAILURE .EQ. 3) THEN
               IF (FCB_CI_STRNCMPARR("signed",                         &
                  LINE(IC:LINELEN),LINELEN-IC+1, 6) .EQ. 0) THEN
                  IC = IC+6
                  VORZEICHEN = 1
                  FAILURE = FAILURE-1
               END IF
               IF (FCB_CI_STRNCMPARR("unsigned",                       &
                  LINE(IC:LINELEN),LINELEN-IC+1, 8) .EQ. 0) THEN
                  IC = IC+8
                  VORZEICHEN = 0
                  FAILURE = FAILURE-1
               END IF
            END IF
            IF (FAILURE .EQ. 2) THEN
               COUNT = 0
               TEXT_BITS = FCB_ATOL_WCNT(LINE(IC:LINELEN),LINELEN-IC+1,COUNT)
               IF (FCB_CI_STRNCMPARR("-bit",                           &
                  LINE(IC+COUNT:LINELEN),LINELEN-IC-COUNT+1,3).EQ.0)THEN
                  IF (COUNT.NE.0.AND.TEXT_BITS.GT.0.AND.TEXT_BITS.LE.64)THEN
                     IC = IC+COUNT+1
                     BITS = TEXT_BITS
                     IF (LINE(IC) .EQ. IACHAR(''` ''`)) IC = IC+1
                     FAILURE = FAILURE-1
                  END IF
               END IF
            END IF
            IF (FAILURE .EQ. 1) THEN
               IF (FCB_CI_STRNCMPARR("integer",                        &
                  LINE(IC:LINELEN),LINELEN-IC+1, 7 ) .EQ. 0) THEN
                  FAILURE=FAILURE-1
                  REELL=0
               ELSE
                  IF (FCB_CI_STRNCMPARR("real",                        &
                     LINE(IC:LINELEN), LINELEN-IC+1, 4 ) .EQ. 0 ) THEN
                     IC = IC+4
                     IF (LINE(IC).EQ.IACHAR(''` ''`)) IC = IC+1
                     IF (FCB_CI_STRNCMPARR("ieee",                     &
                        LINE(IC:LINELEN),LINELEN-IC+1,4).EQ.0) THEN
                        FAILURE=FAILURE-1
                        REELL = 1
                     END IF
                  ELSE
                     IF (FCB_CI_STRNCMPARR("complex",                  &
                        LINE(IC:LINELEN),LINELEN-IC+1,7).EQ.0) THEN
                        IC = IC+7
                        IF (LINE(IC).EQ.IACHAR(''` ''`)) IC = IC+1
                        IF (FCB_CI_STRNCMPARR("ieee",                  &
                           LINE(IC:LINELEN),LINELEN-IC+1,4).EQ.0) THEN
                           FAILURE=FAILURE-1
                           REELL = 1
                        END IF
                     END IF
                  END IF
               END IF
            END IF
            IF (IC .LE. LINELEN) IC = IC+1
         END DO
         IF (FAILURE .NE. 0)GO TO 110
! *** DEBUG *** PRINT *, "VORZEICHEN, BITS, REELL",VORZEICHEN,BITS,REELL

      CASE (6)  ! Byte order of elements (only endian-ness is supported)
         IF (0.EQ.FCB_CI_STRNCMPARR("big_endian",                      &
            LINE(IC:LINELEN),LINELEN-IC+1,10) ) THEN
            BYTEORDER="BIG_ENDIAN"
         ELSE
            IF (0.EQ.FCB_CI_STRNCMPARR("little_endian",                &
               LINE(IC:LINELEN),LINELEN-IC+1,13)) THEN
               BYTEORDER="LITTLE_ENDIAN"
            ELSE
               GO TO 110
            END IF
         END IF
      ! *** DEBUG *** PRINT *, "BYTEORDER: ", BYTEORDER

      CASE(7)   ! Size of fastest dimension (Number of "fast" pixels)
         DIM1 = FCB_ATOL_WCNT(LINE(IC:LINELEN),LINELEN-IC+1,COUNT)
      ! *** DEBUG *** PRINT *,"DIM1: ",DIM1

      CASE(8)   ! Size of second fastest dimension (Number of "slow" pixels)
         DIM2 = FCB_ATOL_WCNT(LINE(IC:LINELEN),LINELEN-IC+1,COUNT)
      ! *** DEBUG *** PRINT *,"DIM2: ",DIM2

      CASE(9)   ! Size of third dimension
         DIM3 = FCB_ATOL_WCNT(LINE(IC:LINELEN),LINELEN-IC+1,COUNT)
      ! *** DEBUG *** PRINT *,"DIM3: ",DIM3

      CASE(10)  ! Size of padding after the data
         PADDING = FCB_ATOL_WCNT(LINE(IC:LINELEN),LINELEN-IC+1,COUNT)
      ! *** DEBUG *** PRINT *,"PADDING: ",PADDING

      CASE (11) ! Overall number of elements
         DIMOVER = FCB_ATOL_WCNT(LINE(IC:LINELEN),LINELEN-IC+1,COUNT)
      ! *** DEBUG *** PRINT *,"DIMOVER: ",DIMOVER

      END SELECT

      ENDDO

100   FCB_NEXT_BINARY = -1
      RETURN
110   FCB_NEXT_BINARY = CBF_FORMAT
      RETURN
      END FUNCTION FCB_NEXT_BINARY'
