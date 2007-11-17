      INTEGER FUNCTION FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
				     REC_IN_FILE,BYTE_IN_FILE,IBYTE)
!-----------------------------------------------------------------------
! Get byte number BYTE_IN_FILE from file  (first byte is BYTE_IN_FILE=1)
!-----------------------------------------------------------------------
      IMPLICIT                   NONE
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE
      INTEGER,   INTENT(INOUT):: REC_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      INTEGER(1),  INTENT(OUT):: IBYTE
      INTEGER                    I,J,K,KREC
!-----------------------------------------------------------------------
      IBYTE=0
      FCB_READ_BYTE=0
      I=(BYTE_IN_FILE-1)/FCB_BYTES_IN_REC
      J=BYTE_IN_FILE-I*FCB_BYTES_IN_REC
      I=I+1
      IF (I.NE.REC_IN_FILE) THEN    !Requested byte is not in BUFFER
        REC_IN_FILE = I
        READ(TAPIN,IOSTAT=FCB_READ_BYTE,REC=REC_IN_FILE)BUFFER
        ! *** DEBUG *** PRINT *,"REC_IN_FILE: ", REC_IN_FILE
        ! *** DEBUG *** PRINT *,"FCB_READ_BYTE: ", FCB_READ_BYTE
        ! *** DEBUG *** PRINT *,"BUFFER: ", BUFFER
        IF (FCB_READ_BYTE.GT.0) THEN
          INQUIRE(TAPIN,NEXTREC=KREC)
          IF (REC_IN_FILE.LT.KREC) THEN
            DO K = 1, FCB_BYTES_IN_REC
              BUFFER(K) = 0
            END DO
            READ(TAPIN,IOSTAT=FCB_READ_BYTE,REC=REC_IN_FILE)BUFFER
            IF (FCB_READ_BYTE.GT.0) FCB_READ_BYTE=0
          END IF
        END IF
        IF (FCB_READ_BYTE.NE.0) RETURN
      END IF
      IBYTE=BUFFER(J)
      RETURN
      END FUNCTION FCB_READ_BYTE
