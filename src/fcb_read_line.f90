      INTEGER FUNCTION FCB_READ_LINE(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,  &
			 BYTE_IN_FILE,REC_IN_FILE,BUFFER,LINE,N,LINELEN)
!-----------------------------------------------------------------------
!     Reads successive bytes into byte array LINE(N), stopping at N,
!     error or first CR(Z'0D') or LF(Z'0A'), discarding a LF after a CR.
!-----------------------------------------------------------------------
      IMPLICIT                   NONE
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC,N
      INTEGER,   INTENT(INOUT):: BYTE_IN_FILE,REC_IN_FILE
      INTEGER,     INTENT(OUT):: LINELEN
      INTEGER(1),INTENT(INOUT):: LAST_CHAR,BUFFER(FCB_BYTES_IN_REC)
      INTEGER(1),  INTENT(OUT):: LINE(N)
      INTEGER                    I,FCB_READ_BYTE
!-----------------------------------------------------------------------
      LINELEN=0
      DO I = 1,N
        BYTE_IN_FILE=BYTE_IN_FILE+1
        FCB_READ_LINE=FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,      &
                                    REC_IN_FILE,BYTE_IN_FILE,LINE(I))
        IF(FCB_READ_LINE.NE.0)EXIT
        IF (I.EQ.1.AND.LAST_CHAR.EQ.Z'0D'.AND.LINE(I).EQ.Z'0A') THEN
          BYTE_IN_FILE=BYTE_IN_FILE+1
          FCB_READ_LINE=FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,   &
                                    REC_IN_FILE,BYTE_IN_FILE,LINE(I))
        ENDIF
        IF(FCB_READ_LINE.NE.0)EXIT
        LAST_CHAR=LINE(I)
        IF (LINE(I).EQ.Z'0A' .OR. LINE(I).EQ.Z'0D')EXIT
        LINELEN=LINELEN+1
      END DO
      ! *** DEBUG *** WRITE(*,'(I5,1X,80A1)')LINELEN,LINE(1:LINELEN)
      RETURN
      END FUNCTION FCB_READ_LINE
