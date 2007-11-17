      INTEGER(8) FUNCTION FCB_ATOL_WCNT(ARRAY,N,CNT)
!-----------------------------------------------------------------------
!     Converts bytes in ARRAY to an INTEGER(8), consuming CNT bytes
!-----------------------------------------------------------------------
      IMPLICIT                NONE
      INTEGER,  INTENT(OUT):: CNT
      INTEGER,   INTENT(IN):: N
      INTEGER(1),INTENT(IN):: ARRAY(N)

      INTEGER, PARAMETER :: I0=Z'30',& !IACHAR('0')
			    I9=Z'39',& !IACHAR('9')
			    IM=Z'2D',& !IACHAR('-')
			    IP=Z'2B',& !IACHAR('+')
			    SP=Z'20',& !IACHAR(' ')
			    HT=Z'09'   !tab position in the ASCII code
      INTEGER               I,K,BLANK,VORZEICHEN
!-----------------------------------------------------------------------
      FCB_ATOL_WCNT = 0
      CNT = 0
      BLANK = 0
      VORZEICHEN  = 0
      DO I = 1,N
	 K=ARRAY(I)
	 IF (K.GE.I0 .AND. K.LE.I9 ) THEN
	    FCB_ATOL_WCNT = FCB_ATOL_WCNT*10+(K-I0)
	    BLANK = -1
	    IF (VORZEICHEN.EQ.0) VORZEICHEN=1
	 ELSE IF (K.EQ.IM .OR. K.EQ.IP ) THEN
	    IF (VORZEICHEN.NE.0) EXIT
	    IF (K.EQ.IM) VORZEICHEN =-1
	    IF (K.EQ.IP) VORZEICHEN = 1
	 ELSE IF (K.EQ.SP .OR. K.EQ.HT ) THEN
	    IF (BLANK.LT.0) EXIT
	    BLANK = BLANK+1
	 ELSE
	    EXIT
	 END IF
	 CNT = CNT+1
      END DO
      IF (VORZEICHEN.LT.0) FCB_ATOL_WCNT = -FCB_ATOL_WCNT
      RETURN
      END FUNCTION FCB_ATOL_WCNT
