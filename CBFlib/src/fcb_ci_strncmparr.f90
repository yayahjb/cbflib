      INTEGER FUNCTION FCB_CI_STRNCMPARR(STRING, ARRAY, N, LIMIT)
!-----------------------------------------------------------------------
! Compares up to LIMIT characters of STRING and ARRAY case insensitive
!-----------------------------------------------------------------------
      IMPLICIT                      NONE
      CHARACTER(LEN=*),INTENT(IN):: STRING
      INTEGER,         INTENT(IN):: N,LIMIT
      INTEGER(1),      INTENT(IN):: ARRAY(N)
      INTEGER                       I,J,K,IA,IZ,UP
!-----------------------------------------------------------------------
      IA=IACHAR('a')
      IZ=IACHAR('z')
      UP=IACHAR('A')-IA

      FCB_CI_STRNCMPARR = 0
      DO I = 1,LIMIT
	 J=0
	 IF (I.LE.LEN(STRING)) THEN
	    J=IACHAR(STRING(I:I))
	    IF ((J.GE.IA).AND.(J.LE.IZ))J=J+UP
	 ENDIF
	 K=0
	 IF (I.LE.N) THEN
	    K=ARRAY(I)
	    IF ((K.GE.IA).AND.(K.LE.IZ))K=K+UP
	 ENDIF
	 FCB_CI_STRNCMPARR = J-K
	 IF (J.NE.K)EXIT
      ENDDO
      RETURN
      END FUNCTION FCB_CI_STRNCMPARR
