      INTEGER FUNCTION FCB_NBLEN_ARRAY(ARRAY, ARRAYLEN)
!-----------------------------------------------------------------------
!     Returns the non-blank length of an array
!-----------------------------------------------------------------------
      IMPLICIT                 NONE
      INTEGER,    INTENT(IN):: ARRAYLEN
      INTEGER(1), INTENT(IN):: ARRAY(ARRAYLEN)
      INTEGER                  I,J
!-----------------------------------------------------------------------
      FCB_NBLEN_ARRAY = 0
      DO I = ARRAYLEN,1,-1
	 IF (FCB_NBLEN_ARRAY.NE.0)EXIT
	 J=ARRAY(I)
	 IF ((J.NE.IACHAR(' ')).AND.(J.NE.Z'09').AND.(J.NE.Z'0A').AND.  &
	     (J.NE.Z'0D').AND.(J.NE.0)) FCB_NBLEN_ARRAY = I
      END DO
      RETURN
      END FUNCTION FCB_NBLEN_ARRAY
