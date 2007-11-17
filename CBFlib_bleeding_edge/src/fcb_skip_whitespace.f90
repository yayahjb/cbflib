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
      IMPLICIT                   NONE
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC,N
      INTEGER,   INTENT(INOUT):: BYTE_IN_FILE,REC_IN_FILE,LINELEN,ICUR, &
				 FRESH_LINE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC),LINE(N),      &
				 LAST_CHAR
! Local variables
      INTEGER                    IC,COMMENT_LEVEL
! External functions called
      INTEGER                    FCB_READ_LINE,FCB_NBLEN_ARRAY
!-----------------------------------------------------------------------
      FCB_SKIP_WHITESPACE = 0
      IF (FRESH_LINE.NE.0) THEN
	 ICUR = LINELEN+1
	 RETURN
      END IF

      IC = ICUR
      COMMENT_LEVEL = 0
      DO
	 IF ((IC.LE.LINELEN).AND.(LINE(IC).NE.IACHAR(' ')).AND.         &
	     (LINE(IC).NE.Z'09').AND.(LINE(IC).NE.IACHAR('(')) )EXIT

	 IF (IC.GT.LINELEN) THEN
	    FCB_SKIP_WHITESPACE = FCB_READ_LINE (TAPIN,LAST_CHAR,       &
		       FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,BUFFER,&
			LINE,N,LINELEN)
	    IF (FCB_SKIP_WHITESPACE.NE.0) RETURN
	    IC = 1
	    IF ((LINELEN.EQ.0)                                          &
	      .OR.(FCB_NBLEN_ARRAY(LINE(1:LINELEN),LINELEN).EQ.0)       &
	      .OR.(FCB_NBLEN_ARRAY(LINE(1:1),1).NE.0)) THEN
	       FRESH_LINE = 1
	       ICUR = LINELEN+1
	       RETURN
	    END IF
	 ELSE
	    IF (LINE(IC) .EQ. IACHAR('(')) THEN
	       IC = IC+1
	       COMMENT_LEVEL = COMMENT_LEVEL+1
	       DO
		  IF (COMMENT_LEVEL.EQ.0)EXIT
		  IF (IC.GT.LINELEN) THEN
		     FCB_SKIP_WHITESPACE=FCB_READ_LINE(TAPIN,LAST_CHAR, &
			FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,      &
			BUFFER,LINE,N,LINELEN)
		     IF (FCB_SKIP_WHITESPACE.NE.0) RETURN
		     IC = 1
		     IF ((LINELEN.EQ.0).OR.                             &
		      (FCB_NBLEN_ARRAY(LINE(1:LINELEN),LINELEN).EQ.0)   &
			.OR. FCB_NBLEN_ARRAY(LINE(1:1),1).NE.0) THEN
			FRESH_LINE = 1
			ICUR = LINELEN+1
			RETURN
		     END IF
		  ELSE
		     SELECT CASE (LINE(IC))
		     CASE (Z'5C')      ! backslash
			IC = IC+1      ! force skip of next character
		     CASE (Z'28')      ! open paren
			COMMENT_LEVEL = COMMENT_LEVEL+1;
		     CASE (Z'29')      ! close paren
			COMMENT_LEVEL = COMMENT_LEVEL-1;
		     END SELECT
		     IC = IC+1;
		  END IF
	       END DO
	    ELSE
	       IC = IC+1;
	    END IF
	 END IF
      END DO
      FRESH_LINE = 0
      FCB_SKIP_WHITESPACE = 0
      ICUR = IC
      RETURN
      END FUNCTION FCB_SKIP_WHITESPACE
