m4_include(`fcblib_defines.m4')m4_dnl
`      INTEGER FUNCTION FCB_READ_BITS(TAPIN,FCB_BYTES_IN_REC,BUFFER,    &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE,             &
				     BITCOUNT,IINT,LINT)
!-----------------------------------------------------------------------
! Get integer value starting at BYTE_IN_FILE from file TAPIN
! continuing through BITCOUNT bits, with sign extension.  
! (first byte is BYTE_IN_FILE=1)
!-----------------------------------------------------------------------
      IMPLICIT                   NONE
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      INTEGER,   INTENT(INOUT):: BCOUNT
      INTEGER(1),INTENT(INOUT):: BBYTE
      INTEGER,      INTENT(IN):: BITCOUNT
      INTEGER,      INTENT(IN):: LINT
      INTEGER(4),  INTENT(OUT):: IINT(LINT)
      INTEGER                    I,J,LBITCOUNT,COUNT,KINTS
      INTEGER(8)                 BITCODE,TBITCODE, M, MASK8
!-----------------------------------------------------------------------

      INTEGER                    MAXBITS, NUMINTS
'
      fcb_interface_FCB_READ_BYTE
`
      MAXBITS = 32
      NUMINTS = (BITCOUNT+MAXBITS-1)/MAXBITS
      MASK8 = Z''`000000FF''`
      
      DO KINTS = 1,NUMINTS
        LBITCOUNT = MAXBITS
        IF (KINTS.EQ.NUMINTS) LBITCOUNT = BITCOUNT-(NUMINTS-1)*32
        COUNT = BCOUNT
        BITCODE = BBYTE
        BITCODE = IAND(BITCODE,MASK8)
        DO
          IF (COUNT .GE. LBITCOUNT) EXIT
          BYTE_IN_FILE=BYTE_IN_FILE+1
          FCB_READ_BITS =                                    &
            FCB_READ_BYTE(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
            REC_IN_FILE,BYTE_IN_FILE,BBYTE)
          IF (FCB_READ_BITS.NE.0) RETURN
	      BCOUNT=8
	      TBITCODE = BBYTE
	      TBITCODE = IAND(TBITCODE,MASK8)
	      CALL MVBITS(TBITCODE,0,MIN(8,32-COUNT),BITCODE,COUNT)
	      COUNT = COUNT+8
        END DO
      
        ! SIGN EXTEND
      
        IF (LBITCOUNT .LT. MAXBITS) THEN
          M = 1
          M = ISHFT(M,LBITCOUNT-1)
          IF (IAND(BITCODE,M).NE.0) THEN
            IINT(KINTS) = IOR(BITCODE,-M)
          ELSE
            IINT(KINTS) = IAND(BITCODE,NOT(-M))
          ENDIF
        ELSE
          IINT(KINTS) = BITCODE
        ENDIF
      
        ! SAVE THE REMAINING BITS FOR NEXT TIME
      
        TBITCODE = BBYTE
        TBITCODE = ISHFT(IAND(TBITCODE,MASK8),-(BCOUNT-(COUNT-LBITCOUNT)) )
        BBYTE = TBITCODE
        BCOUNT = COUNT-LBITCOUNT
      
      END DO
            
      FCB_READ_BITS = 0
      
      RETURN
      
      END FUNCTION FCB_READ_BITS
      


      INTEGER FUNCTION FCB_READ_INTEGER(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE,                &
				     VALSIGN,BITCOUNT,IINT,LINT)
!-----------------------------------------------------------------------
! Get integer value starting at BYTE_IN_FILE from file TAPIN
! continuing through BITCOUNT bits, with optional sign extension.  
! (first byte is BYTE_IN_FILE=1)
!-----------------------------------------------------------------------
      IMPLICIT                   NONE
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      INTEGER,   INTENT(INOUT):: BCOUNT
      INTEGER(1),INTENT(INOUT):: BBYTE
      INTEGER,      INTENT(IN):: VALSIGN,BITCOUNT
      INTEGER,      INTENT(IN):: LINT
      INTEGER(4),  INTENT(OUT):: IINT(LINT)
      
      INTEGER                    SIGNBITS, VALBITS, NUMINTS, FRI
      INTEGER                    I,J,LBITCOUNT,COUNT
      INTEGER(4)                 TVAL(4), BITCODE,TBITCODE, M
      INTEGER(4)                 XSIGN(1)
      
      
'      fcb_errcode_CBF_OVERFLOW

       fcb_interface_FCB_READ_BITS
`
!-----------------------------------------------------------------------

      IF (BITCOUNT .LE. 0) THEN
        IINT(1) = 0
        FCB_READ_INTEGER = 0
        RETURN
      END IF

      SIGNBITS = BITCOUNT-32
      
      IF (SIGNBITS .GT. 0) THEN
        VALBITS = BITCOUNT-SIGNBITS
      ELSE
        VALBITS = BITCOUNT
      END IF
      
      ! READ THE VALUE
      
      FRI = &
        FCB_READ_BITS(TAPIN,FCB_BYTES_IN_REC,BUFFER,                    &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE,             &
				     VALBITS,IINT,LINT)
      FCB_READ_INTEGER = FRI
      IF (FCB_READ_INTEGER .NE. 0) RETURN
      
      IF (VALBITS .LT. 32 .AND. VALSIGN .EQ. 0) THEN
      
        IINT(1) = IAND(IINT(1),NOT(-ISHFT(1,VALBITS)) )
        
      ENDIF
      
      DO
        IF (SIGNBITS .LE. 0) EXIT
        IF (SIGNBITS .LT. 32) THEN
          FRI = &
             FCB_READ_BITS(TAPIN,FCB_BYTES_IN_REC,BUFFER,         &
			   REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE,             &
			   SIGNBITS,XSIGN,1)
		  FCB_READ_INTEGER = FRI
	      IF (FCB_READ_INTEGER .NE. 0) RETURN    
        ELSE
          FRI = &
             FCB_READ_BITS(TAPIN,FCB_BYTES_IN_REC,BUFFER,         &
			   REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE,             &
			   32,XSIGN,1)
		  FCB_READ_INTEGER = FRI
	      IF (FCB_READ_INTEGER .NE. 0) RETURN            
        END IF
        SIGNBITS = SIGNBITS-32
        
        IF ((IINT(1) .LT. 0 .AND. VALSIGN.NE.0 .AND. XSIGN(1).NE.-1)  &
          .OR. ((IINT(1) .GE. 0 .OR. VALSIGN.EQ.0) .AND. XSIGN(1).NE.0)&
          ) THEN
          FCB_READ_INTEGER = CBF_OVERFLOW
          IINT(1) = -1
          IF (VALSIGN.NE.0) THEN
            IF (XSIGN(1).GE. 0) THEN
              IINT(1) = Z''`7FFFFFFF''`
            ELSE
              IINT(1) = Z''`80000000''`
            END IF
          END IF
          RETURN
        END IF
      END DO
      FCB_READ_INTEGER = 0
      RETURN
      END FUNCTION FCB_READ_INTEGER'
