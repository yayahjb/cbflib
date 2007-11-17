m4_include(`fcblib_defines.m4')`      PROGRAM TEST

      IMPLICIT  NONE
      CHARACTER(LEN=100) LINE
      INTEGER(2) IFRAME(1000,1000), DPREV
      INTEGER(4) JFRAME(1000,1000)
      INTEGER(4) KFRAME(50,60,70)
      INTEGER,PARAMETER:: FCB_BYTES_IN_REC='m4_ifelse(`fcb_bytes_in_rec',`',4096,`fcb_bytes_in_rec')`
      INTEGER   IER, I, J, K, TAPIN, SIZE
      INTEGER   BYTE_IN_FILE, REC_IN_FILE, DTARG, ID
      INTEGER(1) LAST_CHAR, BUFFER(FCB_BYTES_IN_REC)
      INTEGER COMPRESSION, BITS, VORZEICHEN, REELL, ENCODING
      INTEGER(8) DIM1, DIM2, DIM3, DIMOVER, PADDING
      INTEGER(8) NELEM, NELEM_READ
      CHARACTER(len=24)   DIGEST
      CHARACTER(len=14)   BYTEORDER

      
'      
      fcb_interface_FCB_EXIT_BINARY
      fcb_interface_FCB_OPEN_CIFIN
      fcb_interface_FCB_NEXT_BINARY
      fcb_interface_FCB_READ_IMAGE_I2
      fcb_interface_FCB_READ_IMAGE_I4
      fcb_interface_FCB_READ_IMAGE_3D_I2
      fcb_interface_FCB_READ_IMAGE_3D_I4
      
      TAPIN=9

`      PRINT *,''` NAME OF TEST CBF ''`
      READ *, LINE
      
      IER = FCB_OPEN_CIFIN(LINE,TAPIN,LAST_CHAR,                &
      FCB_BYTES_IN_REC,BYTE_IN_FILE,REC_IN_FILE,BUFFER)
      IF (IER.NE.0) THEN
        PRINT *,"FILE OPEN ERROR: ", IER
        STOP
      END IF

      ! Read an  array 1000 x 1000 INTEGER(4) in a flat field of 1000
 
       PRINT *, " 1000 x 1000 I4 TEST "

     
      NELEM = 1000*1000
      DIM1 = 1000
      DIM2 = 1000
      IER =  FCB_READ_IMAGE_I4(JFRAME,NELEM,NELEM_READ, &
        1, COMPRESSION, DIM1, DIM2,  PADDING,           &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,            &
        REC_IN_FILE,BUFFER)

      IF (IER.NE.0) THEN 
         PRINT *,"  FCB_READ_IMAGE_I4 ERROR: ", IER
         STOP
      ELSE
        DPREV = 0
        DO J = 1,1000
        DO I = 1,1000
          DTARG = 1000
          IF (JFRAME(I,J).NE.DTARG) THEN
            PRINT *, "IFRAME(",I,",",J,") = ", &
              JFRAME(I,J), ", SHOULD BE ",DTARG
          END IF
        END DO
        END DO
      END IF
      
      IER = FCB_EXIT_BINARY(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,&
              BYTE_IN_FILE,REC_IN_FILE,BUFFER, PADDING )
              
      IF (IER.NE.0) THEN 
         PRINT *,"  FCB_EXIT_BINARY ERROR: ", IER
         STOP
      END IF
      
 
      ! Read an  array 1000 x 1000 INTEGER(2) in a flat field of 1000

      PRINT *, " 1000 x 1000 I2 TEST "

  
      IER =  FCB_READ_IMAGE_I2(IFRAME,NELEM,NELEM_READ, &
        1, COMPRESSION, DIM1, DIM2,  PADDING,           &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,            &
        REC_IN_FILE,BUFFER)

      IF (IER.NE.0) THEN 
         PRINT *,"  FCB_READ_IMAGE_I2 ERROR: ", IER
         STOP
      ELSE
        DPREV = 0
        DO J = 1,1000
        DO I = 1,1000
          DTARG = 1000
          IF (IFRAME(I,J).NE.DTARG) THEN
            PRINT *, "IFRAME(",I,",",J,") = ", &
              IFRAME(I,J), ", SHOULD BE ",DTARG
          END IF
        END DO
        END DO
      END IF
      
      IER = FCB_EXIT_BINARY(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,&
              BYTE_IN_FILE,REC_IN_FILE,BUFFER, PADDING )
              
      IF (IER.NE.0) THEN 
         PRINT *,"  FCB_EXIT_BINARY ERROR: ", IER
         STOP
      END IF
      

      ! Read an  array 1000 x 1000 INTEGER(4) in a flat field of 1000
      !   except for -3 along the main diagonal and its transpose

      PRINT *, " 1000 x 1000 I4 TEST, WITH -3 on diag and transpose "


      IER =  FCB_READ_IMAGE_I4(JFRAME,NELEM,NELEM_READ, &
        1, COMPRESSION, DIM1, DIM2,  PADDING,           &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,            &
        REC_IN_FILE,BUFFER)

      IF (IER.NE.0) THEN 
         PRINT *,"  FCB_READ_IMAGE_I4 ERROR: ", IER
         STOP
      ELSE
        DPREV = 0
        DO J = 1,1000
        DO I = 1,1000
          DTARG = 1000
          IF (I .EQ. J .OR. 1001-I .EQ. J) THEN
            DTARG = -3
          END IF
          IF (JFRAME(I,J).NE.DTARG) THEN
            PRINT *, "IFRAME(",I,",",J,") = ", &
              JFRAME(I,J), ", SHOULD BE ",DTARG
          END IF
        END DO
        END DO
      END IF
     
      IER = FCB_EXIT_BINARY(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,&
              BYTE_IN_FILE,REC_IN_FILE,BUFFER, PADDING )
              
      IF (IER.NE.0) THEN 
         PRINT *,"  FCB_EXIT_BINARY ERROR: ", IER
         STOP
      END IF
      

      ! Read an  array 1000 x 1000 INTEGER(2) in a flat field of 1000
      !   except for -3 along the main diagonal and its transpose


      PRINT *, " 1000 x 1000 I2 TEST, WITH -3 on diag and transpose "

      IER =  FCB_READ_IMAGE_I2(IFRAME,NELEM,NELEM_READ, &
        1, COMPRESSION, DIM1, DIM2,  PADDING,           &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,            &
        REC_IN_FILE,BUFFER)

      IF (IER.NE.0) THEN 
         PRINT *,"  FCB_READ_IMAGE_I2 ERROR: ", IER
         STOP
      ELSE
        DPREV = 0
        DO J = 1,1000
        DO I = 1,1000
          DTARG = 1000
          IF (I .EQ. J .OR. 1001-I .EQ. J) THEN
            DTARG = -3
          END IF
          IF (IFRAME(I,J).NE.DTARG) THEN
            PRINT *, "IFRAME(",I,",",J,") = ", &
              IFRAME(I,J), ", SHOULD BE ",DTARG
          END IF
        END DO
        END DO
      END IF
      

      IER = FCB_EXIT_BINARY(TAPIN,LAST_CHAR,FCB_BYTES_IN_REC,&
              BYTE_IN_FILE,REC_IN_FILE,BUFFER, PADDING )
              
      IF (IER.NE.0) THEN 
         PRINT *,"  FCB_EXIT_BINARY ERROR: ", IER
         STOP
      END IF
      
            
      ! Read an array 50 x 60 x 70 INTEGER(4) in a flat field of 1000, 
      !  except for -3 along the main diagonal and the values i+j+k-3 
      !  every 1000th pixel

      PRINT *, " 50 x 60 x 70 3D_I4 TEST "

      DIM1 = 50
      DIM2 = 60
      DIM3 = 70
      NELEM = DIM1*DIM2*DIM3

      IER =  FCB_READ_IMAGE_3D_I4(KFRAME,NELEM,NELEM_READ, &
        1, COMPRESSION, DIM1, DIM2, DIM3, PADDING,           &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,            &
        REC_IN_FILE,BUFFER)

      IF (IER.NE.0) THEN 
         PRINT *,"  FCB_READ_IMAGE_3D_I4 ERROR: ", IER
         STOP
      ELSE
        DPREV = 0
        DO K = 1,70
        DO J = 1,60
        DO I = 1,50
           DTARG = 1000
          IF (I .EQ. J .OR. J .EQ. K) THEN
            DTARG = -3
          END IF
          IF (MOD(I-1+(J-1)*50+(K-1)*50*60,1000).EQ.0) THEN
            DTARG = I+J+K-3
          END IF
          IF (KFRAME(I,J,K).NE.DTARG) THEN
            PRINT *, "KFRAME(",I,",",J,",",K,") = ", &
              KFRAME(I,J,K), ", SHOULD BE ",DTARG
          END IF
        END DO
        END DO
        END DO
      END IF
      
      PRINT *, "TESTS COMPLETED"


      STOP
      END
'