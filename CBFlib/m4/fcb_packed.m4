m4_dnl  fcb_packed.m4 -- m4 macro file to generate I2, I4, 
m4_dnl                   and 3D_I2 and 3D_I4 versions
m4_dnl                   of FCB_DECOMPRESS_PACKED

m4_include(`fcblib_defines.m4')m4_dnl
m4_define(`fcb_macro_FCB_PACKED',
`!  FCB_PACKED_$1.f90
!
!  Derived from CBFlib cbf_packed.c
!  H. J. Bernstein, 25 February 2007
!
!  Based on J. P. Abrahams pack_c.c
!  incorporated under GPL and LGPL in
!  CBFlib with permission from CCP4 and
!  from J. P. Abramhams
!
!  Also based in part on test_pack.f90
!  translation of an earlier pack_c.c
!  by W. Kabsch and K. Rohm
 
 
!  Update pointers for averaging in J. P. Abrahams CCP4 compression
!  algorithm.  These f90 routines are derived from 
!  cbf_update_jpa_pointers, reworked as one routine
!  for 2-dimensional arrays and one routine for 3-dimensional
!  arrays, with I2 and I4 variants for each.
!
!  This version is for
!
!  m4_ifelse($1,`I2',`2-D INTEGER*2 arrays',
   $1,`I4',`2-D INTEGER*4 arrays',
   $1,`3D_I2',`3-D INTEGER*2 arrays',
   $1,`3D_I4',`3-D INTEGER*4 arrays',`2-D INTEGER*2 arrays')
m4_define(`fcb_3d_flag',`2D')m4_dnl
m4_ifelse($1,`3D_I2',`m4_define(`fcb_3d_flag',`3D')',
          $1,`3D_I4',`m4_define(`fcb_3d_flag',`3D')')m4_dnl
m4_define(`fcb_i2_flag',`I2')m4_dnl
m4_ifelse($1,`I4',`m4_define(`fcb_i2_flag',`I4')',
          $1,`3D_I4',`m4_define(`fcb_i2_flag',`I4')')m4_dnl
!
!  with functions
!  
!  FCB_UPDATE_JPA_POINTERS_$1
!  FCB_DECOMPRESS_PACKED_$1
!
!  In the f90 implementation TRAIL_INDEX_ARRAY(1..8) has been
!  replaced by TRAIL_INDEX_ARRAY(1..4) for 2-dimensional
!  arrays and TRAIL_INDEX_ARRAY(1..8) for 3-dimensional arrays
!  containing array indices or 0, for the array as if it
!  were a linear array.
!
!  On entry, TRAIL_INDEX_ARRAY(1) should point to the data element 
!  immediately prior to the next data element to be processed, either 
!  in the same row (fastest index) or, at the end of the prior row 
!  if the next data element to be processed is at the end of a row 
!   
!  ndim1, ndim2, ndim3 should point to the indices of the same 
!  data element as TRAIL_INDEX_ARRAY(1) points to.  These values 
!  will be incremented to be the indices of the next data element 
!  to be processed before populating TRAIL_INDEX_ARRAY. 
!   
!  On exit, TRAIL_INDEX_ARRAY(1..8) will have been populated with 
!  pointers to the data elements to be used in forming the average. 
!  Elements that will not be used will be set to NULL.   Note 
!  that TRAIL_INDEX_ARRAY(1) may be set to NULL.              
!   
!  If we mark the next element to be processed with a "*" and the 
!  entries in TRAIL_INDEX_ARRAY with their array indices 1 .. 8, the 
!  possible patterns of settings in the general case are: 
!   
!  current section: 
!   
!       - - - - 1 * - - - - 
!       - - - - 4 3 2 - - -  
!       - - - - - - - - - - 
!        
!  prior section: 
!   
!       - - - - - 5 - - - - 
!       - - - - 8 7 6 - - -  
!       - - - - - - - - - - 
!            
!  If there is no prior section (i.e. ndim3 is 0, or  
!  the CBF_UNCORRELATED_SECTIONS flag is set 
!  to indicate discontinuous sections), the values 
!  for TRAIL_INDEX_ARRAY (5..8) will all be NULL.  When 
!  there is a prior section, TRAIL_INDEX_ARRAY(6..8) are 
!  pointers to the elements immediately below the 
!  elements pointed to by TRAIL_INDEX_ARRAY(2..4), but 
!  TRAIL_INDEX_ARRAY(5) is one element further along 
!  its row to be directly below the next element to 
!  be processed. 
!   
!  The first element of the first row of the first section 
!  is a special case, with no averaging.  This function 
!  should not be called for that case.
!
!  In the first row of the first section (ndim2 == 1, 
!  and ndim3 == 1), after the first element (ndim1 > 1),  
!  only TRAIL_INDEX_ARRAY(1) is used 
!   
!  current section: 
!   
!       - - - - 1 * - - - -
 
!  For subsequent rows of the first section (ndim2 > 1, 
!  and ndim3 == 1), for the first element (ndim1 == 1),  
!  two elements from the prior row are used: 
!   
!  current section: 
!   
!       * - - - - - - - - - 
!       3 2 - - - - - - - - 
!       - - - - - - - - - -
 
!  while for element after the first element, but before 
!  the last element of the row, a full set of 4 elements  
!  is used: 
!   
!  current section: 
!   
!       - - - - 1 * - - - - 
!       - - - - 4 3 2 - - -  
!       - - - - - - - - - - 
!        
!  For the last element of a row (ndim1 == dim1-1), two 
!  elements are used 
!   
!  current section: 
!   
!       - - - - - - - - 1 * 
!       - - - - - - - - - 3  
!       - - - - - - - - - - 
!        
!  For sections after the first section, provided the 
!  CBF_UNCORRELATED_SECTIONS flag is not set in compression, 
!  for each non-NULL entry in TRAIL_INDEX_ARRAY (1..4) an entry  
!  is made in TRAIL_INDEX_ARRAY  (5..8), except for the 
!  first element of the first row of a section.  In that 
!  case an entry is made in TRAIL_INDEX_ARRAY(5). 


      INTEGER FUNCTION FCB_UPDATE_JPA_POINTERS_$1(TRAIL_INDEX_ARRAY,&
        NDIM1, NDIM2, m4_ifelse(fcb_3d_flag,`3D',
        `NDIM3, ')ARRAY, DIM1, DIM2, m4_ifelse(fcb_3d_flag,`3D',
        `DIM3, ')AVERAGE, COMPRESSION)
      
      IMPLICIT                   NONE
      INTEGER(8),   INTENT(IN):: DIM1,DIM2`'m4_ifelse(fcb_3d_flag,`3D',`,DIM3')
      INTEGER(8),INTENT(INOUT):: TRAIL_INDEX_ARRAY(`'m4_ifelse(fcb_3d_flag,
                 `3D',`8',`4')), NDIM1, NDIM2`'m4_ifelse(fcb_3d_flag,`3D',`, NDIM3',` ')
      INTEGER(m4_ifelse($1,`I2',2,$1,`3D_I2',2,4)),   INTENT(IN):: ARRAY(DIM1,DIM2`'m4_ifelse(fcb_3d_flag,`3D',`,DIM3'))
      INTEGER(4),  INTENT(OUT):: AVERAGE
      INTEGER,      INTENT(IN):: COMPRESSION
      INTEGER                    I, J, K, IFAST, m4_ifelse(fcb_3d_flag,`3D',`IMID, ')ISLOW
      INTEGER                    LOGTWO(4)
m4_ifelse(fcb_i2_flag,`I2',
`      INTEGER(4),    PARAMETER:: SIGNMASK=Z'''``00008000'''``
      INTEGER(4),    PARAMETER:: LIMMASK=Z'''``0000FFFF'''``')

      fcb_param_CBF_LIST_ALL

      
      DATA LOGTWO / 1,2,0,3 /
      

      AVERAGE = 0
      NDIM1 = NDIM1+1
      IF (NDIM1 .EQ. DIM1+1) THEN
        NDIM1 = 1
        NDIM2 = NDIM2+1
m4_ifelse(fcb_3d_flag,`3D',
``        IF (NDIM2 .EQ. DIM2+1) THEN
          NDIM2 = 1
          NDIM3 = NDIM3+1
        END IF
'')      END IF
      
      DO I = 2,m4_ifelse(fcb_3d_flag,`3D',`8',`4')
        TRAIL_INDEX_ARRAY(I) = 0
      END DO
      
      IF (NDIM2 > 1) THEN            ! NOT IN THE FIRST ROW
        TRAIL_INDEX_ARRAY(2) = TRAIL_INDEX_ARRAY(1)-(DIM1-2)    ! DOWN 1 RIGHT 2
        TRAIL_INDEX_ARRAY(3) = TRAIL_INDEX_ARRAY(1)-(DIM1-1)  ! DOWN 1 RIGHT 1
        IF (NDIM1 > 1) THEN          ! NOT IN THE FIRST COLUMN
          TRAIL_INDEX_ARRAY(4) = TRAIL_INDEX_ARRAY(1)-DIM1      ! DOWN 1
          IF (NDIM1 .EQ. DIM1) THEN  ! LAST COLUMN
            TRAIL_INDEX_ARRAY(2) = 0
            TRAIL_INDEX_ARRAY(4) = 0
          END IF
        ELSE                         ! FIRST COLUMN
          TRAIL_INDEX_ARRAY(1) = 0
        END IF
m4_ifelse(fcb_3d_flag,`3D',
``        IF (NDIM3 .GT. 1 .AND.        &
          IAND(COMPRESSION,CBF_UNCORRELATED_SECTIONS).EQ.0 ) THEN
          IF (TRAIL_INDEX_ARRAY(1).NE.0) THEN
            TRAIL_INDEX_ARRAY(5) =  &
              TRAIL_INDEX_ARRAY(1) - DIM1*DIM2 + 1
          END IF
          DO I = 2,4
            IF (TRAIL_INDEX_ARRAY(I).NE.0) THEN
              TRAIL_INDEX_ARRAY(I+4) =  &
                TRAIL_INDEX_ARRAY(I) - DIM1*DIM2
            END IF
          END DO
        END IF
'')      ELSE                           ! FIRST ROW OF A SECTION
        IF (NDIM1 .EQ. 1 ) THEN
m4_ifelse(fcb_3d_flag,`3D',
``          TRAIL_INDEX_ARRAY(5) = TRAIL_INDEX_ARRAY(1) - (DIM1*DIM2-1)
'')          TRAIL_INDEX_ARRAY(1) = 0
        END IF 
      END IF

      J = 0
      
m4_ifelse(fcb_3d_flag,`3D',
`      DO I = 1,8
        IF (TRAIL_INDEX_ARRAY(I).NE.0) THEN
          J = J+1
          ISLOW = 1+(TRAIL_INDEX_ARRAY(I)-1)/(DIM1*DIM2)
          IMID =  1+MOD(TRAIL_INDEX_ARRAY(I)-1,DIM1*DIM2)
          IMID =  1+(IMID-1)/DIM1
          IFAST = 1+MOD(TRAIL_INDEX_ARRAY(I)-1,DIM1)
          AVERAGE = AVERAGE+ARRAY(IFAST,IMID,ISLOW)
        END IF
      END DO 
',
`      DO I = 1,4
        IF (TRAIL_INDEX_ARRAY(I).NE.0) THEN
          J = J+1
          ISLOW = 1+(TRAIL_INDEX_ARRAY(I)-1)/DIM1
          IFAST = 1+MOD(TRAIL_INDEX_ARRAY(I)-1,DIM1)
          AVERAGE = AVERAGE+ARRAY(IFAST,ISLOW)
        END IF
      END DO 
')      
      K = ISHFT(J,-1)
      IF ( K .GT. 0 ) THEN
m4_ifelse(fcb_i2_flag,`I2',
`        AVERAGE = IAND(AVERAGE,LIMMASK)
        IF (IAND(AVERAGE+K,SIGNMASK).NE.0) AVERAGE = IOR(AVERAGE,NOT(LIMMASK))')
        IF (AVERAGE .GE.0) THEN
          AVERAGE = ISHFT(AVERAGE+K,-LOGTWO(K))
        ELSE
          AVERAGE = NOT(ISHFT(NOT(AVERAGE+K),-LOGTWO(K)))
        ENDIF
      END IF 

      FCB_UPDATE_JPA_POINTERS_$1 = 0
      
      RETURN
      
      END FUNCTION FCB_UPDATE_JPA_POINTERS_$1




      INTEGER FUNCTION FCB_DECOMPRESS_PACKED_$1 (ARRAY,NELEM,NELEM_READ, &
        ELSIGN, COMPRESSION, DIM1, DIM2, m4_ifelse(fcb_3d_flag,`3D',
        ``DIM3, '') &
        TAPIN,FCB_BYTES_IN_REC,BYTE_IN_FILE,                   &
        REC_IN_FILE,BUFFER)
        
      IMPLICIT NONE
        
      INTEGER(8),   INTENT(IN):: DIM1,DIM2`'m4_ifelse(fcb_3d_flag,`3D',
        `,DIM3') 
      INTEGER(m4_ifelse($1,`I2',2,$1,`3D_I2',2,4)),  INTENT(OUT):: ARRAY(DIM1,DIM2`'m4_ifelse(fcb_3d_flag,`3D',
        `,DIM3'))
      INTEGER(8),  INTENT(OUT):: NELEM_READ
      INTEGER(8),   INTENT(IN):: NELEM
      INTEGER,      INTENT(IN):: ELSIGN, COMPRESSION
      INTEGER,      INTENT(IN):: TAPIN,FCB_BYTES_IN_REC
      INTEGER,   INTENT(INOUT):: REC_IN_FILE,BYTE_IN_FILE
      INTEGER(1),INTENT(INOUT):: BUFFER(FCB_BYTES_IN_REC)
      
      INTEGER(8)                 TRAIL_INDEX_ARRAY(m4_ifelse(fcb_3d_flag,`3D',8,4))
      INTEGER                    I, V2FLAG, AVGFLAG, CLIPBITS, BITS
      INTEGER(8)                 NDIM1, NDIM2, m4_ifelse(fcb_3d_flag,`3D',
        `NDIM3,')LDIM1, LDIM2`'m4_ifelse(fcb_3d_flag,`3D',
        `, LDIM3')
      INTEGER                    COUNT, PIXEL, NEXT(1), IINT, IBITS
      INTEGER                    BCOUNT
      INTEGER(1)                 BBYTE
      INTEGER                    ERRORCODE, KBITS, KSIGN, PIXELCOUNT
      
      INTEGER(4)                 VORZEICHEN, UNSIGN, ELEMENT, LIMIT
      INTEGER(4)                 LAST_ELEMENT
      INTEGER(4)                 DISCARD(2), OFFSET(3)
      ! *** DEBUG *** INTEGER(4)                 PREV_ELEMENT, PREV_INDEX
      
      fcb_interface_FCB_READ_INTEGER
      fcb_interface_FCB_READ_BITS               
      fcb_interface_FCB_UPDATE_JPA_POINTERS_$1

      fcb_param_CBF_LIST_ALL
      fcb_errcode_LIST_ALL


      
      ! Version 1 bit lengths
      INTEGER(4),PARAMETER:: &
        CBF_PACKED_BITS1 = 4, &
        CBF_PACKED_BITS2 = 5, &
        CBF_PACKED_BITS3 = 6, &
        CBF_PACKED_BITS4 = 7, &
        CBF_PACKED_BITS5 = 8, &
        CBF_PACKED_BITS6 = 16

      ! Version 2 bit lengths
      INTEGER(4),PARAMETER:: &
        CBF_PACKED_V2_BITS1 = 3, &
        CBF_PACKED_V2_BITS2 = 4, &
        CBF_PACKED_V2_BITS3 = 5, &
        CBF_PACKED_V2_BITS4 = 6, &
        CBF_PACKED_V2_BITS5 = 7, &
        CBF_PACKED_V2_BITS6 = 8, &
        CBF_PACKED_V2_BITS7 = 9, &
        CBF_PACKED_V2_BITS8 = 10, &
        CBF_PACKED_V2_BITS9 = 11, &
        CBF_PACKED_V2_BITS10 = 12, &
        CBF_PACKED_V2_BITS11 = 13, &
        CBF_PACKED_V2_BITS12 = 14, &
        CBF_PACKED_V2_BITS13 = 15, &
        CBF_PACKED_V2_BITS14 = 16

      INTEGER(4) CBF_PACKED_BITS(8), CBF_PACKEDV2_BITS(16)
      
      DATA CBF_PACKED_BITS/ 0, CBF_PACKED_BITS1,              &
        CBF_PACKED_BITS2, CBF_PACKED_BITS3, CBF_PACKED_BITS4, &
        CBF_PACKED_BITS5, CBF_PACKED_BITS6, 65 /
        
      DATA CBF_PACKEDV2_BITS/ 0, CBF_PACKED_V2_BITS1,         &
        CBF_PACKED_V2_BITS2, CBF_PACKED_V2_BITS3,             &
        CBF_PACKED_V2_BITS4, CBF_PACKED_V2_BITS5,             &
        CBF_PACKED_V2_BITS6, CBF_PACKED_V2_BITS7,             &
        CBF_PACKED_V2_BITS8, CBF_PACKED_V2_BITS9,             &
        CBF_PACKED_V2_BITS10, CBF_PACKED_V2_BITS11,           &
        CBF_PACKED_V2_BITS12, CBF_PACKED_V2_BITS13,           &
        CBF_PACKED_V2_BITS14, 65 /
        
      BCOUNT = 0
      BBYTE = 0
        

      ! Discard the file_nelem entry (64 bits) */

      FCB_DECOMPRESS_PACKED_$1 =                             &
        FCB_READ_INTEGER(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE, &
				     0,64,DISCARD,2)
      IF (FCB_DECOMPRESS_PACKED_$1 .NE. 0) RETURN

      ! Discard the minimum element entry (64 bits) */

      FCB_DECOMPRESS_PACKED_$1 =                             &
        FCB_READ_INTEGER(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE, &
				     0,64,DISCARD,2)
      IF (FCB_DECOMPRESS_PACKED_$1 .NE. 0) RETURN

      ! Discard the maximum element entry (64 bits) */

      FCB_DECOMPRESS_PACKED_$1 =                             &
        FCB_READ_INTEGER(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE, &
				     0,64,DISCARD,2)
      IF (FCB_DECOMPRESS_PACKED_$1 .NE. 0) RETURN

      ! Discard the reserved entry (64 bits) */

      FCB_DECOMPRESS_PACKED_$1 =                             &
        FCB_READ_INTEGER(TAPIN,FCB_BYTES_IN_REC,BUFFER,     &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE, &
				     0,64,DISCARD,2)
      IF (FCB_DECOMPRESS_PACKED_$1 .NE. 0) RETURN
      V2FLAG = 0
      IF (IAND(COMPRESSION,CBF_COMPRESSION_MASK).EQ.CBF_PACKED_V2) &
        V2FLAG = 1
        
      AVGFLAG = 1
      IF (DIM1 .EQ. 0 .AND. DIM2 .EQ. 0 `'m4_ifelse(fcb_3d_flag,`3D',`.AND. DIM3 .EQ. 0')) AVGFLAG = 0
      
      BITS = m4_ifelse($1,`I2',16,$1,`3D_I2',16,32)
      CLIPBITS = 0
      IF (AVGFLAG .NE. 0) CLIPBITS=BITS
      
      DO I =1,m4_ifelse(fcb_3d_flag,`3D',8,4)
        TRAIL_INDEX_ARRAY(I) = 0
      END DO
      
      VORZEICHEN = m4_ifelse($1,`I2',Z''`00008000''`,$1,`3D_I2',Z''`00008000''`,Z''`80000000''`)
      UNSIGN = 0
      IF (ELSIGN.NE.0) UNSIGN=VORZEICHEN
      
      LIMIT = m4_ifelse($1,`I2',Z''`0000FFFF''`,$1,`3D_I2',Z''`0000FFFF''`,Z''`FFFFFFFF''`)
      
      ! Initialise the first element 
      
      LAST_ELEMENT = UNSIGN
      ! *** DEBUG ***  PREV_ELEMENT = 0
      ! *** DEBUG ***  PREV_INDEX = 0
       
m4_ifelse(fcb_3d_flag,`3D',`      LDIM3 = DIM3
      IF (DIM3.EQ.0) LDIM3 = 1')
      LDIM2 = DIM2
      IF (DIM2.EQ.0) LDIM2 = 1
      LDIM1 = DIM1
m4_ifelse(fcb_3d_flag,`3D',`      IF (DIM1.EQ.0) LDIM1 = NELEM/(LDIM2*LDIM3)
      IF (LDIM1*LDIM2*LDIM3.NE.NELEM) THEN
',
`      IF (DIM1.EQ.0) LDIM1 = NELEM/LDIM2
      IF (LDIM1*LDIM2.NE.NELEM) THEN')
        FCB_DECOMPRESS_PACKED_$1 = CBF_ARGUMENT
        RETURN
      END IF

      ! Read the elements 

      COUNT = 0
      PIXEL = 0
      NDIM1 = 1
      NDIM2 = 1
m4_ifelse(fcb_3d_flag,`3D',`      NDIM3 = 1')
      
      DO
        IF (COUNT .GE. NELEM) EXIT
        
        ! GET THE NEXT 6 BITS OF DATA
        
        FCB_DECOMPRESS_PACKED_$1 =                           &
          FCB_READ_INTEGER(TAPIN,FCB_BYTES_IN_REC,BUFFER,   &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE, &
				     0,6+V2FLAG,NEXT,1)
        IF (FCB_DECOMPRESS_PACKED_$1 .NE. 0) THEN 
          NELEM_READ=COUNT+PIXEL
          RETURN
        END IF
        
        PIXELCOUNT = ISHFT(1,IAND(NEXT(1),7))
        IF (V2FLAG.NE.0) THEN
          BITS = CBF_PACKEDV2_BITS(1+IAND(ISHFT(NEXT(1),-3),15) )
        ELSE
          BITS = CBF_PACKED_BITS(1+IAND(ISHFT(NEXT(1),-3),7) )
        END IF
        
        IF (AVGFLAG.NE.0 .AND. BITS.EQ. 65) BITS = CLIPBITS
        
        !  READ THE OFFSETS
        IF ( PIXELCOUNT + COUNT .GT. NELEM ) &
          PIXELCOUNT = NELEM - COUNT
          
        DO PIXEL = 0, PIXELCOUNT-1
          ELEMENT = LAST_ELEMENT
          OFFSET(1) = 0
          OFFSET(2) = 0
          OFFSET(3) = 0
          ERRORCODE = 0
          IF (BITS .NE. 0) THEN
            IF (BITS .GT. 32 ) THEN
              IINT = 1
              DO IBITS=0,BITS-1,32
                KSIGN = 1
                IF (IBITS .LT. BITS-32) KSIGN = 0
                KBITS = BITS-32*(IINT-1)
                IF (IBITS .LT. BITS-32) KBITS = 32
              
                ERRORCODE=IOR(ERRORCODE, &
                  FCB_READ_INTEGER(TAPIN,FCB_BYTES_IN_REC,BUFFER,   &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE, &
				     KSIGN,KBITS,OFFSET(IINT:IINT),1) )
				IINT = IINT+1
              END DO
            ELSE
              ERRORCODE = FCB_READ_BITS(TAPIN,FCB_BYTES_IN_REC,BUFFER,  &
				     REC_IN_FILE,BYTE_IN_FILE,BCOUNT,BBYTE,             &
				     BITS,OFFSET(1:1),1)
            END IF
          END IF
          IF (ERRORCODE.NE.0) THEN
            NELEM_READ = COUNT+PIXEL
            FCB_DECOMPRESS_PACKED_$1 = ERRORCODE
            RETURN
          END IF

          ELEMENT = ELEMENT+OFFSET(1)
          ELEMENT = IAND(ELEMENT,LIMIT)
          ELEMENT = ELEMENT-UNSIGN
          TRAIL_INDEX_ARRAY(1) = NDIM1+(NDIM2-1)*LDIM1`'m4_ifelse(fcb_3d_flag,`3D',` &
          +(NDIM3-1)*LDIM1*LDIM2')
          ARRAY(NDIM1,NDIM2`'m4_ifelse(fcb_3d_flag,`3D',`,NDIM3')) = ELEMENT
          ! *** DEBUG *** IF (PREV_ELEMENT.NE.ELEMENT.OR.PREV_INDEX.NE.TRAIL_INDEX_ARRAY(1)-1) THEN  
          ! *** DEBUG ***   PREV_ELEMENT = ELEMENT
          ! *** DEBUG ***   PRINT *, "ARRAY(",TRAIL_INDEX_ARRAY(1),") = ", ELEMENT
          ! *** DEBUG *** END IF
          ! *** DEBUG *** PREV_INDEX= TRAIL_INDEX_ARRAY(1)
          IF (AVGFLAG.NE.0) THEN
            FCB_DECOMPRESS_PACKED_$1 = &
              FCB_UPDATE_JPA_POINTERS_$1(TRAIL_INDEX_ARRAY,&
               NDIM1, NDIM2, m4_ifelse(fcb_3d_flag,`3D',
               `NDIM3,')ARRAY, LDIM1, LDIM2, m4_ifelse(fcb_3d_flag,`3D',
               `LDIM3,')LAST_ELEMENT, COMPRESSION)
            LAST_ELEMENT = LAST_ELEMENT + UNSIGN  
            LAST_ELEMENT = IAND(LAST_ELEMENT,LIMIT)         
          ELSE
            LAST_ELEMENT = ELEMENT+UNSIGN
            NDIM1 = NDIM1+1
            IF (NDIM1 .GT. LDIM1) THEN
              NDIM1 = 1
              NDIM2 = NDIM2+1
m4_ifelse(fcb_3d_flag,`3D',
`              IF(NDIM2 .GT. LDIM2) THEN
                NDIM2 = 1
                NDIM3 = NDIM3+1
              END IF')
            END IF
          ENDIF
        END DO
        COUNT = COUNT+PIXELCOUNT
      END DO
      NELEM_READ = COUNT
      FCB_DECOMPRESS_PACKED_$1 = 0
      RETURN
      END FUNCTION FCB_DECOMPRESS_PACKED_$1')
      
fcb_macro_FCB_PACKED(`I2')
      
fcb_macro_FCB_PACKED(`I4')
      
fcb_macro_FCB_PACKED(`3D_I2')
      
fcb_macro_FCB_PACKED(`3D_I4')
