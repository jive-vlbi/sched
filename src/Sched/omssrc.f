      SUBROUTINE OMSSRC
C
C     Routine for SCHED that writes source information to the
C     .oms file for use by the VLBA correlator bookkeeping software.
C
      INCLUDE     'sched.inc'
C
      INTEGER     ISRC, J, LENR, LEN1, LEND, LENS
      CHARACTER   TDEC20*16, TRA20*16, TFORM*16
C -------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'OMSSRC starting' )
C
      DO ISRC = 1, MSRC
         IF( SUSED(ISRC) ) THEN
            TRA20 = TFORM( RA2000(ISRC), 'T', 0, 2, 9, 'hms' )
            LENR = LEN1( TRA20 )
            IF( D2000(ISRC) .GE. 0.D0 ) THEN
              TDEC20 = TFORM( D2000(ISRC),  ' ', 0, 2, 8, 'd''"' )
            ELSE
              TDEC20 = TFORM( D2000(ISRC),  ' ', 1, 2, 8, 'd''"' )
            END IF
            LEND = LEN1( TDEC20 )
C     
            DO J = 1, MALIAS
               IF( CSUSED(J,ISRC) .NE. ' ' ) THEN
                  LENS = LEN1( SOURCE(J,ISRC) )
C                  
                  WRITE( IOMS, '( 1X, /, A, /, 1X )' )
     1              'BEGIN = SOURCE_INFO'
                  WRITE( IOMS, '( 2A )' )
     1              '    SOURCE_NAME       = ', SOURCE(J,ISRC)(1:LENS)
                  WRITE( IOMS, '( 2A )' )
     1              '    CAL_CODE          = ', CALCODE(ISRC)
                  WRITE( IOMS, '( 2A )' )
     1              '    SOURCE_RA         = ', TRA20(1:LENR)
                  WRITE( IOMS, '( 2A )' )
     1              '    SOURCE_DEC        = ', TDEC20(1:LEND)
                  WRITE( IOMS, '( 1X, /, A )' )
     1              'END = SOURCE_INFO'
               END IF
            END DO
         END IF
      END DO
C
      RETURN
      END
