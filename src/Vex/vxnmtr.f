      CHARACTER*32 FUNCTION VXNMTR( IXX ) 
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 010996 
C     returns a name for an TRACKS block definition
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER IXX, ISET, ICH, NDUMMY, NCH
      CHARACTER NAME*32, MODE*1, S2MDNM*7, CDUMMY(16)*4
      LOGICAL DUBLSB, DUALPL, S2OK
      INTEGER LEN1
C ----------------------------------------------------------------------
C
      ISET = TRISSET( IXX )
C
      NAME = ' '
C
C     no fan in's, mode depends on TAPEMODE too, but not used for name
C
      IF( .NOT. (FORMAT(ISET)(1:6) .EQ. 'VLBA1:' .OR. 
     1    FORMAT(ISET)(1:7) .EQ. 'MARKIII' .OR.
     2    FORMAT(ISET)(1:2) .EQ. 'S2' .OR.
     3    FORMAT(ISET)(1:3) .EQ. 'LBA' .OR.
     4    FORMAT(ISET)(1:6) .EQ. 'MKIV1:' .OR.
     5    FORMAT(ISET)(1:6) .EQ. 'MARK5B' .OR.
     6    FORMAT(ISET)(1:4) .EQ. 'VDIF' .OR.
     7    FORMAT(ISET)(1:4) .EQ. 'NONE' ) ) THEN
         MSGTXT = 'VXNMTR: unsupported recording mode: ' // FORMAT(ISET)
         CALL ERRLOG( MSGTXT )
      END IF
C     
      IF( FORMAT(ISET)(1:7) .EQ. 'MARKIII' ) THEN 
         DUALPL = .FALSE.
         DUBLSB = .FALSE.
         IF( NCHAN(ISET) .GT. 1 ) THEN
            DO ICH = 2, NCHAN(ISET)
               IF( NETSIDE(ICH,ISET) .NE. NETSIDE(1,ISET) ) 
     1             DUBLSB = .TRUE.
               IF( POL(ICH,ISET) .NE. POL(1,ISET) )
     1             DUALPL = .TRUE.
            END DO
            IF( DUBLSB ) THEN
               IF ( DUALPL ) THEN 
                  MODE = 'A'
               ELSE
                  MODE = 'B'
               END IF
            ELSE
               IF ( DUALPL ) THEN
                  MODE = 'C'
               ELSE
                  MODE = 'E'
               END IF
            END IF
         ELSE
            MODE = 'D'
         END IF               
         WRITE( NAME, '( A5, A1, I2.2, A )' )  
     1       'Mk3md',MODE, NCHAN(ISET),'Ch'
C
C     S2 details
C
      ELSE IF( FORMAT(ISET)(1:2) .EQ. 'S2' ) THEN
         CALL VXS2MD( ISET, S2MDNM, S2OK, NDUMMY, CDUMMY, .FALSE. )
         IF( .NOT. S2OK ) 
     1       CALL ERRLOG(' VXNMTR: inconsistent or impossible'//
     2       ' S2 mode ')
         WRITE( NAME, '( A, A )' ) 'S2.', S2MDNM(1:LEN1(S2MDNM))
C
C     FORMAT = NONE 
C
      ELSE IF( FORMAT(ISET)(1:4) .EQ. 'NONE' ) THEN
         WRITE( NAME, '( A )' ) 'TRACKS.NONE'
C
      ELSE
C
C        either MkIV or VLBA or LBA or MARK5B:
C
         IF( FORMAT(ISET) .EQ. 'MARK5B' ) THEN
            NCH = 6
         ELSE
            NCH = 4
         END IF
         IF( NCHAN(ISET) .GT. 9 ) THEN
            WRITE( NAME, '( A, A1, I2, A2, I1, A3, A3, I1 )' )  
     1          FORMAT(ISET)(1:NCH), '.', NCHAN(ISET),'Ch', 
     2          BITS(1,ISET),'bit','1to', NINT(FANOUT(ISET))
         ELSE
            WRITE( NAME, '( A, A1, I1, A2, I1, A3, A3, I1 )' )  
     1          FORMAT(ISET)(1:NCH), '.', NCHAN(ISET),'Ch', 
     2          BITS(1,ISET), 'bit','1to', NINT(FANOUT(ISET))
         ENDIF
      ENDIF
C
      VXNMTR = NAME
C
      RETURN
      END
