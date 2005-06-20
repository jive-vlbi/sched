      SUBROUTINE VXWRPO
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the PO = $PASS_ORDER section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
      INTEGER   IPO, KS, I, IP, IPASS, HDINDX, SPINDX
      INTEGER   LEN1, ASTAT, IMODE
      CHARACTER LINE*132, DEL*1, TPSUBP*1
      LOGICAL   VIOLFS
C ----------------------------------------------------------------------
C
C     Write the PASSORDER sections, this is tough
C
      VIOLFS = .FALSE.
      WRITE( IVEX, '( A, A1 )' ) '$PASS_ORDER', SEP   
      DO IPO = 1, NPOVEX

         KS = POISSET(IPO)
C
C        Find a station in this VEX group
C
         DO IMODE = 1, NMDVEX
            IF ( NSTAHP(IPO,IMODE) .GT. 0 ) THEN 
               ASTAT = ISTAHP(1,IPO,IMODE)
            ENDIF
         ENDDO
C
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1       POLINK(IPO)(1:LEN1(POLINK(IPO))), SEP
         CALL VXSTLI( IPO, NSTAPO, ISTAPO )
C
         LINE = ' ' 
C
         IF( FORMAT(KS) .NE. 'S2' ) THEN 
C
C        Check if PCFS can do
C
            IF( TAPEMODE( KS ) .GT. 36 ) THEN
               VIOLFS = .TRUE.
               WRITE( MSGTXT, '( A, I3, A, A )' ) 
     1             'VXWRPO: WARNING: More than 36 subpasses (',
     2             TAPEMODE( KS ),') in ',
     3             POLINK(IPO)(1:LEN1(POLINK(IPO)))
               CALL WLOG( 1, MSGTXT )
            END IF            

            IF( USETAPE( ASTAT )) THEN
               DO IP = 1, TAPEMODE( KS )
                  IF( IP .EQ. 1 ) THEN 
                     WRITE( LINE(1:17) , '( 5X, A )' ) 'pass_order ='
                  ELSE
                     WRITE( LINE(1:17) , '( 17X )' )
                  ENDIF
                  DO I = 1, NHDPOS(ASTAT)
C
C                 find the right index
C
                     IPASS = ( IP-1 )*NHDPOS(ASTAT) + I
C
C                    get the standard VLBA order from indxhead, 
C
                     CALL INDXHEAD( TAPEMODE(KS), IPASS, 
     1                   HEADMODE(ASTAT), HDINDX, SPINDX, KS )
                     TPSUBP = 'X'
                     IF( SPINDX .EQ. 1) TPSUBP = 'A'
                     IF( SPINDX .EQ. 2) TPSUBP = 'B'
                     IF( SPINDX .EQ. 3) TPSUBP = 'C'
                     IF( SPINDX .EQ. 4) TPSUBP = 'D'
                     IF( SPINDX .EQ. 5) TPSUBP = 'E'
                     IF( SPINDX .EQ. 6) TPSUBP = 'F'
                     IF( SPINDX .EQ. 7) TPSUBP = 'G'
                     IF( SPINDX .EQ. 8) TPSUBP = 'H'
C
C                    all separared by colon, except last
C         
                     DEL = COL
                     IF( I .EQ. NHDPOS(ASTAT)  
     1                   .AND. IP .EQ. TAPEMODE(KS) ) DEL = SEP
                     WRITE(LINE(18+(I-1)*4:17+I*4), '( I2, A1, A1 )' )
     1                   HDINDX, TPSUBP, DEL               
                  END DO
                  WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
               END DO
            ELSE IF( USEDISK(ASTAT) ) THEN
C
C                 No passorder for disks
C
                  WRITE( IVEX, '( A1, 4X, A )' ) COM,
     1                ' Pass orders irrelevant for Disk: empty def'
            END IF
         ELSE
C
C           Quite different
C
            WRITE( LINE(1:21) , '( 5X, A )' ) 'S2_group_order ='
            DO IP = 1, TAPEMODE( KS )    
               DEL = COL
               IF( IP .EQ. TAPEMODE( KS ) ) DEL = SEP
               WRITE(LINE(22+(IP-1)*3:21+IP*3), '( I2, A1 )' )
     1             (IP-1), DEL
            END DO
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END IF
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
      ENDDO
      WRITE( IVEX, '( A )' ) COMLIN

      IF( VIOLFS )
     1    CALL WLOG( 1,'VXWRPO: More than 36 subpasses '//
     2    'not supported in PCFS; this VEX will NOT run!!!!')
      RETURN
      END
