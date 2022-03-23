      SUBROUTINE VXWRPO
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the PO = $PASS_ORDER section 
C     By H.J. van Langevelde, JIVE, 300496 
C
C     Removed tape handling July 20, 2010  RCW.  It used a subroutine
C     that was removed from the main Sched area.
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
      INTEGER   IPO, KS, IP
      INTEGER   LEN1, ASTAT, IMODE
      CHARACTER LINE*132, DEL*1
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
         IF( FORMAT(KS) .NE. 'S2' .AND. FORMAT(KS) .NE. 'LBA' ) THEN 
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
C
            IF( USEDISK(ASTAT) ) THEN
C
C                 No passorder for disks
C
                  WRITE( IVEX, '( A1, 4X, A )' ) COM,
     1                ' Pass orders irrelevant for Disk: empty def'
            END IF
         ELSE IF( FORMAT(KS)(1:3) .EQ. 'LBA' ) THEN
C        
            WRITE( IVEX, '( A1, A )' )  COM,
     1         ' LBA masquerading as S2 needs a dummy value'
            WRITE( IVEX, '( 5X, A )' )  'S2_group_order = 0;'
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
