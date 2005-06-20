      SUBROUTINE VXWRHP
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the HP = $HEAD_POSITION section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
      INTEGER   IHP, KS, I, ASTAT, IMODE
      INTEGER   LEN1, HEADPOS, VXNHDS
      LOGICAL   TWOSTACK
C ----------------------------------------------------------------------
C
C     Write the HEADPOS sections, this is fixed
C
      WRITE( IVEX, '( A, A1 )' ) '$HEAD_POS', SEP   
      DO IHP = 1, NHPVEX
         KS = HPISSET(IHP)
C
C        Find a station in this VEX group
C
         DO IMODE = 1, NMDVEX
            IF ( NSTAHP(IHP,IMODE) .GT. 0 ) THEN 
               ASTAT = ISTAHP(1,IHP,IMODE)
            ENDIF
         ENDDO
C
C        Set a logical if this mode uses two heads (not drives)
C
         TWOSTACK = .FALSE.
         IF ( VXNHDS( KS ) .GT. 1 ) THEN 
            TWOSTACK = .TRUE.
C
C           Uses more than 33 tracks, by 2 heads or 2 recorders?
C
            IF ( TWOSTACK ) THEN
               IF( .NOT. NHEADS(STANUM(ASTAT)) .GT. 1 ) THEN 
                  IF( STNDRIV(STANUM(ASTAT)) .GT. 1 ) THEN
                     TWOSTACK = .FALSE. 
                  ELSE
             CALL ERRLOG('VXWRHP: More than 32 tracks with 1 head?')
                  END IF
               END IF
            END IF
         END IF

         IF( FORMAT(KS) .NE. 'S2' ) THEN
            IF( USETAPE (ASTAT) ) THEN 
               WRITE( IVEX, '( A1 )' ) COM
               WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1             HPLINK(IHP)(1:LEN1(HPLINK(IHP))), SEP
               CALL VXSTLI( IHP, NSTAHP, ISTAHP )
C
C              usually 14, complies with PCFS maximum
C
               DO I = 1, NHDPOS(ASTAT)
                  IF( TWOSTACK ) THEN
                     WRITE( IVEX, '( 5X, A, 1X, I2, 1X, A1, 1X, I4, 
     1                   1X, A, A1, 1X, I4, 1X, A, A1 )' ) 
     1                   'headstack_pos =', I, COL, 
     2                   HEADPOS(I,HEADMODE(ASTAT),1), 'um', COL, 
     3                   HEADPOS(I,HEADMODE(ASTAT),2), 'um', SEP
                  ELSE 
                     WRITE( IVEX, '( 5X, A, 1X, I2, 1X, A1, 1X, I4, 
     1                   1X, A, A1 )' ) 'headstack_pos =', I, COL, 
     2                   HEADPOS(I,HEADMODE(ASTAT),1), 'um', SEP
                  END IF
               END DO
               WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
            ELSE IF( USEDISK (ASTAT) ) THEN 
               WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1             HPLINK(IHP)(1:LEN1(HPLINK(IHP))), SEP  
               CALL VXSTLI( IHP, NSTAHP, ISTAHP )
               WRITE( IVEX, '( A1, 4X, A )' ) COM,
     1             ' Head positions irrelevant for Disk: empty def'
               WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
            END IF
         ELSE
            WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1          HPLINK(IHP)(1:LEN1(HPLINK(IHP))), SEP  
            CALL VXSTLI( IHP, NSTAHP, ISTAHP )
            WRITE( IVEX, '( A1, 4X, A )' ) COM,
     1          ' Head positions irrelevant for S2: empty definition'
            WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
         END IF
      ENDDO
C
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END
