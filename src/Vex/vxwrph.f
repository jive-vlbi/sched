      SUBROUTINE VXWRPH
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the PH = $PHASE_CAL section 
C     By H.J. van Langevelde, JIVE, 300496 
C     Updated for 1.4, 240796
C
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C      
      INTEGER   IPH, ICH, KS, IP, LPOS
      INTEGER   LEN1
      CHARACTER LINE*132
C ----------------------------------------------------------------------
C
      LINE = ' ' 
C
C     write PHASE_CAL section
C         
      WRITE( IVEX, '( A, A1 )' ) '$PHASE_CAL_DETECT', SEP     
      DO IPH = 1, NPHVEX
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        PHLINK(IPH)(1:LEN1(PHLINK(IPH))), SEP      
         CALL VXSTLI( IPH, NSTAPH, ISTAPH )
         KS = PHISSET(IPH)
C
C        write the tone numbers!
C
         IF( DODETECT(IPH) ) THEN
            DO ICH = 1, NTONES(IPH)
               WRITE( LINE(1:), '( 5X, A, A1, A5 )' )
     1             'phase_cal_detect = ', LNK, TONLNK(ICH,IPH)
               DO IP = 1, NTONDET(ICH, IPH)
                  LPOS = LEN1(LINE)+1
                  WRITE( LINE(LPOS:LPOS+4), '( 1X, A1, I3 )' ) COL, 
     1                ITONDET(IP, ICH,IPH)
               END DO
               LPOS = LEN1(LINE)+1
               WRITE( LINE(LPOS:LPOS), '( A1 )' ) SEP
               WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))              
            END DO
         ELSE
            WRITE( IVEX, '( 5X, A, A1, A5, A1 )' ) 
     1          'phase_cal_detect = ', LNK, TONLNK(1,IPH), SEP
         END IF
C
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
      ENDDO
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END
