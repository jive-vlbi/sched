      SUBROUTINE VXWREXT
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the IF = $IF section 
C     By H.J. van Langevelde, JIVE, 300496 
C     Update to 1.4 240796
C
C     Add comment about receiver, receiver LO, and 610 MHz filter
C     for VLBA.  Aug. 11, 2011  R. C. Walker
C     Add a digit to the FIRSTLO output to support the new VLBA
C     synthesizers.  Mar. 20, 2013  RCW  (Synthesizers not in place yet.)
C
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink2.inc' 
C      
      INTEGER IIF, KS, ICH, J, I1, I2
      INTEGER LEN1
      LOGICAL NEWFND, DOWARN
      CHARACTER   VBCOM*40
      CHARACTER   EXTSYNTH1*40, EXTSYNTH2*40, EXTFILTER*40
      INTEGER EXTSYNTH3, MCOUNT
C ----------------------------------------------------------------------
C
C     Write the IF section
C
      DOWARN = .FALSE.
      WRITE( IVEX, '( A, A1 )' ) '$EXTENSIONS', SEP     
C
      DO IIF = 1, NIFVEX
         KS = IFISSET(IIF)
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        IFLINK(IIF)(1:LEN1(IFLINK(IIF))), SEP
C         CALL VXSTLI( IIF, NSTAIF, ISTAIF )
C
C        Loop over channels.
C
         MCOUNT = 0
         DO ICH = 1, NCHAN(KS)
C
C           See if this is the first channel that uses the IF.
C
            NEWFND = .FALSE.
            IF( ICH .EQ. 1 ) THEN
               NEWFND = .TRUE.
            ELSE
               NEWFND = .TRUE.
               DO J = 1, ICH-1
                  IF( IFCHAN( ICH, KS ) .EQ. IFCHAN( J,KS)  ) THEN
                     NEWFND = .FALSE.
                  END IF
               END DO
            END IF
C
C           could save NIF and IFNAMSs
C
C           If this is a new IF, then write most of the if_def line.
C
            IF( NEWFND .AND. MCOUNT .EQ. 0 ) THEN
C
               IF( TONEINT(IIF) .NE. 1.0 ) DOWARN = .TRUE.
C
C              Fill in extensions synth values:
C
               MSGTXT = ' '
               CALL VXVBEXT( KS, ICH, EXTSYNTH1, EXTSYNTH2, EXTSYNTH3,
     1                      VBCOM, EXTFILTER)
C
               IF( EXTSYNTH1 .NE. ' ') THEN
                  WRITE( MSGTXT, '( 5X, A, 1X, A1, 1X, A, 1X, A1,  
     1             A, 1X, A, 1X, A1)' ) 
     2             'extension = NRAO', COL, 'synth1', COL, 
     3             EXTSYNTH1(1:LEN1(EXTSYNTH1)), 'MHz', SEP
                  I1 = LEN1( MSGTXT )
                  MSGTXT = MSGTXT(1:I1)
                  WRITE( IVEX, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
               END IF
C
               IF( EXTSYNTH2 .NE. ' ') THEN   
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 5X, A, 1X, A1, 1X, A, 1X, A1,
     1             A, 1X, A, 1X, A1)' ) 
     2             'extension = NRAO', COL, 'synth2', COL, 
     3             EXTSYNTH2(1:LEN1(EXTSYNTH2)), 'MHz', SEP
                  I1 = LEN1( MSGTXT )
                  MSGTXT = MSGTXT(1:I1)
                  WRITE( IVEX, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
               END IF
C
               IF( EXTSYNTH3 .NE. 0) THEN   
                  IF ( VBCOM .EQ. '7mm' ) THEN
                     EXTSYNTH3 = NINT( EXTSYNTH3 / 3.D0 )
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( 5X, A, 1X, A1, 1X, A, 1X, A1,
     1                I6.2, 1X, A, 1X, A1, 1X, A1, 1X, A)' ) 
     2                'extension = NRAO', COL, 'synth3', COL, 
     3                EXTSYNTH3, 'MHz', SEP, COM, 'multiplied by 3'
                     I1 = LEN1( MSGTXT )
                     MSGTXT = MSGTXT(1:I1)
                     WRITE( IVEX, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
                  ELSE IF ( VBCOM .EQ. '3mm' ) THEN
                     EXTSYNTH3 = NINT( EXTSYNTH3 / 6.D0 )
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( 5X, A, 1X, A1, 1X, A, 1X, A1,
     1                I6.2, 1X, A, 1X, A1, 1X, A1, 1X, A)' ) 
     2                'extension = NRAO', COL, 'synth3', COL, 
     3                EXTSYNTH3, 'MHz', SEP, COM, 'multiplied by 6'
                     I1 = LEN1( MSGTXT )
                     MSGTXT = MSGTXT(1:I1)
                     WRITE( IVEX, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
                  ELSE
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( 5X, A, 1X, A1, 1X, A, 1X, A1,
     1                I6.2, 1X, A, 1X, A1)' ) 
     2                'extension = NRAO', COL, 'synth3', COL, 
     3                EXTSYNTH3, 'MHz', SEP
                     I1 = LEN1( MSGTXT )
                     MSGTXT = MSGTXT(1:I1)
                     WRITE( IVEX, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
                  END IF
               END IF
C
C              Add filter information
C            
               IF( VBCOM .EQ. '50cm' .OR. VBCOM .EQ. '90cm') THEN 
                  IF ( EXTFILTER .EQ. 'NARROW' ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( 5X, A, 1X, A1, 1X, A, 1X, A1,
     1                1X, A, 1X, A1, 1X, A1, 1X, A)' ) 
     2                'extension = NRAO', COL, 'filter50cm', COL, 
     3                '5 MHz', SEP, COM, EXTFILTER
                     I1 = LEN1( MSGTXT )
                     MSGTXT = MSGTXT(1:I1)
                     WRITE( IVEX, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
                  END IF
                  IF ( EXTFILTER .EQ. 'BROAD' ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( 5X, A, 1X, A1, 1X, A, 1X, A1,
     1                1X, A, 1X, A1, 1X, A1, 1X, A)' ) 
     2                'extension = NRAO', COL, 'filter50cm', COL, 
     3                '30 MHz', SEP, COM, EXTFILTER
                     I1 = LEN1( MSGTXT )
                     MSGTXT = MSGTXT(1:I1)
                     WRITE( IVEX, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
                  END IF
               END IF
               MCOUNT = MCOUNT + 1
               
C
C              Add the pulse cal specification and comment.
C
C              Add a blank comment (just a "*") even if there will be
C              no Pcal comment.  This is related to parsing the comment
C              with the receiver information.  The VEX parser used for
C              the VLBA observing system does not easily return the line
C              on which a comment was found.  The simplest way to keep
C              track is to make all if_def line have a comment.  Hopefully
C              future VEX definitions will get us out of this kludge.
C              RCW  Sep. 8, 2011
C
C               I1 = LEN1( MSGTXT ) + 1
C               IF( TONEINT(IIF) .GT. 1E-15 ) THEN               
C                  I2 = I1 + 11
C                  WRITE( MSGTXT(I1:I2), 
C     1                '( 1X, A1, 1X, I1, 1X, A, 1X, A1, 1X, A1 )' ) 
C     2                COL, NINT(TONEINT(IIF)),'MHz', SEP, COM
C               ELSE
C                  I2 = I1+15
C                  WRITE( MSGTXT(I1:I2), '( 1X, A1, 1X, A1, 1X, A  )' ) 
C     1                SEP, COM, 'PCall off!'
C               END IF
C
C              Now add the VLBA receiver information in a comment.
C              Hopefully this is a stop-gap until VEX2 provides a
C              better mechanism.
C
C
C              Write the line.
C
C
            END IF
         END DO
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
C     
      ENDDO
      WRITE( IVEX, '( A )' ) COMLIN
C
C     warn in case of none standard PCAL
C
      IF( DOWARN .AND. .NOT. ALLVLBA ) THEN
         CALL WLOG( 1,'VXWRIF2: WARNING, Phase cal not under '//
     2       'computer control for some stations.' ) 
         CALL WLOG( 1,'        Contact stations by e-mail to '//
     1       'make sure phase cal is switched off ')
         CALL WLOG( 1,'        Phase cal is under computer '//
     1       'control for VLBA stations and many others. ')
      END IF

      RETURN
      END
