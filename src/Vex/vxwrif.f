      SUBROUTINE VXWRIF
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
      INCLUDE 'vxlink.inc' 
C      
      INTEGER IIF, KS, ICH, J, I1, I2
      INTEGER LEN1
      LOGICAL NEWFND, DOWARN
      CHARACTER   VBCOM*40
C ----------------------------------------------------------------------
C
C     Write the IF section
C
      DOWARN = .FALSE.
      WRITE( IVEX, '( A, A1 )' ) '$IF', SEP     
C
      DO IIF = 1, NIFVEX
         KS = IFISSET(IIF)
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        IFLINK(IIF)(1:LEN1(IFLINK(IIF))), SEP
         CALL VXSTLI( IIF, NSTAIF, ISTAIF )
C
C        Loop over channels.
C
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
            IF( NEWFND ) THEN
C
               IF( TONEINT(IIF) .NE. 1.0 ) DOWARN = .TRUE.
C
C              Fill in most of the line to the MSGTXT string:
C
               MSGTXT = ' '
C
               WRITE( MSGTXT, '( 5X, A, A1, A, A, 1X, A1, 
     1             1X, A, 1X, A1, 1X, A1, 1X, A1, F9.2, 1X, A, 1X, 
     2             A1, 1X, A1)' ) 
     3             'if_def = ', LNK, 'IF_', 
     4             IFCHAN(ICH,KS)(1:LEN1(IFCHAN(ICH,KS))), COL, 
     5             IFCHAN(ICH,KS)(1:LEN1(IFCHAN(ICH,KS))), COL,
     6             POL(ICH,KS)(1:1), COL,
     7             FIRSTLO( ICH, KS), 'MHz',
     8             COL, SIDE1( ICH, KS )
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
               I1 = LEN1( MSGTXT ) + 1
               IF( TONEINT(IIF) .GT. 1E-15 ) THEN               
                  I2 = I1 + 11
                  WRITE( MSGTXT(I1:I2), 
     1                '( 1X, A1, 1X, I1, 1X, A, 1X, A1, 1X, A1 )' ) 
     2                COL, NINT(TONEINT(IIF)),'MHz', SEP, COM
               ELSE
                  I2 = I1+15
                  WRITE( MSGTXT(I1:I2), '( 1X, A1, 1X, A1, 1X, A  )' ) 
     1                SEP, COM, 'PCall off!'
               END IF
C
C              Now add the VLBA receiver information in a comment.
C              Hopefully this is a stop-gap until VEX2 provides a
C              better mechanism.
C
               CALL VXVBRX( KS, ICH, VBCOM )
               I1 = LEN1( MSGTXT )
               MSGTXT = MSGTXT(1:I1) // VBCOM
C
C              Write the line.
C
               WRITE( IVEX, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
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
         CALL WLOG( 1,'VXWRIF: WARNING, Phase cal not under '//
     2       'computer control for some stations.' ) 
         CALL WLOG( 1,'        Contact stations by e-mail to '//
     1       'make sure phase cal is switched off ')
         CALL WLOG( 1,'        Phase cal is under computer '//
     1       'control for VLBA stations and many others. ')
      END IF

      RETURN
      END
