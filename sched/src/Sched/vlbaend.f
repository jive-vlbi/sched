      SUBROUTINE VLBAEND( ISTA, POSTPASS, LASTDY, TPCDRIV, LSCN, 
     1       PASSOK )
C
C     Routine for SCHED called by VLBA to finish off the VLBA control
C     file.
C
C     The main task is to unload or postpass the tape if necessary.
C     See more information on this where this task is done at
C     tape change times.  However, unlike that time, here a final
C     stop time 20 seconds after the last scan stop time is provided.
C
C     Note that we are depending on TPCDRIV retaining the value
C     it had at the end of the last valid scan.  Hence its presence
C     among the SAVEd variables in routine VLBA.
C
C     This routine will dismount the tape at the end of the project.
C     This could be a problem if one is trying to get more than one
C     project on a tape.
C
      INCLUDE  'sched.inc'
C
      LOGICAL           POSTPASS, PASSOK
      INTEGER           LASTDY, TPCDRIV, NDR, LSCN, ISTA, LEN1
      INTEGER           LA
      CHARACTER         TSTOP*9, ACTION*8, VLBASTOP*9
      DOUBLE PRECISION  OFFSET
      LOGICAL           DOTWO
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VLBAEND: Starting.' )
C
C     Get the current tape drive number and whether two are in use.
C     Also get the stop time for a scan to stop the tape.
C     These aren't used if .NOT. VLBITP, but they are needed in
C     two places otherwise, so they are here to minimize code
C     duplication.  Efficiency is not an issue.
C
      NDR = TPCDRIV
      DOTWO = TWOHEAD .AND. NDR + 1 .LE. STNDRIV(STANUM(ISTA))
C
C     Deal with wrapping up the recordings.
C
      IF( VLBITP ) THEN
C
C        Turn off the disk, but let tape determine the other details.
C
         IF( USEDISK(ISTA) ) THEN
            WRITE( IUVBA, '( A )' ) 'disk=off'
         END IF
C
C        Give time for tapes to stop before doing something.  At
C        least 2 more seconds will be added to OFFSET before it is
C        used.
C
         OFFSET = 3.D0
C
C        Non-autoallocated tape.  Need to dismount, postpass etc.
C
         IF( USETAPE(ISTA) .AND. .NOT. AUTOALOC(ISTA) ) THEN
C
C           Determine what action to take - UNLOAD or POSTPASS.
C	    
            IF( ( PASSOK .AND. TPFOOT2(LSCN,ISTA) .LT. 300.0 ) .OR.
     1          ( TPLENG(ISTA) .LT. 12000 ) .OR.
     2          ( .NOT. POSTPASS ) ) THEN
               ACTION = 'UNLOAD'
            ELSE
               ACTION = 'POSTPASS'
            END IF
            LA = LEN1( ACTION )
C	    
C           Now do it and tell the operator.
C           Before the most recent changes (Sep97) UNLOADS were not
C           issued.  Neither I nor Peggy could remember a reason so
C           they are in now.  But don't be surprised if someone complains.
C	    
C	    
C           Tell the operator.
C	    
            WRITE( IUVBA, '( 1X )' )
            WRITE( IUVBA,'(''!* ===== '', 3A, I1, '' ==== *!'')') 
     1              ' ', ACTION(1:LA), 'ING DRIVE ', NDR
            IF( DOTWO ) THEN
               WRITE( IUVBA,'(''!* ===== '', 3A, I1, '' ==== *!'')') 
     1              ' ', ACTION(1:LA), 'ING DRIVE ', NDR + 1
            END IF
C	    
C           Stop the tape using the previously determined stop time
C           offset.
C	    
            WRITE( IUVBA, '( A, I1, A, I1, A )' )
     1              'tape=(', NDR, ',STOP)    write=(', NDR, 
     2              ',off)   dur=0s '
            IF( DOTWO ) THEN
               WRITE( IUVBA, '( A, I1, A, I1, A )' )
     1                 'tape=(', NDR+1, ',STOP)    write=(', NDR+1, 
     2                 ',off)   dur=0s '
            END IF
            OFFSET = OFFSET + 2.D0
            TSTOP = VLBASTOP( STOPJ(LSCN), OFFSET, LASTDY, TWOPI, 
     1              IUVBA )
            WRITE( IUVBA, '( A, A9, A  )' )
     1          'stop=', TSTOP, '  !NEXT! '
C	    
C           Now request the postpass or unload.
C	    
            OFFSET = OFFSET + 2.D0
            TSTOP = VLBASTOP( STOPJ(LSCN), OFFSET, LASTDY, TWOPI, 
     1              IUVBA )
            WRITE( IUVBA, '( A, I1, 3A, A9  )' )
     1          'tape=(', NDR, ',', ACTION(1:LA), ')  stop=', TSTOP
            WRITE( IUVBA, '( A  )' ) '  !NEXT! '
C	    
C           Postpass the second tape too if needed.  This must be
C           a separate scan.
C	    
            IF( DOTWO ) THEN
               OFFSET = OFFSET + 2.D0
               TSTOP = VLBASTOP( STOPJ(LSCN), OFFSET, LASTDY, TWOPI, 
     1                 IUVBA )
               WRITE( IUVBA, '( A, I1, 3A, A9  )' )
     1             'tape=(', NDR+1, ',', ACTION(1:LA), ')  stop=', TSTOP
               WRITE( IUVBA, '( A  )' ) '  !NEXT! '
            END IF
C
         END IF
C
C        If autoallocation is being used, stop the tape.
C
         IF( USETAPE(ISTA) .AND. AUTOALOC(ISTA) ) THEN
            WRITE( IUVBA, '( 1X, /, A, I1, A, I1, A )' )
     1              'tape=(', NDR, ',STOP)    write=(', NDR, 
     2              ',off)   dur=0s '
            IF( DOTWO ) THEN
               WRITE( IUVBA, '( A, I1, A, I1, A )' )
     1              'tape=(', NDR+1, ',STOP)    write=(', NDR+1, 
     2              ',off)   dur=0s '
            END IF
         END IF
C
C        Add a final termination scan.  It seems that it may be 
C        necessary to add a !NEXT! before the final !QUIT! to 
C        get the POSTPASS to take effect.
C
         OFFSET = OFFSET + 2.D0
         TSTOP = VLBASTOP( STOPJ(LSCN), OFFSET, LASTDY, TWOPI, IUVBA )
         WRITE( IUVBA, '( A, A9, A )' )
     1       'stop=', TSTOP, '   !NEXT!'
C
      END IF
C
C     Finally really quit the program for all cases.
C
      WRITE( IUVBA, '(A)' ) '     !QUIT! '
C
      RETURN
      END

