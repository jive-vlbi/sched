      SUBROUTINE DIVERT( VALUE, KC, KI, ISCN )
C
C     Routine for SCHED, called by SCHIN, that deals with requests
C     to do something other than process a schedule.  If this routine
C     actually does anything except decode a few parameters, it will
C     end with a STOP, bringing SCHED to a close.
C
C     The tasks handled are:
C
C        Frequency list:   Write a list of the possible frequency
C                          setups in the requested range to file
C                          frequencies.list.  This happens if
C                          FREQLIST(1) 
C
C        Template:         Make a schedule template.  NOT YET AVAILABLE.
C
      INCLUDE 'sched.inc'
C
      DOUBLE PRECISION  VALUE(*)
      INTEGER           KI(*), I1, KEYPTR, ISCN
      CHARACTER         KC(*)*8, KCHAR*256
C ----------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'DIVERT: Starting' )
C
C     Frequency list.
C     First get the inputs related to this.
C
      I1 = KEYPTR( 'FREQLIST', KC, KI )
      FREQLIST(1) = VALUE(I1)
      FREQLIST(2) = VALUE(I1+1)
C
C     Now make the list if requested.  Don't try to parse FREQFILE
C     unless it is because it might not have been specified yet.
C
      IF( FREQLIST(1) .NE. UNSET ) THEN
         FREQFILE = KCHAR( 'FREQFILE', 80, .FALSE., VALUE, KC, KI )
         CALL ENVIR( FREQFILE )
         CALL GETFREQ
         CALL WLOG( 1, 'DIVERT:   Frequency table written.  Stopping.' )
         STOP
      END IF
C
      RETURN
      END

