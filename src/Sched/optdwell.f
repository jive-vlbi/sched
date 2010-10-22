      SUBROUTINE OPTDWELL( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
C
C     Routine for handling scans in a schedule that uses DWELL time
C     scheduling, but not more powerful optimization.
C
C     ISCN (output) = KSCN(input) is the sched scan number.
C     LASTISCN(ISTA) is the last scan that includes station ISTA.
C
C
      INCLUDE 'sched.inc'
C
      INTEGER           LASTISCN(MAXSTA), KSCN, ISCN, I
      INTEGER           ISTA
      DOUBLE PRECISION  TAPPROX, TBEGIN, T_AVAIL, LASTTIME
      DOUBLE PRECISION  TBEGSRT(MAXSTA), DTEMP
      LOGICAL           KEEP, ADJUST, DONE
C ---------------------------------------------------------------------
      IF( DEBUG .AND. KSCN .LE. 3 ) CALL WLOG( 0, 'OPTDWELL: Starting.')
C
      KEEP  = .TRUE.
      DONE  = KSCN .GT. NSCANS
      IF( DONE ) GO TO 999
C
C     Copy the input scan to the output if necessary.
C
      IF( KSCN .NE. ISCN ) THEN
         CALL SCNDUP( ISCN, KSCN, .TRUE. )
      END IF
C
C     Now determine the start time to use.  The start time is allowed
C     to be adjusted if the user specified only a duration or only
C     a stop time.  These are not allowed for the first scan, but
C     treat it like the others anyway.  ADJUST is really a statement
C     that we are allowed to change the start time.  The stop time
C     is separate.
C
      ADJUST = ( DURONLY(ISCN) .EQ. 1 .OR. DURONLY(ISCN) .EQ. 4 ) 
     1          .AND. ISCN .GT. 1
C
C     Get the time for which to do the geometric calculations.
C
      IF( ADJUST ) THEN
C
C        Get the most recent stop time for any antenna 
C        scheduled to be in this scan.   
C        Use the current start time if this is the first scan for
C        all stations.  Also, make sure if some antennas are new,
C        they don't set the start time.
C
         TAPPROX = 0.D0
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               IF( LASTISCN(ISTA) .NE. 0 ) THEN
                  TAPPROX = MAX( TAPPROX, 
     1                  STOPJ(LASTISCN(ISTA)) + GAP(ISCN) )
               END IF
            END IF
         END DO
         IF( TAPPROX .EQ. 0.D0 ) TAPPROX = STARTJ(ISCN)
C
C        For DWELL scans, get the times the antennas are available 
C        after the slew.  For DUR scans, just use the time found
C        above for TAPPROX.  Don't ignore GAP.
C
C        TBEGIN will be an estimated start time for the scan.
C        It can be earlier than TAPPROX if the station that set
C        that time is not selected.  If no stations had a previous
C        time, use the assigned start time of the scan.
C        Note that slews etc are based on TAPPROX which will not be
C        quite right, so the new start time may not be quite right.
C        But SCHOPT will tweak the final times as long as ADJUST is
C        true.
C
         IF( DWELL(ISCN) ) THEN
        write(*,*) 'optdwell Got dwell scan ', dwell(iscn)
C
C           Dwell scan.
C
            DO I = 1, NOWAIT(ISCN) + 1
              TBEGSRT(I) = 0.D0
            END DO
            TBEGIN = 0.D0
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) ) THEN
                  CALL OPTGEO( ISCN, ISTA, TAPPROX, LASTISCN(ISTA),  
     1                         LASTTIME, T_AVAIL )
                  IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. 
     1                UP2(ISCN,ISTA) .EQ. ' ' .AND. 
     2                LASTISCN(ISTA) .NE. 0 ) THEN
C
                     IF( LASTISCN(ISTA) .NE. 0 ) THEN
                        T_AVAIL = MAX( T_AVAIL, 
     1                       STOPJ(LASTISCN(ISTA)) + GAP(ISCN) )
                     END IF
C
C                    Add this station in its place in the list
C                    of start times sorted into inverse time order.
C                    This is to use with NOWAIT to not wait for
C                    the last stations.
C                    Start by replacing the last element of TBEGSRT
C                    of interest if the new T_AVAIL is later.  Note
C                    that this works if NOWAIT(ISCN) = 0
C
             write(*,*) 'optdwell add station ', iscn, ista, t_avail
                     IF( T_AVAIL .GT. TBEGSRT(NOWAIT(ISCN)+1) ) THEN
                        TBEGSRT(NOWAIT(ISCN)+1) = T_AVAIL
C
C                       Now, if NOWAIT is more than zero, push the new 
C                       time up the stack to put in it's proper place.
C
                        IF( NOWAIT(ISCN) .GE. 1 ) THEN
                           DO I = NOWAIT(ISCN), 1, -1
                              IF( TBEGSRT(I+1) .GT. TBEGSRT(I) ) THEN
                                 DTEMP = TBEGSRT(I)
                                 TBEGSRT(I) = TBEGSRT(I+1)
                                 TBEGSRT(I+1) = DTEMP
                              END IF
                           END DO
                        END IF
            write(*,*) 'optdwell new times ', iscn, ista,
     1           (tbegsrt(i),i=1,nowait(iscn)+1)
                     END IF
C
                  END IF
               END IF
            END DO
C
C           Pick off the one we want.  Do something reasonable
C           if none of the choices are good.
C
            TBEGIN = TBEGSRT(NOWAIT(ISCN)+1)
       write(*,*) 'optdwell ',iscn, nowait(ISCN), tbegsrt(1), 
     1      tbegsrt(2),tbegsrt(3),tbegsrt(4), tbegin
C
C           See if any other start times work (like the number of 
C           stations in the scan was less than NOWAIT).
C           
            IF( TBEGIN .EQ. 0.D0 .AND. NOWAIT(ISCN) .GE. 1 ) THEN
               DO I = 1, NOWAIT(ISCN)
                  IF( TBEGSRT(I) .GT. 0.D0 ) THEN
                     TBEGIN = TBEGSRT(I)
                  END IF
               END DO
            END IF
C
C           If all else fails (it shouldn't), use TAPPROX.
C
            IF( TBEGIN .EQ. 0.D0 ) THEN
               TBEGIN = TAPPROX
            END IF
C
         ELSE
C
C           Duration scan.
C
            TBEGIN = TAPPROX
C
         END IF
C
         STARTJ(ISCN) = TBEGIN
C
C        The stop time can be adjusted only if neither START or STOP
C        were specified (ie a duration was given and the START is
C        free to float).  All other cases fix the stop time.
C
         IF( DURONLY(ISCN) .LE. 1 ) THEN
            STOPJ(ISCN)  = TBEGIN + DUR(ISCN)
         END IF
C
      END IF
C
  999 CONTINUE
C
      RETURN
      END
