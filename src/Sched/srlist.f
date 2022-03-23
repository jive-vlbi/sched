      SUBROUTINE SRLIST( SNAME, RA8, DEC8, 
     1                   RAE, DECE, SEQUINOX, SCALC, SRUSED )
C
C     Routine for SCHED that makes a source list, using the
C     variables in srlist.inc, of all sources read in the catalogs.
C     This list should not be used for schedule creation, but is
C     meant for schedule planning, plotting etc.  It contains far
C     less information and precision than the main source information
C     kept in variables defined in sched.inc.
C
C     SNAME should be the first name from the catalog.  SCALC is the
C     calcode.  
C
C     All positions will be converted kept in their input equinox for
C     now, but will be converted by SCHPRE.
C
      INCLUDE            'srlist.inc'
C
      CHARACTER          SNAME*12, SCALC*1, SEQUINOX*5, MSGTXT*256
      DOUBLE PRECISION   RA8, DEC8
      REAL               RAE, DECE
      LOGICAL            WARNEPO, WARNN, SRUSED
      DATA               WARNEPO / .TRUE. /
      DATA               WARNN / .TRUE. /
C ----------------------------------------------------------------------
C     Count sources.
C
      SRLN = SRLN + 1
C
C     Make sure we are not overflowing the available arrays.
C
      IF( SRLN .GT. MAXSRL ) THEN
         SRLN = SRLN - 1
C
         IF( WARNN ) THEN
C
C           Write error message, but don't make fatal.  Too often, this
C           list is just not needed by the user.
C
            CALL WLOG( 1, 'SRLIST: Too man sources in source catalogs '
     1           // 'for complete list used by plot routines.' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I6 )' ) 
     1            '        Maximum number is ', MAXSRL
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '        This only matters if you plan to '
     1           // 'ask SCHED for an RD plot of all catalog sources.' )
            WARNN = .FALSE.
         END IF
C
      ELSE
C
C        Accumulate the data.
C
         SRLNAME(SRLN) = SNAME
         SRLCALC(SRLN) = SCALC
         SRLRA(SRLN)   = RA8
         SRLDEC(SRLN)  = DEC8
         SRLRAE(SRLN)  = RAE
         SRLDECE(SRLN) = DECE
         SRLUSED(SRLN) = SRUSED
C
C        Get the source position in J2000 coordinates.
C        If an unrecognized equinox is used, just take the
C        coordinates unmodified, but give a warning.
C
         IF( SEQUINOX .EQ. 'J2000' ) THEN
            SRLEPO(SRLN) = 'J'
         ELSE IF( SEQUINOX .EQ. 'B1950' ) THEN
            SRLEPO(SRLN) = 'B'
         ELSE IF( SEQUINOX .EQ. 'DATE' ) THEN
            SRLEPO(SRLN) = 'D'
         ELSE
            SRLEPO(SRLN) = 'J'
            IF( WARNEPO ) THEN
               CALL WLOG( 1, 'SRLIST:  There are unrecognized EQUINOXs'
     1              // ' in the catalogs (' //  SEQUINOX // ')' )
               CALL WLOG( 1, 
     1              '         If you do RD plots of all sources, '
     2              // 'they may be based on unprecessed coordinates.' )
               WARNEPO = .FALSE.
            END IF
         END IF
      END IF
C
      RETURN
      END





