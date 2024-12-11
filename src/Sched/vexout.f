      SUBROUTINE VEXOUT
C
C     Routine for SCHED called by the top level routine that 
C     sets up to call the VEX writer.  It makes some decisions
C     about whether such a call should be allowed.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER   ISET, ISCN, ISRC
      LOGICAL   FMTNONE, ALLNONE
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VEXOUT starting.' )
C
C     For the moment (9feb2001), the VEX writer cannot deal with
C     FORMAT=NONE.  Therefore block it, but give Huib a back
C     door for testing.
C     Modified Oct 27, 2008 to pass schedules that only have some
C     format NONE scans.  Those scans will simply be left out of
C     the Vex file.  Don't pass schedules that are all format NONE
C     but want VEX.  This will change more in the near future as we
C     figure out how to do pointing runs with VEX.
C
C     As the switch to the DiFX correlator occurs, most interferometry
C     observations will use VEX.  But block VEX output when doing
C     OBSTYPE='CONFIG' where there may be a special binary with MSET
C     = MAXMOD set too small to actually use.
C
C     Allow VEX files to be written for moving sources, but warn that
C     the positions should be obtained elsewhere for correlation.
C     (Feb 27, 2010 RCW)
C
C     Nov. 2011.  We need Vex files for pointing observations, so
C     no longer block non-recording projects.  The Vex files will
C     now drive the new control systems at VLBA stations which are
C     setting all the switches in the antenna.
C
C
      IF( DOVEX ) THEN
C
C        Look for recording scans on planets or satellites.  We don't
C        know how to describe those in Vex yet.
C
         MOVING = .FALSE.
         DO ISCN = SCAN1, SCANL
            ISRC = SRCNUM(ISCN)
            IF( SUSED(ISRC) .AND. .NOT. NOREC(ISCN) .AND.
     1         ( PLANET(ISRC) .OR. SATEL(ISRC) .OR.
     2          DRA(ISRC) .GE. 0.0001/15.0 .OR. 
     3          DDEC(ISRC) .GE. 0.0001  ) ) THEN
               MOVING = .TRUE.
            END IF
         END DO
         IF( MOVING ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '(A,A)' )
     1        '++++ VEXOUT: Cannot pass accurate positions for ',
     2        'planets or satellites in the VEX file.'
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            WRITE( MSGTXT, '(A,A)' )
     1        '        For correlation, get the source positions ',
     2        'elsewhere - like from ephemeris.'
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            WRITE( MSGTXT, '(A,A)' )
     1        '        For VLBA pointing observations as of 2011, the ',
     2        'VEX positions'
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            WRITE( MSGTXT, '(A,A)' )
     1        '        are not used so this is ok.  You may ignore the',
     2        ' warnings from VXSUDT.'
            CALL WLOG( 1, MSGTXT )

         END IF
C
C        Check for any FORMAT 'NONE' and all FORMAT 'NONE'.  Don't
C        write VEX for all FORMAT 'NONE'
C
         FMTNONE = .FALSE.
         ALLNONE = .TRUE.
         DO ISET = 1, NSET
            IF( USED(ISET) .AND. FORMAT(ISET) .EQ. 'NONE' ) 
     1           FMTNONE = .TRUE.
            IF( USED(ISET) .AND. FORMAT(ISET) .NE. 'NONE' ) 
     1           ALLNONE = .FALSE.
         END DO
C
C        Actually write the VEX file, or explain why not.
C        Only allow all format='none' for VLBA pointing files.
C        Can I change that eventually?
C        Don't write it for a configuration test.
C
C         IF( OVERRIDE .OR. .NOT. FMTNONE ) THEN
         IF( ( OVERRIDE .OR. .NOT. ALLNONE .OR. 
     1         OBSTYP .EQ. 'PTVLBA') .AND. 
     2         OBSTYP .NE. 'CONFIG' ) THEN
C
            CALL VXWRT
C
C           Also write a template v2d file for DiFX correlation.
C
            IF( .NOT. OBSTYP .EQ. 'PTVLBA' ) CALL V2DOUT
C
         ELSE IF( ALLNONE ) THEN
            WRITE( MSGTXT, '(A,A)' )
     1        'VEXOUT: Cannot use VEX with a schedule in which ',
     2        'all scans have FORMAT=NONE.'
            CALL ERRLOG( MSGTXT )
         ELSE IF( CONFIG ) THEN
            WRITE( MSGTXT, '(A,A)' )
     1        'VEXOUT: Do not mix VEX with Configuration tests. ',
     2        'Set DOVEX=-1'
            CALL ERRLOG( MSGTXT )
         ELSE
            WRITE( MSGTXT, '(A,A)' )
     1        'VEXOUT: Programming error - should not get here.',
     2        ' VEX file not being written.'
         END IF
      END IF
C
      RETURN
      END
