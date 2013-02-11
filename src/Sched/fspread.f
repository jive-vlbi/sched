      SUBROUTINE FSPREAD( NEEDFMT )
C
C     This routine is likely to be obsolete thanks to disk 
C     systems that don't end up in management nightmares when
C     the number of tracks change and to correlators that don't
C     care when different stations have different track bit 
C     rates.
C
C     Routine for SCHED, called by SETFORM, that propagates
C     track bit rates across setup files and numbers of track
C     through time for each station.  It is part of the format
C     setting mechanism.  Such behavior is confined to formats
C     for such systems as require these constrains, mainly 
C     VLBA and MKIV, although Mark III formats are checked, 
C     but not set (they were already forced by the first SETFMT
C     call).
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER  KS, ISETF
      INTEGER  NTRACK, MINSTRK, USETRK
      REAL     MAXTOT, TRKBPS
      REAL     TTBPS(MAXSET), BTBPS(MAXSET)
      REAL     SETTBPS(MAXSET), FSAMPR(MAXSET), MAXFTOT(MAXSET)
      LOGICAL  NEEDFMT(*), OK
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'FSPREAD starting' )
C
C     Ask for feedback if the routine is called.
C
      CALL WLOG( 1, 'FSPREAD:  Please inform Craig Walker at '//
     1  'cwalker@nrao.edu if you see this message.' )
      CALL WLOG( 1, '          It means you reached a part of SCHED '//
     1  'not thought to still be active.' )
      CALL WLOG( 1, '          Please include your input file.' )
C
C     Get some information about the parameters in the setup files.
C
      MAXTOT = 0.0
      DO ISETF = 1, NSETF
         TTBPS(ISETF)   = 1.E5
         BTBPS(ISETF)   = 0.0
         FSAMPR(ISETF)  = 0.0
         MAXFTOT(ISETF) = 0.0
C         SETTBPS(ISETF) = 0.0
      END DO
      DO KS = 1, NSET
         ISETF = ISETNUM(KS)
         IF( RECUSED(KS) .AND.
     1     ( VLBAMKIV(KS) .OR. FORMAT(KS) .EQ. 'MARKIII' ) ) THEN  
C
C           Get the minimum and maximum acceptable track bit rates
C           for each setup file.  Any incompatibility in previously
C           set files should show up here.  Include MARKIII, although
C           mixed mode observations should be a thing of the past.
C           Should S2 be included?  I don't think so.
C
            IF( NEEDFMT(KS) ) THEN
               TTBPS(ISETF) = MIN( TTBPS(ISETF), MAXTBPS(KS) )
               BTBPS(ISETF) = MAX( BTBPS(ISETF), MINTBPS(KS) )
            ELSE
               TTBPS(ISETF) = MIN( TTBPS(ISETF), TBPS(KS) )
               BTBPS(ISETF) = MAX( BTBPS(ISETF), TBPS(KS) )
            END IF
C
C           Enforce constant sample rate in a setup file.
C           This is done later by CHKSFIL, but we need it here.
C
            IF( FSAMPR(ISETF) .NE. 0.0 .AND. 
     1          FSAMPR(ISETF) .NE. SAMPRATE(KS) ) THEN
               CALL WLOG( 1, 'FSPREAD: Sample rate not constant ' //
     1            'across setup groups in setup file:' )
               CALL WLOG( 1, '       ' // SETFILE(ISETF) )
               CALL ERRLOG( 'Fix the unequal rates.' )
            ELSE IF( FSAMPR(ISETF) .EQ. 0.0 ) THEN
               FSAMPR(ISETF) = SAMPRATE(KS)
            END IF
C
C           With the disk systems, the following is not needed so
C           don't do it (RCW  Jan. 11, 2011).
C 
C           Enforce constant track bit rate in a setup file.
C           This is also done later, but again we need it here.
C           This is only for groups whose format has already been set.
C
C            IF( .NOT. NEEDFMT(KS) ) THEN
C               IF( SETTBPS(ISETF) .NE. 0.0 .AND. 
C     1             SETTBPS(ISETF) .NE. TBPS(KS) ) THEN
C                  CALL WLOG( 1, 'FSPREAD: Track bit rate not ' //
C     1            'constant across setup groups in setup file:' )
C                  CALL WLOG( 1, '       ' // SETFILE(ISETF) )
C                  CALL ERRLOG( 'Fix the unequal rates.' )
C               ELSE IF( SETTBPS(ISETF) .EQ. 0.0 ) THEN
C                  SETTBPS(ISETF) = TBPS(KS)
C               END IF
C            END IF
C
C           Get the maximum total bit rate for the setup file and 
C           overall.  Use the nominal rates TOTBPS rather than the
C           format specific true bit rates WRTBPS.
C
            MAXFTOT(ISETF) = MAX( MAXFTOT(ISETF), TOTBPS(KS) )
            MAXTOT = MAX( MAXTOT, TOTBPS(KS) )
C
         END IF
      END DO
C
C     Now deal with the setup files.  Reuse SETTBPS.
C
      DO ISETF = 1, NSETF
         SETTBPS(ISETF) = 0.0
C
C        Moan if the setup groups are incompatible.  With equal
C        sample rates, I think this is hard, especially with the
C        checks already done.
C
         IF( TTBPS(ISETF) .LT. BTBPS(ISETF) ) THEN
            CALL WLOG( 1, 'FSPREAD: Cannot set all VLBA and MKIV ' //
     1         ' setup groups in a setup file to the same ' )
            CALL WLOG( 1, '         track bit rate.  This is ' //
     1                 'not allowed.' )
            CALL ERRLOG( ' The setup file is: ' // SETFILE(ISETF) )
         END IF
C
C        Set any formats in this setup file if we can, which is when
C        there is no remaining choice in track bit rate.  The obvious
C        case is when a setup has been forced.  But other cases
C        might arise when two setup groups have allowed ranges that
C        only overlap by one.
C
         IF( TTBPS(ISETF) .EQ. BTBPS(ISETF) ) THEN
            SETTBPS(ISETF) = TTBPS(ISETF)
            DO KS = 1, NSET
               IF( ISETF .EQ. ISETNUM(KS) .AND. VLBAMKIV(KS) .AND.
     1             RECUSED(KS) .AND. NEEDFMT(KS) ) THEN
                  TRKBPS = SETTBPS(ISETF)
                  CALL SETFMT( KS, TRKBPS, OK )
                  IF( OK ) NEEDFMT(KS) = .FALSE.
               END IF
            END DO
         END IF            
C
      END DO
C
C
C     Now try to get all setup files to use the same number of
C     tracks.  I've driven myself nuts trying to do this on a
C     per-station basis, but still enforcing constant track bit
C     rate per setup file and allowing some setups, either previously
C     set or still unset, to use fewer tracks than the rest.
C     So now, I will assume that all stations use the same number
C     of tracks.  Note that we haven't dropped the number of tracks
C     at single head/drive stations yet, so this approximation is
C     not that bad.  It is almost certainly possible to fool this
C     scheme using setup files for subsets of antennas, but I'll let
C     the user fix those by forcing TPMODE or FORMAT.
C
C     Determine the minimum number of tracks that the observation can
C     use (USETRK).  Hopefully this will end up being equal to the
C     number used by setups with preset formats, but it could
C     be larger.  It is not allowed to be smaller.  Larger is not 
C     good because tape will be wasted in the scans using the forced 
C     setups, so try to keep any excess to a minimum.
C     NTRACK is the number of tracks required to support preset 
C     formats.  MINSTRK is the minimum number required to support 
C     other setups at the maximum track bits per second allowed
C     for the setup file (TTBPS) (that should be the same or larger than
C     what would be derived using MINTRAK(KS) because of the way TTBPS
C     is derived).
C     
      MINSTRK = 0
      NTRACK = 0
      DO KS = 1, NSET
         ISETF = ISETNUM(KS)
         IF( RECUSED(KS) .AND. 
     1    ( VLBAMKIV(KS) .OR. FORMAT(KS) .EQ. 'MARKIII' ) ) THEN
            MINSTRK = MAX( MINSTRK, NINT( TOTBPS(KS) / TTBPS(ISETF) ) )
            IF( .NOT. NEEDFMT(KS) ) THEN
               NTRACK = MAX( NTRACK, NINT( TOTBPS(KS) / TBPS(KS) ) )
            END IF
         END IF
      END DO
      USETRK = MAX( NTRACK, MINSTRK )
C
C     If there were no forced setups, we can't do any more so jump
C     over the rest.
C
      IF( NTRACK .NE. 0 ) THEN      
C
C        Ok, we have something to force the choices.  Try it.
C        Simply set all setups to use USETRK or fewer tracks.
C        Only take the fewer option when absolutely necessary.
C        If SETTBPS is not zero, the setup has already been forced
C        if it can be and all forced setups have SETTBPS non zero.
C        MAXFTOT will only be non-zero if the setup file was used
C        in a recording scan and is of appropriate type.
C
         DO ISETF = 1, NSETF
            IF( SETTBPS(ISETF) .EQ. 0.0 ) THEN
               SETTBPS(ISETF) = MAXFTOT(ISETF) / USETRK
C
C              Sanity check and adjust if needed.  Note logic above
C              forces SETTBPS <= TTBPS so don't check that.
C
               IF( SETTBPS(ISETF) .NE. 0.0 .AND.
     1             SETTBPS(ISETF) .LT. BTBPS(ISETF) ) THEN
                  SETTBPS(ISETF) = BTBPS(ISETF)
                  CALL WLOG( 1, 'FSPREAD: Setup file below will ' //
     1               'use fewer tracks than other setups.' )
                  CALL WLOG( 1, '         Some tape will be ' //
     1               'wasted.' )
                  CALL WLOG( 1, '         File: '// SETFILE(ISETF) )
               END IF
            END IF
         END DO
C
C        Loop through setup groups forcing the formats
C
         DO KS = 1, NSET
            ISETF = ISETNUM(KS)
            IF( SETTBPS(ISETF) .NE. 0.0 .AND. NEEDFMT(KS) .AND.
     1          VLBAMKIV(KS) .AND. RECUSED(KS) ) THEN
               TRKBPS = SETTBPS(ISETF)
               CALL SETFMT( KS, TRKBPS, OK )
               IF( OK ) NEEDFMT(KS) = .FALSE.
            END IF
         END DO
C
      END IF
C
      RETURN
      END
