      SUBROUTINE CHKVLA( KS, ERRS )
C
C     Subroutine for SCHED called by CHKSET that checks some
C     VLA specific setup file parameters.  It also checks some
C     VLA specific scan parameters.
C
C     If ERRS is returned as TRUE, CHKSET will give the setup
C     file name and then crash the program.
C
C     Get the band names from program VLABANDS which also gets lots
C     of stuff for subroutine VLASETF.
C
C     Possible future additions:
C       Check that requested frequencies fit through the filters.
C       Check VLAIF and VLAROT file names (how?)
C         No longer needed (2007 or earlier).  They're no longer used.
C
C     2004 Aug. 27.  Got some new frequency options from Ken 
C     Sowinski.  It seems that the front end synthesizers can tune
C     over a wider range than used to be possible - done to support
C     some Q band observations.  RCW.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER     KS, I, J, NBANDS, ICH, IB, JB
      PARAMETER   (NBANDS=19)
      CHARACTER   BNAME(NBANDS)*2
      DOUBLE PRECISION  VLO(4), VFLO(4), VBP(2,4), DIFF
      DOUBLE PRECISION  LFLUKEA, LFLUKEB
      LOGICAL     ERRS, GOTLO, GOTFI, OK
      LOGICAL     GOTBD, BADLO, FLKWARN
      CHARACTER   DIG(10)*1
      SAVE        DIG, BNAME, LFLUKEA, LFLUKEB, FLKWARN
      DATA        LFLUKEA, LFLUKEB / 0.D0, 0.D0 /
      DATA        FLKWARN / .TRUE. /
      DATA        DIG / '0','1','2','3','4','5','6','7','8','9' /
      DATA        (BNAME(I), I=1,NBANDS) /
     1     '44','4P','PP','LP','HH','LL','18','CC','XX','UU',
     2     'KK','QQ','VP','VL','VC','VX','VU','VK','VQ' /
C  --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVLA: Starting.' )
C
C     It is not wise to make VLA schedules longer than 24 hours.
C     There is no day number on the scans so, if there is a restart,
C     they can end up on the wrong day.
C
      IF( TEND - TFIRST .GT. 1.D0 ) THEN
         CALL WRTMSG( 0, 'CHKVLA', 'vlaover24' )
      END IF
C
C     Check VLABAND (someday check against all valid modes).
C
      GOTBD = .FALSE.
      DO IB = 1, NBANDS
         IF( VLABAND(KS) .EQ. BNAME(IB) ) THEN
            GOTBD = .TRUE.
            JB = IB
         END IF
      END DO
C
      IF( .NOT. GOTBD ) THEN
         IF( VLABAND(KS) .EQ. 'ZZ' ) THEN
            CALL WLOG( 1, 'CHKVLA: VLABAND must be specified.' )
         ELSE 
            CALL WLOG( 1, 'CHKVLA: Invalid VLABAND '//VLABAND(KS) )
         END IF
         ERRS = .TRUE.
      END IF
C     
C     Set any VLA parameters to default for band if they weren't set.
C     Will check some VLAFEAB cases.
C
      IF( GOTBD ) THEN
         CALL VLASETF( VLABAND(KS), FLUKEA(KS), FLUKEB(KS), 
     1                 VLAFEAB(KS), VLAFECD(KS),
     2                 VLASYNA(KS), VLASYNB(KS), 
     3                 FEFILTER(KS), VLAIF(KS), VLAROT(KS),
     4                 VLABW(KS), VLALOFI(KS), ERRS )
      END IF
C
C     Check the VLA information.
C
C     Check VLABW
C
      DO I = 1, 4
         OK = .FALSE.
         DO J = 1, 10
            IF( VLABW(KS)(I:I) .EQ. DIG(J) ) OK = .TRUE.
         END DO
         IF( .NOT. OK ) THEN
            CALL WLOG( 1, 'CHKVLA: Bad VLABW: '//VLABW(KS) )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Check FEFILTER
C
      DO I = 1, 4
         IF( FEFILTER(KS)(I:I) .NE. ' ' .AND. 
     1       FEFILTER(KS)(I:I) .NE. '0' .AND.
     2       FEFILTER(KS)(I:I) .NE. '1' .AND. 
     3       FEFILTER(KS)(I:I) .NE. '2' ) THEN
            CALL WLOG( 1, 'CHKVLA: Invalid FEFILTER ('//FEFILTER(KS)//
     1          ') - see manual.' )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Test what we have.  Note VLAFEAB and B can be 0.0 for 6cm.
C     This will also check the defaults, but that is ok.
C
      GOTLO = VLASYNA(KS) .NE. 0.0 .AND. VLASYNB(KS) .NE. 0.0
      GOTFI = FLUKEA(KS)  .NE. 0.0 .AND. FLUKEB(KS)  .NE. 0.0 
      IF( .NOT. ( GOTLO .AND. GOTFI ) ) THEN
         CALL WLOG( 1, 'CHKVLA: Some, but not enough, parameters '//
     1         'specified for LO and FI cards.' )
         CALL WLOG( 1, '        VLASYNA, VLASYNB, FLUKEA, and '//
     1         'FLUKEB are minimum requirements.' )
         ERRS = .TRUE.
      END IF
C
C     Check the VLASYNA and VLASYNB for valid values.
C     They must be N*50+-10.1 MHz.  There are odd values for 4
C     and P band.
C
C     There is a limited frequency range
C     The IF must fit through a filter that is 4355 to 5145 MHz.
C     That is mixed with the VLASYN + 900 + Fluke, I think and
C     the upper sideband from that mix is kept.  For X and U bands,
C     the first mix at RF was lower sideband so the second mix keeps
C     it at lower sideband. 
C
C     The EVLA system is quite different, and for now frequencies
C     are being set by sticking in values that conform to the N*50+-10.1
C     but are not necessarily within the old VLA bands.  Check against
C     the old bands, but only abort if EVLA is not >0.
C
C       write(*,*) 'CHKVLA', ks, evla(ks), vlasyna(ks)
      IF( VLABAND(KS)(1:1) .EQ. '4' ) THEN
         IF( BADLO( 'VLASYNA', VLASYNA(KS), 50.0D0, 1, 10.1D0, 0.D0,
     1              -961.D0, -939.D0, SETMSG ) ) ERRS = .TRUE.
      ELSE IF( VLABAND(KS)(1:1) .EQ. 'P' .OR. VLABAND(KS) .EQ. 'VP' )
     1       THEN
         IF( BADLO( 'VLASYNA', VLASYNA(KS), 50.0D0, 1, 10.1D0, 0.D0, 
     1              -711.D0, -689.D0, SETMSG ) ) ERRS = .TRUE.
      ELSE
         IF( EVLA(KS) .NE. 0 ) THEN
            IF( BADLO( 'VLASYNA', VLASYNA(KS), 50.0D0, 1, 10.1D0, 0.D0, 
     1              0.0D0, 60000.0D0, SETMSG ) ) ERRS = .TRUE.
         ELSE
            IF( BADLO( 'VLASYNA', VLASYNA(KS), 50.0D0, 1, 10.1D0, 0.D0, 
     1              3300.0D0, 4075.0D0, SETMSG ) ) ERRS = .TRUE.
         END IF
      END IF
C
      IF( VLABAND(KS)(2:2) .EQ. '4' ) THEN
         IF( BADLO( 'VLASYNB', VLASYNB(KS), 50.0D0, 1, 10.1D0, 0.D0, 
     1              -961.D0, -939.D0, SETMSG ) ) ERRS = .TRUE.
      ELSE IF( VLABAND(KS)(2:2) .EQ. 'P' ) THEN
         IF( BADLO( 'VLASYNB', VLASYNB(KS), 50.0D0, 1, 10.1D0, 0.D0, 
     1              -711.D0, -689.D0, SETMSG ) ) ERRS = .TRUE.
      ELSE
         IF( EVLA(KS) .NE. 0 ) THEN
            IF( BADLO( 'VLASYNB', VLASYNB(KS), 50.0D0, 1, 10.1D0, 0.D0, 
     1              0.0D0, 60000.0D0, SETMSG ) ) ERRS = .TRUE.
         ELSE
            IF( BADLO( 'VLASYNB', VLASYNB(KS), 50.0D0, 1, 10.1D0, 0.D0, 
     1              3300.0D0, 4075.0D0, SETMSG ) ) ERRS = .TRUE.
         END IF
      END IF
C
C     Check FLUKEA and FLUKEB
C
      IF( BADLO( 'FLUKEA', FLUKEA(KS), 0.D0, -1, 0.D0, 0.D0, 
     1              99.5D0, 150.0D0, SETMSG ) ) ERRS = .TRUE.
      IF( BADLO( 'FLUKEB', FLUKEB(KS), 0.D0, -1, 0.D0, 0.D0, 
     1              199.5D0, 250.0D0, SETMSG ) ) ERRS = .TRUE.
C
C     If the flukes are not set to N*10 MHz, they will not return
C     to the same phase after a frequency change.  Warn users of
C     this.
C
      IF( LFLUKEA .EQ. 0.D0 ) LFLUKEA = FLUKEA(KS)
      IF( LFLUKEB .EQ. 0.D0 ) LFLUKEB = FLUKEB(KS)
      IF( FLKWARN ) THEN
         IF( ( FLUKEA(KS) .NE. LFLUKEA .AND.
     1         ( MOD( FLUKEA(KS), 10.D0 ) .NE. 0.D0  .OR.
     2           MOD( LFLUKEA, 10.D0 ) .NE. 0.D0 ) )   . OR.
     3       ( FLUKEA(KS) .NE. LFLUKEA .AND.
     4         ( MOD( FLUKEA(KS), 10.D0 ) .NE. 0.D0  .OR.
     5           MOD( LFLUKEA, 10.D0 ) .NE. 0.D0 ) ) ) THEN
C
            CALL WRTMSG( 0, 'CHKVLA', 'vlaflukephase' )
            CALL WLOG( 1, ' ' )
            CALL WLOG( 1, 'CHKVLA:  **** See warning about ' //
     1           'VLA Fluke freqencies in sched.runlog' )
            CALL WLOG( 1, ' ' )
            FLKWARN = .FALSE.
C
         END IF
      END IF
C
C     Check the flukeset.
C
      IF( FLUKESET(KS) .NE. 0 .AND. FLUKESET(KS) .NE. 1 .AND.
     1       FLUKESET(KS) .NE. 2 ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( A, I6 )' ) ' Invalid FLUKESET: ', 
     1          FLUKESET(KS)
         CALL WLOG( 1, 'CHKVLA: '//SETMSG )
         ERRS = .TRUE.
      END IF
C
C     Check the VLAFEAB and VLAFECD
C     Bands forced to have 0.0 or -3.2 checked in VLASETF.
C     This is 4, P, L, and C bands and some mixtures.
C
C     For the moment do not do this check when EVLA is specified.
C     There might be some useful checking to do, but I don't know
C     what it is as of Oct. 27, 2008  RCW.
C     
C
      IF( GOTBD .AND. EVLA(KS) .EQ. 0 ) THEN
C
C        Go through the distinct cases.
C
         IF( VLABAND(KS)(2:2) .EQ. 'X' ) THEN
C
C            Valid For X band:  11.8, 12.2, 12.4, 12.8, 13.0, 13.4,
C            (N*0.6+-0.2)       13.6, 14.0, 14.2, 14.6, 14.8, 15.2
C
            IF( BADLO( 'VLAFEAB', VLAFEAB(KS), 0.6D0, 1, 0.2D0, 
     1              0.D0, 11.7D0, 15.3D0, SETMSG ) .OR. 
     2          BADLO( 'VLAFECD', VLAFECD(KS), 0.6D0, 1, 0.2D0, 
     3              0.0D0, 11.7D0, 15.3D0, SETMSG ) ) THEN
               CALL WLOG( 1, 'CHKVLA: Invalid VLAFEAB and/or '//
     1             'VLAFECD for X band.  See manual.' )
               ERRS = .TRUE.
            END IF
C
         ELSE IF( VLABAND(KS)(2:2) .EQ. 'U' .OR. 
     1            VLABAND(KS)(2:2) .EQ. 'K' ) THEN
C
C            Valid for 1 and 2 cm:   (N*0.3+-0.1)
C                15.1, 15.2, 15.4, 15.5, 15.7, 15.8, 16.0,
C                16.1, 16.3, 16.4, 16.6, 
C                16.7, 16.9, 17.0, 17.2, 17.3, 17.5, 17.6, 
C                17.8, 17.9, 18.1, 18.2, 18.4, 18.5, 18.7, 
C                18.8, 19.0, 19.1, 19.3, 19.4, 19.6, 19.7, 
C                19.9, 20.0, 20.2, 20.3, 20.5, 20.6
C            Note that many of the above settings will not 
C            produce usable signals. 
C
            IF( BADLO( 'VLAFEAB', VLAFEAB(KS), 0.3D0, 1, 0.1D0, 
     1              0.0D0, 15.05D0, 20.7D0, SETMSG ) .OR. 
     2          BADLO( 'VLAFECD', VLAFECD(KS), 0.3D0, 1, 0.1D0, 
     3              0.0D0, 15.05D0, 20.7D0, SETMSG ) ) THEN
               CALL WLOG( 1, 'CHKVLA: Invalid VLAFEAB and/or '//
     1            'VLAFECD for U or K band.  See manual.' )
               ERRS = .TRUE.
            END IF
         ELSE IF( VLABAND(KS)(2:2) .EQ. 'Q' ) THEN
C
C           For Q band, VLAFEAB should be a 3*valid K/U band value 
C           and VLAFECD should be a valid X band value.
C           The first value (51.6 is a typical one) will produce a 
C           lower sideband IF in the 4 cm part of the spectrum which 
C           is then fed to the 4 cm frequency conversion system. 
C
            IF( BADLO( 'VLAFEAB', VLAFEAB(KS), 0.9D0, 1, 0.3D0, 
     1              0.0D0, 3.D0*16.6D0, 3.D0*20.7D0, SETMSG ) .OR. 
     2          BADLO( 'VLAFECD', VLAFECD(KS), 0.6D0, 1, 0.2D0, 
     3              0.0D0, 11.7D0, 15.3D0, SETMSG ) ) THEN
               CALL WLOG( 1, 'CHKVLA: Invalid VLAFEAB and/or '//
     1           'VLAFECD for Q band.  See manual.' )
               ERRS = .TRUE.
            END IF
C
         END IF
      ELSE IF( GOTBD .AND. EVLA(KS) .NE. 0 ) THEN
C
C        Don't do anything until I find out more about the restrictions.
C
      END IF
C
C     Get the observing frequencies.
C     VLAFREQ gets the frequencies in arrays of 4, one for each IF.
C
      CALL VLAFREQ( KS, VLO, VFLO, VBP, ERRS )
C
C     Now check the FIRSTLO values for VLBI experiments.
C
      IF( .NOT. VLAONLY ) THEN
C
C        The new digital patch panel at the VLA allows some checks
C        to be tightened.  With that panel, the VLA IF A goes to
C        the VLBA IF A etc except in special cases which we won't
C        try to handle at the moment.
C
C        There is a precision problem with comparing the frequencies.
C        Assume a match to a part in 2.E10 is adequate
C
         DO ICH = 1, NCHAN(KS)
            DIFF = ABS( FIRSTLO(ICH,KS) / 2.D10 )
            IF( ( IFCHAN(ICH,KS) .EQ. 'A' .AND.
     1          ABS( FIRSTLO(ICH,KS) - VFLO(1) ) .GT. DIFF ) .OR.
     2          ( IFCHAN(ICH,KS) .EQ. 'B' .AND.
     3          ABS( FIRSTLO(ICH,KS) - VFLO(2) ) .GT. DIFF ) .OR.
     4          ( IFCHAN(ICH,KS) .EQ. 'C' .AND.
     5          ABS( FIRSTLO(ICH,KS) - VFLO(3) ) .GT. DIFF ) .OR.
     6          ( IFCHAN(ICH,KS) .EQ. 'D' .AND.
     7          ABS( FIRSTLO(ICH,KS) - VFLO(4) ) .GT. DIFF ) ) THEN
               CALL WLOG( 1, 'CHKVLA: FIRSTLO does not agree '//
     1             'with value derived from VLA parameters.' )
               SETMSG = ' '
               WRITE( SETMSG, '( A, I3, 3A, F10.2 )' )
     1             'Channel:', ICH, ' IFCHAN:', IFCHAN(ICH,KS),
     2             '   FIRSTLO:', FIRSTLO(ICH,KS)
               CALL WLOG( 1, '        '//SETMSG)
               SETMSG = ' '
               WRITE( SETMSG, '( A, 4F10.2 )' )
     1             'Derived FIRSTLOs for VLA A, B, C, and D:', 
     2             VFLO(1), VFLO(2), VFLO(3), VFLO(4)
               CALL WLOG( 1, '        '//SETMSG)
               ERRS = .TRUE.
            END IF
         END DO
      END IF
C
C     Check VLARFANT
C
      IF( VLARFANT .LT. 1 .OR. VLARFANT .GT. 28 ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( A, I7, A )' ) 'CHKVLA: VLARFANT ', 
     1      VLARFANT, ' out of range 1 to 28.'
         CALL WLOG( 1, SETMSG )
         ERRS = .TRUE.
      END IF
C
      RETURN
      END
