      SUBROUTINE SETBBC( KS )
C
C     Routine for SCHED, called by SETDEFS, that assigns BaseBand 
C     Converters (BBCs) to channels.  It also assigns the IF's if
C     they were not specified in the setup file.  This used to be
C     in SETFCAT, but the Mark IV complications made it better to
C     put it here.
C
C     Two channels can share a BBC if they have the same FREQSET and
C     the same IFCHAN.  Otherwise they have to be different.
C     Note that they don't have to share the same sideband.
C
C     Note that the maximum number of channels in this whole setup file
C     is MSCHN(ISETNUM(KS)).  The minimum number of BBC's available
C     is MINBBC(ISETNUM(KS)).
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
      INCLUDE    'schfreq.inc'
C
      INTEGER    ICH, KS, KF, KIF
C --------------------------------------------------------------------
      IF( SDEBUG ) CALL WLOG( 0, 'SETBBC: Starting.' )
C
C     Assign the IF channels based on the frequency catalog
C     if they were not in the setup file.
C
      DO ICH = 1, NCHAN(KS)
         IF( IFREQNUM(ICH,KS) .GE. 1 ) THEN
            KF = IFREQNUM(ICH,KS)
            KIF = IFREQIF(ICH,KS)
            IF( IFCHAN(ICH,KS) .EQ. ' ' ) THEN
               IFCHAN(ICH,KS) = FIFNAM(KIF,KF)
            END IF
         ELSE
C
C           If there is no corresponding frequency catalog entry, 
C           require that the IFCHANS have been set.
C
            IF( IFCHAN(ICH,KS) .EQ. ' ' ) THEN
               WRITE( MSGTXT, '( A, I3, A, I3, A )' )
     1             'SETBBC: Cannot set IFCHANs.  First bad channel: ',
     2             ICH, ' of ', NCHAN(KS), ' total.'
               CALL WLOG( 1, MSGTXT )
               CALL ERRSET( KS )
            END IF

         END IF
      END DO
C
C
C     Try assigning BBC's.  Each of these IF's, except the 
C     first used to have a dependence on the FORMAT, 
C     requiring VLBA or MARKIII for the VLBA and VLBAG 
C     DARs and MKIV or MARKIII for the VLBA4 and MKIV 
C     DARs.  I don't think those dependencies are required,
C     and without them, the BBC's can be set without 
C     knowledge of the recording - desirable with the 
C     proliferation of systems.
C
      IF( RECORDER(ISETSTA(KS)) .EQ. 'S2' .AND.
     1       ( DAR(ISETSTA(KS)) .EQ. 'VLBA' .OR.
     2         DAR(ISETSTA(KS)) .EQ. 'VLBAG' .OR.
     3         DAR(ISETSTA(KS)) .EQ. 'VLBA4' ) ) THEN
C
C        S2 system on any type of VLBA DAR.
C
         CALL BBCVS2( KS )    
C
      ELSE IF( DAR(ISETSTA(KS)) .EQ. 'VLBA' ) THEN
C
C        Normal VLBA systems.
C
         CALL BBCVLBA( KS )
C
      ELSE IF( DAR(ISETSTA(KS))(1:4) .EQ. 'RDBE' ) THEN
C
C        RDBE and RDBE2 digital backend, including VLBA
C
         CALL BBCRDBE( KS )
C
      ELSE IF( DAR(ISETSTA(KS)) .EQ. 'WIDAR' ) THEN
C
C        WIDAR (VLA) digital backend.
C
         CALL BBCWIDAR( KS )
C
      ELSE IF( DAR(ISETSTA(KS)) .EQ. 'VLBAG' ) THEN
C
C        VLBA systems with "geodetic wiring".
C
         CALL BBCGEO( KS )
C
      ELSE IF( DAR(ISETSTA(KS)) .EQ. 'VLBA4' ) THEN
C
C        VLBA systems with "geodetic wiring" and Mark IV formatters.
C
         CALL BBCGEO( KS )
C
      ELSE IF( DAR(ISETSTA(KS)) .EQ. 'MKIV' ) THEN
C
C        Mark IV systems.  This hopefully works for S2 attached
C        to Mark IV.
C
         CALL BBCM4( KS )
C
      ELSE IF( DAR(ISETSTA(KS)) .EQ. 'DBBC') THEN
C
C     Gino Tuccari's DBBC
C
         CALL BBCDBBC( KS )
C
      ELSE IF( DAR(ISETSTA(KS)) .EQ. 'CDAS') THEN
C
C        Chinese DAR
C
         CALL BBCCDAS( KS )
C
      ELSE IF( DAR(ISETSTA(KS)) .EQ. 'R1002') THEN
C
C
         CALL BBCKVSR( KS )
C
      ELSE IF( DAR(ISETSTA(KS)) .EQ. 'LBA') THEN
C
C     Australian LBA system
         CALL BBCLBA( KS )
C
      ELSE IF( DAR(ISETSTA(KS)) .NE. 'NONE' ) THEN
C
C        Do not try to default with other formats.
C
         DO ICH = 1, NCHAN(KS)
            IF( BBC(ICH,KS) .EQ. 0 ) THEN
               WRITE( MSGTXT, '( 4A )' )
     1           'SETBBC: SCHED does not set default BBCs for format: ',
     2           FORMAT(KS), ', DAR type: ', DAR(ISETSTA(KS))
               CALL WLOG( 1, MSGTXT )
               CALL ERRSET( KS )
            END IF
         END DO
      END IF
C
      RETURN
      END
