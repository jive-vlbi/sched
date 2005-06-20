      SUBROUTINE TPPACK( MODE, TPDAT, TPCHG, TPFF, TPRW, TPPASS, 
     1                TPDRIV, TPDIR, TPINDX, TPHEAD )
C
C     Routine for SCHED tape handling that allows many pieces of
C     information about the status of the tapes to be stored in a
C     reasonably compact form.
C     Note that DOTAPE=TPCHG is pulled directly from TPDAT in 
C      SCHSUM, SNAP, CRDNRAO and CRDVLA, so beware of any changes.
C
C     The pieces of information are:
C         name   type  word/digits  decscription
C    1    TPCHG:    L    1 / 1     Change tape at start of scan.
C    1    TPFF:     L    1 / 2     Fast forward before scan.
C    1    TPRW:     L    1 / 3     Rewind before scan.
C    2    TPPASS    I4   1 / 4-    Tape pass number, scan stop.
C    1    TPDRIV:   I4   2 / 1     Tape drive, scan stop.
C    1    TPDIR:    I4   2 / 2     Tape direction (+-1), scan stop.
C    2    TPINDX:   I4   2 / 5-    Head index position, scan stop.
C    2    TPHEAD:   I4   2 / 3-4   Head group, scan stop.
C
C     They are packed into TPDAT which will be have (2,ISCN,ISTA)
C     dimensions outside of this routine.
C
C     MODE='PACK' packs the info into TPDAT.
C     MODE='UNPACK' takes it out.
C
      INTEGER    TPDAT(2), TPPASS
      INTEGER    TPDRIV, TPDIR, TPINDX, TPHEAD
      LOGICAL    TPCHG, TPFF, TPRW
      INTEGER    ITPCHG, ITPFF, ITPRW
      CHARACTER  MODE*6
C -----------------------------------------------------------------
      IF( MODE(1:4) .EQ. 'PACK' ) THEN
C
C        Convert the logicals to integers.
C
         ITPCHG = 0
         IF( TPCHG ) ITPCHG = 1
         ITPFF = 0
         IF( TPFF ) ITPFF = 1
         ITPRW = 0
         IF( TPRW ) ITPRW = 1
C
C        Pack the data. 
C        DO NOT change packing of ITPCHG without seeking out all
C        instances of DOTAPE in other routines.
C
         TPDAT(1) = ITPCHG + 10 * ITPFF + 100 * ITPRW + 1000 * TPPASS
         TPDAT(2) = TPDRIV + 10 * ( TPDIR + 1 ) + 100 * TPHEAD +
     1        10000 * TPINDX
C
      ELSE IF( MODE .EQ. 'UNPACK' ) THEN
C
C        Unpack the data.
C
         TPCHG   = MOD( TPDAT(1), 10 ) .EQ. 1
         TPFF    = MOD( INT( TPDAT(1) / 10 ), 10 ) .EQ. 1 
         TPRW    = MOD( INT( TPDAT(1) / 100 ), 10 ) .EQ. 1
         TPPASS  = TPDAT(1) / 1000
C
         TPDRIV  = MOD( TPDAT(2), 10 )
         TPDIR   = MOD( INT( TPDAT(2) / 10 ), 10 ) - 1
         TPHEAD  = MOD( INT( TPDAT(2) / 100 ), 100 )
         TPINDX  = TPDAT(2) / 10000
C
      ELSE
C         CALL ERRLOG( 'TPPACK: What do you think you are doing? '//
C     1        MODE )
      END IF
C
      RETURN
      END
