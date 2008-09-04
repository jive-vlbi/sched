      SUBROUTINE TPAUTO( ISCN, ISTA, FIRSTS, IPASS, 
     1       TPMODMIN, SCNFOOT, TPCFOOT, TPCDIR ) 
C
C     Obsolete - only called if tapes in use.
C
C
C     Routine for SCHED called by TPSCH to deal with automatic tape
C     handling
C
C     If AUTOTAPE=1, the VLBA stations will have AUTOALLOCATE set and
C     the on-line system will determine where on the tape to put data.
C     However if a scan does not fit going in the current direction,
C     the tape will stop when it reaches the low tape sense.  If the
C     on-line system knows that it is within 450 feet of the end of
C     tape, it will start the scan in the opposite direction.  This
C     only happens after the first forward pass.  For that pass, it
C     runs to the low tape sense to determine the tape length.
C     
C     If AUTOTAPE=2, AUTOALLOCATE and AUTOREVERSE will be set.  Again,
C     the on-line system determines where the data are put.  In this
C     case, if the tape runs out before the end of scan, the tape will
C     be reversed, costing a few seconds of data.  If either option is
C     used, it is probably best to use AUTOTAPE=2 to avoid lost data.
C     In these cases, SCHED cannot predict tape usage precisely.  Note
C     that, as for AUTOTAPE=1, after the first pass, scans starting
C     less than 450 feed from the end will start in the other
C     direction.
C
C     This routine will try to second guess the automatic tape handling.
C     All tape footages, directions etc should be considered to be 
C     approximate.  The TAPEINI parameters can be used if the user has
C     some idea where the tape will be at the start of the experiment.
C     The routine is a lot like TPSCH, except that commands like
C     TAPE, REWIND, FASTFOR, and REVERSE will be ignored and the 
C     criteria for what to do near turnaround time are different.
C     
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      REAL               TPCLEN, SCNFOOT, TPCFOOT, RPASSES, FPASS
      REAL               TPTOL, TPADD, TOTFOOT(MAXSTA), STFOOT(MAXSTA)
      INTEGER            ISCN, ISTA, NPASST, LIPASS(MAXSTA)
      INTEGER            NTAPES, NDRIVES, TPMODMIN(MAXSTA)
      LOGICAL            FIRSTS, WARNLONG
      SAVE               TOTFOOT, WARNLONG, LIPASS, STFOOT
C
C     Items that get packed into the TPDAT array.
C
      INTEGER          TPCDIR, TPCINDX, TPCHEAD, IPASS, TPCDRIV
      LOGICAL          DOTAPE
C
      DATA             WARNLONG / .TRUE. /
C  --------------------------------------------------------------------
      IF( DEBUG .AND. FIRSTS ) CALL WLOG( 0, 'TPAUTO: Station start: '
     1     // STATION(STANUM(ISTA)) )
C
C     Get the tape length and the number of passes per tape.
C
      TPCLEN = TPLENG(ISTA)
      NPASST = NHDPOS(ISTA) * TPMODMIN(ISTA)
C
C     Just allow a bit of slop so when at the very end of the tape,
C     the pass number etc will not be incremented.
C
      TPTOL  = 25
C
C     Moan a bit about a scan longer than a tape.
C
      IF( SCNFOOT .GT. TPCLEN .AND. WARNLONG ) THEN
         CALL WLOG( 1, 'TPAUTO:  * You have specified a scan longer '//
     1       'than a tape pass.  This will work' )
         CALL WLOG( 1, '           with auto allocation ' //
     2       'at VLBA sites, but is not necessarily a good idea.' )
         WARNLONG = .FALSE.
      END IF
C
C     Get where we start on the tape.  TOTFOOT keeps track of the
C     total footage along the tape, including what was used by a
C     known previous user (set by the TPINI stuff).
C     STFOOT is the start value of TOTFOOT.
C
      IF( FIRSTS ) THEN
         STFOOT(ISTA)  = ( IPASS - 1 ) * TPCLEN
         TOTFOOT(ISTA) = STFOOT(ISTA)
         LIPASS(ISTA)  = IPASS

      END IF
C     
C     Deal with reversals with automatic tape handling.  First look
C     for cases where the tape will turn around at the start of
C     the scan.  TPADD is the amount of footage wasted by not 
C     reaching the end.  TPCDIR here is the direction at the
C     start of the scan.
C     
      TPADD = 0.0
      IF( TPCDIR .EQ. 1 .AND. TPCLEN - TPCFOOT .LT. 450 ) THEN
         TPADD = ( TPCLEN - TPCFOOT ) * 2.0
         TPCDIR = -1
      ELSE IF( TPCDIR .EQ. -1 .AND. TPCFOOT .LT. 450 ) THEN
         TPADD = TPCFOOT * 2.0
         TPCDIR = 1
      END IF
C
C     Now add the footage for the scan.  For autoreverse, it is easy -
C     the tape keeps going back and forth until the scan is over.
C     If AUTOREV is not set, the scan only goes to the end of tape
C     and stops.
C
      IF( AUTOREV(ISTA) ) THEN
         TOTFOOT(ISTA) = TOTFOOT(ISTA) + SCNFOOT + TPADD
      ELSE
         IF( TPCDIR .EQ. 1 ) THEN
            TOTFOOT(ISTA) = TOTFOOT(ISTA) + 
     1             MIN( SCNFOOT + TPADD, TPCLEN - TPCFOOT )
         ELSE
            TOTFOOT(ISTA) = TOTFOOT(ISTA) + 
     1             MIN( SCNFOOT + TPADD, TPCFOOT )
         END IF
      END IF
C
C     Now TOTFOOT tells where we are in total tape usage.  From that
C     we can deduce what the pass, direction, footage etc is at the
C     end of the scan.  I'm not counting turnarounds because a
C     scan is now allowed to be several passes.
C
      TPFOOT1(ISCN,ISTA) = TPCFOOT
      IPASS = 1 + MOD( INT( (TOTFOOT(ISTA)-TPTOL) / TPCLEN ), NPASST )
      DOTAPE = IPASS .LT. LIPASS(ISTA)
      LIPASS(ISTA) = IPASS
C
C     Get the tape direction.
C
      IF( MOD( IPASS, 2 ) .EQ. 0 ) THEN
         TPCDIR = -1
      ELSE
         TPCDIR = 1
      END IF
C
C     Get the tape drive number.  The TPTOL allows the tape to be at
C     then end without incrementing the drive number.
C
      NTAPES = INT( (TOTFOOT(ISTA) - TPTOL ) / ( TPCLEN * NPASST ) )
      NDRIVES = STNDRIV(STANUM(ISTA))
      IF( TWOHEAD ) NDRIVES = MAX( 1, NDRIVES / 2 )
      TPCDRIV = 1 + MOD( NTAPES, NDRIVES )
C
C     Get the ending tape footage.
C
      TPFOOT2(ISCN,ISTA) = MOD( TOTFOOT(ISTA) - TPTOL, TPCLEN ) + TPTOL
      IF( TPCDIR .EQ. -1 ) THEN
         TPFOOT2(ISCN,ISTA) = TPCLEN - TPFOOT2(ISCN,ISTA)
      END IF
C
C     Keep track of the number of tapes and passes used for the summary.
C     These are different from NTAPES above because they don't have
C     the possible offset for a midtape start.
C     Is NTAPES used for anything?  It is local to this routine.  The earlier
C     version is used for getting TPCDRIV.
C     TAPES is used in the summary.  Keep from calling barely over
C     N tapes N+1.
C     If using a two tape mode, double the tape count.
C
C     Jump through some hoops so that, eg,  13.5 passes counts as 14
C     and so does 14.0.
C     
      EXPTAPE(ISTA) = ( TOTFOOT(ISTA) - STFOOT(ISTA) - TPTOL ) /
     1        ( TPCLEN * NPASST  )
      RPASSES = (TOTFOOT(ISTA)-STFOOT(ISTA)-TPTOL ) / TPCLEN 
      PASSES(ISTA) = INT( RPASSES )
      FPASS = RPASSES - PASSES(ISTA)
      IF( FPASS .GT. 0.02 ) PASSES(ISTA) = PASSES(ISTA) + 1
      NTAPES = INT( (TOTFOOT(ISTA) - STFOOT(ISTA) - TPTOL ) / 
     1         ( TPCLEN * NPASST ) )
      TAPES(ISTA) = INT( EXPTAPE(ISTA) - 0.01 + 1.0 )
      IF( TWOHEAD .AND. STNDRIV(STANUM(ISTA)) .GE. 2 ) THEN
         TAPES(ISTA) = 2 * TAPES(ISTA)
      END IF
C     
C     Get head index position and group for the end of the scan.
C     
      CALL INDXHEAD( TPMODMIN(ISTA), IPASS, HEADMODE(ISTA), TPCINDX, 
     1               TPCHEAD, SETNAME(LS) )
C     
C     Fill the TPDAT array.
C     
      CALL TPPACK( 'PACK', TPDAT(1,ISCN,ISTA),
     1          DOTAPE, .FALSE., .FALSE., IPASS, TPCDRIV, TPCDIR,
     2          TPCINDX, TPCHEAD )
C
      RETURN
      END







