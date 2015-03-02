      PROGRAM NEWSRC
C
C
C     This program may no longer be needed as of February 2015.  David Gordon 
C     of GSFC is supplying a catalog with aliases, including, where needed, (see 
C     egdelzn.key) the J2000 names with only 2 dec digits.  Petrov is resisting
C     adding aliases, but maybe that will just be the price of using his
C     catalog, which has a lot more sources.  Using unmodified catalogs will be
C     a lot less work for the mythical new SCHED maintainer.
C     Craig Walker  Feb. 21, 2015.
C
C
C
C     Read an old SCHED catalog.
C       As of Dec. 2014, this will generally be the alias file.
C     Read a new catalog, as from the geodesy community.
C     Replace coordinates and errors in the old catalog.
C     Add any new aliases.
C     Add any new sources.
C     Sort.
C
C
C     For sanity checks:
C        Look for duplicates by position.
C        Look for duplicate names.
C
C     Write a new SCHED catalog.

C     Partial history:
C       Added a SCHED source catalog format to the possible
C       inputs.  Nov. 2011  RCW.
C
C     If the input is a Petrov SCHED file, that file, as of
C     2011d, needs to have SRCCAT / at the start and ENDCAT
C     at the end removed.  It also needs FLUXREF added.
C     Some time before Dec. 2014, this is no longer necessary.
C     For KEYIN format files, copy all comments before the first line with "SOURCE".
C     to the output file.
C
C     Dec. 2014.  Adding new GSFCAS2 format
C     Also adding hardwired option to make alias file.
C     I expect not to use it often.  If needed, set DOALIAS=.TRUE.
C     and the output file will be heavily filtered.
C
C     Dec. 2014.  Trying to get GSFC to deliver keyin format files.  Got one in Feb 2015.
C
C     Getting the GSFC file made me look at the meaning or RAERR.  The manual 
C     says it is the angular error in mas.  To get the coordinate error, one would
C     need to divide by 15 to get time units and cos(dec) for the converging RA lines.
C     But past catalogs have considered it to be a coordinate error, apparently
C     in mas, not time msec.  Awkward.
C     Feb 2015:  Modify this program to make the handling of RAERR more explicit.
C     We are still debating which form to use, but for now, assume mas of coordinate error.
C     Convert all input files to that form, then at the end, convert to the desired type.
C     Then there is only one change if we decide to use something else.
C     Assume old catalogs are corrdinate mas, so no change needed.
C
C  The inputs to Newsrc are.  Items in [] are suggestions when using alias file.
C    1.  The old catalog.  Usually the pared down catalog with the aliases [sources.aliases]
C    2.  Y/N question whether to keep sources not in new solution.         [N]
C    3.  Which to keep if source in both files (NEW, BEST).                [NEW]
C    4.  Y/N question whether to keep fluxes not in the new solution.      [N]
C    5.  New input solution data file.
C    6.  New input solution file format (PETROV, GSFC, GSFCAST, SCHED)
C    7.  Output SCHED catalog file name.
C    8.  REMARK to be added to each new source, if not in input file.
C    9.  FLUXREF if there are fluxes but no associated FLUXREF


C
      INCLUDE 'rdcat.inc'
      INCLUDE 'newsrc.inc'
C
      INTEGER          MBIN
      PARAMETER        (MBIN=20)
      INTEGER          LEN1, IND(MSRC), I, IISRC, ISRC, ICAT
      INTEGER          J, J1, K, K1, MRA(MAL), MDEC(MAL)
      INTEGER          IO, IOJ, NOLD, NNEW, ICH, LIMATCH
      INTEGER          NIN, NOUT, NMATCH, NNOUT, NVLA, NJVAS, IB
      INTEGER          NRABIN(MBIN), NDECBIN(MBIN)
      REAL             RASORT(MSRC), VBIN(MBIN)
      DOUBLE PRECISION PI, SEP, RSCALE
      REAL             URAE, NURAE, UDECE, NUDECE, RSEP, DSEP
      REAL             SIGTOL, RSEPTOL, DSEPTOL
      REAL             RDIFE, DDIFE, RRMSSEP, DRMSSEP
      LOGICAL          GOTFLUX, MATCH(MSRC), UNIQUE
      LOGICAL          NAMEKEEP, DOALIAS
      LOGICAL          NEWBEST, OLDBEST, MATCHNAM, MATCHPOS, MATCHFLG
      logical          stripped
      CHARACTER        TEXT*132, LASTEQ*5
      CHARACTER        RAOUT*16, DECOUT*17, TFORM*17
      CHARACTER        OUTLINE*132, OUTNAME(MAL)*12
C  -------------------------------------------------------------------
C
C     Get PI (ie: 3.14...) the tricky way.
C
      PI = 4.0D0 * DATAN( 1.D0 )
      RADSEC = PI / ( 3600.D0 * 180.D0 )
C
C     Set the RMS separation bin boundaries.
C
      VBIN(1) = 0.0
      DO IB = 1, MBIN
         VBIN(IB) = ( IB - 1 ) * 0.5
         NRABIN(IB) = 0
         NDECBIN(IB) = 0
      END DO
C
C     Get the user input and open the files.  Copy comments.
C
      CALL GETUIN
C
C     Get the data from the old SCHED catalog.
C
      CALL GETOLD( NIN )
C     
C     Deal with the new incoming file.
C
C     First, read through the new catalog and fix any undesirable
C     characters (mostly in Petrov files).  
C
      CALL FIXFILE
C
C     Read the new catalog.  Check for duplicate names.
C
      CALL GETNEW
C
C     Get the tolerance for when to warn of matching names and not
C     matching positions.
C     SIGTOL sets the limit at 6 sigma.  It also converts from 
C     mas to radians.
C     With 6 sigma, there are a fairly large number of matches
C     by name but not position.  See later.
C
      SIGTOL = 6.0
      WRITE(*,*) ' '
      WRITE(*,*) 'Matching sources between catalogs. '
      WRITE(*,*) 
     1  '   Warnings issued for pairs more than 6 sigma apart.'
      WRITE(*,*) ' '
C
      SIGTOL = SIGTOL * RADSEC / 1000.0
C
C     Now we have all the data.  Try to match sources.
C     Loop over the new sources, looking for what to do relative
C     to the old sources.
C
      NADD = 0
      NMATCH = 0
      NREPLACE = 0

      DO ICAT = 1, NCAT
C
         INOLD(ICAT) = .FALSE.
         OLDBEST = .FALSE.
C
C        Find if this source matches any old catalog source.  If so, 
C        transfer the numbers.  Also flag as .......
C
C        Assume the source is the same
C        if a name matches or if the separation is small.  Use a casual 
C        separation calculation that will certainly not work for
C        large distances.  If CHOOSE is 'BEST', only take the new source
C        if both sources have error bars (ie always take the new one if
C        the old one does not have errors) and the geometric sum of the
C        errors is better.
C
C        Ugh - Petrov's catalog has some sources that are not the same,
C        but have the same 1950 name.  That is trouble on several fronts.
C
C        Play a bit of a game with indices to speed things up by starting
C        near the area of interest.
C
C        Note that the GSFC astro file usually has NEWNAME(1)=NEWNAME(2) 
C        which means that the IVS name and the B1950 name are the same.
C        From the way we collect names, the SCHED file will end up having
C        that name only once.
C
C        LIMATCH is the ISRC of the previous match.
C
         MATCH(ICAT) = .FALSE.
         LIMATCH = 0
         DO IISRC = 1, NSRC
            ISRC = 1 + MOD( IISRC + LIMATCH - 6 + NSRC, NSRC )
C
C           Do a crude position check, good to about 0.5 deg.  This
C           avoids examining sources that are clearly not matched.
C
            IF( ABS( RA(ISRC) - NEWRA(ICAT) ) .LT. 0.01 .AND. 
     1          ABS( DEC(ISRC) - NEWDEC(ICAT) ) .LT. 0.01 ) THEN
               MATCHNAM = .FALSE.
               MATCHPOS = .FALSE.
C
C              First check the number of aliases.
C
               NOLD = 0
               NNEW = 0
               DO J = 1, MAL
                  IF( NAME(J,ISRC) .NE. ' ' ) NOLD = J
                  IF( NEWNAME(J,ICAT) .NE. ' ' ) NNEW = J
               END DO
               DO I = 1, NNEW
                  DO J = 1, NOLD
                     IF( NEWNAME(I,ICAT) .EQ. NAME(J,ISRC) ) THEN
                        MATCHNAM = .TRUE.
                     END IF
                  END DO
               END DO
C
C              Check the separation.  Recall RA etc are in radians.
C              Do the check in arc sec.  There are now some sources
C              that appear to actually be distinct that really are
C              very close.  Check the separation using the 
C              claimed errors.
C
C              Get the separation tolerance.  If the error is 0, assume
C              is it 40 mas. (ok for JVAS etc).  
C
C              In practice, the RAERRs have been coordinate errors, but
C              in mas, not time seconds.  They need a cos(dec) correction
C              to be angle on the sky, or the comparisons need to not
C              have a cos(dec) correction.  This is all being considered
C              in Feb. 2015.  For now, keeping coordinate error in mas.
C              If filling fake errors, don't worry about RAERR vs Dec.
C
               URAE = RAERR(ISRC)
               IF( URAE .EQ. 0.0 ) URAE = 40.0
               UDECE = DECERR(ISRC)
               IF( UDECE .EQ. 0.0 ) UDECE = 40.0
               NURAE = NEWRAE(ICAT)
               IF( NURAE .EQ. 0.0 ) NURAE = 40.0
               NUDECE = NEWDECE(ICAT)
               IF( NUDECE .EQ. 0.0 ) NUDECE = 40.0
C
C              Get the tolerance.
C              SIGTOL sets the limit at 6 sigma.  It also converts from 
C              mas to radians.  See where it is set above.
C              With 6 sigma, there are a fairly large number of matches
C              by name but not position.  See later.

C              Note that the RA error is in the corrdinate angle, not
C              angle on the sky, the error numbers are high near the pole.
C              Dec. 2014.  Make a minimum tolerance for the separation of
C              5 mas so that especially tiny error bars don't preclude clear
C              matches.   
C              Here the coordinate difference is compared with the coordinate
C              errors, so we don't need any explicity cos(dec) adjustments.
C
               RDIFE = SQRT( URAE**2 + NURAE**2 )
               DDIFE = SQRT( UDECE**2 + NUDECE**2 )
               RSEPTOL = SIGTOL * MAX( RDIFE, 5.0 )
               DSEPTOL = SIGTOL * MAX( DDIFE, 5.0 )
C
C              Finally test for a match.  RSEP and DSEP are in radians.
C              Compare coordinate angle, not sky angle.
C              Do not scale by DCOS(DEC(ISRC))
C
               RSEP = ABS( RA(ISRC) - NEWRA(ICAT) )
               DSEP = ABS( DEC(ISRC) - NEWDEC(ICAT) )
               MATCHPOS = RSEP .LT. RSEPTOL .AND. DSEP .LT. DSEPTOL
C
C              Decide if the the pair of sources match. Until Dec. 2014,
C              we kept sources that matched either name or position.
C              That was changed to requiring both match as we go away
C              from using any catalogs except Petrov and GSFC.
C
               MATCH(ICAT) = MATCHNAM .AND. MATCHPOS
C
C              If there is a name match without a position match, complain.
C              This would have been the symptom with some misidentifications
C              I've seen in the past.  Actually maybe should flag both 
C              cases.
C
C              This flags a lot of cases, so only pay attention to the
C              ones that seem serious.
C              RSCALE converts from radians to milli-arcsec
C
               RSCALE = 1000.D0 / RADSEC
               MATCHFLG = MATCHPOS
               IF( .NOT. MATCHFLG ) THEN
                  IF( RAERR(ISRC) .EQ. 0.0 .AND. 
     1                RSEP * RSCALE .LT. 1000. .AND.
     2                DSEP * RSCALE .LT. 1000. ) MATCHFLG = .TRUE.
                  IF( RSEP * RSCALE .LT. 200. .AND. 
     2                DSEP * RSCALE .LT. 200. ) MATCHFLG = .TRUE.
               END IF
C
               IF( MATCH(ICAT) .AND. .NOT. ( MATCHNAM .AND. MATCHFLG ) )
     1              THEN
                  IF( MATCHNAM ) WRITE(*, '( 2A )' )
     1               'Names match, but not positions.  ',
     2               'Keep only the new information. '
                  IF( MATCHPOS ) WRITE(*, '( 2A )' )
     1               'Positions match, but not names. ',
     2               'Keep only the new information.'
                  WRITE(*, '( A, 8(1X, A ))' ) '   Old names: ',
     1               ( NAME(I,ISRC)(1:LEN1(NAME(I,ISRC))),I=1,8 ) 
                  WRITE(*, '( A, 8(1X, A ))' ) '   New names: ',
     1               ( NEWNAME(I,ICAT)(1:LEN1(NEWNAME(I,ISRC))),I=1,8 ) 
                  WRITE(*, '( A, 2F8.2, 3X, 2F8.2 )' ) 
     1              '   RA sep, RA tol, Dec sep, Dec tol         ', 
     2              RSEP * RSCALE, RSEPTOL * RSCALE, 
     3              DSEP * RSCALE, DSEPTOL * RSCALE
                  WRITE(*, '( A, 2F8.2, 3X, 2F8.2 )' ) 
     1              '   Errors: Old RA, New RA, Old Dec, New Dec ',
     2              RAERR(ISRC), NEWRAE(ICAT), 
     3              DECERR(ISRC), NEWDECE(ISRC)
                  WRITE(*,'(2A)') '   Old REMARK: ', 
     1                REMARK(ISRC)(1:LEN1(REMARK(ISRC)))
                  WRITE(*,'(2A)') '   New REMARK: ',
     1                  INREMARK(ICAT)(1:LEN1(INREMARK(ICAT)))
               END IF
C
C              Accumulate some statistics for matching sources.
C              Get the RA and Dec differences as multiples of the RMS's.
C
               IF( MATCH(ICAT) ) THEN
                  RRMSSEP = RSEP * RSCALE / RDIFE
                  DRMSSEP = DSEP * RSCALE / DDIFE
                  DO IB = 1, MBIN - 1
                     IF( RRMSSEP .GE. VBIN(IB) .AND. 
     1                   RRMSSEP .LT. VBIN(IB+1) )
     2                   NRABIN(IB) = NRABIN(IB) + 1
                     IF( DRMSSEP .GE. VBIN(IB) .AND. 
     1                   DRMSSEP .LT. VBIN(IB+1) )
     2                   NDECBIN(IB) = NDECBIN(IB) + 1
                  END DO
               END IF
C
            END IF
C
C           If there is a match, grab any aliases regardless of
C           which will be kept.  Make the first output names match
C           the new catalog, then save any from the old one that are unique.
C           This is best done
C
            IF( MATCH(ICAT) ) THEN            
               LIMATCH = ISRC
               NMATCH = NMATCH + 1
C
C              First check the number of names.
C
               NOLD = 0
               NNEW = 0
               DO J = 1, MAL
                  IF( NAME(J,ISRC) .NE. ' ' ) NOLD = J
                  IF( NEWNAME(J,ICAT) .NE. ' ' ) NNEW = J
               END DO
C
C              First transfer the new names to the output.  Don't
C              allow duplicates (will happen with GSFC files).
C              The new ones are likely to be a bit better controlled 
C              than the old ones.
C
               NNOUT = 1
               OUTNAME(1) = NEWNAME(1,ICAT)
               IF( NNEW .GE. 2 ) THEN
                  DO K = 2, NNEW
                     UNIQUE = .TRUE.
                     DO J = 1, NNOUT
                        IF( NEWNAME(K,ICAT) .EQ. OUTNAME(J) )
     1                      UNIQUE = .FALSE.
                     END DO
                     IF( UNIQUE .AND. NEWNAME(K,ICAT) .NE. ' ' ) THEN
                        NNOUT = NNOUT + 1
                        IF( NNOUT .GT. MAL ) WRITE(*, '( A, A )' )
     1                     'Too many names for ', OUTNAME(1)
                        OUTNAME(NNOUT) = NEWNAME(K,ICAT)
                     END IF
                  END DO
               END IF
C
C              Now gather any more unique names from the old catalog.
C
               DO K = 1, NOLD
                  UNIQUE = .TRUE.
                  DO J = 1, NNOUT
                     IF( NAME(K,ISRC) .EQ. OUTNAME(J) )
     1                   UNIQUE = .FALSE.
                  END DO
                  IF( UNIQUE .AND. NAME(K,ISRC) .NE. ' ' ) THEN
                     NNOUT = NNOUT + 1
                     IF( NNOUT .GT. MAL ) WRITE(*, '( A, A )' )
     1                  'Too many names for ', OUTNAME(1)
                     OUTNAME(NNOUT) = NAME(K,ISRC)
                  END IF
               END DO
C
C              Replace the old catalog NAME list with OUTNAME.
C              Clear any unused elements.
C
               DO K = 1, NNOUT
                  NAME(K,ISRC) = OUTNAME(K)
               END DO                   
               IF( NOLD .GT. NNOUT ) THEN
                  DO K = NNOUT + 1, NOLD
                     NAME(K,ISRC) = ' '
                  END DO
               END IF
C
C              Now pick which source to keep if there is a MATCH, plus
C              choose based on CHOOSE.  Assume anything with no error
C              listed is worse than the new solution.  This is almost
C              certainly true as the ones without errors are JVAS etc.
C
C              March 9, 2014.  Found the sense of the error comparison
C              was backwards.  Earlier found the "**2" squaring was 
C              missing on the DECERR.  I suspect the wrong sources
C              were picked in 2012 in many cases.  Ugh.
C
               NEWBEST = RAERR(ISRC) .LE. 0.0 .OR. DECERR(ISRC) .LE. 0.0
               NEWBEST = NEWBEST .OR. 
     1               ( ( NEWRAE(ICAT)**2 + NEWDECE(ICAT)**2 ) .LE. 
     2               ( RAERR(ISRC)**2 + DECERR(ISRC)**2 ) )
               IF( CHOOSE .EQ. 'NEW' .OR. 
     1             CHOOSE .EQ. 'BEST' .AND. NEWBEST ) THEN            
C
C                 Complain if this is not the first match for this old 
C                 source.
C
                  IF( ISNEW(ISRC) .NE. 0 ) THEN
                     WRITE(*, '( A, I6, 1X, A, A, L1, 1X, L1, A )' )
     1                 'Second match found for old source ', ISRC,
     2                 NAME(1,ISRC), ' Matched name, position: ',
     3                 MATCHNAM, MATCHPOS, '.'
                     WRITE(*, '( A, I6, 1X, A )' )
     1                 '  New source: ', ICAT, NEWNAME(1,ICAT) 
                     WRITE(*, '( A, I6, 1X, A )' ) '  Previous match: ',
     1                   ISNEW(ISRC), NEWNAME(1,ISNEW(ISRC)) 
                  END IF
                  INOLD(ICAT) = .TRUE.
                  ISNEW(ISRC) = ICAT
                  NREPLACE = NREPLACE + 1
                  RA(ISRC) = NEWRA(ICAT)
                  DEC(ISRC) = NEWDEC(ICAT)
                  RAERR(ISRC) = NEWRAE(ICAT)
                  DECERR(ISRC) = NEWDECE(ICAT)
                  REMARK(ISRC) = INREMARK(ICAT)
                  EQUINOX(ISRC) = INEQUIN(ICAT)
C	 	 
C                 Deal with the flux information, if there is any.
C                 Don't trust IFLXREF to tell what is happening.
C                 IF IFLX(1,ICAT) is not zero, assume that the new 
C                 catalog has flux densities.
C
C                 Note that this is an all or nothing scheme for
C                 now - it doesn't combine old and new fluxes.  
C                 Do something about band by band in the future.
C
C                 Follow user instructions about whether to keep
C                 old fluxes when new ones are not provided.
C	 	 
                  IF( IFLX(1,ICAT) .NE. 0.0 ) THEN
                     FLUXREF(ISRC) = IFLXREF(ICAT)
                     DO K = 1, MFLX
                        FLUX(K,ISRC) = IFLX(K,ICAT)
                     END DO
                  ELSE IF( .NOT. KEEPFLUX ) THEN
                     FLUXREF(ISRC) = ' '
                     DO K = 1, MFLX
                        FLUX(K,ISRC) = 0.0
                     END DO
                  END IF
               ELSE
                  OLDBEST = .TRUE.
               END IF
C
C              Since we have a match, don't go through the rest
C              of the old sources.
C
               GO TO 250
            END IF
         END DO
  250    CONTINUE
C
C        Finished trying to match new to old sources.
C
      END DO
C
C     Now loop through the new sources and add any that did not
C     match old sources to the end of the old source list.  MATCH
C     was set regardless of whether the new one replaced the old
C     one or not so it is the appropriate flag.
C
      DO ICAT = 1, NCAT
         IF( .NOT. MATCH(ICAT) ) THEN
C
C           Increment the count of the total number of sources.
C
            NSRC = NSRC + 1
            IF( NSRC .GT. MSRC ) THEN
               WRITE(*,*) 'Need to allow more sources to fit new ones.'
               STOP
            END IF
C
C           Count the added sources.
C
            NADD = NADD + 1
            DO I = 1, MAL
              IF( NEWNAME(I,ICAT) .NE. ' ' ) THEN
                 NAME(I,NSRC) = NEWNAME(I,ICAT)
              ELSE
                 NAME(I,NSRC) = ' '
              END IF
            END DO
            RA(NSRC) = NEWRA(ICAT)
            DEC(NSRC) = NEWDEC(ICAT)
            RAERR(NSRC) = NEWRAE(ICAT)
            DECERR(NSRC) = NEWDECE(ICAT)
            REMARK(NSRC) = INREMARK(ICAT)
            IF( INEQUIN(ICAT) .EQ. ' ' ) THEN
               EQUINOX(NSRC) = 'J2000'
            ELSE
               EQUINOX(NSRC) = INEQUIN(ICAT)
            END IF
C
C           None of the files have a calcode.  Set it for
C           the replaced sources.
C
            CALCODE(NSRC) = 'V'
            FLUXREF(NSRC) = IFLXREF(ICAT)
            DO K = 1, MFLX
               FLUX(K,NSRC) = IFLX(K,ICAT)
            END DO
C
C           Mark this source as being from the new catalog.
C
            ISNEW(NSRC) = ICAT
C
         END IF
C
      END DO
C
C     Talk to the user.
C
      WRITE(*,*) 'Finished building the full combined source list. ', 
     1    NSRC, ' sources.'
C
C -----------------------
C     Sort the resulting file.  Rather than building a new set of
C     sorted arrays, just write out the new SCHED file based on the
C     sorted indices.
C
      DO ISRC = 1, NSRC
         RASORT(ISRC) = RA(ISRC)
         IND(ISRC) = ISRC
      END DO
      CALL SORTCW( NSRC, RASORT, IND )
      WRITE(*,*) 'Finished sorting new data.'
      LASTEQ = '-----'
C
C     ========= Now write the output file. ================
C
      NOUT = 0
      NVLA = 0
      NJVAS = 0
      DO ISRC = 1, NSRC
         IO = IND(ISRC)
C
C        For special code version that makes the alias file, detect
C        whether to keep the source.  Only sources with aliases not
C        in the "new" catalog, and meeting other criteria, will be kept.
C        Most sources will be dropped here because there are no aliases 
C        that need to be maintined.  Usually DOALIAS should be false 
C        when not creating the alias file.
C
         NAMEKEEP = .TRUE.
C         DOALIAS = .TRUE.      
         DOALIAS = .FALSE.
         IF( DOALIAS ) THEN
C
C           Get the number of names for the source.
C        
            NNEW = 0
            DO J = 1, MAL
               IF( NAME(J,IO) .NE. ' ' ) NNEW = J
            END DO
C
C           Some reasons not to keep the source.
C              Only 1 or 2 names - aliases not needed.
C              VLA or JVAS source.
C              Not in latest rfc or GSFC catalog.
C
            IF( NNEW .LE. 2 ) NAMEKEEP = .FALSE.
            IF( INDEX( REMARK(IO), 'VLA' ) .NE. 0 ) NAMEKEEP = .FALSE.
            IF( INDEX( REMARK(IO), 'JVAS' ) .NE. 0 ) NAMEKEEP = .FALSE.
            IF( .NOT. ( INDEX( REMARK(IO), 'GSFC 2011B' ) .NE. 0 .OR.
     1           INDEX( REMARK(IO), 'rfc_2014d' ) .NE. 0 ) )
     2           NAMEKEEP = .FALSE.
C
C           Don't keep the source if the J name ends in
C           a letter.  This indicates there are close pairs and 
C           aliases might not be reliable.
C
            DO J = 1, NNEW
               K = LEN1( NAME(J,IO) )
               IF( NAME(J,IO)(1:1) .EQ. 'J' .AND. (
     1             NAME(J,IO)(K:K) .EQ. 'A' .OR. 
     2             NAME(J,IO)(K:K) .EQ. 'B' .OR. 
     3             NAME(J,IO)(K:K) .EQ. 'C' .OR. 
     4             NAME(J,IO)(K:K) .EQ. 'D' ) )
     5             NAMEKEEP = .FALSE.
            END DO
C
C           Don't keep 2 J names that differ only in the last digit of
C           one of the coordinates.  Don't do this for B1950 names because
C           the proper B1950 name and the IVS name may differ.  Don't 
C           worry about the Dec sign.
C
            stripped = .false.
C            write(*, '(A, 2I5, L2, 2X, 8(1X,A12) )') 
C     1           '=== Testing', io, isnew(io),
C     2           namekeep, (name(k,io),k=1,nnew)
C
            DO J = 1, NNEW
               K = LEN1( NAME(J,IO) )
               IF( NAME(J,IO)(1:1) .EQ. 'J' .AND. 
      1            LEN1( NAME(J,IO) ) .EQ. 10 ) THEN
                  READ( NAME(J,IO)(2:5), '( I4 )', ERR = 100 ) MRA(J) 
                  READ( NAME(J,IO)(7:10), '( I4 )', ERR = 100 ) MDEC(J) 
               ELSE
                  MRA(J) = -999
                  MDEC(J) = -999
               END IF
               GO TO 101
  100          CONTINUE
               MRA(J) = -999
               MDEC(J) = -999
  101          CONTINUE             
               stripped = .false.
               IF( J .GT. 2 ) THEN
                  DO K = 1, J - 1
                     IF( ABS( MRA(K) - MRA(J) ) .LE. 1 .AND. 
     1                   ABS( MDEC(K) - MDEC(J) ) .LE. 1 .AND.
     2                   MRA(K) .NE. -999 .AND. MRA(J) .NE. -999 .AND.
     3                   MDEC(K) .NE. -999 .AND. MDEC(K) .NE. -999) THEN
C                        write(*,*) 'Removing ', name(j,io)
                        stripped = .true.
                        NAME(J,IO) =  ' '
                     END IF
                  END DO
               END IF
            END DO            
C
C           Left shift the array over the blanks.  Get new NNEW
C
            K = 0
            DO J = 1, NNEW
               IF( NAME(J,IO) .NE. ' ' ) THEN
                  K = K + 1
                  NAME(K,IO) = NAME(J,IO)
               END IF
            END DO
            IF( K .LT. NNEW ) THEN
               DO J = K+1, NNEW
                  NAME(J,IO) = ' '
               END DO
            END IF
            NNEW = K
            IF( NNEW .LE. 2 ) NAMEKEEP = .FALSE.
C
C
C            if( stripped ) then
C               write(*, '( A, 6X, L2, 2X, 8(1X,A12) )') 
C     1          ' stripped down:', namekeep, (name(k,io),k=1,nnew)
C            end if
C
C           Now write a list of cases where there are two names that
C           differ only in the last character.  Objects of hand edit?
C           Warning, according to the 2011 GSFC file, the B1950 name and
C           the IVS name (used in calc-solve) in the same format can differ
C           So those cases should not be eliminated.
C
            IF( ( ISNEW(IO) .NE. 0 .OR. KEEPOLD ) .AND. NAMEKEEP ) THEN
               DO J = 2, NNEW
                  K = LEN1( NAME(J,IO) )
                  DO J1 = 1, J - 1
                     K1 = LEN1( NAME(J1,IO) )
                     IF( K .EQ. K1 .AND. 
     1                   NAME(J,IO)(1:K-1) .EQ. NAME(J1,IO)(1:K1-1) ) 
     2                   THEN
                        WRITE(*,'(A,1X, 2I5,8(1X,A8))') 
     1                    'Very similar names ', IO, ISRC, 
     2                    (NAME(I,IO), I=1,NNEW)
                     END IF
                  END DO
               END DO
            END IF
C
C
         END IF      
C
C        Now write the new source if wanted.
C
         TEXT = ' '
         IF( ( ISNEW(IO) .NE. 0 .OR. KEEPOLD ) .AND. NAMEKEEP ) THEN
C            write(*,*) ' writing source ', io
            NOUT = NOUT + 1
            IF( EQUINOX(IO) .NE. LASTEQ ) THEN
               WRITE( 12, '( A, A )' ) 'EQUINOX = ', EQUINOX(IO)
               LASTEQ = EQUINOX(IO)
            END IF
            WRITE( TEXT, '( A )' ) 'SOURCE='
            DO I = 1, MAL
               IF( NAME(I,IO) .NE. ' ' ) THEN
                  IF( I .EQ. 6 ) THEN
                     K = LEN1(TEXT)
                     WRITE( 12, '(A)' ) TEXT(1:K)
                     TEXT = ' '
      write(*,*) 'Source with more than 5 aliases: ', name(1,IO)
                  END IF
                  J = LEN1( TEXT ) + 1
                  IF( J .EQ. 1 ) J = 8
                  K = J + LEN1(NAME(I,IO)) + 2
                  TEXT(J:K) = ''''//NAME(I,IO)(1:LEN1(NAME(I,IO)))//
     1                 ''','
               END IF
            END DO
            K = LEN1(TEXT) - 1
            WRITE( 12, '(A)' ) TEXT(1:K)
            RAOUT = TFORM( RA(IO), 'T', 0, 2, 10, ':: ' )
            DECOUT = TFORM( DEC(IO), 'D', 1, 2, 9, ':: ' )
            WRITE( 12, '( 5A, F8.3, A, F8.3, 3A )' )
     1          '     RA=', RAOUT(1:LEN1(RAOUT)), 
     2          ' DEC= ', DECOUT(1:LEN1(DECOUT)),
     3          ' RAERR=', RAERR(IO), ' DECERR=', DECERR(IO),
     4          ' CALCODE=''', CALCODE(IO), ''' '
            WRITE( 12, '( A, A, A )' )
     1          '     REMARKS=''', REMARK(IO)(1:LEN1(REMARK(IO))), ''''
            GOTFLUX = .FALSE.
            DO I = 1, MFLX-2, 3
               IF( FLUX(I+1,IO) .GT. -9.0 .AND. 
     1             FLUX(I+1,IO) .NE. 0.0 .AND.
     2             FLUX(I+2,IO) .GT. -9.0 .AND.
     3             FLUX(I+2,IO) .NE. 0.0 ) GOTFLUX = .TRUE.
            END DO
            IF( GOTFLUX ) THEN
               WRITE( OUTLINE, '( A )' ) '     FLUX = '
               DO I = 1, MFLX-2, 3
                  IF( ( FLUX(I+1,IO) .GT. -9. .AND. 
     1                  FLUX(I+1,IO) .NE. 0.0 ) .OR.
     2                ( FLUX(I+2,IO) .GT. -9. .AND.
     3                  FLUX(I+2,IO) .NE. 0.0 ) ) THEN
                     ICH = LEN1( OUTLINE )
                     IF( OUTLINE(ICH:ICH) .NE. '=' ) THEN
                        ICH = ICH + 1
                        OUTLINE(ICH:ICH) = ','
                     END IF
                     IF( ICH .GT. 85 ) THEN
                        WRITE( 12, '( A )' ) OUTLINE(1:ICH+1)
                        OUTLINE = '      '
                        ICH = 6
                     END IF
                     WRITE( OUTLINE(ICH+1:ICH+27), 
     1                   '( F7.2, A, F6.2, A, F6.2 )' )
     2                   FLUX(I,IO), ',', FLUX(I+1,IO), 
     3                   ',', FLUX(I+2,IO)
                  END IF         
               END DO
               ICH = LEN1( OUTLINE )
               IF( ICH .LT. 70 ) THEN
                  OUTLINE = OUTLINE(1:ICH) // 
     1               '    FLUXREF = ''' // 
     2               FLUXREF(IO)(1:LEN1(FLUXREF(IO))) // ''''
                  WRITE( 12, '( A )' ) OUTLINE(1:LEN1(OUTLINE))
                  OUTLINE = ' '
               ELSE
                  WRITE( 12, '( A )' ) OUTLINE(1:ICH)
                  OUTLINE = '      '
                  WRITE( 12, '( A, A, A )' ) '     FLUXREF = ''', 
     1                  FLUXREF(IO)(1:LEN1(FLUXREF(IO))) // ''''
               END IF
            END IF
C
C           Get the number of VLA and JVAS sources in the output.
C
            IF( INDEX( REMARK(IO), 'VLA' ) .NE. 0 ) NVLA = NVLA + 1
            IF( INDEX( REMARK(IO), 'JVAS' ) .NE. 0 ) NJVAS = NJVAS+ 1
            WRITE( 12, '( A )' ) ' /'
         END IF
      END DO
C
C     Summary of sources processed.
C
      WRITE(*,*) ' '
      WRITE(*, '( A )' )  '    SUMMARY '
      WRITE(*, '( I6, A, A )' ) NIN, ' sources read from old file: ',
     1      SRCFILE(1:LEN1(SRCFILE))
      WRITE(*, '( I6, A, A )' ) 
     1      NCAT, ' sources read from new file: ', 
     2      GEOFILE(1:LEN1(GEOFILE))
      WRITE(*, '( I6, A )' ) NSRC, ' distinct sources processed.'
      WRITE(*, '( I6, A, A )' ) 
     1      NOUT, ' sources written to output file: ', 
     2      NEWFILE(1:LEN1(NEWFILE))
      WRITE(*, '( I6, A, A )') NMATCH, ' new sources matched sources ',
     1     'in the old catalog.'
      WRITE(*, '( I6, A, A )') NREPLACE, ' new sources replaced ',
     1     'sources from the old catalog.'
      WRITE(*, '( I6, A )' ) NADD, ' new sources added.'
      WRITE(*, '( I6, A )' ) NOUT - NMATCH - NADD, 
     1     ' sources from the old catalog kept.'
      IF( DOALIAS ) THEN
         WRITE(*,*) '   Above number is likely to be wrong when '//
     1       'making an alias file.'
      END IF
      WRITE(*, '( 5X, I6, A )' ) NVLA, ' VLA calibrator list sources'//
      1      ' included.  May not be good for VLBI'
      WRITE(*, '( 5X, I6, A )' ) NJVAS, ' JVAS survey sources'//
      1      ' included.  May not be good for VLBI'
C
C     Do a duplicate source check - somewhat looser than the one
C     done earlier for matching sources.  Do only for the output 
C     sources.
C
      WRITE(*,*) ' '
      WRITE(*,*) 'Possible duplicate sources with separation in arcsec.'
      WRITE(*,*) '  Full merged catalog tested.  May include sources '//
     1           'not in the output catalog.'
      DO ISRC = 1, NSRC - 1
         IO = IND(ISRC)
         DO J = ISRC + 1, MIN( ISRC + 50, NSRC )
            IOJ = IND(J)
            IF( ( ISNEW(IO) .NE. 0 .AND. ISNEW(IOJ) .NE. 0 ) 
     1            .OR. KEEPOLD ) THEN
               SEP = DSQRT( (( RA(IO) - RA(IOJ) ) * DCOS(DEC(IO)) )**2
     1               + ( DEC(IO) - DEC(IOJ) ) ** 2 )
               SEP = SEP / RADSEC
               IF( SEP .LT. 5.D0 ) THEN
                  WRITE( *, '( 3A, F9.4, 2F20.15 )' )
     1              NAME(1,IO), '   ', NAME(1,IOJ), SEP
                  WRITE( *, '( 12X, A, I5, 2A )' ) NAME(1,IO), IO,
     1              '  ', REMARK(IO)(1:LEN1(REMARK(IO)))
                  WRITE( *, '( 12X, A, I5, 2A )' ) NAME(1,IOJ), IOJ, 
     1              '  ', REMARK(IOJ)(1:LEN1(REMARK(IOJ)))
               END IF
            END IF
         END DO
      END DO
C
C     Give the matching source statistics.
C
      WRITE(*,*) ' '
      WRITE(*,'( A )') 
     1    ' Histogram of separations in sigma for matching sources'
      WRITE(*,'( A )') 
     1    '  Bin range # RA  # Dec '
      DO IB = 1, MBIN - 1
         WRITE( *, '( 2F6.1, 2I6 )' )
     1      VBIN(IB), VBIN(IB+1), NRABIN(IB), NDECBIN(IB)
      END DO
C
C
      WRITE(*,*) ' '
      WRITE(*,*) '  Consider making flux system more sophisticated by'
      WRITE(*,*) '  keeping old fluxes at frequencies not in the input'
      WRITE(*,*) '  file.'
C
      STOP
      END
