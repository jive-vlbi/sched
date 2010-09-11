      SUBROUTINE ADDGEO( LASTISCN, ISCN, GEOOPT, KEEP )
C
C     This is the main routine for the automatic insertion of
C     geodetic segments, mainly for atmospheric calibration.
C
C     LASTISCN, as usual, is an array giving the scan number
C     of the last scan for each station.
C
C     ISCN is the output scan number.  The first scan is SCAN1.
C     Scans 1 to NSCANS are an input scans list.  If geodetic
C     segment insertions are being done, scans will be added to
C     that list, so SCAN1 will be NSCAN+1.  
C
C     GEOOPT is a variable set here (other than being initialized
C     in SCHOPT) that is a count of the number of additional scans
C     that need to be inserted before this geo segment is done.
C     Other parts of SCHOPT test it's value and basically keep 
C     handing control back to this routine until it is zero.  This
C     is very analagous to how the pointing scan insertion happens.
C
C     KEEP tells SCHOPT to keep the scan.  Just set it.
C
C     Addition of a geo segment is requested by giving a source 
C     name of "GEOSEG".  
C

      INCLUDE 'sched.inc'
C
      INTEGER            LASTISCN(*), ISCN, GEOOPT
      LOGICAL            KEEP
C
C     SEGSRCS are the source numbers in the geodetic source list of
C     sources we want to observe.  It is used rather than the main
C     catalog source numbers (GEOSRCI(SEGSRCS(I))) because getting 
C     the desired source name out of the source catalog is a bit
C     messy.
C
      INTEGER            SEGSRCS(MSEG)
      INTEGER            NSEG, JSCN, IOUT, NGOOD, ISTA
      LOGICAL            OKSTA(MAXSTA)
      LOGICAL            GSTASCN(MSEG,MAXSTA), SSTASCN(MAXSTA)
      DOUBLE PRECISION   TAPPROX, GSTARTJ(MSEG)
C
C     Want to keep some values between calls.
C
      SAVE               JSCN, SEGSRCS, NSEG, GSTASCN, GSTARTJ
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'ADDGEO starting.' )
C
C     Will only be in this routine if GEOLEN .GT. 0.D0 (starting new
C     geodetic segment, although later scans in the segment will also
C     have the original placeholder's scan's GEOLEN) or we are in the
C     middle of writing geodetic scans (GEOOPT .GE. 1 )
C
C     On initial scan requesting a segment:
C
      IF( GEOOPT .EQ. 0 ) THEN
C
C        Save the template scan number
C
         JSCN = GEOISCN(ISCN)
C
C        Pick the geo sources to use.
C
         CALL GEOMAKE( LASTISCN, JSCN, ISCN, SEGSRCS, NSEG, GSTASCN,
     1                 GSTARTJ )
C
         GEOOPT = NSEG
      END IF
C
C     Send out next scan of the segment.
C        Copy most scan parameters from the scan that had source "GEOSEG"
C        Insert the source.
C        Determine if source is up - set STASCN.
C        Determine scan timing with OPTTIM
C     Actually GEOOPT should never be 0 at this point, but test anyway.
C
      IF( GEOOPT .NE. 0 ) THEN
C
C        Get the index for this source in the SEGSRCS array.
C        This is ISEG in the routines that make the segments.
C
         IOUT = NSEG - GEOOPT + 1         
C
C        Make the next scan.
C        Copy all scan parameters from the template but get the source,
C        the antennas to use, and the start time from what came 
C        from MAKEGEO. 
C
         DO ISTA = 1, NSTA
            SSTASCN(ISTA) = GSTASCN(IOUT,ISTA)
         END DO
         TAPPROX = GSTARTJ(IOUT)
         CALL GMKSCN( LASTISCN, ISCN, JSCN, GEOSRCI(SEGSRCS(IOUT)),
     1        GEOSRC(SEGSRCS(IOUT)), TAPPROX, OPMINEL(JSCN), 0,
     2        NGOOD, OKSTA, SSTASCN, 'FORCE' )
C
C        Get the number of scans left to do and tell SCHED to keep 
C        this scan.
C
         GEOOPT = GEOOPT - 1
         KEEP = .TRUE.
      END IF
C
C     At end, GEOOPT will be zero on the last output scan.
C
      RETURN
      END
