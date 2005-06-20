      SUBROUTINE SRINSERT( JSCN, KSRC, SRNAME, 
     1                     STARTK, STOPK, LASTISCN, MSCN )
C
C     Routine for SCHED, used by optimization related routines such as
C     ADDPEAK, that takes a template scan and changes the times and 
C     source.  It also gets the geometry and slew parameters for
C     two scans, the one being modified (JSCN), and a following one
C     for which the slew to times are of interest in ADDPEAK.
C
C     KSRC is the source catalog list entry for the new source.
C     STARTK and STOPK are the new start and stop times.
C
C     The routine also does the geometry, including slew, calculations
C     for the new scan and for scan MSCN which is assumed to follow.
C     This helps determine if there is adequate time for insertion
C     of scans like peaking scans and also helps determine which
C     possible source to use for an inserted scan.
C
C     STASCN is assumed to have been determined externally.
C
      INCLUDE       'sched.inc'
      INCLUDE       'schpeak.inc'
C
      INTEGER            KSRC, JSCN, MSCN, LASTISCN(MAXSTA)
      INTEGER            LASTKSCN(MAXSTA), ISTA, NGOOD
      DOUBLE PRECISION   STARTK, STOPK
      CHARACTER          SRNAME*(*)
C ---------------------------------------------------------------------
      IF( DEBUG .AND. MSCN .LT. 5 ) CALL WLOG( 0, 'SRINSERT starting.' )
C
C     Put in the new source.  Also reset a variety of parameters that
C     probably don't have appropriate settings.  Note that some 
C     items like SETNUM and NSETUP will be added by MAKEPTG for pointing
C     scans.  For here, they will be left as for the original.
C     Treat the scan times as not adjustable (DURONLY=6 will do it)
C

      PRESCAN(JSCN) = 0.D0
      DUR(JSCN)     = STOPK - STARTK
      GAP(JSCN)     = 0.D0
      STARTJ(JSCN)  = STARTK
      STOPJ(JSCN)   = STOPK
      SRCNUM(JSCN)  = KSRC
      IDOPSRC(JSCN) = KSRC
      QUAL(JSCN)    = 0
      DURONLY(JSCN) = 6
      DWELL(JSCN)   = .FALSE.
      ANNOT(JSCN)   = 'Scan added by Sched for reference pointing'
      SCNSRC(JSCN)  = SRNAME
      DOPSRC(JSCN)  = SCNSRC(JSCN)
      VLAPHS(JSCN)  = SCNSRC(JSCN)
      IVLAPHS(JSCN) = SRCNUM(JSCN)
C
C     Get the last scans to use for the geometry calculations.
C
      DO ISTA = 1, NSTA
         IF( STASCN(JSCN,ISTA) ) THEN
            LASTKSCN(ISTA) = JSCN
         ELSE
            LASTKSCN(ISTA) = LASTISCN(ISTA)
         END IF
      END DO
C
C     Get the geometry for scans JSCN and MSCN using appropriate 
C     last scan arrays.
C
      CALL SCNGEO( LASTISCN, NGOOD, JSCN )
      CALL SCNGEO( LASTKSCN, NGOOD, MSCN )
C
      RETURN
      END
