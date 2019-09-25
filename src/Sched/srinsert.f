      SUBROUTINE SRINSERT( JSCN, KSRC, SRNAME, 
     1                     STARTK, STOPK, LASTISCN, MSCN )
Cf2py intent(in) JSCN, KSRC, SRNAME, STARTK, STOPK, LASTISCN, MSCN
C
C     Routine for SCHED used by ADDPEAK while adding pointing scans.
C     It could conceivably be used by others some day.  ADDPEAK 
C     takes a template scan and changes the times and source with
C     the help of this routine.  This action is done multiple times
C     so it was easier to split it out.  This routine gets the 
C     geometry and slew parameters for the new scan (JSCN) and for 
C     another, usually the one with the original target source (MSCN).
C     ADDPEAK is especially interested in the slew time between the
C     two, calculated under the assumption that MSCN immediately
C     follows JSCN for the stations involved in both.
C     This helps determine if there is adequate time for insertion
C     of scans like peaking scans and also helps determine which
C     possible source to use for an inserted scan.
C
C     KSRC is the source catalog list entry for the new source.
C     STARTK and STOPK are the new start and stop times.
C
C     STASCN is assumed to have been determined externally.
C
C     The first calls will be for hypothetical sources.  The final
C     one for the source/scan actually to be used.
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
C
C     Treat the scan times as not adjustable.  DURONLY=6 will cause
C     appropriate constraints to be set in OPTTIM (new as of October 22, 
C     2013. RCW).
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
      ANNOT(JSCN)   = 
     1  'Following scan added by Sched for reference pointing'
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
