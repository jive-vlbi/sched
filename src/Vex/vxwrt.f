      SUBROUTINE VXWRT
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 030596 
C     opens vex file, does some sorting and writes setup and 
C     schedule sections
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER    VLBOPE, LEN1, IOERR, IXX, ISTA, JSTA
      CHARACTER  VEXFILE*80, OPTEXT*255, OPSTAT*4, VXNMPR*32
      LOGICAL    EXISTS, VIOLFS
      REAL       PLTVER, VERJPL
C ----------------------------------------------------------------------
C
C     Create a VEX output file
C
      VIOLFS = .FALSE.
      IF( DEBUG ) CALL WLOG( 1, 'VXWRT: Starting VEX section' )
      WRITE( VEXFILE, '( A, A )' )  EXPCODE(1:LEN1(EXPCODE)), '.VEX'
      CALL DWCASE( VEXFILE )
C     
C     Find out if the .vex file already exists.
C
      INQUIRE( FILE=VEXFILE, EXIST=EXISTS )
      IF( EXISTS .AND. ( OVERWRIT ) ) THEN
         OPSTAT = 'OLD'
      ELSE IF( EXISTS ) THEN
         CALL WLOG( 1, 'VXWRT: '//VEXFILE//' already exists.' )
         CALL ERRLOG( 'VXWRT: You need to delete old output files' )
      ELSE
         OPSTAT = 'NEW'
      END IF
C     
C     OPEN VEX file 
C
      CALL WLOG( 1, 'VXWRT :  Writing  V E X  file ' //
     1     VEXFILE(1:LEN1(VEXFILE)) )
      IOERR = VLBOPE( IVEX, VEXFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IOERR .NE. 1 ) CALL ERRLOG( ' Open problem:'//OPTEXT )
C
C     First line defines version of VEX we are trying to adopt
C
      WRITE( IVEX, '( A, A3, A1 )' ) 'VEX_rev = ', VEXVER, SEP
C
C     Give version of Sched
C
      IF( VERSION .NE. ' ' ) 
     1     WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'SCHED vers: ', 
     2     VERSION(1:LEN1(VERSION))      
C
C     Version of VX software
C
      WRITE( IVEX, '( A1, 4X, A, A3, A3 )' ) COM, 'VEX/SCHED:  ',
     1    VEXVER, VXSOFT
C
C     Version of Sched main
C
C      CALL VERSCHED(VERSCH)
      CALL PLVER(PLTVER)
      CALL JPLVER(VERJPL)
      
      WRITE( IVEX, '( A1, 4X, A, F5.1, 2X, A, F4.2, 2X, A, 
     1          F4.2 )' ) 
     2    COM, 'Other versions: Sched: ', VERNUM, 
     3    'Plot: ', PLTVER, 'JPL-ephem: ', 
     4    VERJPL
      WRITE( IVEX, '( A )' ) COMLIN
C
C     Warn if there are moving sources and the Vex file positions
C     should not be used for correlation.
C
      IF( MOVING ) THEN
         WRITE( IVEX, '( A, / A, /, A, /, A, /, A )' )
     1      '*',
     2      '*   ++++  WARNING:  File contains moving sources ',
     3      '*          Do no use positions for correlation.',
     4      '*          Moving coordinates not described in VEX.',
     5      '*'
         WRITE( IVEX, '( A )' ) COMLIN
      END IF
C
C     first issue warnings if violates PCFS constraints
C      
      IF( NSTA .GT. 35 ) THEN
         WRITE( MSGTXT, '( A, I3, A )' ) 
     1       'VXWRT: WARNING: More than 35 stations (', NSTA,
     2       ') in this schedule.' 
         CALL WLOG( 1,MSGTXT)
         VIOLFS = .TRUE.
      END IF
C
      IF( SCANL-SCAN1 .GT. 2000 ) THEN
         WRITE( MSGTXT, '( A, I3, A )' ) 
     1       'VXWRT: WARNING: More than 2000 scans (', (SCANL-SCAN1),
     2       ') in this schedule.' 
         CALL WLOG( 1,MSGTXT)
         VIOLFS = .TRUE.
      END IF
C
C     And if there are non-unique station codes
C      
      IF( NSTA .GT. 2 ) THEN 
         DO ISTA = 1, NSTA-1
            DO JSTA = ISTA+1, NSTA
               IF( STCODE(STANUM(ISTA)) .EQ. STCODE(STANUM(JSTA)) ) THEN
                  WRITE( MSGTXT, '( A, A2, A )' )
     1                'VXWRT: WARNING: The station code ', 
     2                STCODE(STANUM(ISTA)), ' occurs twice '
                  CALL WLOG( 1,MSGTXT)
                  VIOLFS = .TRUE.
               END IF
            END DO
         END DO
      END IF
C     
      IF( VIOLFS )
     1    CALL WLOG( 1,'VXWRT: WARNING Violating the limits '//
     2    'of PCFS, this VEX will NOT run!!!!')
C
C     
C     Find out if any of the VLBAs are doing something different to the
C     other VLBAs, so we can treat these as special cases later.
C     CR: 050801. VXVLBS no longer necessary (all stations get own setups)
C      CALL VXVLBS 
C     first find out how many modes there are
C
      CALL VXMODE
C
C     sort out how many FQ sections exist and how they are referenced
C
      CALL VXSORT( 'FQ', NFQVEX, FQISSET, NMODFQ, IMODFQ,
     1     NSTAFQ, ISTAFQ, FQLINK )
C
      CALL VXSORT( 'IF', NIFVEX, IFISSET, NMODIF, IMODIF,
     1     NSTAIF, ISTAIF, IFLINK )
C
      CALL VXSORT( 'BB', NBBVEX, BBISSET, NMODBB, IMODBB,
     1     NSTABB, ISTABB, BBLINK )
C
      CALL VXSORT( 'TR', NTRVEX, TRISSET, NMODTR, IMODTR,
     1     NSTATR, ISTATR, TRLINK )
C
      CALL VXSORT( 'HP', NHPVEX, HPISSET, NMODHP, IMODHP,
     1     NSTAHP, ISTAHP, HPLINK )
C
      CALL VXSORT( 'PO', NPOVEX, POISSET, NMODPO, IMODPO,
     1     NSTAPO, ISTAPO, POLINK )
C
      CALL VXSORT( 'RL', NRLVEX, RLISSET, NMODRL, IMODRL,
     1     NSTARL, ISTARL, RLLINK )
C
      CALL VXSORT( 'PH', NPHVEX, PHISSET, NMODPH, IMODPH,
     1     NSTAPH, ISTAPH, PHLINK )
C
C
C     the Data acquisition info is station based, as are site and antenna
C
      CALL VXTELS( 'DA', NDAVEX, ISTADA, DALINK )
C
      CALL VXTELS( 'SI', NSIVEX, ISTASI, SILINK )
C
      CALL VXTELS( 'AN', NANVEX, ISTAAN, ANLINK )
C
C     most commonly used FQ and PH values have to be stored by mode
C     rather than SET, to make sorting in schedule possible (VXSCNS).
C
      CALL VXFQVX
C
C     setup the phase cal tone links and tone1 (sort out relations
C     between tones and freqs, needs to be run first to detect new
C     freq and phasecal blocks
C
      CALL VXTONE
C
C     sort out the extra modes from changes in the schedule
C
      CALL VXSCNS
C
C     Find the freq and phasecal blocks for the new modes
C     redoes some of the work, no problem.
C
      CALL VXTONE
C
C
C     and setting up PROCEDURES is completely trivial
C
      NPRVEX = NMDVEX
      DO IXX = 1, NPRVEX
         PRLINK( IXX ) = VXNMPR( IXX )
         CALL VXUNQL( IXX, PRLINK )
      END DO
C
C     now actually write the setups
C
      CALL VXDEFS
C
C     and finally, write the schedule
C
      CALL VXSCH
C
      CLOSE( UNIT=IVEX )
C
      IF( DEBUG ) CALL WLOG( 1, 'VXWRT: Done with VEX section' )
      RETURN
      END
