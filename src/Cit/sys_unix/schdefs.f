      SUBROUTINE SCHDEFS( FTYPE, FILENAME )
C
C     Routine for SCHED, which can be machine dependent, to hold the
C     default file names for SCHED.  It can be called by a variety
C     of routines.
C
C     This is the Unix/Linux version.
C
      CHARACTER    FTYPE*(*), FILENAME*(*)
Cf2py intent(in, out) FILENAME
      CHARACTER    FTEXT*80
C----------------------------------------------------------------------
      IF( FTYPE .EQ. 'location' ) THEN
         FILENAME = '$SCHED/catalogs/locations.dat'
      ELSE IF( FTYPE .EQ. 'stations' ) THEN
         FILENAME = '$SCHED/catalogs/stations_RDBE.dat'
      ELSE IF( FTYPE .EQ. 'sources' ) THEN
C
C        This could be sources.gsfc or sources.petrov.
C        sources.gsfc has more aliases.  Use it.
C        sources.petrov has many more sources as of Feb. 2015
C
         FILENAME = '$SCHED/catalogs/sources.gsfc'
      ELSE IF( FTYPE .EQ. 'frequency' ) THEN
         FILENAME = '$SCHED/catalogs/freq_RDBE.dat'
      ELSE IF( FTYPE .EQ. 'peakcommand' ) THEN
         FILENAME = '$SCHED/catalogs/peak.cmd'
      ELSE IF( FTYPE .EQ. 'refpointing' ) THEN
         FILENAME = '$SCHED/catalogs/sources.peak'
      ELSE IF( FTYPE .EQ. 'messages' ) THEN
         FILENAME = '$SCHED/catalogs/messages.txt'
      ELSE
         FTEXT = 'SCHFILES: Unrecognized file type:' // FTYPE
         CALL PUTOUT( FTEXT )
         CALL ERROR( 'Programming problem' )
      END IF
C
      RETURN
      END


