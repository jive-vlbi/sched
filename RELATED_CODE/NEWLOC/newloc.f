      PROGRAM      NEWLOC
C
C     This program creates a locations.dat file for SCHED
C     from geodetic station files for position, velocity and
C     axis offset and from VLA data available from a web tool
C     that reads parameters.  It should be combined
C     with a separate locations file that has any stations 
C     that are not included in the input files.  Earlier
C     versions updated a locations.dat, but that became 
C     problematic with the introduction of time ranges.
C
C     Includes conversion of  VLA station positions in the VLA frame
C     into ITRF positions, given an ITRF position for at least
C     one station.  The position for PT will be compared with the
C     ITRF position to confirm that the rotations are ok.
C     Note with the new on-line system, the VLA positions are in an
C     array centered coordinate that is otherwise thought to be 
C     oriented in the same way as the ITRF.
C
C     Someday, get the user input information from a KEYIN file or 
C     some such.  Also, don't have hardwired information which is now
C     in the include file.
C
C     NEWLOC is based on VLA2ITRF which was basically one large file.
C     VLA2ITRF is based on VLABASE (see vlba/VLAbaseline/2000oct) which
C     was used for the first accurate tie.
C
C     Made VLBIJDAY to have station specific epoch.  March 18, 2010 RCW.
C
C     Enable episodic positions (like due to sudden shifts from 
C     earthquakes)  Sept. 28, 2011
C
      INCLUDE 'newloc.inc'
C
      INTEGER     JERR, LEN1
C
C -----------------------------------------------------------------
      WRITE(*,*) 'newloc starting.'
C
C     Get PI (as in 3.1415...) the tricky way.
C
      PI = 4.0D0 * DATAN( 1.D0 )
      RADSEC = PI / ( 3600.D0 * 180.D0 )
C
C     Get VLALONG.  This used to be the VLA longitude, but is now
C     the rotation between the VLA "ITRF" frame and the VLBI ITRF.
C     The values are set in parameters in the include file.
C
      VLALONG = ( VLALD*3600.D0 + VLALM*60.D0 + VLALS) * RADSEC
      WRITE(*,'(A,3F14.9)') 'VLA to ITRF rotation: ', 
     1       VLALONG, RADSEC, PI
C
C---------------------------   Remove this soon  --------------------
C     Get the old locations.dat file.  We will be updating it.
C
C     Note that this program forces VLA-N8 to be a station.
C     This locations.dat should have all the desired stations.
C
C40    CONTINUE
C      WRITE(*,*) 'Sched locations.dat file that is being updated:'
C      WRITE(*,*) 'Should have all desired stations.'
C      READ(*,'(A)') DBFILE
C      WRITE(*,*) 'locations file: ', DBFILE(1:LEN1(DBFILE))
C
C     Call RDLOC (originally from $SCHED/src/Cit/rdloc.f, but in the
C     libcit library based on a copy) to read the file.
C     This entangles this program with SCHED, but I think that is ok.
C
C      LOCFILE = DBFILE
C      CALL RDLOC( JERR )
C      IF( JERR .NE. 0 ) THEN
C         WRITE( *, * ) 'Error from rdloc: ', JERR
C         STOP
C      END IF
C      NS = NDB
C      TEXT = ' '
C      IC = 0
C      WRITE( *, '( 1x, /, 2I10, A, /, 4X, A )' )  
C     1      NDB, NS, 'Stations in locations.dat:', DBFILE
C      DO IS = 1, NS
C         STA(IS) = DBNAME(IS)
C         WRITE( TEXT(IC*10+1:IC*10+8),*) STA(IS)(1:LEN1(STA(IS))) 
C         IC = IC + 1
C         IF( IC .GE. 8 ) THEN
C            WRITE(*,*) TEXT
C            IC = 0
C            TEXT = ' '
C         END IF
C      END DO
C ---------------------------------  End of segment to remove.
C
      NS = 0
C
C     Get the geodetic solution results.
C
      CALL RDGEO
C
C     Read the VLA pad location file.
C
      CALL RDVLA
C
C     Adjust the VLA positions as needed and sanity check.
C
      CALL ADJVLA
C
C     Give some information on what was done.
C
      CALL WRTDAT
C
C     Write new locations file.
C
      CALL WRTLOC
C
      WRITE(*,*) 'NEWLOC ending'
      STOP
      END
 
