      SUBROUTINE PGQINF( ITEM, VALUE, LENGTH )
C
C     Stub version for the PGPLOT routine that can deliver the PGPLOT
C     version number.
C
      CHARACTER   ITEM*(*), VALUE*(*)
      INTEGER     LENGTH
C ----------------------------------------------------------------------
      IF( ITEM .EQ. 'VERSION' ) THEN
         LENGTH = 4
         VALUE = '0.00'
      ELSE
         CALL PUTOUT( 'Stub version of PGQINF - bad ITEM request' )
         LENGTH = 4
         VALUE = '----'
      END IF
      RETURN
      END
