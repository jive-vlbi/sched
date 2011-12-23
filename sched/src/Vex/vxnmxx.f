      SUBROUTINE VXNMXX( BLOCK, NXX, XXLINK )
C
C     Routine specific for the VEX extension of SCHED. 
C     Generic naming function, returns a unique name for a definition
C     in a certain $BLOCK, following VEX rules (special char 32)
C     Uses a #02 - #99 extension if identical names occur
C     By H.J. van Langevelde, JIVE, 300496 
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C      
      CHARACTER BLOCK*2, XXLINK(MAXMOD)*32
      CHARACTER VXNMHP*32, VXNMRL*32, VXNMPO*32, VXNMPH*32, VXNMFQ*32
      CHARACTER VXNMIF*32, VXNMBB*32, VXNMTR*32 
      CHARACTER VXNMDA*32, VXNMSI*32, VXNMAN*32
      INTEGER NXX
C ----------------------------------------------------------------------
C
C     first call a specific naming function
C
      IF( BLOCK .EQ. 'HP' ) THEN
         XXLINK(NXX) = VXNMHP( NXX )
      ELSE IF( BLOCK .EQ. 'RL' ) THEN
         XXLINK(NXX) = VXNMRL( NXX )
      ELSE IF( BLOCK .EQ. 'PO' .AND. .NOT. OBSTYP .EQ. 'PTVLBA' ) THEN
         XXLINK(NXX) = VXNMPO( NXX )
      ELSE IF( BLOCK .EQ. 'PH' ) THEN
         XXLINK(NXX) = VXNMPH( NXX, .FALSE. )
      ELSE IF( BLOCK .EQ. 'FQ' ) THEN
         XXLINK(NXX) = VXNMFQ( NXX, .FALSE. )
      ELSE IF( BLOCK .EQ. 'IF' ) THEN
         XXLINK(NXX) = VXNMIF( NXX, .FALSE. )
      ELSE IF( BLOCK .EQ. 'BB' ) THEN
         XXLINK(NXX) = VXNMBB( NXX )
C      ELSE IF( BLOCK .EQ. 'TR' .AND. .NOT. OBSTYP .EQ. 'PTVLBA' ) THEN
      ELSE IF( BLOCK .EQ. 'TR' ) THEN
         XXLINK(NXX) = VXNMTR( NXX )
      ELSE IF( BLOCK .EQ. 'DA' ) THEN
         XXLINK(NXX) = VXNMDA( NXX )
      ELSE IF( BLOCK .EQ. 'SI' ) THEN
         XXLINK(NXX) = VXNMSI( NXX )
      ELSE IF( BLOCK .EQ. 'AN' ) THEN
         XXLINK(NXX) = VXNMAN( NXX )
      ELSE
         CALL ERRLOG( 'VXNMXX: unsupported BLOCK type ')
         WRITE( XXLINK(NXX), '( A, I2.2 )' )  BLOCK//'Set', NXX
      END IF
C
C     check whether it is valid
C
      CALL VXUNQL( NXX, XXLINK )
C
      RETURN
      END
