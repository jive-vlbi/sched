      SUBROUTINE VXNMXX2( BLOCK, NXX, XXLINK )
C
C     Routine specific for the VEX extension of SCHED. 
C     Generic naming function, returns a unique name for a definition
C     in a certain $BLOCK, following VEX rules (special char 32)
C     Uses a #02 - #99 extension if identical names occur
C     By H.J. van Langevelde, JIVE, 300496 
C
      INCLUDE 'sched2.inc'
      INCLUDE 'schset2.inc'
      INCLUDE 'vxlink2.inc'
C      
      CHARACTER BLOCK*2, XXLINK(MAXMOD)*32
      CHARACTER VXNMHP2*32, VXNMRL2*32, VXNMPO2*32, VXNMPH2*32 
      CHARACTER VXNMFQ2*32, VXNMIF2*32, VXNMBB2*32, VXNMTR2*32 
      CHARACTER VXNMDA2*32, VXNMSI2*32, VXNMAN2*32
      INTEGER NXX
C ----------------------------------------------------------------------
C
C     first call a specific naming function
C
      IF( BLOCK .EQ. 'HP' ) THEN
         XXLINK(NXX) = VXNMHP2( NXX )
      ELSE IF( BLOCK .EQ. 'RL' ) THEN
         XXLINK(NXX) = VXNMRL2( NXX )
      ELSE IF( BLOCK .EQ. 'PO' .AND. .NOT. OBSTYP .EQ. 'PTVLBA' ) THEN
         XXLINK(NXX) = VXNMPO2( NXX )
      ELSE IF( BLOCK .EQ. 'PH' ) THEN
         XXLINK(NXX) = VXNMPH2( NXX, .FALSE. )
      ELSE IF( BLOCK .EQ. 'FQ' ) THEN
         XXLINK(NXX) = VXNMFQ2( NXX, .FALSE. )
      ELSE IF( BLOCK .EQ. 'IF' ) THEN
         XXLINK(NXX) = VXNMIF2( NXX, .FALSE. )
      ELSE IF( BLOCK .EQ. 'BB' ) THEN
         XXLINK(NXX) = VXNMBB2( NXX )
C      ELSE IF( BLOCK .EQ. 'TR' .AND. .NOT. OBSTYP .EQ. 'PTVLBA' ) THEN
      ELSE IF( BLOCK .EQ. 'TR' ) THEN
         XXLINK(NXX) = VXNMTR2( NXX )
      ELSE IF( BLOCK .EQ. 'DA' ) THEN
         XXLINK(NXX) = VXNMDA2( NXX )
      ELSE IF( BLOCK .EQ. 'SI' ) THEN
         XXLINK(NXX) = VXNMSI2( NXX )
      ELSE IF( BLOCK .EQ. 'AN' ) THEN
         XXLINK(NXX) = VXNMAN2( NXX )
      ELSE
         CALL ERRLOG( 'VXNMXX2: unsupported BLOCK type ')
         WRITE( XXLINK(NXX), '( A, I2.2 )' )  BLOCK//'Set', NXX
      END IF
C
C     check whether it is valid
C
      CALL VXUNQL2( NXX, XXLINK )
C
      RETURN
      END
