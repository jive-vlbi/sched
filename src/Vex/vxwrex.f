      SUBROUTINE VXWREX
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the EX = $EXPERIMENT section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
C     Huib's local variables 
C      
      INTEGER YEAR, DAY, DOY, MONTH, JD
      INTEGER   LEN1, I
      CHARACTER VEXVAL*32, MNAME*3, DNAME*3, TMPEXP*32, TFORM*9, TTIME*9
      DOUBLE PRECISION STOPT,STARTT

C ----------------------------------------------------------------------
C     
      WRITE( IVEX, '( A, A1 )' ) '$EXPER', SEP
C     
C     We will have only one experiment, designated by it's name
C
      WRITE( IVEX, '( A1 )' ) COM
      TMPEXP = EXPCODE
      CALL VXSTNM( TMPEXP,.FALSE. )
      WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1    TMPEXP(1:LEN1(TMPEXP)), SEP
      TMPEXP = EXPCODE
      CALL VXSTKY( TMPEXP, .FALSE. )
      WRITE( IVEX, '( 5X, A, A, A1 )' ) 'exper_name = ',
     1    TMPEXP(1:LEN1(TMPEXP)), SEP
C
C     experiment description
C     protect against zero length strings where they are possible.
C
      WRITE( IVEX, '( 5X, A, A1, A, A1, A1 )' ) 'exper_description = ', 
     1     QOT, EXPT(1:MAX(1,LEN1(EXPT))), QOT, SEP
C
C     find PI name
C
      VEXVAL = PINAME(1:32)
      CALL VXSTKY( VEXVAL, .FALSE.)
      WRITE( IVEX, '( 5X, A, A, A1 )' ) 'PI_name = ', 
     1     VEXVAL(1:MAX(1,LEN1(VEXVAL))), SEP
C
C     PI e-mail
C
      VEXVAL = EMAIL(1:32)
      CALL VXSTKY( VEXVAL, .FALSE.)
      WRITE( IVEX, '( 5X, A, A, A1 )' ) 'PI_email = ', 
     1     VEXVAL(1:MAX(1,LEN1(VEXVAL))), SEP      
C
C     other stuff that we happen to have available goes
C     into comments, like adresses
C
      WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'address:   ', 
     1     ADDRESS(1)(1:MAX(1,LEN1(ADDRESS(1))))
      DO I = 2,4
         WRITE( IVEX, '( A1, 4X, A, A )' ) COM, '           ', 
     1       ADDRESS(I)(1:MAX(1,LEN1(ADDRESS(I))))
      END DO
      WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'phone:     ', 
     1     PHONE(1:MAX(1,LEN1(PHONE)))
      WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'during obs:', 
     1     OBSPHONE(1:MAX(1,LEN1(OBSPHONE)))
      WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'fax:       ', 
     1     FAX(1:MAX(1,LEN1(FAX)))
      IF( NOTE(1) .NE. ' ' ) 
     1     WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'notes:     ', 
     2     NOTE(1)(1:MAX(1,LEN1(NOTE(1))))
      DO I = 2,4
         IF( NOTE(I) .NE. ' ' ) 
     1       WRITE( IVEX, '( A1, 4X, A, A )' ) COM, '           ', 
     2       NOTE(I)(1:MAX(1,LEN1(NOTE(I))))
      END DO
C
C     good idea to write some dates, computers should read $SCHED
C
C
      CALL TIMEJ( STOPJ(1), YEAR, DOY, STOPT )
      MONTH = 1
      DAY = DOY
      CALL TDATECW( YEAR, MONTH, DAY, JD, MNAME, DNAME )
C
      WRITE( IVEX, '( A1 )' ) COM
      WRITE( IVEX, '( A1, 4X, A, I4, A, I3 )' ) COM, 'year, doy: ', 
     1     YEAR,', ',DOY
      WRITE( IVEX, '( A1, 4X, A, A3, 1X, I2, 1X, A3, 1X, I4 )' )
     $    COM, 'date     : ', 
     1    DNAME, DAY, MNAME, YEAR
      WRITE( IVEX, '( A1, 4X, A, I5 )' ) COM, 'MJD      : ', 
     1    INT( JD - 2400000.5 )

C
C     Write experiment nominal start and end time
C
      CALL TIMEJ( STARTJ(1), YEAR, DOY, STARTT )
      TTIME = TFORM( STARTT, 'T', 0, 2, 2, 'hms' )
      WRITE( IVEX, '( 5X, A, I4, A, I3,A, A, A)' )  
     1      'exper_nominal_start=', YEAR,'y',DOY,'d', 
     2      TTIME, SEP
      CALL TIMEJ( STOPJ(SCANL), YEAR, DOY, STOPT )
      TTIME = TFORM( STOPT, 'T', 0, 2, 2, 'hms' )
      WRITE( IVEX, '( 5X, A, I4, A, I3,A, A, A)' )  
     1      'exper_nominal_stop=', YEAR,'y',DOY,'d', 
     2      TTIME, SEP
C
C     Include cover letter
C
      CALL VXCOVR( IVEX )
C
C     Target Correlator can be supplied
C
      VEXVAL = CORREL(1:32)
      CALL VXSTKY( VEXVAL, .FALSE.)
      IF( VEXVAL .NE. ' ' ) THEN
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( 5X, A, A, A1 )' ) 'target_correlator = ', 
     1     VEXVAL(1:MAX(1,LEN1(VEXVAL))), SEP
      END IF
C
C     Others correlation parameters currently in comments:
C
      IF( CORAVG .NE. 0. )  THEN
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A1, 4X, A, F9.3, A )' ) COM, 
     1     'integr_time    : ',CORAVG, ' s'
      END IF
C
      IF( CORCHAN .NE. 0 ) 
     1     WRITE( IVEX, '( A1, 4X, A, I4 )' ) COM, 'number_channels:',
     2     CORCHAN
C
      IF( CORNANT .NE. 0 ) 
     1     WRITE( IVEX, '( A1, 4X, A, I3 )' ) COM, 'number_antenna :',
     2     CORNANT
C
      IF( CORPOL ) THEN 
         WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'cross_polarize : ',
     1       'Yes'
      ELSE
         WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'cross_polarize : ',
     1       'No'
      END IF
C
      IF( CORWTFN .NE. ' ' ) 
     1     WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'weight_func    : ',
     2     CORWTFN(1:MAX(1,LEN1(CORWTFN)))
C
      IF( CORTAPE .NE. ' ' ) 
     1     WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'distrib_medium : ',
     2     CORTAPE(1:MAX(1,LEN1(CORTAPE)))
C
      IF( CORSRCS .NE. ' ' ) 
     1     WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'source_pos_cat : ',
     2     CORSRCS(1:MAX(1,LEN1(CORSRCS)))
C
      IF( CORSHIP(1) .NE. ' ' ) 
     1    WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'distribute_to  : '
      DO I = 1, 4
         IF( CORSHIP(I) .NE. ' ' ) 
     1       WRITE( IVEX, '( A1, 4X, A, A )' ) COM, '                 ',
     2       CORSHIP(I)(1:MAX(1,LEN1(CORSHIP(I))))
      END DO
C
      IF( CORNOTE(1) .NE. ' ' ) 
     1    WRITE( IVEX, '( A1, 4X, A, A )' ) COM, 'corr_notes : '
      DO I = 1, 4
         IF( CORNOTE(I) .NE. ' ' ) 
     1       WRITE( IVEX, '( A1, 4X, A, A )' ) COM, '             ',
     2       CORNOTE(I)(1:MAX(1,LEN1(CORNOTE(I))))
      END DO
C
C     done for the day
C
      WRITE( IVEX, '( A1 )' ) COM
      WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP  
C
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END
