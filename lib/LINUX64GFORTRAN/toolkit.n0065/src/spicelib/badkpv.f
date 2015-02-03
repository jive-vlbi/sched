C$Procedure BADKPV ( Bad Kernel Pool Variable )
 
      LOGICAL FUNCTION BADKPV ( CALLER, NAME, COMP, SIZE, DIVBY, TYPE )
 
C$ Abstract
C
C     Determine if a kernel pool variable is present and if so
C     that it has the correct size and type.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C      None.
C
C$ Keywords
C
C      ERROR
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         CALLER
      CHARACTER*(*)         NAME
      CHARACTER*(*)         COMP
      INTEGER               SIZE
      INTEGER               DIVBY
      CHARACTER*(*)         TYPE
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CALLER     I   Name of the routine calling this routine.
C     NAME       I   Name of a kernel pool variable
C     COMP       I   Comparison operator.
C     SIZE       I   Expected size of the kernel pool variable
C     DIVBY      I   A divisor of the size of the kernel pool variable.
C     TYPE       I   Expected type of the kernel pool variable
C
C     The function returns FALSE if the kernel pool variable is OK
C
C$ Detailed_Input
C
C     CALLER     is the name of the routine calling this routine
C                to check correctness of kernel pool variables.
C
C     NAME       is the name of a kernel pool variable that the
C                calling program expects to be present in the
C                kernel pool.
C
C     COMP       is the comparison operator to use when comparing
C                the number of components of the kernel pool variable
C                specified by NAME with the integer SIZE.  If DIM is
C                is the actual size of the kernel pool variable then
C                BADKPV will check that the sentence
C
C                    DIM COMP SIZE
C
C                is a true statement.  If it is not a true statement
C                an error will be signalled.
C
C                Allowed values for COMP and their meanings are:
C
C                '='      DIM .EQ. SIZE
C                '<'      DIM .LT. SIZE
C                '>'      DIM .GT. SIZE
C                '=>'     DIM .GE. SIZE
C                '<='     DIM .LE. SIZE
C
C
C     SIZE       is an integer to compare with the actual
C                number of components of the kernel pool variable
C                specified by NAME.
C
C     DIVBY      is an integer that is one of the factors of the
C                actual dimension of the specified kernel pool variable.
C                In other words, it is expected that DIVBY evenly
C                divides the actual dimension of NAME. In those
C                cases in which the factors of the dimension of NAME
C                are not important, set DIVBY to 1 in the calling
C                program.
C
C     TYPE       is the expected type of the kernel pool variable.
C                Recognize values are
C
C                  'C' for character type
C                  'N' for numeric type (integer and double precision)
C
C                The case of type is insignificant.  If the value
C                of TYPE is not one of the 2 values given above
C                no check for the type of the variable will be
C                performed.
C
C
C$ Detailed_Output
C
C     The function returns the value FALSE if the kernel pool variable
C     has the expected properties.  Otherwise the routine signals
C     an error and returns the value .TRUE.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the kernel pool variable specified by NAME is not
C        present in the kernels pool, the error
C        'SPICE(VARIABLENOTFOUND)' will be signalled and the
C        routine will return the value .TRUE.
C
C     2) If the comparison operator specified by COMP is unrecognized
C        the error 'SPICE(UNKNOWNCOMPARE)' will be signalled and the
C        routine will return the value .TRUE.
C
C     3) If the comparison of the actual size of the kernel pool
C        variable with SIZE is not satisfied, the error
C        'SPICE(BADVARIABLESIZE)' will be signalled and the
C        routine will return the value .TRUE.
C
C     4) If the variable does not have the expected type, the error
C        'SPICE(BADVARIABLETYPE)' will be signalled and the routine
C        will return the value .TRUE.
C
C$ Particulars
C
C     This routine takes care of routine checking that often needs
C     to be done by programs and routines that rely upon kernel
C     pool variables being present and having the correct attributes.
C
C     It checks for the presence of the kernel pool variable and
C     examines the type and dimension of the variable to make sure
C     they conform to the requirements of the calling routine.
C
C$ Examples
C
C     Suppose that you need to fetch a number of variables
C     from the kernel pool and want to check that the requested
C     items are in fact available prior to performing further
C     computations. The following shows how you might use
C     this routine to handle the details of checking of
C     the various items.
C
C        CALLER  = 'MYROUTINE'
C
C        We need some data for body 399 and we expect there to be an
C        even number of items available and at least 4 such items.
C        Moreover we expect these items to be numeric.  Note that 
C        The variable assignments below are comments and are present
C        only to assist in understanding the calls to BADKPV.
C
C  C        NAME  = 'BODY_399_DATA'
C  C        COMP  = '=>'
C  C        SIZE  =  4
C  C        DIVBY =  2
C  C        TYPE  = 'N'
C
C        In addition we need the units associated with this data.
C        We expect the units to be character and that the number
C        of components is 1. Since we expect only one item, the
C        number of items should be divisible by 1.
C
C  C        NAME  = 'BODY_399_DATAUNIT'
C  C        COMP  = '='
C  C        SIZE  = 1
C  C        DIVBY = 1
C  C        TYPE  = 'C'
C
C        IF (    BADKPV( CALLER, 'BODY_399_DATA',      '=>', 4, 2, 'N')
C    .      .OR. BADKPV( CALLER, 'BODY_399_DATAUNITS', '=',  1, 1, 'C'))
C    .   THEN
C
C           CALL CHKOUT ( 'MYROUTINE' )
C           RETURN
C
C        END IF
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.1.1, 10-MAY-2000 (WLT)
C
C        Modified the example section so that it is consistent with
C        calling sequence for BADKPV.
C 
C-    SPICELIB Version 1.1.0, 26-AUG-1997 (WLT)
C
C        Moved the initial assignment of BADKPV to the lines
C        prior to the check of RETURN().  This avoids returning
C        without having assigned value to BADKPV.
C
C-    SPICELIB Version 1.0.0, 09-APR-1997 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Check the properties of a kernel pool variable
C
C-&
C
C     SPICELIB Functions
C
      LOGICAL               EQCHR
      LOGICAL               RETURN
 
C
C     Local Variables
C
 
      CHARACTER*(1)         CLASS
 
      INTEGER               DIM
      INTEGER               RATIO
 
      LOGICAL               OK
      LOGICAL               FOUND
 
 
C
C     Until we know otherwise, we shall assume that we have
C     a bad kernel pool variable.
C
      BADKPV = .TRUE.

      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN  ( 'BADKPV' )
 
C
C     Look up the attributes of this variable in the kernel pool.
C
      CALL DTPOOL ( NAME, FOUND, DIM, CLASS )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( '#: The kernel pool variable ''#'' is not '
     .   //            'currently present in the kernel pool. '
     .   //            'Possible reasons are that the '
     .   //            'appropriate text kernel file has not '
     .   //            'been loaded via a call to FURNSH or that '
     .   //            'the routine CLPOOL has been called after '
     .   //            'loading the appropriate file. ' )
 
         CALL ERRCH  ( '#', CALLER )
         CALL ERRCH  ( '#', NAME   )
         CALL SIGERR ( 'SPICE(VARIABLENOTFOUND)' )
         CALL CHKOUT ( 'BADKPV' )
         RETURN
 
      END IF
 
C
C     Compare the dimension of the specified variable with the
C     input SIZE.
C
      IF      ( COMP .EQ. '='  ) THEN
 
         OK = DIM .EQ. SIZE
 
      ELSE IF ( COMP .EQ. '<'  ) THEN
 
         OK = DIM .LT. SIZE
 
      ELSE IF ( COMP .EQ. '>'  ) THEN
 
         OK = DIM .GT. SIZE
 
      ELSE IF ( COMP .EQ. '<=' ) THEN
 
         OK = DIM .LE. SIZE
 
      ELSE IF ( COMP .EQ. '=>' ) THEN
 
         OK = DIM .GE. SIZE
 
      ELSE
 
         CALL SETMSG ( '#: The comparison operator ''#'' is not '
     .   //            'a recognized value.  The recognized '
     .   //            'values are ''<'', ''<='', ''='', ''=>'', '
     .   //            '''>''. ' )
         CALL ERRCH  ( '#', CALLER )
         CALL ERRCH  ( '#', COMP   )
         CALL SIGERR ( 'SPICE(UNKNOWNCOMPARE)' )
         CALL CHKOUT ( 'BADKPV'    )
         RETURN
 
      END IF
 
C
C     If the comparison was not favorable, signal an error
C     and return.
C
      IF ( .NOT. OK ) THEN
 
 
         CALL SETMSG ( '#: The kernel pool variable ''#'' is '
     .   //            'expected to have a number of components '
     .   //            'DIM such that the comparison DIM # # is '
     .   //            'TRUE.  However, the current number of '
     .   //            'components for ''#'' is #. ' )
 
         CALL ERRCH  ( '#', CALLER )
         CALL ERRCH  ( '#', NAME   )
         CALL ERRCH  ( '#', COMP   )
         CALL ERRINT ( '#', SIZE   )
         CALL ERRCH  ( '#', NAME   )
         CALL ERRINT ( '#', DIM    )
         CALL SIGERR ( 'SPICE(BADVARIABLESIZE)' )
         CALL CHKOUT ( 'BADKPV' )
         RETURN
 
      END IF
 
 
C
C     Check to see that DIVBY evenly divides the dimension of
C     the variable.
C
      IF ( DIVBY .NE. 0 ) THEN
         RATIO = DIM / DIVBY
      ELSE
         RATIO = 1
      END IF
 
      IF ( DIVBY*RATIO .NE. DIM ) THEN
 
         CALL SETMSG ( '#: The number of components of the '
     .   //            'kernel pool variable ''#'' is required '
     .   //            'to be divisible by #.  However, the '
     .   //            'actual number of components is # which '
     .   //            'is not evenly divisible by #. '  )
 
         CALL ERRCH  ( '#', CALLER )
         CALL ERRCH  ( '#', NAME   )
         CALL ERRINT ( '#', DIVBY  )
         CALL ERRINT ( '#', DIM    )
         CALL ERRINT ( '#', DIVBY  )
         CALL SIGERR ( 'SPICE(BADVARIABLESIZE)'  )
         CALL CHKOUT ( 'BADKPV' )
         RETURN
 
      END IF
 
C
C     Finally check the type of the variable.
C
      IF      ( EQCHR ( TYPE, 'C' ) ) THEN
 
         IF ( CLASS .NE. 'C' ) THEN
 
 
            CALL SETMSG ( '#: The kernel pool variable ''#'' '
     .      //            'must be of type "CHARACTER". '
     .      //            'However, the current type is numeric. ' )
            CALL ERRCH  ( '#', CALLER )
            CALL ERRCH  ( '#', NAME   )
            CALL SIGERR ( 'SPICE(BADVARIABLETYPE)' )
            CALL CHKOUT ( 'BADKPV' )
            RETURN
 
         END IF
 
 
      ELSE IF ( EQCHR ( TYPE, 'N' ) ) THEN
 
 
         IF ( CLASS .NE. 'N' ) THEN
 
            CALL SETMSG ( '#: The kernel pool variable ''#'' '
     .      //            'must be of type "NUMERIC".  However, '
     .      //            'the current type is character. ' )
            CALL ERRCH  ( '#', CALLER )
            CALL ERRCH  ( '#', NAME   )
            CALL SIGERR ( 'SPICE(BADVARIABLETYPE)' )
            CALL CHKOUT ( 'BADKPV' )
            RETURN
 
         END IF
 
      END IF
 
 
      BADKPV = .FALSE.
      CALL CHKOUT ( 'BADKPV' )
 
      RETURN
      END
