C$Procedure ZZHOLDD ( Private --- hold a scalar DP )

      SUBROUTINE ZZHOLDD ( OP, ID, OK, VALUE )

C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Persistently store double precision values or retrieve stored 
C     double precision values. That's it, not really rocket science.
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
C     None.
C
C$ Keywords
C
C     STORE DP VALUE
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzholdd.inc'

      INTEGER               OP
      INTEGER               ID
      LOGICAL               OK
      DOUBLE PRECISION      VALUE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     OP         I   Key for operation to execute.
C     ID         I   The ID for the item to apply OP.
C     OK         O   Boolean indicating success of get operation.
C     VALUE     I-O  Double precision value returned or to store.
C
C$ Detailed_Input
C
C     OP          The scalar integer key for the operation to execute.
C                 Proper values of OP:
C
C                    ZZPUT     store a double precision value for
C                              later use (put).
C
C                    ZZGET     retrieve a stored double precision
C                              value (get).
C
C                    ZZRESET   reset function to require a ZZPUT prior
C                              to a subsequent ZZGET (clear).
C
C     ID          The scalar integer ID for the item to get/put etc.
C                 Proper values of ID:
C
C                    GEN       general value, primarily for testing.
C
C                    GF_REF    user defined GF reference value.
C
C                    GF_TOL    user defined GF convergence tolerance.
C
C                    GF_DT     user defined GF step for numeric
C                              differentiation.
C
C     VALUE       The scalar double precision value to store (put).
C
C     The include file "zzholdd.inc" lists all accepted values for
C     ID and OP.
C
C$ Detailed_Output
C
C     OK          The logical flag indicating if a get operation
C                 returned a valid value for ID. OK returns false if a
C                 get operation occurs before a put.
C
C                 This argument has no meaning except when performing
C                 a get operation.
C
C     VALUE       The scalar double precision value retrieved (get).
C
C$ Parameters
C
C    None.
C
C$ Exceptions
C
C     1)  The error SPICE(UNKNOWNID) signals if the value of ID is
C         not one of those coded in zzholdd.inc.
C
C     2)  The error SPICE(UNKNOWNOP) signals if the value of OP is
C         not one of those coded in zzholdd.inc.
C
C$ Files
C
C    zzholdd.inc
C
C$ Particulars
C
C     This routine simply stores double precision values for later 
C     retrieval. 
C
C     A get operation may succeed or fail based on whether
C     a put operation preceded the put. 
C
C        A ZZHOLDD get operation for an ID called before a put operation
C        for that ID returns with OK as false, VALUE as 0. 
C
C        A ZZHOLDD get operation for an ID called after a put operation
C        for that ID returns with OK as true, VALUE as the value
C        assigned by the put.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     Store values using ZZHOLDD then attempt to retrieve the values.
C
C           PROGRAM ZZHOLDD_T
C           IMPLICIT NONE
C
C           INCLUDE 'zzholdd.inc'
C
C           DOUBLE PRECISION     VALUE
C           DOUBLE PRECISION     X
C           DOUBLE PRECISION     Y
C           DOUBLE PRECISION     Z
C           LOGICAL              OK
C
C           X = -11.D0
C           Y =  22.D0
C           Z = -33.D0
C
C     C
C     C     Perform a put then get.
C     C
C           VALUE = 0.D0
C           OK    = .FALSE.
C           CALL ZZHOLDD ( ZZPUT, GEN, OK, X)
C           CALL ZZHOLDD ( ZZGET, GEN, OK, VALUE )
C
C           IF (OK) THEN
C              WRITE(*,*) 'Check 1 ', VALUE
C           ELSE
C              WRITE(*,*) 'Error 1 '
C           END IF
C
C     C
C     C     Reset then get without put.
C     C
C           VALUE = 0.D0
C           OK    = .FALSE.
C
C           CALL ZZHOLDD ( ZZRESET,   GEN, OK, VALUE )
C           CALL ZZHOLDD ( ZZGET,     GEN, OK, VALUE )
C
C           IF (OK) THEN
C              WRITE(*,*) 'Error 2 '
C           ELSE
C              WRITE(*,*) 'Check 2 ', VALUE
C           END IF
C
C     C
C     C     Now put.
C     C
C           CALL ZZHOLDD ( ZZPUT, GEN, OK, Y)
C           CALL ZZHOLDD ( ZZGET, GEN, OK, VALUE )
C
C           IF (OK) THEN
C              WRITE(*,*) 'Check 3 ', VALUE
C           ELSE
C              WRITE(*,*) 'Error 3 '
C           END IF
C
C
C     C
C     C     Now another put with a different value.
C     C
C           CALL ZZHOLDD ( ZZPUT, GEN, OK, Z)
C           CALL ZZHOLDD ( ZZGET, GEN, OK, VALUE )
C
C           IF (OK) THEN
C              WRITE(*,*) 'Check 4 ', VALUE
C           ELSE
C              WRITE(*,*) 'Error 4 '
C           END IF
C
C           END
C
C   The program outputs:
C
C      Check 1   -11.000000000000000
C      Check 2    0.0000000000000000
C      Check 3    22.000000000000000
C      Check 4   -33.000000000000000
C
C    As expected.
C
C$ Restrictions
C
C    None.
C
C$ Literature_References
C
C    None.
C
C$ Author_and_Institution
C
C    E.D. Wright    (JPL)
C
C$ Version
C
C-   SPICELIB Version 1.1.0  03-DEC-2013 (EDW)
C
C       Added ID and OK arguments to routine, generalizing use.
C
C       Added RETURN() check.
C
C-   SPICELIB Version 1.0.0  16-FEB-2010 (EDW)
C
C-&

C$ Index_Entries
C
C    store a double precision value
C    retrieve a stored double precision value
C
C-&

C
C     SPICELIB functions
C
      INTEGER               BRCKTI

      LOGICAL               RETURN

C
C     Local variables.
C

      DOUBLE PRECISION      SVALUE  (NID)
      LOGICAL               INIT
      LOGICAL               FIRST   (NID)
      INTEGER               I

      SAVE                  SVALUE
      SAVE                  FIRST
      SAVE                  INIT

      DATA                  INIT  / .TRUE. /

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF


C
C     Confirm a proper ID value.
C
      IF ( BRCKTI(ID, 1, NID) .NE. ID ) THEN

         VALUE = 0.D0
         OK    = .FALSE.

         CALL CHKIN  ( 'ZZHOLDD'                               )
         CALL SETMSG ( 'ID value unknown. ID value #1 '
     .         //      'not an element of [1, #2]. Confirm'
     .         //      'the ID value exists in the zzholdd.inc '
     .         //      'parameter file.'                       )
         CALL ERRINT ( '#1', ID                                )
         CALL ERRINT ( '#2', NID                               )
         CALL SIGERR ( 'SPICE(UNKNOWNID)'                      )
         CALL CHKOUT ( 'ZZHOLDD'                               )
         RETURN

      END IF


C
C     Initialize the FIRST array; perform once per program run.
C
      IF ( INIT ) THEN

         DO I = 1, NID
            FIRST( I ) = .TRUE.
         END DO

         INIT = .FALSE.

      END IF


C
C     Perform the operation as described by OP.
C
      IF ( OP .EQ. ZZGET ) THEN

C
C        Attempt to retrieve a stored double precision value for ID.
C
C          - Return the value stored by a put operation and OK
C            as true.
C
C          - If no previous set to this ID, return value as zero and
C            OK as false.
C

         IF ( FIRST( ID ) ) THEN

            VALUE = 0.D0
            OK    = .FALSE.

         ELSE

C
C           Return the stored value.
C
            VALUE = SVALUE( ID )
            OK    = .TRUE.

         END IF


      ELSE IF ( OP .EQ. ZZPUT ) THEN

C
C        Store a value for later use. Set FIRST to false
C        so subsequent get calls will work.
C
         IF ( FIRST( ID ) ) THEN

            FIRST( ID ) = .FALSE.

         END IF

         SVALUE( ID ) = VALUE

      ELSE IF ( OP .EQ. ZZRESET ) THEN

C
C        Reset FIRST( ID ) forcing a put before a get.
C
         FIRST( ID ) = .TRUE.

      ELSE

C
C        Unknown value for 'OP'. Signal an error.
C
         VALUE = 0.D0
         OK    = .FALSE.

         CALL CHKIN  ( 'ZZHOLDD'                          )
         CALL SETMSG ( 'Unknown operation. Confirm the OP '
     .         //      'value # exists in the zzholdd.inc '
     .         //      'parameter file.'                  )
         CALL ERRINT ( '#', OP                            )
         CALL SIGERR ( 'SPICE(UNKNOWNOP)'                 )
         CALL CHKOUT ( 'ZZHOLDD'                          )
         RETURN

      END IF

      END

