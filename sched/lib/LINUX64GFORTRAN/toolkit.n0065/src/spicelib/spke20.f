C$Procedure      SPKE20 ( SPK, evaluate Chebyshev polynomials, type 20 )

      SUBROUTINE SPKE20 ( ET, RECORD, XYZDOT )

C$ Abstract
C
C     Evaluate a single data record from an SPK or PCK segment of type
C     20 (Chebyshev polynomials, velocity only).
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
C     SPK
C     PCK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      XYZDOT   ( 6 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Evaluation epoch.
C     RECORD     I   Data record.
C     XYZDOT     O   Three function components and their derivatives.
C
C$ Detailed_Input
C
C     ET          is the epoch at which a state vector or Euler angle
C                 state is to be computed. The epoch is represented as
C                 seconds past J2000 TDB.
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will yield three function components and their
C                 derivatives with respect to time. The record
C                 structure for SPK type 20 data is:
C
C                    +--------------------------------------+
C                    | record size (excluding this element) |
C                    +--------------------------------------+
C                    | Coverage interval midpoint           |
C                    +--------------------------------------+
C                    | Coverage interval radius             |
C                    +--------------------------------------+
C                    | Coeffs for X velocity component      |
C                    +--------------------------------------+
C                    | Coeffs for Y velocity component      |
C                    +--------------------------------------+
C                    | Coeffs for Z velocity component      |
C                    +--------------------------------------+
C                    | X position component                 |
C                    +--------------------------------------+
C                    | Y position component                 |
C                    +--------------------------------------+
C                    | Z position component                 |
C                    +--------------------------------------+
C
C                 In the above record
C
C                    - Times are expressed as seconds past J2000 TDB.
C                    - Position components have units of km.
C                    - Velocity coefficients have units of km/s.
C
C                 See PCKE20 for a description of PCK type 20 records.
C
C                 PCK type 20 records contain coefficients for Euler
C                 angle rates and Euler angles corresponding to the
C                 interval midpoint. See PCKE20 for a more detailed
C                 description of the contents of PCK type 20 records.
C                 
C
C$ Detailed_Output
C
C     XYZDOT      is a 6-vector. In order, the components of XYZDOT are
C                 X, Y, Z, X', Y', and Z'. Units for state evaluations
C                 will be km and km/sec. Units for angles will be
C                 radians and radians/sec.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input record contains an invalid coefficient count, 
C        the error SPICE(INVALIDCOUNT) will be signaled.
C
C     2) If the input record contains invalid domain transformation
C        parameters, the error will be diagnosed by a routine in the
C        call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine evaluates both type 20 SPK and PCK records.
C
C     The exact format and structure of type 20 (Chebyshev polynomials,
C     position only) segments are described in the SPK and PCK Required
C     Reading files.
C
C     A type 20 record contains three sets of Chebyshev coefficients---
C     one set each for velocity components dX/dt, dY/dt, and dZ/dt. It
C     also contains a position vector (or for a PCK record, Euler
C     angles) associated with the midpoint of the record's coverage
C     interval. The position (or orientation) is obtained from the
C     indefinite integral of the velocity and the given vector.
C
C$ Examples
C
C     The data returned by the routine is in its rawest form,
C     taken directly from the segment. As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.
C
C     The code fragment below demonstrates reading and evaluating
C     a type 20 SPK record.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 20 ) THEN
C
C              CALL SPKR20 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE20 ( ET, RECORD, XYZDOT )
C                  .
C                  .  Check out the evaluated state.
C                  .
C           END IF
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     R.E. Thurman    (JPL)
C     K.S. Zukor      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (RET) (KSZ)
C
C-&
C
C$ Index_Entries
C
C     evaluate type_20 spk segment
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      INTGRL ( 3 )

      INTEGER               DEGP
      INTEGER               I
      INTEGER               J
      INTEGER               NCOF
      INTEGER               POSLOC
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKE20' )
 
C
C     The first number in the record is the record size. This is the
C     number of elements in the record, excluding the size itself.
C     Following it are the record's midpoint and radius, then the three
C     sets of coefficients. The record ends with three values which
C     represent either a position or orientation at the record coverage
C     interval's midpoint. The number of coefficients for each variable
C     can be determined from the record size, since there are the same
C     number of coefficients for each variable.
C
C     The number of items counted by RECORD(1), other than the
C     Chebyshev coefficients, is 5.
C     
      NCOF = ( INT( RECORD( 1 ) ) - 5 ) / 3

      IF ( NCOF .LT. 1 ) THEN

         CALL SETMSG ( 'The input record''s coefficient count NCOF '
     .   //            'should be positive but was #.'              )
         CALL ERRINT ( '#', NCOF                                    )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
         CALL CHKOUT ( 'SPKE20'                                     )
         RETURN

      END IF
 
C
C     The degree of each polynomial is one less than the number of
C     coefficients.
C
      DEGP = NCOF - 1

C
C     Pass the Chebyshev coefficient portion of the record into CHBIGR,
C     which will evaluate the expansion and its integral for each
C     component. The constants of integration are selected so that the
C     integrals are zero when the input time is the interval midpoint.
C
      DO I = 1, 3

         J = 4 + (I-1)*NCOF

         CALL CHBIGR ( DEGP, RECORD(J),   RECORD(2), 
     .                 ET,   XYZDOT(3+I), INTGRL(I)  )
      END DO

C
C     Add the position vector or Euler angles at the interval midpoint
C     to the integral.
C
      POSLOC = 4  +  ( 3 * NCOF )

      CALL VADD ( RECORD(POSLOC), INTGRL, XYZDOT )


      CALL CHKOUT ( 'SPKE20' )
      RETURN
      END
 
 
 
 
