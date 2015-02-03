 
C$Procedure      SPKE14 ( S/P Kernel, evaluate, type 14 )
 
      SUBROUTINE SPKE14 ( ET, RECORD, STATE )
 
C$ Abstract
C
C     Evaluate a single data record from a type 14 SPK segment.
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
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch for which a state is desired.
C     RECORD     I   Record from a type 14 SPK segment valid for ET.
C     STATE      O   State (position and velocity) at epoch ET.
C
C$ Detailed_Input
C
C     ET       is the epoch for which a state vector is desired.
C
C     RECORD   is a record from a type 14 SPK segment which, when
C              evaluated at epoch ET, will give the state (position
C              and velocity) of some body, relative to some center, in
C              some inertial reference frame.
C
C$ Detailed_Output
C
C     STATE    is the state vector at epoch ET. Its contents are, in
C              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of a type 14 SPK segment is
C     described in the SPK Required Reading.
C
C     A type 14 record contains six sets of Chebyshev coefficients,
C     one set each for the position coordinates X, Y, and Z, and one
C     set each for the velocity coordinates X', Y', and Z' of a state
C     vector.  SPKE14 calls the routine CHBVAL to evalute each
C     Chebyshev polynomial, and arrive at the complete state.
C
C$ Examples
C
C     The SPKEnn routines are almost always used in conjunction with
C     the corresponding SPKRnn routines, which read the records from
C     SPK files.
C
C     The data returned by the SPKRnn routine is in a raw form, taken
C     directly from the segment.  As such, it will be not be directly
C     useful to a user unless they have a complete understanding of the
C     structure of the data type.  Given that understanding, however,
C     the SPKRnn routines could be used to "dump" and check segment data
C     for a particular epoch before evaluating the record to obtain a
C     state vector, as in the example which follows.
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
C
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 14 ) THEN
C
C              CALL SPKR14 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE14 ( ET, RECORD, STATE )
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
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 10-MAR-1995 (KRG)
C
C-&
 
C$ Index_Entries
C
C     evaluate type_14 spk segment
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               COFLOC
      INTEGER               DEGREE
      INTEGER               I
      INTEGER               NCOEFF
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKE14' )
      END IF
 
C
C     The first number in the record is the number of Chebyshev
C     Polynomial coefficients used to represent each component of the
C     state vector. Following it are two numbers that will be used
C     when evaluating the sets of coefficients, and finally the six sets
C     of coefficients.
C
      NCOEFF = INT( RECORD( 1 ) )
 
C
C     The degree of each polynomial is one less than the number of
C     coefficients.
C
      DEGREE = NCOEFF - 1
 
C
C     Call CHBVAL once for each quantity to evaluate the position
C     and velocity values.
C
      DO I = 1, 6
C
C        The coefficients for each variable are located contiguously,
C        following the first three words in the record.
C
         COFLOC = NCOEFF * ( I - 1 ) + 4
C
C        CHBVAL needs as input the coefficients, the degree of the
C        polynomial, also two variable transformation parameters, which
C        are located in the second and third slots of the record, and
C        the epoch. We get back the appropriate element of a state
C        vector.
C
         CALL CHBVAL ( RECORD(COFLOC), DEGREE, RECORD(2), ET, STATE(I) )
 
      END DO
 
      CALL CHKOUT ( 'SPKE14' )
      RETURN
 
      END
