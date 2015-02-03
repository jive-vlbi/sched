C$Procedure PCKE02 ( PCK, evaluate data record from type 2 segment )

      SUBROUTINE PCKE02 ( ET, RECORD, EULANG )

C$ Abstract
C
C     Evaluate a single PCK data record from a segment of type 2
C     (Chebyshev Polynomials, position only).
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
C     PCK
C
C$ Keywords
C
C     TRANSFORMATION
C     ROTATION
C
C$ Declarations

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      EULANG   ( 6 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C     RECORD     I   Data record.
C     EULANG     O   Euler angles and their derivatives.
C
C$ Detailed_Input
C
C     ET          is an epoch, at which the Euler angles are to
C                 be computed.
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will give the Euler angles of some body.
C
C$ Detailed_Output
C
C     EULANG       the Euler angles and their derivatives at
C                  time ET.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     The exact format and structure of type 2 (Chebyshev polynomials,
C     position only) segments are described in the PCK Required Reading
C     file.
C
C     A type 2 segment contains three sets of Chebyshev coefficients,
C     one set each for the Euler angles phi, delta and psi.  PCKE02
C     calls the routine SPKE02 for each set to evalute the polynomial
C     AND its first derivative.
C
C$ Examples
C
C     The PCKEnn routines are almost always used in conjunction with
C     the corresponding PCKRnn routines, which read the records from
C     binary PCK files.
C
C     The data returned by the PCKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the PCKRnn
C     routines might be used to examine raw segment data before
C     evaluating it with the PCKEnn routines.
C
C
C  Here we load a binary PCK files and use PCKE02 to get the
C  Euler angles.
C
C  C
C  C  Load binary PCK file.
C  C
C     CALL PCKLOF ('example.pck', HANDLE)
C
C
C  C  Get a segment applicable to a specified body and epoch.
C
C     CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     IF ( FOUND ) THEN
C
C
C        Look at parts of the descriptor.
C
C        CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
C        TYPE   = ICD( NT )
C        REF    = ICD( NR )
C
C        IF ( TYPE .EQ. 2 ) THEN
C
C           Read in Chebyshev coefficients from segment.
C
C           CALL PCKR02 ( HANDLE, DESCR, ET, RECORD )
C
C
C           Call evaluation routine to get Euler angles
C           phi, delta, w.
C
C           CALL PCKE02 ( ET, RECORD, EULANG )
C
C
C  The Euler angles and their derivatives are returned
C  in EULANG.
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      K. S. Zukor   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Removed unneeded Revisions section.
C
C-     SPICELIB Version 1.1.0, 13-MAR-1995 (KSZ)
C
C         Added error handling.
C
C-     SPICELIB Version 1.0.0, 30-SEP-1994 (KSZ)
C
C-&

C$ Index_Entries
C
C     get Euler angles and their derivatives
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      TWOPI
      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKE02' )
      END IF

C
C     Call evaluation routine to get Euler angles
C     phi, delta, w.
C
      CALL SPKE02 ( ET, RECORD, EULANG )

C
C     Mod the 3rd element of the state by TWOPI.
C     We do this because we've always done this.
C
      EULANG(3) = MOD ( EULANG(3), TWOPI() )

      CALL CHKOUT ( 'PCKE02' )

      RETURN
      END






