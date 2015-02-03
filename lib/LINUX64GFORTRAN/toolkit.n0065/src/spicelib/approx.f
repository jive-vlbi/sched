C$Procedure            APPROX ( Approximate equality )
 
      LOGICAL FUNCTION APPROX ( X, Y, TOL )
 
C$ Abstract
C
C     True if two double precision numbers are equal to within some
C     tolerance.
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
C     COMPARE
C     NUMBERS
C
C$ Declarations
 
 
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      TOL
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     X,
C     Y          I   Double precision numbers.
C     TOL        I   Tolerance.
C
C     The function is true whenever |X - Y| < TOL.
C                                           -
C
C$ Detailed_Input
C
C     X,
C     Y           are arbitrary double precision numbers.
C
C     TOL         is a tolerance. X and Y are considered to be equal
C                 if they differ by no more than this amount. If TOL
C                 is negative, X and Y are never considered equal.
C
C$ Detailed_Output
C
C     The function is true whenever |X - Y| < TOL, and is false
C     otherwise.                            -
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     C
C     C     If the eccentricity is near one, this a parabola.
C     C
C           IF ( APPROX ( ECC, 1.D0, 10.D-12 ) ) THEN
C              TYPE = 'PARABOLA'
C
C           ELSE IF ( ECC .LT. 1 ) THEN
C              TYPE = 'ELLIPSE'
C
C           ELSE
C              TYPE = 'HYPERBOLA'
C           END IF
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990  (IMU)
C
C-&
 
C$ Index_Entries
C
C     approximate equality
C
C-&
 
 
 
C
C     Just shorthand, really.
C
      APPROX = ( ABS ( X - Y ) .LE. TOL )
 
      RETURN
      END
