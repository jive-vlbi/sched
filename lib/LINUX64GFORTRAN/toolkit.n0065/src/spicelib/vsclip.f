C$Procedure      VSCLIP ( Vector scaling, 3 dimensions, in place )
 
      SUBROUTINE VSCLIP ( S, V )
 
C$ Abstract
C
C     Multiply a scalar and a 3-dimensional double precision vector,
C     replacing the input vector with the result.
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
C     VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION   S
      DOUBLE PRECISION   V ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S          I   Scalar by which to multiply a vector.
C     V         I-O  Vector to be multiplied/result of multiplication.
C
C$ Detailed_Input
C
C     S    This is a double precision scalar used to multiply the
C          vector V.
C
C     V    This is a 3-dimensional, double precision vector which is
C          to be scaled by S.
C
C$ Detailed_Output
C
C     V    This is a 3-dimensional, double precision vector resulting
C          from the scalar multiplication
C 
C             S * V
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is provided for situation where it is convenient to
C     scale a vector in place rather than store the result in a
C     separate variable.  Note that the call
C
C        CALL VSCL ( S, V, V )
C
C     is not permitted by the ANSI Fortran 77 standard; this routine
C     can be called instead to achieve the same result.
C
C     VSCLIP multiplies each component of V by S to form the respective
C     components of the output vector.  No error checking is performed.
C
C$ Examples
C
C     The following table shows the output V as a function of the
C     the inputs V and S.
C
C        V on input         S          V on output
C        -------------------------------------------------------
C        (1D0, -2D0, 0D0)   -1D0       (-1D0, 2D0, 0D0)
C        (0D0, 0D0, 0D0)     5D0       (0D0, 0D0, 0D0)
C
C$ Restrictions
C
C     The user is responsible for insuring that no floating point
C     overflow occurs from multiplying S by any component of V. No
C     error recovery or reporting scheme is incorporated in this
C     subroutine.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.M. Owen       (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB) (WMO)
C
C-&
 
C$ Index_Entries
C
C     3-dimensional vector scaling in place
C
C-&
      V(1) = S * V(1)
      V(2) = S * V(2)
      V(3) = S * V(3)
 
      RETURN
      END
