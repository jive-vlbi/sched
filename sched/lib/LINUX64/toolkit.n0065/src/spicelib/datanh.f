C$Procedure  DATANH  ( Double precision arc hyperbolic tangent )
 
      DOUBLE PRECISION FUNCTION  DATANH ( X )
 
C$ Abstract
C
C      Return the inverse hyperbolic tangent of a double
C      precision argument.
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
C      HYPERBOLIC,  MATH
C
C$ Declarations
 
      DOUBLE PRECISION   X
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       X         I     Number whose inverse hyperbolic tangent is
C                       desired. X must lie in the range -1 < X < +1.
C
C$ Detailed_Input
C
C      X      is any double precision number greater than or equal to 1.
C
C$ Detailed_Output
C
C      DATANH is the inverse hyperbolic tangent of X.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      This function simply implements the definition of the inverse
C      hyperbolic tangent as follows:
C
C      DATANH = 0.5D0 * DLOG ( (1+X) / (1-X) )
C
C      If the input value is not valid, an error is signalled.
C
C$ Examples
C
C      The following table gives a few values for X and the resulting
C      value of DATANH.
C
C       X                       DATANH(X)
C      ----------------------------------------------
C      -0.2000000000000000     -0.2027325540540822
C      -0.1000000000000000     -0.1003353477310756
C       0.0000000000000000E+00  0.0000000000000000E+00
C       0.1000000000000000      0.1003353477310756
C       0.2000000000000000      0.2027325540540822
C       0.4000000000000000      0.4236489301936018
C       0.8000000000000000       1.098612288668110
C
C$ Restrictions
C
C      The value of the input variable X must be between -1.0 and 1.0,
C      otherwise an error is signalled.
C
C$ Exceptions
C
C      1) If X is not between -1.0 and 1.0, the error
C         SPICE(INVALIDARGUMENT) is signalled.
C
C
C$ Files
C
C      None
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      W.M. Owen       (JPL)
C
C$ Literature_References
C
C      Any good book of mathematical tables and formulae, for example
C      the "Standard Mathematical Tables" published by the Chemical
C      Rubber Company.
C
C$ Version
C
C-     SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C        Set the default function value to either 0, 0.0D0, .FALSE.,
C        or blank depending on the type of the function.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     d.p. arc hyperbolic_tangent
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL            RETURN
 
 
 
 
 
C
C     Set up the error processing.
C
      IF ( RETURN () ) THEN
         DATANH = 0.0D0
         RETURN
      ELSE
         CALL CHKIN ( 'DATANH' )
         DATANH = 0.0D0
      END IF
 
C
C     Check that -1 < X < +1.
C
      IF ( DABS(X) .GE. 1.0D0 ) THEN
 
         CALL SETMSG ( 'DATANH: Argument out of range.' )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )
 
         CALL CHKOUT ( 'DATANH' )
         RETURN
      END IF
 
      DATANH = 0.5D0 * DLOG ( (1.D0+X) / (1.D0-X) )
 
      CALL CHKOUT ( 'DATANH' )
 
      RETURN
      END
