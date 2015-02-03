C$Procedure      VSUB ( Vector subtraction, 3 dimensions )
 
      SUBROUTINE VSUB ( V1, V2, VOUT )
 
C$ Abstract
C
C     Compute the difference between two 3-dimensional, double
C     precision vectors.
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
 
      DOUBLE PRECISION  V1   ( 3 )
      DOUBLE PRECISION  V2   ( 3 )
      DOUBLE PRECISION  VOUT ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I     First vector (minuend).
C     V2         I     Second vector (subtrahend).
C     VOUT       O     Difference vector, V1 - V2. 
C
C$ Detailed_Input
C
C     V1    This can be any 3-dimensional, double precision vector.
C
C     V2    Ditto.
C
C$ Detailed_Output
C
C     VOUT   This is a 3-dimensional, double precision vector which
C            represents the vector difference, V1 - V2.
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
C     This routine simply performs subtraction between components of V1
C     and V2.  No checking is performed to determine whether floating
C     point overflow has occurred.
C
C$ Examples
C
C     The following table shows the output VOUT as a function of the
C     the input V1 and V2 from the subroutine VSUB.
C
C     V1                  V2              ---> VOUT
C     --------------      --------------       --------------
C     ( 1.0, 2.0, 3.0)    ( 4.0,  5.0, 6.0)    (-3.0, -3.0, -3.0)
C     (1D-7, 1D23,0.0)    (1D24, 1D23, 0.0)    (-1D24, 0.0,  0.0)
C
C$ Restrictions
C
C     The user is required to determine that the magnitude each
C     component of the vectors is within the appropriate range so as
C     not to cause floating point overflow.  No error recovery or
C     reporting scheme is incorporated in this subroutine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.M. Owen       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.2, 07-NOV-2003 (EDW)
C
C        Corrected a mistake in the second example's value
C        for VOUT, i.e. replaced (1D24, 2D23, 0.0) with
C        (-1D24, 0.0, 0.0).
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     3-dimensional vector subtraction
C
C-&
 
      VOUT(1) = V1(1) - V2(1)
      VOUT(2) = V1(2) - V2(2)
      VOUT(3) = V1(3) - V2(3)
 
      RETURN
      END
