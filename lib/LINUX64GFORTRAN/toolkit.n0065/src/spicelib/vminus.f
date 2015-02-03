C$Procedure      VMINUS ( Minus V, "-V", 3 dimensions )
 
      SUBROUTINE VMINUS ( V1, VOUT )
 
C$ Abstract
C
C     Negate a double precision 3-dimensional vector.
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
      DOUBLE PRECISION  VOUT ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   Vector to be negated.
C     VOUT       O   Negated vector -V1.
C
C$ Detailed_Input
C
C     V1      This may be any 3-dimensional, double precision vector.
C
C$ Detailed_Output
C
C     VOUT    This will be the negation (additive inverse) of V1.  It
C             is a 3-dimensional, double precision vector. 
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
C     VMINUS implements (by components) the expression VMINUS = -V1. No
C     error checking is performed since overflow can occur ONLY if the
C     dynamic range of positive floating point numbers is not the same
C     size as the dynamic range of negative floating point numbers AND
C     at least one component of V1 falls outside the common range.  The
C     likelihood of this occuring is so small as to be of no concern.
C
C$ Examples
C
C     The following table shows the output VOUT as a function of the
C     the input V1 from the subroutine VMINUS.
C
C        V1                     VOUT
C        ---------------------------------------
C        (1D0, -2D0, 0D0)       (-1D0, 2D0, 0D0)
C        (0D0, 0D0, 0D0)        (0D0, 0D0, 0D0)
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
C     W.M. Owen       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
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
C     negate a 3-dimensional vector
C
C-&
 
      VOUT(1) = -V1(1)
      VOUT(2) = -V1(2)
      VOUT(3) = -V1(3)
C
      RETURN
      END
