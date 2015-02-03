C$Procedure      VHAT ( "V-Hat", unit vector along V, 3 dimensions )
 
      SUBROUTINE VHAT ( V1, VOUT )
 
C$ Abstract
C
C      Find the unit vector along a double precision 3-dimensional
C      vector.
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
C      VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION   V1   ( 3 )
      DOUBLE PRECISION   VOUT ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       V1        I     Vector to be normalized.
C       VOUT      O     Unit vector V1 / |V1|.
C                       If V1 = 0, VOUT will also be zero.
C                       VOUT can overwrite V1.
C
C$ Detailed_Input
C
C      V1      This is any double precision, 3-dimensional vector.  If
C              this vector is the zero vector, this routine will detect
C              it, and will not attempt to divide by zero.
C
C$ Detailed_Output
C
C      VOUT    VOUT contains the unit vector in the direction of V1. If
C              V1 represents the zero vector, then VOUT will also be the
C              zero vector.  VOUT may overwrite V1.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      VHAT determines the magnitude of V1 and then divides each
C      component of V1 by the magnitude.  This process is highly stable
C      over the whole range of 3-dimensional vectors.
C
C$ Examples
C
C      The following table shows how selected V1 implies VOUT.
C
C      V1                    VOUT
C      ------------------    ------------------
C      (5, 12, 0)            (5/13, 12/13, 0)
C      (1D-7, 2D-7, 2D-7)    (1/3, 2/3, 2/3)
C
C
C$ Restrictions
C
C      There is no known case whereby floating point overflow may occur.
C      Thus, no error recovery or reporting scheme is incorporated
C      into this subroutine.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C      W.M. Owen       (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     unitize a 3-dimensional vector
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 10-FEB-1989 (HAN) (NJB)
C
C         Contents of the Exceptions section was changed
C         to "error free" to reflect the decision that the
C         module will never participate in error handling.
C         Also, the declaration of the unused variable I was
C         removed.
C-&
 
      DOUBLE PRECISION VNORM
      DOUBLE PRECISION VMAG
C
C  Obtain the magnitude of V1
C
      VMAG = VNORM(V1)
C
C   If VMAG is nonzero, then normalize.  Note that this process is
C   numerically stable: overflow could only happen if VMAG were small,
C   but this could only happen if each component of V1 were small.
C   In fact, the magnitude of any vector is never less than the
C   magnitude of any component.
C
      IF (VMAG.GT.0.D0) THEN
         VOUT(1) = V1(1) / VMAG
         VOUT(2) = V1(2) / VMAG
         VOUT(3) = V1(3) / VMAG
      ELSE
         VOUT(1) = 0.D0
         VOUT(2) = 0.D0
         VOUT(3) = 0.D0
      END IF
 
      RETURN
      END
