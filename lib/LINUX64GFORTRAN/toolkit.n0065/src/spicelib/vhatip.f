C$Procedure VHATIP ( "V-Hat", 3-d unit vector along V, in place )
 
      SUBROUTINE VHATIP ( V )
 
C$ Abstract
C
C      Scale a three-dimensional vector to unit length.
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
 
      DOUBLE PRECISION   V ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V         I-O  Vector to be normalized/unit vector.
C
C$ Detailed_Input
C
C     V        This is any double precision, 3-dimensional vector.  If
C              this vector is the zero vector, this routine will detect
C              it, and will not attempt to divide by zero.
C
C$ Detailed_Output
C
C     V        V contains the unit vector in the direction of the input
C              vector.  If on input V represents the zero vector, then
C              V will be returned as the zero vector.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) The zero vector is returned if the input value of V is the
C        zero vector.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is provided for situation where it is convenient
C     to scale a vector to unit length in place rather than store
C     the result in a separate variable.  Note that the call
C    
C        CALL VHAT ( V, V )
C
C     is not permitted by the ANSI Fortran 77 standard; this routine
C     can be called instead to achieve the same result.
C
C     VHATIP determines the magnitude of V and then, if the magnitude
C     is non-zero, divides each component of V by the magnitude.  This
C     process is highly stable over the whole range of 3-dimensional
C     vectors.
C
C$ Examples
C
C     The following table shows how selected vectors are mapped to
C     unit vectors
C
C     V on input            V on output
C     ------------------    ------------------
C     (5, 12, 0)            (5/13, 12/13, 0)
C     (1D-7, 2D-7, 2D-7)    (1/3, 2/3, 2/3)
C
C$ Restrictions
C
C     There is no known case whereby floating point overflow may occur.
C     Thus, no error recovery or reporting scheme is incorporated
C     into this subroutine.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     W.M. Owen       (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB) (HAN) (WMO) (WLT)
C
C-&
 
C$ Index_Entries
C
C     unitize a 3-dimensional vector in place
C
C-&
 

 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM

C
C     Local variables
C
      DOUBLE PRECISION      VMAG


C
C     Obtain the magnitude of V.
C
      VMAG = VNORM ( V )

C
C     If VMAG is nonzero, then normalize. Note that this process is
C     numerically stable: overflow could only happen if VMAG were
C     small, but this could only happen if each component of V1 were
C     small. In fact, the magnitude of any vector is never less than
C     the magnitude of any component.
C
      IF ( VMAG .GT. 0.D0 ) THEN

         V(1) = V(1) / VMAG
         V(2) = V(2) / VMAG
         V(3) = V(3) / VMAG

      ELSE

         V(1) = 0.D0
         V(2) = 0.D0
         V(3) = 0.D0

      END IF
 
      RETURN
      END
