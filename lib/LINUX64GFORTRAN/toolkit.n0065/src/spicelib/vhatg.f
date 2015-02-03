C$Procedure    VHATG ( "V-Hat", unit vector along V, general dimension )
 
      SUBROUTINE VHATG ( V1, NDIM, VOUT )
 
C$ Abstract
C
C     Find the unit vector along a double precision vector of
C     arbitrary dimension.
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
 
      INTEGER            NDIM
      DOUBLE PRECISION   V1   ( NDIM )
      DOUBLE PRECISION   VOUT ( NDIM )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   Vector to be normalized.
C     NDIM       I   Dimension of V1 (and also VOUT).
C     VOUT       O   Unit vector V1 / |V1|.
C                    If V1 = 0, VOUT will also be zero.
C
C$ Detailed_Input
C
C     V1       This is any double precision vector of arbitrary
C              dimension.  This routine will detect if V1 the zero
C              vector, and will not attempt to divide by zero.
C
C     NDIM     is the dimension of V1 (and also VOUT).
C
C$ Detailed_Output
C
C     VOUT     VOUT contains the unit vector in the direction of V1. If
C              V1 represents the zero vector, then VOUT will also be the
C              zero vector.  
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
C     VHATG determines the magnitude of V1 and then divides each
C     component of V1 by the magnitude.  This process is highly stable
C     over the whole range of multi-dimensional vectors.
C
C$ Examples
C
C     The following table shows how selected V1 implies VOUT.
C
C        V1                    NDIM            VOUT
C        --------------------------------------------------------
C        (5, 12, 0, 0)           4            (5/13, 12/13, 0, 0)
C        (1D-7, 2D-7, 2D-7)      3            (1/3, 2/3, 2/3)
C
C$ Restrictions
C
C     The relative number of cases whereby floating point overflow may
C     occur is negligible. Thus, no error recovery or reporting scheme
C     is incorporated into this subroutine.
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
C-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
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
C     unitize a n-dimensional vector
C
C-&
 
      DOUBLE PRECISION VNORMG
      DOUBLE PRECISION VMAG
      INTEGER I
C
C  Obtain the magnitude of V1
C
      VMAG = VNORMG (V1,NDIM)
C
C   If VMAG is nonzero, then normalize.  Note that this process is
C   numerically stable: overflow could only happen if VMAG were small,
C   but this could only happen if each component of V1 were small.
C   In fact, the magnitude of any vector is never less than the
C   magnitude of any component.
C
      IF (VMAG.GT.0.D0) THEN
         DO I = 1, NDIM
            VOUT(I) = V1(I) / VMAG
         END DO
      ELSE
         DO I = 1, NDIM
            VOUT(I) = 0.D0
         END DO
      END IF
C
      RETURN
      END
