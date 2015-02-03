C$Procedure      VEQU ( Vector equality, 3 dimensions )
 
      SUBROUTINE VEQU ( VIN, VOUT )
 
C$ Abstract
C
C      Make one double precision 3-dimensional vector equal to
C      another.
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
C      None.
C
C$ Keywords
C
C      ASSIGNMENT,  VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION  VIN   ( 3 )
      DOUBLE PRECISION  VOUT  ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       VIN       I   3-dimensional double precision vector.
C       VOUT      O   3-dimensional double precision vector set equal
C                     to VIN.
C
C$ Detailed_Input
C
C      VIN      This may be ANY 3-dimensional double precision vector.
C
C$ Detailed_Output
C
C      VOUT    This 3-dimensional double precision vector is set equal
C              to VIN.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      VEQU simply sets each component of VOUT in turn equal to VIN.  No
C      error checking is performed because none is needed.
C
C$ Examples
C
C     Let  STATE be a state vector. The angular momentum vector is
C     determined by the cross product of the position vector and the
C     velocity vector.
C
C      CALL VEQU ( STATE(1), R )
C      CALL VEQU ( STATE(4), V )
C
C      CALL VCRSS ( R, V, H )
C
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.M. Owen       (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
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
C     assign a 3-dimensional vector to another
C
C-&
 
      VOUT(1) = VIN(1)
      VOUT(2) = VIN(2)
      VOUT(3) = VIN(3)
C
      RETURN
      END
