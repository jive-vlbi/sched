C$Procedure      VUPACK ( Unpack three scalar components from a vector )
 
      SUBROUTINE VUPACK ( V, X, Y, Z )
 
C$ Abstract
C
C      Unpack three scalar components from a vector.
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
 
      DOUBLE PRECISION V ( 3 )
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      V          I   Input vector.
C      X,
C      Y,
C      Z          O   Scalar components of the vector.
C
C$ Detailed_Input
C
C      V           is a vector with components V(1) = X
C                                              V(2) = Y
C                                              V(3) = Z
C$ Detailed_Output
C
C      X,
C      Y,
C      Z           are the scalar components of the vector.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Basically, this is just shorthand notation for the common
C      sequence
C
C            X = V(1)
C            Y = V(2)
C            Z = V(3)
C
C      The routine is useful largely for two reasons. First, it
C      reduces the chance that the programmer will make a "cut and
C      paste" mistake, like
C
C            X = V(1)
C            Y = V(1)
C            Z = V(1)
C
C      Second, it makes conversions between equivalent units simpler,
C      and clearer. For instance, the sequence
C
C            X = V(1) * RPD
C            Y = V(2) * RPD
C            Z = V(3) * RPD
C
C      can be replaced by the (nearly) equivalent sequence
C
C            CALL VSCL   ( RPD,  V, V )
C            CALL VUPACK ( V, X, Y, Z )
C
C$ Examples
C
C      See: Detailed_Description.
C
C$ Restrictions
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
C$ Author_and_Institution
C
C      I.M. Underwood  (JPL)
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
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     unpack three scalar components from a vector
C
C-&
 
 
 
 
C
C     Just shorthand, like it says above.
C
      X = V(1)
      Y = V(2)
      Z = V(3)
 
      RETURN
      END
