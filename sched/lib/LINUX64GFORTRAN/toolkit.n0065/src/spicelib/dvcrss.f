C$Procedure      DVCRSS ( Derivative of Vector cross product )
 
      SUBROUTINE DVCRSS ( S1, S2, SOUT )
 
C$ Abstract
C
C     Compute the cross product of two 3-dimensional vectors
C     and the derivative of this cross product.
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
C     DERIVATIVE
C
C$ Declarations
 
 
      DOUBLE PRECISION    S1   ( 6 )
      DOUBLE PRECISION    S2   ( 6 )
      DOUBLE PRECISION    SOUT ( 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S1        I   Left hand state for cross product and derivative.
C     S2        I   Right hand state for cross product and derivative.
C     SOUT      O   State associated with cross product of positions.
C
C$ Detailed_Input
C
C     S1       This may be any state vector.  Typically, this
C              might represent the apparent state of a planet or the
C              Sun, which defines the orientation of axes of
C              some coordinate system.
C
C     S2       A state vector.
C
C$ Detailed_Output
C
C     SOUT     This variable represents the state associated with the
C              cross product of the position components of S1 and S2.
C              In other words, if S1 = (P1,V1) and S2 = (P2,V2) then
C              SOUT is ( P1xP2, d/dt{ P1xP2 } ).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If S1 and S2 are large in magnitude (taken together,
C        their magnitude surpasses the limit allowed by the
C        computer) then it may be possible to generate a
C        floating point overflow from an intermediate
C        computation even though the actual cross product and
C        derivative may be well within the range of double
C        precision numbers.
C
C        DVCRSS does NOT check the magnitude of S1 or S2 to
C        insure that overflow will not occur.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     DVCRSS calculates the three-dimensional cross product of two
C     vectors and the derivative of that cross product according to
C     the definition.
C
C$ Examples
C
C            S1                    S2                   SOUT
C     -----------------------------------------------------------------
C     (0, 1, 0, 1, 0, 0)  ( 1,  0,  0, 1, 0, 0)  (0, 0, -1, 0,  0, -1 )
C     (5, 5, 5, 1, 0, 0)  (-1, -1, -1, 2, 0, 0)  (0, 0,  0, 0, 11,-11 )
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
C
C
C-&
 
 
C$ Index_Entries
C
C     Compute the derivative of a cross product
C
C-&
C
 
C
C     Local Variables
C
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      DVTMP1 ( 3 )
      DOUBLE PRECISION      DVTMP2 ( 3 )
 
C
C     Calculate the cross product of S1 and S2, store it in VTEMP.
C
      CALL VCRSS ( S1(1), S2(1), VTEMP  )
 
C
C     Calculate the two components of the derivative of S1 x S2.
C
      CALL VCRSS ( S1(4), S2(1), DVTMP1 )
      CALL VCRSS ( S1(1), S2(4), DVTMP2 )
 
C
C     Put all of the pieces into SOUT.
C
      CALL VEQU  ( VTEMP,          SOUT(1) )
      CALL VADD  ( DVTMP1, DVTMP2, SOUT(4) )
 
      RETURN
      END
 
 
