C$Procedure      DVDOT  ( Derivative of Vector Dot Product, 3-D )
 
      DOUBLE PRECISION FUNCTION DVDOT ( S1, S2 )
 
C$ Abstract
C
C     Compute the derivative of the dot product of two double
C     precision position vectors.
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
 
 
      DOUBLE PRECISION   S1 ( 6 )
      DOUBLE PRECISION   S2 ( 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S1        I     First state vector in the dot product.
C     S2        I     Second state vector in the dot product.
C
C     The function returns the derivative of the dot product <S1,S2>
C
C$ Detailed_Input
C
C     S1      Any state vector.  The componets are in order
C             (x, y, z, dx/dt, dy/dt, dz/dt )
C
C     S2      Any state vector.
C
C$ Detailed_Output
C
C     The function returns the derivative of the dot product of the
C     position portions of the two state vectors S1 and S2.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     Given two state vectors S1 and S2 made up of position and
C     velocity components (P1,V1) and (P2,V2) respectively,
C     DVDOT calculates the derivative of the dot product of P1 and P2,
C     i.e. the time derivative
C
C           d
C           -- < P1, P2 > = < V1, P2 > + < P1, V2 >
C           dt
C
C     where <,> denotes the dot product operation.
C
C$ Examples
C
C     Suppose that given two state vectors (S1 and S2)whose position
C     components are unit vectors, and that we need to compute the
C     rate of change of the angle between the two vectors.
C
C     We know that the Cosine of the angle THETA between them is given
C     by
C
C        COSINE(THETA) = VDOT(S1,S2)
C
C     Thus by the chain rule, the derivative of the angle is given
C     by:
C
C        SINE(THETA) dTHETA/dt = DVDOT(S1,S2)
C
C     Thus for values of THETA away from zero we can compute
C
C     dTHETA/dt as
C
C     DTHETA = DVDOT(S1,S2) / SQRT ( 1 - VDOT(S1,S2)**2 )
C
C     Note that position components of S1 and S2 are parallel, the
C     derivative of the  angle between the positions does not
C     exist.  Any code that computes the derivative of the angle
C     between two position vectors should account for the case
C     when the position components are parallel.
C
C$ Restrictions
C
C     The user is responsible for determining that the states S1 and
C     S2 are not so large as to cause numeric overflow.  In most cases
C     this won't present a problem.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 18-MAY-1995 (WLT)
C
C
C-&
 
 
C$ Index_Entries
C
C     Compute the derivative of a dot product
C
C-&
C
 
      DVDOT = S1(1)*S2(4) + S1(2)*S2(5) + S1(3)*S2(6)
     .      + S1(4)*S2(1) + S1(5)*S2(2) + S1(6)*S2(3)
 
      RETURN
      END
 
 
 
 
 
