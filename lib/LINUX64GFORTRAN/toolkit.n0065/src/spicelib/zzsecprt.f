C$Procedure ZZSECPRT ( Calculate dot terms for DPSPCE perturbation )
 
      SUBROUTINE ZZSECPRT ( ISYNFL, DG   , DEL   , XNI  ,
     .                      OMEGAO, ATIME, OMGDOT, XLI  ,
     .                      XFACT , XLDOT, XNDOT , XNDDT )
 
C$ Abstract
C
C    Routine to calculate the dot terms for the secular perturbation
C    of a vehicle.
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
C     SECULAR PERTURBATION
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               ISYNFL
 
      DOUBLE PRECISION      OMEGAO
      DOUBLE PRECISION      ATIME
      DOUBLE PRECISION      OMGDOT
      DOUBLE PRECISION      XLI
      DOUBLE PRECISION      DG     ( 10 )
      DOUBLE PRECISION      XFACT
      DOUBLE PRECISION      XLDOT
      DOUBLE PRECISION      XNI
      DOUBLE PRECISION      DEL    (  3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ISYNFL     I   Resonance flag
C     DG         I   Parameter array
C     DEL        I   Parameter array of delta values
C     XNI        I   An intermediate linear term passed from the
C                    calling routine
C     OMEGAO     I   Original argument of perigee
C     ATIME      I   An intermediate time term passed from the calling
C                    routine
C     OMGDOT     I   Time rate of change of argument of perigee
C     XLI        I   An intermediate angular term passed from the
C                    calling routine
C     XFACT      I   The value BFACT - XNQ
C     XLDOT      O   Time rate of change of XL
C     XNDOT      O   Time rate of change of XN
C     XNDDT      O   Time rate of change of XNDOT
C
C$ Detailed_Input
C
C     ISYNFL      is the flag used to indicate the need for resonance
C                 calculations.
C
C     DG          is the parameter array replacing the Dxxxx values.
C
C     DEL         is the parameter array replacing DEL1, DEL2 and DEL3.
C
C     XNI         is an intermediate linear term passed from the main
C                 term for the calculation of XLDOT = XNI  +  XFACT
C
C     OMEGAO      is the original value for the argument of perigee.
C
C     ATIME       is an intermediate time term passed from the main
C                 routine used to calculate the time dependent
C                 argument of perigee term XOMI
C
C     OMGDOT      is the time derivative of the argument of the perigee.
C
C     XLI         is an intermediate angular term
C
C     XFACT       is the value BFACT - XNQ calculated in ZZDPINIT
C
C$ Detailed_Output
C
C     XLDOT       time derivative of the XL term.
C
C     XNDOT       time derivative of the XN term.
C
C     XNDDT       second time derivative of XN, time derivative of the
C                 time derivative.
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
C     None.
C
C$ Particulars
C
C     This subroutine was constructed from a section of code in ZZDPSEC
C     in the original Spacetrack 3 report.  The code block was called
C     using a set of conditional GO TO's.  The block has been written as
C     this subroutine to improve clarity and maintainability and to
C     conform to the NAIF style standard.
C
C$ Examples
C
C     None needed.
C
C$ Restrictions
C
C     1)  This routine should be called only by ZZDPSEC as part of the
C     DPSPCE subroutine package.  It has no other use.
C
C$ Author_and_Institution
C
C     E.D. Wright      (JPL)
C
C$ Literature_References
C
C     Spacetrack 3 report.
C
C$ Version
C
C-   SPICELIB Version 1.1.0, 24-MAR-1999 (EDW)
C
C       Correction made to format of Version descriptions.
C       Name of variable OMGDT changed to OMGDOT to be consistent
C       with name useage in other deep space two line elements
C       routines.
C
C-   SPICELIB Version 1.0.0, MAY-19-1997 (EDW)
C
C
C-&
 
C$ Index_Entries
C
C     perturbed dot terms
C
C-&
 
C
C     Local variables.
C
      DOUBLE PRECISION      X2LI
      DOUBLE PRECISION      X2OMI
      DOUBLE PRECISION      XNDDT
      DOUBLE PRECISION      XNDOT
      DOUBLE PRECISION      XOMI
 
      DOUBLE PRECISION      G22
      PARAMETER           ( G22    = 5.7686396D0  )
 
      DOUBLE PRECISION      G32
      PARAMETER           ( G32    = 0.95240898D0 )
 
      DOUBLE PRECISION      G44
      PARAMETER           ( G44    = 1.8014998D0  )
 
      DOUBLE PRECISION      G52
      PARAMETER           ( G52    = 1.0508330D0  )
 
      DOUBLE PRECISION      G54
      PARAMETER           ( G54    = 4.4108898D0  )
 
      DOUBLE PRECISION      FASX2
      PARAMETER           ( FASX2  = 0.13130908D0 )
 
      DOUBLE PRECISION      FASX4
      PARAMETER           ( FASX4  = 2.8843198D0  )
 
      DOUBLE PRECISION      FASX6
      PARAMETER           ( FASX6  = 0.37448087D0 )
 
 
 
C
C     Calculate the dot terms with respect to the state of the
C     resonance flag.
C
 
      IF ( ISYNFL .EQ. 0 ) THEN
 
C
C        Resonance flag set.
C
 
         XOMI  = OMEGAO + OMGDOT * ATIME
         X2OMI = XOMI   + XOMI
         X2LI  = XLI    + XLI
 
         XNDOT =  DG( 1  )*DSIN( X2OMI + XLI  - G22 )
     .          + DG( 2  )*DSIN( XLI          - G22 )
     .          + DG( 3  )*DSIN( XOMI  + XLI  - G32 )
     .          + DG( 4  )*DSIN(-XOMI  + XLI  - G32 )
     .          + DG( 5  )*DSIN( X2OMI + X2LI - G44 )
     .          + DG( 6  )*DSIN( X2LI         - G44 )
     .          + DG( 7  )*DSIN( XOMI  + XLI  - G52 )
     .          + DG( 8  )*DSIN(-XOMI  + XLI  - G52 )
     .          + DG( 9  )*DSIN( XOMI  + X2LI - G54 )
     .          + DG( 10 )*DSIN(-XOMI  + X2LI - G54 )
 
         XNDDT =  DG( 1  )*DCOS( X2OMI + XLI  - G22 )
     .          + DG( 2  )*DCOS( XLI          - G22 )
     .          + DG( 3  )*DCOS( XOMI  + XLI  - G32 )
     .          + DG( 4  )*DCOS(-XOMI  + XLI  - G32 )
     .          + DG( 7  )*DCOS( XOMI  + XLI  - G52 )
     .          + DG( 8  )*DCOS(-XOMI  + XLI  - G52 )
     .   +2.D0*(  DG( 5  )*DCOS( X2OMI + X2LI - G44 )
     .          + DG( 6  )*DCOS( X2LI         - G44 )
     .          + DG( 9  )*DCOS( XOMI  + X2LI - G54 )
     .          + DG( 10 )*DCOS( XOMI  + X2LI - G54 ))
 
      ELSE
 
C
C        Resonance flag not set
C
 
         XNDOT =  DEL(1)*DSIN(        XLI - FASX2   )
     .          + DEL(2)*DSIN( 2.D0*( XLI - FASX4 ) )
     .          + DEL(3)*DSIN( 3.D0*( XLI - FASX6 ) )
 
         XNDDT =      DEL(1)*DCOS(        XLI - FASX2   )
     .         + 2.D0*DEL(2)*DCOS( 2.D0*( XLI - FASX4 ) )
     .         + 3.D0*DEL(3)*DCOS( 3.D0*( XLI - FASX6 ) )
 
      END IF
 
 
      XLDOT = XNI  +  XFACT
      XNDDT = XNDDT * XLDOT
 
C
C     Hi!  What are you doing way down here?  Did you bring pizza?
C
 
      RETURN
      END
