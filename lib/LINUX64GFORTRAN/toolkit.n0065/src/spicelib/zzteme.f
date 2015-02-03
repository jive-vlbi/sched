C$Procedure ZZTEME ( J2000 to TEME at epoch )

      SUBROUTINE ZZTEME( ET, MT )
      IMPLICIT NONE

C$ Abstract
C
C     J2000 to TEME, probably.
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
C     FRAMES
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations

      DOUBLE PRECISION    ET
      DOUBLE PRECISION    MT(6,6)

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch of the state transformation operator.
C     MT         O   The state transformation operator.
C
C$ Detailed_Input
C
C     ET         time expressed in terms of TDB seconds past epoch J2000
C                for which to to calculate the J2000 frame to TEME frame
C                transformation operator.
C
C$ Detailed_Output
C
C     MT         the 6x6 operator that transforms cartesian states from
C                the J2000 to TEME reference frames.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     TETE    True equator true equinox
C     MEME    Mean equator mean equinox
C     TEME    True equator mean equinox
C
C$ Examples
C
C     None.
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
C     N.J. Bachman   (JPL)
C     S.C. Krening   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-MAR-2012 (NJB) (SCK) (EDW)
C
C-&

C$ Index_Entries
C
C   tranformation J2000 frame to pseudo TEME
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      M1     ( 6, 6 )
      DOUBLE PRECISION      M1INV  ( 6, 6 )
      DOUBLE PRECISION      M2     ( 6, 6 )
      DOUBLE PRECISION      M2INV  ( 6, 6 )
      DOUBLE PRECISION      XTEMP  ( 6, 6 )
      DOUBLE PRECISION      XJ2000 ( 6 )
      DOUBLE PRECISION      Z      ( 6 )
      DOUBLE PRECISION      ZJ2000 ( 6 )

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZTEME' )

C
C     Extract the MEME +X vector and its derivative, both
C     expressed relative to the J2000 frame, into X.
C
      CALL ZZEPRC76( ET, M1    )
      CALL INVSTM  ( M1, M1INV )

      CALL MOVED ( M1INV(1,1), 6, XJ2000 )

C
C     Extract the TETE +Z vector and its derivative, both
C     expressed relative to the MEME frame, into Z.
C
      CALL ZZENUT80( ET, M2    )
      CALL INVSTM  ( M2, M2INV )

      CALL MOVED ( M2INV(1,3), 6, Z )

C
C     Transform Z to the J2000 frame.
C
      CALL MXVG ( M1INV, Z, 6, 6, ZJ2000 )

C
C     Compute the TEME to J2000 state transformation;
C     invert this to produce the output matrix.
C
      CALL ZZTWOVXF ( ZJ2000, 3, XJ2000, 1, XTEMP )
      CALL INVSTM   ( XTEMP,  MT )

      CALL CHKOUT ( 'ZZTEME' )
      RETURN
      END
