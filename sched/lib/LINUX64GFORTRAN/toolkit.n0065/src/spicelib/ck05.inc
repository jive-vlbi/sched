C$ Abstract
C
C     Declare parameters specific to CK type 05.
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
C     CK
C
C$ Keywords
C
C     CK
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 20-AUG-2002 (NJB)
C
C-&

C
C     CK type 5 subtype codes:
C 
C
C     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion
C                 and quaternion derivatives only, no angular velocity
C                 vector provided. Quaternion elements are listed
C                 first, followed by derivatives. Angular velocity is
C                 derived from the quaternions and quaternion
C                 derivatives.
C
      INTEGER               C05TP0
      PARAMETER           ( C05TP0 = 0 )


C
C     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion
C                 only. Angular velocity is derived by differentiating
C                 the interpolating polynomials.
C
      INTEGER               C05TP1
      PARAMETER           ( C05TP1 = C05TP0 + 1 )


C
C     Subtype 2:  Hermite interpolation, 14-element packets.
C                 Quaternion and angular angular velocity vector, as
C                 well as derivatives of each, are provided. The
C                 quaternion comes first, then quaternion derivatives,
C                 then angular velocity and its derivatives.
C
      INTEGER               C05TP2
      PARAMETER           ( C05TP2 = C05TP1 + 1 )

C
C     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion
C                 and angular velocity vector provided.  The quaternion
C                 comes first.
C
      INTEGER               C05TP3
      PARAMETER           ( C05TP3 = C05TP2 + 1 )

C
C     Packet sizes associated with the various subtypes:
C
      INTEGER               C05PS0
      PARAMETER           ( C05PS0 = 8 )

      INTEGER               C05PS1
      PARAMETER           ( C05PS1 = 4 )

      INTEGER               C05PS2
      PARAMETER           ( C05PS2 = 14 )

      INTEGER               C05PS3
      PARAMETER           ( C05PS3 = 7 )

C
C     End of file ck05.inc.
C     
