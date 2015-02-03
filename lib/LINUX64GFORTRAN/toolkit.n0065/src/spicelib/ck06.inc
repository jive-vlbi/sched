C$ Abstract
C
C     Declare parameters specific to CK type 06.
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
C     B.V. Semenov      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 10-MAR-2014 (NJB) (BVS)
C
C-&


C
C     Maximum polynomial degree supported by the current
C     implementation of this CK type. 
C
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 23 )

C
C     Integer code indicating `true':
C
      INTEGER               ITRUE
      PARAMETER           ( ITRUE  =  1 )

C
C     Integer code indicating `false':
C
      INTEGER               IFALSE
      PARAMETER           ( IFALSE = -1 )


C
C     CK type 6 subtype codes:
C 
C
C     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion
C                 and quaternion derivatives only, no angular velocity
C                 vector provided. Quaternion elements are listed
C                 first, followed by derivatives. Angular velocity is
C                 derived from the quaternions and quaternion
C                 derivatives.
C
      INTEGER               C06TP0
      PARAMETER           ( C06TP0 = 0 )


C
C     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion
C                 only. Angular velocity is derived by differentiating
C                 the interpolating polynomials.
C
      INTEGER               C06TP1
      PARAMETER           ( C06TP1 = C06TP0 + 1 )


C
C     Subtype 2:  Hermite interpolation, 14-element packets.
C                 Quaternion and angular angular velocity vector, as
C                 well as derivatives of each, are provided. The
C                 quaternion comes first, then quaternion derivatives,
C                 then angular velocity and its derivatives.
C
      INTEGER               C06TP2
      PARAMETER           ( C06TP2 = C06TP1 + 1 )

C
C     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion
C                 and angular velocity vector provided.  The quaternion
C                 comes first.
C
      INTEGER               C06TP3
      PARAMETER           ( C06TP3 = C06TP2 + 1 )

C
C     Number of subtypes:
C     
      INTEGER               C06NST
      PARAMETER           ( C06NST = 4 )

C
C     Packet sizes associated with the various subtypes:
C
      INTEGER               C06PS0
      PARAMETER           ( C06PS0 = 8 )

      INTEGER               C06PS1
      PARAMETER           ( C06PS1 = 4 )

      INTEGER               C06PS2
      PARAMETER           ( C06PS2 = 14 )

      INTEGER               C06PS3
      PARAMETER           ( C06PS3 = 7 )

C
C     Maximum packet size for type 6:
C
      INTEGER               C06MXZ
      PARAMETER           ( C06MXZ = C06PS2 )

C
C     Minimum packet size for type 6:
C
      INTEGER               C06MNZ
      PARAMETER           ( C06MNZ = C06PS1 )


C
C     The CKPFS record size declared in ckparam.inc must be at least as
C     large as the maximum possible size of a CK type 6 record.
C
C     The largest possible CK type 6 record has subtype 3 (note that
C     records of subtype 2 have half as many epochs as those of subtype
C     3, for a given polynomial degree). A subtype 3 record contains
C
C        - The evaluation epoch
C        - The subtype and packet count
C        - MAXDEG+1 packets of size C06PS3
C        - MAXDEG+1 time tags
C

      INTEGER               MAXRSZ
      PARAMETER           ( MAXRSZ =  4 + (MAXDEG+1)*(C06PS3+1) )

C
C     End of file ck06.inc.
C     
