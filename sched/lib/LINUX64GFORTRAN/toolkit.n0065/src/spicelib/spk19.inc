C$ Abstract
C
C     Declare parameters specific to SPK type 19.
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
C     SPK
C
C$ Keywords
C
C     SPK
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
C-    SPICELIB Version 1.0.0, 07-MAR-2014 (NJB) (BVS)
C
C-&

 
C
C     Maximum polynomial degree supported by the current
C     implementation of this SPK type. 
C
C     The degree is compatible with the maximum degrees
C     supported by types 13 and 21.
C
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 27 )

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
C     SPK type 19 subtype codes:
C 
C
C     Subtype 0:  Hermite interpolation, 12-element packets.
C
      INTEGER               S19TP0
      PARAMETER           ( S19TP0 = 0 )

C
C     Subtype 1:  Lagrange interpolation, 6-element packets.
C
      INTEGER               S19TP1
      PARAMETER           ( S19TP1 = S19TP0 + 1 )

C
C     Packet sizes associated with the various subtypes:
C
      INTEGER               S19PS0
      PARAMETER           ( S19PS0 = 12 )

      INTEGER               S19PS1
      PARAMETER           ( S19PS1 =  6 )

C
C     Number of subtypes:
C
      INTEGER               S19NST
      PARAMETER           ( S19NST = 2 )

C
C     Maximum packet size for type 19:
C
      INTEGER               S19MXZ
      PARAMETER           ( S19MXZ = S19PS0 )

C
C     Minimum packet size for type 19:
C
      INTEGER               S19MNZ
      PARAMETER           ( S19MNZ = S19PS1 )

C
C     The SPKPVN record size declared in spkrec.inc must be at least as
C     large as the maximum possible size of an SPK type 19 record.
C
C     The largest possible SPK type 19 record has subtype 1 (note that
C     records of subtype 0 have half as many epochs as those of subtype
C     1, for a given polynomial degree). A type 1 record contains
C
C        - The subtype and packet count
C        - MAXDEG+1 packets of size S19PS1
C        - MAXDEG+1 time tags
C
      INTEGER               MAXRSZ
      PARAMETER           ( MAXRSZ =  2 + (MAXDEG+1)*(S19PS1+1) )

C
C     End of include file spk19.inc.
C     
