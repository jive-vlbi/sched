C$ Abstract
C
C     The parameters below form an enumerated list of the recognized
C     frame types.  They are: INERTL, PCK, CK, TK, DYN.  The meanings
C     are outlined below.
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
C$ Parameters
C
C     INERTL      an inertial frame that is listed in the routine
C                 CHGIRF and that requires no external file to
C                 compute the transformation from or to any other
C                 inertial frame.
C
C     PCK         is a frame that is specified relative to some
C                 INERTL frame and that has an IAU model that
C                 may be retrieved from the PCK system via a call
C                 to the routine TISBOD.
C
C     CK          is a frame defined by a C-kernel.
C
C     TK          is a "text kernel" frame.  These frames are offset
C                 from their associated "relative" frames by a
C                 constant rotation.
C
C     DYN         is a "dynamic" frame.  These currently are
C                 parameterized, built-in frames where the full frame
C                 definition depends on parameters supplied via a 
C                 frame kernel.
C
C     ALL         indicates any of the above classes. This parameter
C                 is used in APIs that fetch information about frames
C                 of a specified class.
C
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 4.0.0, 08-MAY-2012 (NJB)
C
C       The parameter ALL was added to support frame fetch APIs.
C
C-    SPICELIB Version 3.0.0, 28-MAY-2004 (NJB)
C
C       The parameter DYN was added to support the dynamic frame class.
C
C-    SPICELIB Version 2.0.0, 12-DEC-1996 (WLT)
C
C        Various unused frames types were removed and the
C        frame time TK was added.
C
C-    SPICELIB Version 1.0.0, 10-DEC-1995 (WLT)
C
C-&
 
 
 
      INTEGER               INERTL
      PARAMETER           ( INERTL = 1 )
 
      INTEGER               PCK
      PARAMETER           ( PCK    = INERTL + 1 )
 
      INTEGER               CK
      PARAMETER           ( CK     = PCK    + 1 )
 
      INTEGER               TK
      PARAMETER           ( TK     = CK     + 1 )
 
      INTEGER               DYN
      PARAMETER           ( DYN    = TK     + 1 )

      INTEGER               ALL
      PARAMETER           ( ALL    = -1 )

C
C     End of INCLUDE file frmtyp.inc
C
