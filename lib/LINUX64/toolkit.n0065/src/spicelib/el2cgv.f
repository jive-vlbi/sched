C$Procedure      EL2CGV ( Ellipse to center and generating vectors )
 
      SUBROUTINE EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
 
C$ Abstract
C
C     Convert a SPICELIB ellipse to a center vector and two generating
C     vectors.  The selected generating vectors are semi-axes of the
C     ellipse.
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
C     ELLIPSES
C
C$ Keywords
C
C     ELLIPSE
C     GEOMETRY
C
C$ Declarations
 
      INTEGER               UBEL
      PARAMETER           ( UBEL    =   9 )
 
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      CENTER (    3 )
      DOUBLE PRECISION      SMAJOR (    3 )
      DOUBLE PRECISION      SMINOR (    3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ELLIPS     I   A SPICELIB ellipse.
C     CENTER,
C     SMAJOR,
C     SMINOR     O   Center and semi-axes of ELLIPS.
C
C$ Detailed_Input
C
C     ELLIPS         is a SPICELIB ellipse.
C
C$ Detailed_Output
C
C     CENTER,
C     SMAJOR,
C     SMINOR         are, respectively, a center vector, a semi-major
C                    axis vector, and a semi-minor axis vector that
C                    generate the input ellipse.  This ellipse is the
C                    set of points
C
C                       CENTER + cos(theta) SMAJOR + sin(theta) SMINOR
C
C                    where theta ranges over the interval (-pi, pi].
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     SPICELIB ellipses serve to simplify calling sequences and reduce
C     the chance for error in declaring and describing argument lists
C     involving ellipses.
C
C     The set of ellipse conversion routines is
C
C        CGV2EL ( Center and generating vectors to ellipse )
C        EL2CGV ( Ellipse to center and generating vectors )
C
C     A word about the output of this routine:   the semi-major axis of
C     an ellipse is a vector of largest possible magnitude in the set
C
C        cos(theta) VEC1  +  sin(theta) VEC2,
C
C     where theta is in the interval (-pi, pi].  There are two such
C     vectors; they are additive inverses of each other. The semi-minor
C     axis is an analogous vector of smallest possible magnitude.  The
C     semi-major and semi-minor axes are orthogonal to each other.  If
C     SMAJOR and SMINOR are choices of semi-major and semi-minor axes,
C     then the input ellipse can also be represented as the set of
C     points
C
C
C        CENTER + cos(theta) SMAJOR + sin(theta) SMINOR
C
C     where theta ranges over the interval (-pi, pi].
C
C$ Examples
C
C     1)  Find the semi-axes of the limb of an ellipsoid.
C
C         C
C         C     Our viewing location is VIEWPT.  The radii of the
C         C     ellipsoid are A, B, and C.
C         C
C               CALL EDLIMB ( A, B, C, VIEWPT, LIMB )
C
C               CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )
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
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     ellipse to center and generating vectors
C
C-&
 
 
 
C
C     Local parameters
C
 
C
C     SPICELIB ellipses contain a center vector, a semi-major
C     axis vector, and a semi-minor axis vector.  These are
C     located, respectively, in elements
C
C        CTRPOS through CTRPOS + 1
C
C        MAJPOS through MAJPOS + 1
C
C        MINPOS through MINPOS + 1
C
C
      INTEGER               CTRPOS
      PARAMETER           ( CTRPOS = 1 )
 
      INTEGER               MAJPOS
      PARAMETER           ( MAJPOS = 4 )
 
      INTEGER               MINPOS
      PARAMETER           ( MINPOS = 7 )
 
 
C
C     The center of the ellipse is held in the first three elements.
C     The semi-major and semi-minor axes come next.
C
      CALL VEQU ( ELLIPS(CTRPOS), CENTER )
      CALL VEQU ( ELLIPS(MAJPOS), SMAJOR )
      CALL VEQU ( ELLIPS(MINPOS), SMINOR )
 
      RETURN
      END
