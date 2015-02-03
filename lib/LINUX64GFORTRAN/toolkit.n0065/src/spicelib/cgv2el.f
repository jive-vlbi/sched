C$Procedure      CGV2EL ( Center and generating vectors to ellipse )
 
      SUBROUTINE CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )
 
C$ Abstract
C
C     Form a SPICELIB ellipse from a center vector and two generating
C     vectors.
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
 
      DOUBLE PRECISION      CENTER (    3 )
      DOUBLE PRECISION      VEC1   (    3 )
      DOUBLE PRECISION      VEC2   (    3 )
      DOUBLE PRECISION      ELLIPS ( UBEL )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CENTER,
C     VEC1,
C     VEC2       I   Center and two generating vectors for an ellipse.
C     ELLIPS     O   The SPICELIB ellipse defined by the input vectors.
C
C$ Detailed_Input
C
C     CENTER,
C     VEC1,
C     VEC2           are a center and two generating vectors defining
C                    an ellipse in three-dimensional space.  The
C                    ellipse is the set of points
C
C                       CENTER  +  cos(theta) VEC1  +  sin(theta) VEC2
C
C                    where theta ranges over the interval (-pi, pi].
C                    VEC1 and VEC2 need not be linearly independent.
C
C$ Detailed_Output
C
C     ELLIPS         is the SPICELIB ellipse defined by the input
C                    vectors.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If VEC1 and VEC2 are linearly dependent, ELLIPS will be
C         degenerate.  SPICELIB ellipses are allowed to represent
C         degenerate geometric ellipses.
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
C$ Examples
C
C     1)  Find the intersecton of an ellipse with a plane.  The ellipse
C         is defined by the vectors CENTER, VEC1, and VEC2.  The plane
C         is defined by the normal vector N and the constant C.
C
C            C
C            C    Make a SPICELIB ellipse.  Make a plane while
C            C    we're at it.
C            C
C                 CALL CGV2EL ( CENTER, VEC1, VEC2,  ELLIPS  )
C                 CALL NVC2PL ( N,      C,           PLANE   )
C
C            C
C            C    Find the intersection of the ellipse and plane.
C            C    NXPTS is the number of intersection points; XPT1
C            C    and XPT2 are the points themselves.
C            C
C                 CALL INELPL ( ELLIPS, PLANE, NXPTS, XPT1, XPT2 )
C
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
C     center and generating vectors to ellipse
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
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
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CGV2EL' )
      END IF
 
C
C     The center of the ellipse is held in the first three elements.
C
      CALL VEQU ( CENTER, ELLIPS(CTRPOS) )
 
C
C     Find the semi-axes of the ellipse.  These may be degenerate.
C
      CALL SAELGV ( VEC1, VEC2, ELLIPS(MAJPOS), ELLIPS(MINPOS) )
 
      CALL CHKOUT ( 'CGV2EL' )
      RETURN
      END
