C$Procedure PRNAME ( PRintable body NAME for BRIEF display )

      SUBROUTINE PRNAME ( OBJECT, SOBJ, P1, WD, P2, SIZE, KERTYP, NAME )

C$ Abstract
C
C     Construct complete printable body name for BRIEF display.
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
C     Construct the printname for an object.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     None.
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               OBJECT ( 3 )
      INTEGER               SOBJ
      CHARACTER*(*)         P1
      CHARACTER*(*)         WD
      CHARACTER*(*)         P2
      INTEGER               SIZE
      CHARACTER*(*)         KERTYP
      CHARACTER*(*)         NAME

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OBJECT     I   IDs: body+frameclass or body+center+frameclass
C     SOBJ       I   Number of elements in OBJECT
C     P1         I   Pattern string for body: p1, p2, .. (see brief.pgm)
C     WD         I   Separator string (e.g. 'w.r.t')
C     P2         I   Pattern string for center: p1, .. (see brief.pgm)
C     SIZE       I   1 (if only P1 is set) or 3 (if P1, WD, P2 are set)
C     KERTYP     I   Kernel type: 'SPK', 'PCK'
C     NAME       O   Complete printable name.
C
C$ Detailed_Input
C
C     See Brief_I/O.
C
C$ Detailed_Output
C
C     See Brief_I/O.
C
C$ Parameters
C
C     TBD.
C
C$ Exceptions
C
C     TBD.
C
C$ Files
C
C     TBD.
C
C$ Particulars
C
C     TBD.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     TBD.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    BRIEF Version 2.0.0, 05-NOV-2007 (BVS)
C
C        Changed calling sequence to include KERTYP. Changed code to
C        map SPK ID for physical object names and PCK frame IDs and
C        frame class IDs to frame names. Added header.
C
C-    BRIEF Version 1.0.0, 14-MAR-1996 (WLT)
C
C        Bill's initial version.
C
C-&


C$ Index_Entries
C
C     get complete printable body name for BRIEF display
C
C-&

      INTEGER               R
      INTEGER               RTRIM


C
C     Get name of the body.
C
      NAME = ' '
      CALL GETNAM ( OBJECT(1), P1, KERTYP, 'OBJECT', NAME )

C
C     Indicate non-inertial frame.
C
      IF ( OBJECT(SOBJ) .NE. 1 ) THEN
         CALL SUFFIX ( '*', 0, NAME )
      END IF

C
C     If center is included, add center name to the printanle name.
C
      IF ( SIZE .GT. 1 ) THEN

         CALL SUFFIX ( WD,      1,  NAME  )
         R  = RTRIM  ( NAME ) + 2

         IF ( R .LT. LEN(NAME) ) THEN
            CALL GETNAM ( OBJECT(2), P2, KERTYP, 'CENTER', NAME(R:) )
         END IF

      END IF

      RETURN
      END
