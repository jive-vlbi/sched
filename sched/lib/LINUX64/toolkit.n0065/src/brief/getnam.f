C$Procedure GETNAM ( GET body NAMe for BRIEF display )
 
      SUBROUTINE GETNAM ( IDCODE, PATTRN, KERTYP, IDTYPE, NAME )
 
C$ Abstract
C
C     Construct partial body name for BRIEF display.
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
C     None.
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               IDCODE
      CHARACTER*(*)         PATTRN
      CHARACTER*(*)         KERTYP
      CHARACTER*(*)         IDTYPE
      CHARACTER*(*)         NAME

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IDCODE     I   ID code to be mapped to name.
C     PATTRN     I   Pattern string: p1, p2, etc. (see brief.pgm)
C     KERTYP     I   Kernel type string: 'SPK' or 'PCK'
C     IDTYPE     I   Is ID for object ('OBJECT') or center ('CENTER')
C     NAME       O   Name corresponding to ID.
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
C        Changed calling sequence to include KERTYP and IDTYPE. Changed
C        code to map SPK ID for physical object names and PCK frame IDs
C        and frame class IDs to frame names. Added header.
C
C-    BRIEF Version 1.0.0, 14-MAR-1996 (WLT)
C
C        Bill's initial version.
C
C-&
 
 
C$ Index_Entries
C
C     get partial body name for BRIEF display
C
C-&
 
C
C     Local parameters.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 64 )

C
C     Local variables.
C
      CHARACTER*(WDSIZE)    STRING

      INTEGER               FRCODE
      INTEGER               CENT
      INTEGER               FRCLSS
      INTEGER               CLSSID
 
      LOGICAL               FOUND
 
C
C     Attemp to map ID to name. 
C
C     For SPK 'object' and 'center' IDs, use BODN2C as they are IDs of
C     physical objects.
C
C     For PCK 'object' IDs, use CCIFRM as they are frame class IDs.
C
C     For PCK 'center' IDs, use FRINFO/FRMNAM as they are frame IDs.
C
      IF      ( KERTYP .EQ. 'SPK' ) THEN

         CALL BODC2N ( IDCODE, STRING, FOUND )

      ELSE IF ( KERTYP .EQ. 'PCK' .AND. IDTYPE .EQ. 'OBJECT' ) THEN

         CALL CCIFRM ( 2, IDCODE, FRCODE, STRING, CENT, FOUND )

      ELSE IF ( KERTYP .EQ. 'PCK' .AND. IDTYPE .EQ. 'CENTER' ) THEN

         CALL FRINFO ( IDCODE, CENT, FRCLSS, CLSSID, FOUND )
         IF ( FOUND ) THEN
            CALL FRMNAM ( IDCODE, STRING )
         END IF

      ELSE

         FOUND = .FALSE.

      END IF

C
C     If ID could not be mapped to name, turn ID into a string and
C     return.
C 
      IF ( .NOT. FOUND ) THEN
         CALL INTSTR ( IDCODE, NAME )
         RETURN
      END IF

C
C     Depending on requested pattern, append ID to the name.
C 
      IF      (  PATTRN .EQ. 'p1' ) THEN

         CALL SUFFIX ( '(#)',   1,          STRING )
         CALL REPMI  ( STRING, '#', IDCODE, STRING )
         NAME = STRING

      ELSE IF ( PATTRN .EQ. 'p2' ) THEN

         CALL INTSTR ( IDCODE, NAME )

      ELSE

         CALL PREFIX ( '#',     1,          STRING )
         CALL REPMI  ( STRING, '#', IDCODE, STRING )
         NAME = STRING

      END IF
 
      RETURN
      END
