C$Procedure MAKNAM ( GET body NAMe for BRIEF display )

      SUBROUTINE MAKNAM ( OBJECT, OBJSIZ, NAMORD, KERTYP, OBJNAM ) 

C$ Abstract
C
C     Construct object name for use in BRIEF's symbol table string
C     coverages.
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

      INTEGER               OBJECT ( * )
      INTEGER               OBJSIZ
      LOGICAL               NAMORD
      CHARACTER*(*)         KERTYP
      CHARACTER*(*)         OBJNAM
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OBJECT     I   IDs: body+frameclass or body+center+frameclass
C     OBJSIZ     I   Number of elements in OBJECT
C     NAMORD     I   Flag indicating whether name ordering is needed 
C     KERTYP     I   Kernel type: 'SPK', 'PCK'
C     NAME       O   Body for use in BRIEF's symbol table for coverages.
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
C     make body name for BRIEF coverage symbol table
C
C-&

C
C     Local parameters.
C     
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 48 )
 
C
C     Local variables.
C
      CHARACTER*(WDSIZE)    NAME
      INTEGER               I
      LOGICAL               FOUND
      DOUBLE PRECISION      X
 
      INTEGER               FRCODE
      INTEGER               CENT
      INTEGER               FRCLSS
      INTEGER               CLSSID


  
      OBJNAM = ' '
 
      DO I = 1, OBJSIZ - 1
 
         NAME = ' '

C
C        If name ordering was requested, try to get a name.
C
         IF ( NAMORD ) THEN
 
C
C           Attemp to map ID to name. 
C
C           For SPK 'object' and 'center' IDs, use BODN2C as they are
C           IDs of physical objects.
C
C           For PCK 'object' IDs (OBJECT(1)), use CCIFRM as they are
C           frame class IDs.
C
C           For PCK 'center' IDs (OBJECT(2)), use FRINFO/FRMNAM as they
C           are frame IDs.
C
            IF      ( KERTYP .EQ. 'SPK' ) THEN

               CALL BODC2N ( OBJECT(I), NAME, FOUND )

            ELSE IF ( KERTYP .EQ. 'PCK' .AND. I .EQ. 1 ) THEN

               CALL CCIFRM ( 2, OBJECT(I), FRCODE, NAME, CENT, FOUND )

            ELSE IF ( KERTYP .EQ. 'PCK' .AND. I .EQ. 2 ) THEN

               CALL FRINFO ( OBJECT(I), CENT, FRCLSS, CLSSID, FOUND )
               IF ( FOUND ) THEN
                  CALL FRMNAM ( OBJECT(I), NAME )
               END IF

            ELSE
               FOUND = .FALSE.
            END IF

C
C           If ID could not be mapped to name, turn ID into a string.
C 
            IF ( .NOT. FOUND ) THEN
               X = DBLE(OBJECT(I))
               CALL DPFMT  ( X,    '+0XXXXXXXXXXX', NAME )
               CALL REPLCH ( NAME, '-', '$',        NAME )
            END IF
 
         ELSE

C
C           If name ordering was not requested, turn ID into a string.
C
            X = DBLE(OBJECT(I))
            CALL DPFMT  ( X,    '+0XXXXXXXXXXX', NAME )
            CALL REPLCH ( NAME, '-', '$',        NAME )

         END IF
 
         CALL SUFFIX ( NAME, 1, OBJNAM )
 
      END DO
 
      RETURN
      END
