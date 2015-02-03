C$Procedure      M2THNQ ( Find a META/2 qualified @then directive )
 
      SUBROUTINE M2THNQ ( STRING, POSITN, LABEL )
      IMPLICIT NONE
 
C$ Abstract
C
C      This utility routine locates a META/2 qualified @then directive
C      and returns the position in the string immediately preceeding
C      the directive as well as the label portion of the directive.
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
C      The META/2 book.
C
C$ Keywords
C
C     PARSING
C     UTILITY
C
C$ Declarations
 
 
      CHARACTER*(*)         STRING
      INTEGER               POSITN
      CHARACTER*(*)         LABEL
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A META/2 language specication string.
C     POSITN     O   The position of the last character before @then(%*)
C     LABEL      O   The label portion of the @then directive.
C
C$ Detailed_Input
C
C     STRING     A META/2 language specication string.
C
C$ Detailed_Output
C
C     POSITN     The index of the last character before a word
C                that begins with '@then('.  If there is no such word
C                POSITN is assigned the index of the last character
C                of the string.
C
C     LABEL      The label portion of the @then directive.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1)  If there is no qualified @then, POSITN is set to the index of
C         the last character of the string and LABEL is set to ' '.
C
C$ Particulars
C
C     This is a utility routine that locates the first character
C     before the first occurance of a substring of the form '@then(%*)'.
C
C     It is intended for use only by META/2.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C     Beta Version 1.0.0, 18-MAY-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               UPTO
 
C
C     Local variables
C
      INTEGER               LENGTH
      INTEGER               I
      INTEGER               J
 
      SAVE
 
 
 
C
C     Get the lengtH of the string.
C
      LENGTH = LEN ( STRING )
 
C
C     See if there is a qualified @then.
C
      POSITN = UPTO( STRING, '@then(', 1 )
 
      IF ( POSITN .EQ. LENGTH ) THEN
         LABEL = ' '
      ELSE
         CALL FNDNWD ( STRING, POSITN, I, J )
 
         IF ( J .LE. I + 6 ) THEN
            POSITN = LENGTH
            LABEL  = ' '
         ELSE
            LABEL  = STRING(I+6:J-1)
         END IF
 
      END IF
 
 
      RETURN
      END
