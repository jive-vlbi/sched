C$Procedure      M2EPOC ( Determine whether or not a word is an epoch )
 
      LOGICAL FUNCTION M2EPOC ( WORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     This function is true if the input string is an epoch in the
C     sense of META/2.
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
C     META/2 a language specification language.
C
C$ Keywords
C
C     ALPHANUMERIC
C     ASCII
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
 
      CHARACTER*(*)         WORD
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WORD       I   A character string word
C
C     The function is returned as .TRUE. if word is a META/2 epoch.
C
C$ Detailed_Input
C
C     WORD      is a character string that is assumed to have no
C               spaces between the first and last non-blank characters.
C
C$ Detailed_Output
C
C     M2EPOC    returns as .TRUE. if WORD passes throught TPARSE without
C               error. Otherwise M2EPOC is returned .FALSE.
C
C$ Error_Handling
C
C     None.
CC
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Particulars
C
C     This is a utility routine for the subroutine META2.  It
C     determines whether or not a word is an epoch in the sense
C     of the language META/2.
C
C$ Examples
C
C     WORD                                  M2EPOC
C     -------                               ------
C     SPAM                                  .FALSE.
C     _SPUD                                 .FALSE.
C     1:23:1927                             .TRUE.
C     jan/1/1988                            .TRUE.
C     4-1-1988/24:13:48.28                  .TRUE.
C     1988-MAR-8/23:59:60.281               .TRUE.
C     19:3:1                                .FALSE.
C     88-JAN-89                             .FALSE.
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
C     Version B1.0.0, 22-MAR-1988 (WLT) (IMU)
C
C-&
 
 
 
 
C
C     SPICE functions
C
      LOGICAL               M2TIME
      LOGICAL               M2YEAR
      LOGICAL               M2MON
 
C
C     Local variables
C
      INTEGER               TCODE
      CHARACTER*80          ERROR
 
 
 
      SAVE
 
 
 
 
      IF ( M2TIME(WORD) ) THEN
 
         M2EPOC = .FALSE.
 
      ELSE IF ( M2YEAR(WORD) ) THEN
 
         M2EPOC = .TRUE.
 
      ELSE IF ( M2MON(WORD) ) THEN
 
         M2EPOC = .FALSE.
 
      ELSE
 
         CALL M2CAL ( WORD, ERROR, TCODE )
         M2EPOC =   ( ERROR .EQ.  ' ' )
 
      END IF
 
      RETURN
      END
