C$Procedure     PRTRAP
 
      SUBROUTINE PRTRAP ( COMMAND, TRAN )
      IMPLICIT NONE
 
C$ Abstract
C
C     Determine whether the given command should be trapped (left
C     untranslated).
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
C$ Keywords
C
C     PERCY
C
C$ Declarations
 
      CHARACTER*(*)    COMMAND
      LOGICAL          TRAN
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     COMMND     I   PERCY command to be evaluated.
C     TRAN       I   True if further translation is needed.
C
C$ Detailed_Input
C
C     COMMAND    is the input PERCY command. The following commands
C                should not be translated fully. (A moment's thought
C                will show why.)
C
C                        - SHOW SYMBOL <symbol>
C
C                        - INQUIRE <symbol>
C
C                If translation has proceeded far enough for either
C                of these statements to be recognized, then it has
C                gone far enough.
C
C$ Detailed_Output
C
C     TRAN       is true if further translation of COMMAND is okay.
C                If any of the statements mentioned above is recognized,
C                TRAN is false. (This will prevent PERCY from trying
C                to resolve any more symbols.)
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Input_Output_Common
C
C     See 'SYMBOLS.INC'.
C
C$ Detailed_Description
C
C     Get the first three words of COMMAND.
C
C         - If the first two words are SHOW SYMBOL, and the
C           third word is not blank and does not end with '?',
C           then this should be trapped.
C
C         - If the first word is INQUIRE and the second word
C           is not blank and does not end with '?', then this
C           should be trapped.
C
C     If the statement should be trapped, set TRAN to false and return.
C
C$ Examples
C
C     Command                                 Trap?
C     ------------------------------------    -----
C     'SHOW SYMBOL CARROT        '              Y
C     'SHOW SYMBOL               '              N
C     'SHOW SYMBOL SYMBOL_NAME?  '              N
C
C     'INQUIRE PRIMARY_PLANET    '              N
C     'INQUIRE                   '              Y
C     'INQUIRE QUERY_NAME?       '              Y
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W. L. Taber     (JPL)
C     I. M. Underwood (JPL)
C
C$ Version_and_Date
C
C     Version 1, 17-SEP-1986
C
C-&
 
 
C
C     Spicelib Functions
C
      INTEGER               RTRIM
C
C     Local variables
C
      CHARACTER*33          WORD (3)
      INTEGER               LOC
      INTEGER               I
 
 
 
C
C     Get the first three words of COMMAND.
C
      DO I = 1, 3
         CALL NTHWD ( COMMAND, I, WORD(I), LOC )
         CALL UCASE ( WORD(I),    WORD(I)      )
      END DO
 
 
C
C     Is this a SHOW SYMBOL command?
C
      IF ( WORD(1) .EQ. 'SHOW' .AND.  WORD(2) .EQ. 'SYMBOL' ) THEN
 
C
C        The third word must not be blank, and must not end with '?'.
C        (WORD is longer than any allowable symbol or query, so there
C        should always be a blank at the end.)
C
         IF ( WORD(3) .NE. ' ' ) THEN
 
            LOC = RTRIM(WORD(3))
 
            IF ( WORD(3)(LOC:LOC) .NE. '?' ) THEN
 
               TRAN = .FALSE.
               RETURN
 
            END IF
 
         END IF
 
C
C     Is this an INQUIRE command?
C
      ELSE IF ( WORD(1) .EQ. 'INQUIRE' ) THEN
 
C
C        The second word must not be blank, and must not end with '?'.
C
         IF ( WORD(2) .NE. ' ' ) THEN
 
            LOC = RTRIM( WORD(2) )
 
            IF ( WORD(2)(LOC:LOC) .EQ. '?' ) THEN
 
               TRAN = .FALSE.
 
               CALL CHKIN ( 'PRTRAP' )
               CALL SETMSG ( 'INQUIRE commands must be of the '
     .         //            'form INQUIRE <symbol_name>,  You '
     .         //            'have INQUIRE # which is inquiring '
     .         //            'for the value of a query. This '
     .         //            'kind of INQUIRE is not supported. ' )
               CALL ERRCH  ( '#', WORD(2) )
               CALL SIGERR ('INVALID_INQUIRE')
               CALL CHKOUT ( 'PRTRAP' )
               RETURN
 
            END IF
 
         END IF
 
      END IF
 
C
C     No reason to trap this.
C
      TRAN = .TRUE.
 
      RETURN
      END
