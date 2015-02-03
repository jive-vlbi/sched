C$Procedure CNFIRM ( Return status of a yes/no query )

      SUBROUTINE CNFIRM ( PRMPT, TORF )
      IMPLICIT NONE

C$ Abstract
C
C     Return the .TRUE./.FALSE. status of a query which has a yes/no
C     response.
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
C     PARSING
C     UTILITY
C
C$ Declarations

      EXTERNAL              PROMPT

      CHARACTER*(*)         PRMPT
      LOGICAL               TORF

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PRMPT      I   The prompt used to elicit a yes/no response.
C     TORF       O   The truth value of a yes/no response.
C
C$ Detailed_Input
C
C     PRMPT    The prompt which is used to elicit a yes/no response.
C
C$ Detailed_Output
C
C     TORF     A logical flag which indicates the truth value of a
C              yes/no response to a continue/try again prompt. If the
C              response was equivalent to yes, TORF = .TRUE.. If the
C              response was equivalent to no, TORF = .FALSE..
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)   Any input value that is not equivalent to 'Y', 'YES', 'N'
C          or 'NO' (or lower case equivalents), will cause the routine
C          to redisplay the prompt. A yes/no response MUST be given,
C          there are no implicit values for any other response.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Often a program needs to ask whether or not a user wishes
C     to exercise some option. This routine simplifies the task
C     of converting the answer to a logical value.
C
C     If the response to a yes/no question is logically equivalent
C     to 'YES' the variable TORF will be set to a value of .TRUE.
C     If the response to a yes/no question is logically equivalent
C     to 'NO' the variable TORF will be set to a value of .FALSE.
C     Any other response will cause the routine to redisplay the
C     prompt.
C
C$ Examples
C
C     Suppose you need to ask a user whether or not diagnostic
C     information about the behaviour of a program should be
C     written to a file.  Using this routine, you can easily
C     take the action desired and avoid the details of parsing
C     the user's answer.
C
C        PRMPT = 'Log information to a file? (Yes/No) '
C        OK = .FALSE.
C        CALL CONFRM( PRMPT, OK )
C
C        IF ( OK ) THEN
C
C        ...enable recording diagnostics in the log file.
C
C        ELSE
C
C        ...disable recording of diagnostics.
C
C        END IF
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
C     K.R. Gehringer (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 14-DEC-2010 (EDW)
C
C         Declared PROMPT as EXTERNAL.
C
C-    Beta Version 1.0.0, 09-SEP-1992 (KRG)
C
C-&

C$ Index_Entries
C
C      prompt with a yes/no query and return logical response
C
C-&


C
C     SPICELIB functions
C
C     None.
C

C
C     Local Parameters
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 256 )
C
C     Local Variables
C
      CHARACTER*(LINLEN)    RESPNS
      LOGICAL               YESNO


C
C     Do while we have not gotten a yes/no response
C
      YESNO = .FALSE.
      DO WHILE ( .NOT. YESNO )
C
C        Prompt for a response
C
         CALL PROMPT( PRMPT, RESPNS )
C
C        Left justify the response string, RESPNS, and convert it to
C        uppercase.
C
         CALL LJUST ( RESPNS, RESPNS )
         CALL UCASE ( RESPNS, RESPNS )

         IF ( ( RESPNS .EQ. 'Y' ) .OR. ( RESPNS .EQ. 'YES' ) ) THEN

            TORF  = .TRUE.
            YESNO = .TRUE.

         ELSE IF( ( RESPNS .EQ. 'N' ) .OR. ( RESPNS .EQ. 'NO' ) ) THEN

            TORF  = .FALSE.
            YESNO = .TRUE.

         END IF

      END DO

      RETURN
      END
