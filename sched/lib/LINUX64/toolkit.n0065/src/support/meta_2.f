C$Procedure      META_2 ( Percy's interface to META_0 )
 
      SUBROUTINE META_2 ( COMMAND, TEMPS, NTEMPS, TEMP, BTEMP, ERROR )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given a collection of acceptable syntax's and a statement
C     (COMMAND) this routine determines if the statement is
C     syntactically correct.
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
C     The META/2 Book.
C
C$ Keywords
C
C     COMPARE
C     PARSING
C     SEARCH
C
C$ Declarations
 
      CHARACTER*(*)         COMMAND
      CHARACTER*(*)         TEMPS  ( * )
      INTEGER               NTEMPS
      CHARACTER*(*)         TEMP
      INTEGER               BTEMP
      CHARACTER*(*)         ERROR  ( 2 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     COMMAND    I   A candidate PERCY command.
C     TEMPS      I   A collection of language definition statements
C     NTEMPS     I   The number of definition statements
C     TEMP       -   Work space required for comparison of statements.
C     BTEMP      O   The first of the def statements that best matches.
C     ERROR      O   Non-blank if none of the def's match.
C
C$ Detailed_Input
C
C     COMMAND    A candidate PERCY command.
C     TEMPS      A collection of language definition statements
C     NTEMPS     The number of definition statements
C     TEMP       Work space required for comparison of statements.
C                TEMP should be declared to have the same length
C                as the character strings that make up TEMPS.
C
C$ Detailed_Output
C
C     BTEMP      The first of the def statements that best matches.
C     ERROR      Non-blank if none of the def's match.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     Later.
C
C$ Examples
C
C     Later.
C
C$ Restrictions
C
C
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     H.A. Neilan    (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-     META/2 Configured Version 3.0.0, 11-AUG-1995 (WLT)
C
C         The control flow through this routine was modified
C         so that it will now re-try all templates (starting
C         with the best previous match) if a spelling error
C         is encountered.  This should fix the confused
C         responses that META/2 gave occassionally before.
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994
C
C         Added a pretty print formatting capability to the
C         error diagnostics.
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C-    Beta Version 2.0.0, 14-JAN-1993 (HAN)
C
C        Assigned the value 'INTERACTIVE' to the variable MODE, and
C        replaced calls to VTLIB routines with calls to more
C        portable routines.
C
C-    Beta Version 1.0.0, 13-JUL-1988 (WLT) (IMU)
C
C-&
 
 
 
 
C
C     Spice Functions
C
      INTEGER               CARDC
      INTEGER               RTRIM
      LOGICAL               BATCH
C
C     Local variables.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =  6 )
 
      INTEGER               NUMKEY
      PARAMETER           ( NUMKEY = 10 )
 
      INTEGER               KEYLEN
      PARAMETER           ( KEYLEN = 32 )
 
      INTEGER               I
      INTEGER               J
      INTEGER               BSCORE
 
      CHARACTER*32          THNWDS ( LBCELL : 1 )
      INTEGER               SBEG
      LOGICAL               REASON
      INTEGER               CUTOFF
      LOGICAL               PSSTHN
      INTEGER               M2CODE
      INTEGER               SCORE
 
 
      CHARACTER*16          MODE
      LOGICAL               INTRCT
      CHARACTER*600         TRYIT
      CHARACTER*(KEYLEN)    KWORDS ( LBCELL:NUMKEY )
      CHARACTER*(NAMLEN)    KEYNAM   ( NUMKEY )
 
      INTEGER               B
      INTEGER               E
      CHARACTER*(KEYLEN)    PICK
      LOGICAL               FIXIT
      CHARACTER*(128)       MARGNS
      CHARACTER*(128)       STYLE
      CHARACTER*(80)        QUESTN
 
C
C     Saved variables
C
      LOGICAL               PASS1
 
      SAVE
 
C
C     Initial values
C
      DATA                  PASS1   / .TRUE. /
      DATA                  MARGNS  / 'LEFT 1 RIGHT 75 ' /
 
      DATA                ( KEYNAM(I), I=1,10 ) / '1',
     .                                            '2',
     .                                            '3',
     .                                            '4',
     .                                            '5',
     .                                            '6',
     .                                            '7',
     .                                            '8',
     .                                            '9',
     .                                            '10' /
 
C%&END_DECLARATIONS
 
 
 
 
 
C
C     Take care of first pass initializations.
C
      IF ( PASS1 ) THEN
 
         PASS1 = .FALSE.
 
         CALL SSIZEC ( 1,  THNWDS )
         CALL SCARDC ( 0,  THNWDS )
 
         CALL SSIZEC ( NUMKEY, KWORDS )
         CALL SCARDC ( 0,      KWORDS )
 
C
C        Determine if were in batch or interactive mode.
C
         IF ( BATCH() ) THEN
            MODE = 'BATCH'
         ELSE
            MODE = 'INTERACTIVE'
         END IF
 
      END IF
 
      INTRCT = MODE .NE. 'BATCH'
      STYLE  = MARGNS
      CALL SUFFIX ( 'NEWLINE /cr VTAB /vt HARDSPACE , ', 1, STYLE )
 
      I      =   0
      BSCORE = - 1
      M2CODE = - 1
      CUTOFF =  72
      REASON = .TRUE.
 
C
C     Look through the templates until we get a match or we
C     run out of templates to try.
C
      DO I = 1, NTEMPS
 
         SCORE  = 0
         TEMP   = TEMPS(I)
         SBEG   = 1
         M2CODE = 0
 
         CALL M2GMCH ( TEMP,   THNWDS, COMMAND, SBEG,
     .                 REASON, CUTOFF, PSSTHN,
     .                 M2CODE, SCORE,  ERROR  )
C
C        If M2CODE comes back zero, we are done with the work
C        of this routine.
C
         IF ( M2CODE .EQ. 0 ) THEN
            BTEMP = I
            RETURN
         END IF
 
         IF ( SCORE .GT. BSCORE ) THEN
            BSCORE = SCORE
            BTEMP  = I
         END IF
 
      END DO
 
 
C
C     If we get here, we know we didn't have a match.  Examine the
C     highest scoring template to get available diagnostics
C     about the mismatch.
C
 
      TEMP   =  TEMPS(BTEMP)
      SBEG   =  1
      FIXIT  = .TRUE.
      M2CODE =  0
 
      CALL M2GMCH ( TEMP,   THNWDS, COMMAND, SBEG,
     .             .TRUE.,  CUTOFF, PSSTHN,
     .              M2CODE, SCORE,  ERROR          )
 
 
C
C     If we are in interactiive mode and we have a spelling error, we
C     can attempt to fix it.  Note this occurs only if the M2CODE
C     is less than 100 mod 10000.
C
      DO WHILE (      ( MOD(M2CODE,10000) .LT. 100 )
     .            .AND. INTRCT
     .            .AND. FIXIT                       )
 
C
C        Construct a friendly message; display it; and
C        get the user's response as to whether or not the
C        command should be modified.
C
 
         TRYIT = ERROR(1)
 
         CALL PREFIX ( 'Hmmmm.,,,', 1, TRYIT )
         CALL SUFFIX ( '/cr/cr I can repair this if you like.',
     .                  0, TRYIT )
 
         WRITE (6,*)
         CALL NICEIO_3 ( TRYIT, 6, STYLE )
         WRITE (6,*)
         WRITE (6,*)
         WRITE (6,*)
         WRITE (6,*)
 
 
         CALL M2RCVR   ( B, E, KWORDS )
 
         IF ( CARDC( KWORDS ) .EQ. 1 ) THEN
 
            QUESTN = 'Should I change "'
     .               // COMMAND(B:E)
     .               //'" to "'
     .               // KWORDS(1)(1:RTRIM(KWORDS(1)))
     .               //'" ?'
            CALL CNFIRM_1 ( QUESTN(1:RTRIM(QUESTN)), FIXIT )
 
         ELSE
 
            CALL CNFIRM_1 ( 'Should I fix it?', FIXIT )
 
         END IF
 
C
C        If the user has elected to have us fix the command
C        we have a few things to do...
C
         IF ( FIXIT ) THEN
C
C           Look up the suggested fixes.  If there is more than
C           one possibility, see which one the user thinks is
C           best.  Otherwise, no more questions for now.
C
            CALL M2RCVR ( B, E, KWORDS )
 
            IF ( CARDC(KWORDS) .GT. 1 ) THEN
 
               DO I = 1, CARDC(KWORDS) - 4
                  WRITE (6,*)
               END DO
 
               CALL GETOPT_1 ( 'Which word did you mean?',
     .                           CARDC(KWORDS),
     .                           KEYNAM(1),
     .                           NAMLEN,
     .                           KWORDS(1),
     .                           KEYLEN,
     .                           KWORDS(1),
     .                           PICK                        )
 
            ELSE
 
               PICK = KWORDS(1)
 
            END IF
C
C           Make the requested repairs on the command, and
C           redisplay the command.
C
            CALL REPSUB   ( COMMAND, B, E, PICK,    COMMAND )
            CALL CMPRSS   ( ' ',     1,    COMMAND, COMMAND )
            WRITE (6,*)     ' '
            WRITE (6,*)     ' '
            CALL NICEIO_3 ( COMMAND, 6, STYLE       )
            WRITE (6,*)
 
C
C           Look through the templates again until we get a match or we
C           run out of templates to try.  Note however, that this time
C           we will start in a different spot.  We already have a best
C           matching template.  We'll start our search for a match
C           there and simulate a circular list of templates so that
C           we can examine all of them if necessary.
C
            ERROR(1) = ' '
            ERROR(2) = ' '
            BSCORE = - 1
            M2CODE = - 1
            CUTOFF =  72
            REASON = .TRUE.
            J      = BTEMP - 1
 
 
            DO I = 1, NTEMPS
C
C              Get the index of the next template to examine.
C
               J = J + 1
               DO WHILE  ( J .GT. NTEMPS )
                  J = J - NTEMPS
               END DO
C
C              Set the template, score for this template, spot to
C              begin examining it and the M2CODE so far.
C
               TEMP   = TEMPS(J)
               SBEG   = 1
               SCORE  = 0
               M2CODE = 0
 
               CALL M2GMCH ( TEMP,   THNWDS, COMMAND, SBEG,
     .                        REASON, CUTOFF, PSSTHN,
     .                        M2CODE, SCORE,  ERROR  )
C
C              If we get back a zero M2CODE we've got a match
C              This routine's work is done.
C
               IF ( M2CODE .EQ. 0 ) THEN
                  BTEMP = I
                  RETURN
               END IF
 
C
C              Hmmph.  No match.  See if we've got a better
C              matching score so far and then go on to the next
C              template if any are left.
C
               IF ( SCORE .GT. BSCORE ) THEN
                  BSCORE = SCORE
                  BTEMP  = I
               END IF
 
            END DO
 
C
C           If we made it to this point the command doesn't properly
C           match any of the templates.  Get the best match and
C           determine the diagnostics for this template.
C
            TEMP   = TEMPS(BTEMP)
            SBEG   = 1
            M2CODE = 0
            SCORE  = 0
 
            CALL M2GMCH ( TEMP,   THNWDS, COMMAND, SBEG,
     .                    REASON, CUTOFF, PSSTHN,
     .                    M2CODE, SCORE,  ERROR  )
 
 
         END IF
 
      END DO
 
C
C     If you get to this point. We didn't have a match set up
C     the second level of mismatch diagnostics using the best
C     matching template.  (BTEMP already points to it.)
C
      TEMP = TEMPS(BTEMP)
 
      CALL CMPRSS(' ', 1, TEMP, TEMP     )
      CALL PREPSN(              TEMP     )
      CALL PREPSN(              ERROR(2) )
      CALL PREFIX('/cr/cr(-3:-3) ',                      1, ERROR(2))
      CALL PREFIX(TEMP,                                  1, ERROR(2))
      CALL PREFIX('/cr/cr(3:3) ',                        1, ERROR(2))
      CALL PREFIX('a command with the following syntax:',3, ERROR(2))
      CALL PREFIX('I Believe you were trying to enter',  1, ERROR(2))
      CALL PREFIX('META/2:',                             1, ERROR(2))
 
 
      RETURN
 
 
 
 
 
C
C     The following entry point allows user's to adjust the margins
C     of the META/2 error messages.
C
      ENTRY M2MARG ( TEMP )
 
      MARGNS = TEMP
      RETURN
 
      END
