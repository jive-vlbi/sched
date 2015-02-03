C$Procedure      M2DIAG ( META/2 diagnostics formatting utility. )
 
      SUBROUTINE M2DIAG ( FILLER, BEGMRK, ENDMRK,
     .                    STRING, SB, SE, MESSGE )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine contains the two entry points M2SERR and M2MARK that
C     are used by META/2 template matching routines.  It serves as
C     a diagnostic formatting utility.
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
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         FILLER
      CHARACTER*(*)         BEGMRK
      CHARACTER*(*)         ENDMRK
      CHARACTER*(*)         STRING
      INTEGER               SB
      INTEGER               SE
      CHARACTER*(*)         MESSGE
 
 
C     See the entry point headers for description of each of the
C     input/output arguements.
 
C$ Detailed_Input
C
C     See individual entry points.
C
C$ Detailed_Output
C
C     See individual entry points.
C
C$ Exceptions
C
C     See individual entry points.
C
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
C     This routine is a dummy that serves as an home for the entry
C     points M2SERR and M2MARK that are utility formatting routines
C     used by the template matching routines of META/2.
C
C$ Examples
C
C     To set the markers and filler used to offset the marked portion
C     of a command that fails syntax checking, call the routine
C
C     M2SERR
C
C     To append a marked command to a diagnostic message call M2MARK.
C
C$ Restrictions
C
C     See the entry points for appropriate restrictions.
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
C     Beta Version 1.0.0, 1-JUN-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     Entry points
C
C     M2MARK
C     M2SERR
C
C
C     SPICELIB functions
C
      INTEGER               LASTNB
C
C     Local variables
C
      INTEGER               PLACE
      INTEGER               B
      INTEGER               E
      INTEGER               BPAD
 
 
      CHARACTER*80          FILL
 
      INTEGER               PAD
 
      CHARACTER*16          EMARK
      CHARACTER*16          BMARK
 
 
      SAVE
 
 
      DATA                  FILL    / ' '       /
      DATA                  PAD     / 1         /
      DATA                  BMARK   / '.....<'  /
      DATA                  EMARK   / '>.....'  /
 
 
      RETURN
 
C$Procedure M2SERR ( Set the META/2 error markers )
 
      ENTRY M2SERR ( FILLER, BEGMRK, ENDMRK  )
 
C$ Abstract
C
C     Set the error markers and padding between the end of the error
C     message and the beginning of the marked copy of the input string
C     in diagnostic messages.
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
C     The META/2 book.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         FILLER
C     CHARACTER*(*)         BEGMRK
C     CHARACTER*(*)         ENDMRK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILLER     I   string to leave between message and marked string
C     BEGMRK     I   String to put at beginning of marked part of string
C     ENDMRK     I   String to put at end of marked part of string
C
C$ Detailed_Input
C
C     FILLER     substring to leave between message and marked string
C
C     BEGMRK     String to put at beginning of marked part of string
C
C     ENDMRK     String to put at end of marked part of string
C
C$ Detailed_Output
C
C     None.
C
C$ Error_Handling
C
C     No errors are detected by this entry point.
C
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
C      This entry point is used to set the space padding between the
C      diagnostic message produced by a META/2 routine and to
C      select what strings that will be used to mark the location
C      of a problem that  occured in in the input string when
C      attempting to match a template.
C
C      Since diagnostic messages can be quite long, it is important
C      to be able to set a space between the end of the diagnostic
C      and the start of the marked string.  If the messages are to
C      be output through use of some kind of string breaking routine
C      such as the NAIF routine CUTSTR.  By selecting the padding
C      sufficiently large you can insure that the message will break
C      before printing the marked string.
C
C$ Examples
C
C      When printing error messages it is handy to have the marked
C      portion of the string appear highlighted.  For a machine that
C      interprets VT100 escape sequences the following markers
C      might prove very effective.
C
C            BEGMRK = '<ESC>[7m'       ! Turn on  reverse video.
C            ENDMRK = '<ESC>[0m'       ! Turn off reverse video.
C
C            SPACE = '      '
C
C            CALL M2SERR ( SPACE, BEGMRK, ENDMRK )
C
C
C      When an diagnostic message comes back, the following will
C      code will ensure that the message is broken nicely and that
C      the marked string begins on a new line.
C
C            BEG  = 1
C            MORE = .TRUE.
C
C            DO WHILE ( MORE )
C
C               CALL  CUTSTR ( CAUSE,         80, ' ,', BEG, END, MORE )
C               WRITE (6,*)    CAUSE(BEG:END)
C
C               BEG = END + 1
C
C            END DO
C
C     Non-printing beginning and ending markers can also be useful
C     in the event that you want to do your own processing of the
C     diagnostic message for display.
C
C
C$ Restrictions
C
C     The marking strings will be truncated to the first 16 characters.
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
C     Version B1.0.0, 7-APR-1988 (WLT) (IMU)
C
C-&
 
 
      PAD   = MIN(80,LEN(FILLER))
      BMARK = BEGMRK
      EMARK = ENDMRK
      FILL  = FILLER
 
      RETURN
 
 
C$Procedure      M2MARK (META/2 Error Marking Utility)
 
      ENTRY  M2MARK ( STRING, SB, SE, MESSGE )
 
C$ Abstract
C
C      This is a utility routine used for constructing diagnostic
C      message for META2.  It is not intended for genereal consumption.
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
C      None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         STRING
C     INTEGER               SB
C     INTEGER               SE
C     CHARACTER*(*)         MESSGE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   String to concatenate to end of a partial message
C     SB         I   Position of first string character to mark.
C     SE         I   Position of last string character to mark.
C     MESSGE    I/O  String to append marked string to and return.
C
C$ Detailed_Input
C
C     STRING     is a string that contains some sequence of characters
C                that should be marked and then appended to a partially
C                constructed message string.
C
C     SB         is the index of the first character in STRING that
C                should be marked for output with some character string.
C
C     SE         is the index of the last character in STRING that
C                should be marked for output with some character string.
C
C     MESSGE     Is a partially constructed string to which the marked
C                string should be appended.
C
C$ Detailed_Output
C
C     MESSGE     is the original string concatenated with the marked
C                string.
C
C$ Exceptions.
C
C     If MESSGE is not long enough to contain everything that should
C     go into it it will be truncated.
C
C
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
C      This is a utility routine for use in constructing messages
C      of the form:
C
C      "The input string contained an unrecognized word SPIM. ||
C       >>SPIM<< THE WHEEL."
C
C       The inputs to the routine are
C
C          The first part of the message
C          The string that was recognized to have some problem
C          The index of the first character of the problem.
C          The index of the last character of the problem.
C
C      The actual effect of this routine is to put the string
C
C         MESSGE(1: LASTNB(MESSGE) + 1 ) // STRING(1   :SB-1         )
C                                        // BMARK (1   :LASTNB(BMARK))
C                                        // STRING(SB  :SE           )
C                                        // EMARK (1   :LASTNB(EMARK))
C                                        // STRING(SB+1:             )
C
C      Into the string MESSGE.
C
C      In fact this is what you would probably do if standard Fortran
C      allowed you to perform these operations with passed length
C      character strings.  Since you cant't this routine does it for
C      you cleaning up the appearance of your code and handling all of
C      the pathologies for you.
C
C$ Examples
C
C      Inputs
C
C         MESSGE = 'I believe the word "FILW" should have been
C                   "FILE" in the input string. || "
C
C         STRING = 'SEND EPHEMERIS TO FILW OUTPUT.DAT'
C                   123456789012345678901234567890123
C
C         SB     = 19
C         SE     = 22
C
C         BMARK  = '>>>'
C         EMARK  = '<<<'
C
C      Output
C
C         MESSGE = 'I believe the word "FILW" should have been
C                   "FILE" in the input string. || SEND EPHEMERIS
C                    TO >>>FILW<<< OUTPUT.DAT'
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
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
C     Version B1.0.0, 17-APR-1988 (WLT)
C
C-&
 
 
C
C                    The end of MESSGE looks like
C
C                        . . . xxx  xxxxxx
C                                             ^
C                                             |
C                                             PLACE = LASTNB(CAUSE)+PAD
C
C
C                    After suffixing STRING to CAUSE with one space
C                    it will look like:
C
C
C                       . . . xx x  xxxxxx     string beginning
C                                              ^
C                                              |
C                                              PLACE + 1
C
C                    and the beginning and end  of the marked string
C                    will be at PLACE + SB and PLACE+SE respectively.
C
 
      B      = LASTNB ( BMARK  )
      E      = LASTNB ( EMARK  )
      BPAD   = LASTNB ( MESSGE ) + 1
 
      IF ( PAD .LT. 1 ) THEN
         PLACE = LASTNB ( MESSGE )
      ELSE
         PLACE              = LASTNB ( MESSGE ) + PAD
         CALL SUFFIX ( STRING, PAD,   MESSGE )
         MESSGE(BPAD:PLACE) = FILL(1:PAD)
      END IF
 
 
      IF ( E .GT. 0 ) THEN
         CALL ZZINSSUB ( MESSGE, EMARK(1:E), PLACE+SE+1, MESSGE )
      END IF
 
      IF ( B .GT. 0 ) THEN
         CALL ZZINSSUB ( MESSGE, BMARK(1:B), PLACE+SB,    MESSGE )
      END IF
 
      RETURN
      END
