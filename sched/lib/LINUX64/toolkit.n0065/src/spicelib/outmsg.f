C$Procedure      OUTMSG ( Output Error Messages )
 
      SUBROUTINE OUTMSG ( LIST )
 
C$ Abstract
C
C     Output error messages.
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
C     ERROR
C
C$ Keywords
C
C     ERROR
C
C$ Declarations
 
      INCLUDE 'errhnd.inc'
 
      CHARACTER*(*)           LIST
 
      INTEGER                FILEN
      PARAMETER            ( FILEN  = 255 )
 
      INTEGER                LL
      PARAMETER            ( LL     =  80 )
 
      INTEGER                NAMLEN
      PARAMETER            ( NAMLEN = 32 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LIST       I   A list of error message types.
C     FILEN      P   Maximum length of file name.
C     NAMLEN     P   Maximum length of module name. See TRCPKG.
C     LL         P   Output line length.
C
C$ Detailed_Input
C
C     LIST           is a list of error message types.  A list is a
C                    character string containing one or more words
C                    from the following list, separated by commas.
C
C                       SHORT
C                       EXPLAIN
C                       LONG
C                       TRACEBACK
C                       DEFAULT
C
C                    Each type of error message specified in LIST will
C                    be output when an error is detected, if it is
C                    enabled for output.  Note that DEFAULT does
C                    NOT refer to the "default message selection,"
C                    but rather to a special message that is output
C                    when the error action is 'DEFAULT'.  This message
C                    is a statement referring the user to the error
C                    handling documentation.
C
C                    Messages are never duplicated in the output; for
C                    instance, supplying a value of LIST such as
C
C                       'SHORT, SHORT'
C
C                    does NOT result in the output of two short
C                    messages.
C
C                    The words in LIST may appear in mixed case;
C                    for example, the call
C
C                       CALL OUTMSG ( 'ShOrT' )
C
C                    will work.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     FILEN          is the maximum device name length that can be
C                    accommodated by this routine.
C
C     NAMELN         is the maximum length of an individual module name.
C
C     LL             is the maximum line length for the output message.
C                    If the output message string is very long, it is
C                    displayed over several lines, each of which has a
C                    maximum length of LL characters.
C
C$ Exceptions
C
C     1)  This routine detects invalid message types in the argument,
C         LIST.   The short error message in this case is
C         'SPICE(INVALIDLISTITEM)'
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      This routine is part of the SPICELIB error handling
C      mechanism.
C
C      This routine outputs the error messages specified in LIST that
C      have been enabled for output (use the SPICELIB routine ERRPRT
C      to enable or disable output of specified types of error
C      messages).  A border is written out preceding and following the
C      messages.  Output is directed to the current error output device.
C
C$ Examples
C
C      1)  Output the short and long error messages:
C
C         C
C         C     Output short and long messages:
C         C
C               CALL OUTMSG ( 'SHORT, LONG' )
C
C$ Restrictions
C
C      1)  This routine is intended for use by the SPICELIB error
C          handling mechanism.  SPICELIB users are not expected to
C          need to call this routine.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      K.R. Gehringer  (JPL)
C      H.A. Neilan     (JPL)
C      M.J. Spencer    (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.27.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 5.26.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 5.25.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 5.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 5.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 5.22.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 5.21.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 5.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 5.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 5.18.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 5.17.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 5.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 5.15.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 5.14.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 5.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 5.12.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 5.11.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 5.10.0, 01-MAR-2009 (NJB)
C
C        Bug fix: truncation of long words in
C        output has been corrected. Local parameter
C        TMPLEN was added and is used in declaration
C        of TMPMSG.
C
C-    SPICELIB Version 5.9.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 5.8.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 5.7.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 5.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 5.5.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 5.4.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 5.3.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 5.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 5.1.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.1.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.1.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-     SPICELIB Version 5.1.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-     SPICELIB Version 5.1.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-     SPICELIB Version 5.1.0, 13-JAN-1999 (BVS)
C
C         ``errhnd.inc'' file was included. Long and short error
C         message lengths parameter declarations were deleted. Long
C         and short error message string sizes were changed to those
C         declared in ``errhnd.inc''.
C
C-     SPICELIB Version 5.0.0, 08-APR-1998 (NJB)
C
C         Module was updated for the PC-LINUX platform.
C
C-     SPICELIB Version 4.0.0, 09-MAY-1996 (KRG)
C
C         Added the toolkit version to the output error message.
C
C         Updated this routine to be consistent with the trace package
C         revisions. This primarily affects the creation of the
C         traceback string.
C
C         Long error messages are now wrapped on word boundaries when
C         they are longer than the output line length. Note that this
C         only happens for long error messages obtained from GETLMS,
C         and not for the error messages displayed by this subroutine
C         and other error handling subroutines that write their own
C         error messages.
C
C-     SPICELIB Version 3.0.0, 09-NOV-1993 (HAN)
C
C         Module was updated to include the value for FILEN
C         for the Silicon Graphics, DEC Alpha-OSF/1, and
C         NeXT platforms. Also, the previous value of 256 for
C         Unix platforms was changed to 255.
C
C-     SPICELIB Version 2.2.0, 12-OCT-1992 (HAN)
C
C        Updated module for multiple environments. Moved the parameter
C        LL to the Declarations section of the header since it's
C        environment dependent.
C
C        The code was also reformatted so that a utility program can
C        create the source file for a specific environment given a
C        master source file.
C
C-     SPICELIB Version 2.1.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 2.1.0, 15-MAY-1991 (MJS)
C
C         Module was updated to include the value of LL for the
C         Macintosh.
C
C-     SPICELIB Version 2.0.0, 28-MAR-1991 (NJB)
C
C         Work-around for MS Fortran compiler error under DOS 3.10
C         was made.  Some substring bounds were simplified using RTRIM.
C         Updates were made to the header to clarify the text and
C         improve the header's appearance.  The default error message
C         was slightly de-uglified.
C
C         The IBM PC version of this routine now uses an output line
C         length of 78 characters rather than 80.  This prevents
C         wrapping of the message borders and default error message.
C
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     None.
C
C-&
 
C$ Revisions
C
C-     SPICELIB Version 5.1.0, 13-JAN-1999 (BVS)
C
C         ``errhnd.inc'' file was included. Long and short error
C         message lengths parameter declarations were deleted. Long
C         and short error message string size were changed to those
C         declared in ``errhnd.inc''.
C
C-     SPICELIB Version 5.0.0, 08-APR-1998 (NJB)
C
C         Module was updated for the PC-LINUX platform.
C
C-     SPICELIB Version 4.0.0, 09-MAY-1996 (KRG)
C
C         Added the toolkit version to the output error message.
C
C         Updated this routine to be consistent with the trace package
C         revisions. This primarily affects the creation of the
C         traceback string.
C
C         Long error messages are now wrapped on word boundaries when
C         they are longer than the output line length. Note that this
C         only happens for long error messages obtained from GETLMS,
C         and not for the error messages displayed by this subroutine
C         and other error handling subroutines that write their own
C         error messages.
C
C-     SPICELIB Version 3.0.0, 9-NOV-1993 (HAN)
C
C         Module was updated to include the value for FILEN
C         for the Silicon Graphics, DEC Alpha-OSF/1, and
C         NeXT platforms. Also, the previous value of 256 for
C         Unix platforms was changed to 255.
C
C-     SPICELIB Version 2.2.0, 12-OCT-1992 (HAN)
C
C        Updated module for multiple environments. Moved the
C        parameter LL to the Declarations section of the header since
C        it's environment dependent.
C
C        The code was also reformatted so that a utility program can
C        create the source file for a specific environment given a
C        master source file.
C
C-     SPICELIB Version 2.1.0, 15-MAY-1991 (MJS)
C
C         Module was updated to include the value of LL for the
C         Macintosh.
C
C-     SPICELIB Version 2.0.0, 28-MAR-1991 (NJB)
C
C         1)  Work-around for MS Fortran compiler error under DOS 3.10
C             was made.  The compiler did not correctly handle code that
C             concatenated strings whose bounds involved the intrinsic
C             MAX function.
C
C         2)  Some substring bounds were simplified using RTRIM.
C
C         3)  Updates were made to the header to clarify the text and
C             improve the header's appearance.
C
C         4)  Declarations were re-organized.
C
C         5)  The default error message was slightly de-uglified.
C
C         6)  The IBM PC version of this routine now uses an output line
C             length of 78 characters rather than 80.  This prevents
C             wrapping of the message borders and default error message.
C
C-     Beta Version 1.3.0, 19-JUL-1989 (NJB)
C
C         Calls to REMSUB removed; blanking and left-justifying used
C         instead.  This was done because REMSUB handles substring
C         bounds differently than in previous versions, and no longer
C         handles all possible inputs as required by this routine.
C         LJUST, which is used now, is error free.
C
C         Also, an instance of .LT. was changed to .LE.   The old code
C         caused a line break one character too soon.  A minor bug, but
C         a bug nonetheless.
C
C         Also, two substring bounds were changed to ensure that they
C         remain greater than zero.
C
C-     Beta Version 1.2.0, 16-FEB-1989 (NJB)
C
C         Warnings added to discourage use of this routine in
C         non-error-handling code.  Parameters section updated to
C         describe FILEN and NAMLEN.
C
C         Declaration of unused function FAILED removed.
C
C-     Beta Version 1.1.0, 06-OCT-1988 (NJB)
C
C         Test added to ensure substring upper bound is greater than 0.
C         REMAIN must be greater than 0 when used as the upper bound
C         for a substring of NAME.  Also, substring upper bound in
C         WRLINE call is now forced to be greater than 0.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               LASTNB
      INTEGER               RTRIM
      INTEGER               WDCNT
 
      LOGICAL               MSGSEL
 
C
C     Local parameters
C
 
C
C     These parameters are system-independent.
C
      CHARACTER*(*)         BORCHR
      PARAMETER           ( BORCHR = '=' )
 
      INTEGER               DEFLEN
      PARAMETER           ( DEFLEN =   4 )
 
      INTEGER               ENDLEN
      PARAMETER           ( ENDLEN =   1 )
 
      INTEGER               MAXWRD
      PARAMETER           ( MAXWRD =   5 )
 
      INTEGER               WORDLN
      PARAMETER           ( WORDLN =   9 )
 
      INTEGER               STRLEN
      PARAMETER           ( STRLEN =   3 )
 
      INTEGER               VERLEN
      PARAMETER           ( VERLEN =  80 )
 
      INTEGER               XLEN
      PARAMETER           ( XLEN   =  80 )
 
      INTEGER               TMPLEN
      PARAMETER           ( TMPLEN = SMSGLN + XLEN )
C
C     Local variables
C
      CHARACTER*(LL)        BORDER
      CHARACTER*(LL)        DEFMSG ( DEFLEN )
      CHARACTER*(FILEN)     DEVICE
      CHARACTER*(LL)        LINE
      CHARACTER*(LMSGLN)    LMSG
      CHARACTER*(NAMLEN)    NAME
      CHARACTER*(LMSGLN)    OUTWRD
      CHARACTER*(SMSGLN)    SMSG
      CHARACTER*(TMPLEN)    TMPMSG
      CHARACTER*(WORDLN)    UPWORD
      CHARACTER*(VERLEN)    VERSN
      CHARACTER*(WORDLN)    WORDS  ( MAXWRD )
      CHARACTER*(XLEN)      XMSG
 
      INTEGER               DEPTH
      INTEGER               I
      INTEGER               INDEX
      INTEGER               LENGTH
      INTEGER               NUMWRD
      INTEGER               REMAIN
      INTEGER               START
      INTEGER               WRDLEN
 
      LOGICAL               DFAULT
      LOGICAL               EXPL
      LOGICAL               FIRST
      LOGICAL               LONG
      LOGICAL               OUTPUT
      LOGICAL               SHORT
      LOGICAL               TRACE
 
C
C     Saved variables
C
      SAVE                  DEFMSG
      SAVE                  FIRST
      SAVE                  BORDER
 
C
C     Initial Values:
C
      DATA DEFMSG(1)( 1:40) /'Oh, by the way:  The SPICELIB error hand'/
      DATA DEFMSG(1)(41:LL) /'ling actions are USER-TAILORABLE.  You'  /
      DATA DEFMSG(2)( 1:40) /'can choose whether the Toolkit aborts or'/
      DATA DEFMSG(2)(41:LL) /' continues when errors occur, which   '  /
      DATA DEFMSG(3)( 1:40) /'error messages to output, and where to s'/
      DATA DEFMSG(3)(41:LL) /'end the output.  Please read the ERROR'  /
      DATA DEFMSG(4)( 1:40) /'"Required Reading" file, or see the rout'/
      DATA DEFMSG(4)(41:LL) /'ines ERRACT, ERRDEV, and ERRPRT.      '  /
 
      DATA FIRST / .TRUE. /
 
C
C     Executable Code:
C
 
C
C     The first time through, set up the output borders.
C
      IF ( FIRST ) THEN
 
         FIRST      = .FALSE.
 
         DO I = 1, LL
            BORDER (I:I) = BORCHR
         END DO
 
      END IF
 
C
C     No messages are to be output which are not specified
C     in LIST:
C
      SHORT  = .FALSE.
      EXPL   = .FALSE.
      LONG   = .FALSE.
      TRACE  = .FALSE.
      DFAULT = .FALSE.
 
 
C     We parse the list of message types, and set local flags
C     indicating which ones are to be output.  If we find
C     a word we don't recognize in the list, we signal an error
C     and continue parsing the list.
C
      CALL LPARSE ( LIST,  ',' ,  MAXWRD,  NUMWRD, WORDS )
 
      DO I = 1, NUMWRD
 
         CALL UCASE ( WORDS(I), UPWORD )
 
         IF ( UPWORD  .EQ. 'SHORT' ) THEN
 
            SHORT  =  .TRUE.
 
         ELSE IF ( UPWORD  .EQ. 'EXPLAIN'   ) THEN
 
            EXPL   =  .TRUE.
 
         ELSE IF ( UPWORD  .EQ. 'LONG'      ) THEN
 
            LONG   =  .TRUE.
 
         ELSE IF ( UPWORD  .EQ. 'TRACEBACK' ) THEN
 
            TRACE  =  .TRUE.
 
         ELSE IF ( UPWORD  .EQ. 'DEFAULT'   ) THEN
 
            DFAULT =  .TRUE.
 
         ELSE
C
C           Unrecognized word!  This is an error...
C
C           We have a special case on our hands; this routine
C           is itself called by SIGERR, so a recursion error will
C           result if this routine calls SIGERR.  So we output
C           the error message directly:
C
            CALL GETDEV ( DEVICE )
 
            CALL WRLINE ( DEVICE,  'SPICE(INVALIDLISTITEM)'            )
            CALL WRLINE ( DEVICE,  ' '                                 )
            CALL WRLINE ( DEVICE,  'OUTMSG:  An invalid message type '
     .      //                     'was specified in the type list. '  )
            CALL WRLINE ( DEVICE,  'The invalid message type was '
     .      //                      WORDS(I)                           )
         END IF
 
      END DO
 
C
C     LIST has been parsed.
C
C     Now, we output those error messages that were specified by LIST
C     and which belong to the set of messages selected for output.
C
 
C
C     We get the default error output device:
C
      CALL GETDEV ( DEVICE )
 
      OUTPUT =        (  SHORT   .AND.  MSGSEL ( 'SHORT'     )  )
     .          .OR.  (  EXPL    .AND.  MSGSEL ( 'EXPLAIN'   )  )
     .          .OR.  (  LONG    .AND.  MSGSEL ( 'LONG'      )  )
     .          .OR.  (  TRACE   .AND.  MSGSEL ( 'TRACEBACK' )  )
     .          .OR.  (  DFAULT  .AND.  MSGSEL ( 'DEFAULT'   )  )
     .          .AND. (  DEVICE  .NE.   'NULL'                  )
 
C
C     We go ahead and output those messages that have been specified
C     in the list and also are enabled for output. The order of the
C     cases below IS significant; the order in which the messages
C     appear in the output depends on it.
C
 
C
C     If there's nothing to output, we can leave now.
C
      IF ( .NOT. OUTPUT ) THEN
         RETURN
      END IF
 
C
C     Write the starting border: skip a line, write the border,
C     skip a line.
C
      CALL WRLINE ( DEVICE, ' '    )
      CALL WRLINE ( DEVICE, BORDER )
      CALL WRLINE ( DEVICE, ' '    )
C
C     Output the toolkit version and skip a line.
C
      CALL TKVRSN ( 'TOOLKIT', VERSN )
 
      LINE = 'Toolkit version: ' // VERSN
 
      CALL WRLINE ( DEVICE, LINE )
      CALL WRLINE ( DEVICE, ' '  )
C
C     Next, we output the messages specified in the list
C     that have been enabled.
C
C     We start with the short message and its accompanying
C     explanation.  If both are to be output, they are
C     concatenated into a single message.
C
      IF  (        (  SHORT .AND. MSGSEL ( 'SHORT'   )  )
     .      .AND.  (  EXPL  .AND. MSGSEL ( 'EXPLAIN' )  )   ) THEN
 
C
C        Extract the short message from global storage; then get
C        the corresponding explanation.
C
         CALL GETSMS ( SMSG       )
         CALL EXPLN  ( SMSG, XMSG )
 
         TMPMSG  =  SMSG( :RTRIM(SMSG) )  // ' -- '// XMSG
 
         CALL WRLINE ( DEVICE, TMPMSG )
         CALL WRLINE ( DEVICE, ' '    )
 
      ELSE IF  (  SHORT .AND. MSGSEL ( 'SHORT'   )  )  THEN
C
C        Output the short error message without the explanation.
C
         CALL GETSMS ( SMSG )
 
         CALL WRLINE ( DEVICE, SMSG  )
         CALL WRLINE ( DEVICE, ' '   )
 
      ELSE IF (  EXPL .AND. MSGSEL ( 'EXPLAIN' )  )  THEN
C
C        Obtain the explanatory text for the short error
C        message and output it:
C
         CALL GETSMS ( SMSG       )
         CALL EXPLN  ( SMSG, XMSG )
 
         CALL WRLINE ( DEVICE, XMSG )
         CALL WRLINE ( DEVICE, ' '  )
 
      END IF
 
      IF (  LONG .AND. MSGSEL ( 'LONG' )  ) THEN
C
C        Extract the long message from global storage and
C        output it:
C
         CALL GETLMS  (  LMSG  )
C
C        Get the number of words in the error message.
C
         NUMWRD = WDCNT ( LMSG )
         LINE   = ' '
         START  = 1
C
C        Format the words into output lines and display them as
C        needed.
C
         DO I = 1, NUMWRD
 
            CALL NEXTWD ( LMSG, OUTWRD, LMSG )
 
            WRDLEN = RTRIM ( OUTWRD )
 
            IF ( START + WRDLEN .LE. LL ) THEN
 
              LINE(START: ) = OUTWRD
              START         = START + WRDLEN + 1
 
            ELSE
 
               IF ( WRDLEN .LE. LL ) THEN
C
C                 We had a short word, so just write the line and
C                 continue.
C
                  CALL WRLINE  ( DEVICE, LINE )
                  START = WRDLEN + 2
                  LINE  = OUTWRD
 
               ELSE
C
C                 We got a very long word here, so we break it up and
C                 write it out. We fit as much of it as we an into line
C                 as possible before writing it.
C
C                 Get the remaining space. If START is > 1 we have at
C                 least one word already in the line, including it's
C                 trailing space, otherwise the line is blank. If line
C                 is empty, we have all of the space available.
C
                  IF ( START .GT. 1 ) THEN
                     REMAIN = LL - START
                  ELSE
                    REMAIN = LL
                  END IF
C
C                 Now we stuff bits of the word into the output line
C                 until we're done, i.e., until we have a word part
C                 that is less than the output length. First, we
C                 check to see if there is a "significant" amount of
C                 room left in the current output line. If not, we
C                 write it and then begin stuffing the long word into
C                 output lines.
C
                  IF ( REMAIN .LT. 10 ) THEN
 
                     CALL WRLINE  ( DEVICE, LINE )
                     LINE   = ' '
                     REMAIN = LL
                     START  = 1
 
                  END IF
C
C                 Stuff the word a chunk at a time into output lines
C                 and write them. After writing a line, we clear the
C                 part of the long word that we just wrote, left
C                 justifying the remaining part before proceeding.
C
                  DO WHILE ( WRDLEN .GT. LL )
 
                     LINE(START:) = OUTWRD(1:REMAIN)
                     CALL WRLINE ( DEVICE, LINE )
 
                     OUTWRD(1:REMAIN) = ' '
 
                     CALL LJUST ( OUTWRD, OUTWRD )
 
                     LINE   = ' '
                     WRDLEN = WRDLEN - REMAIN
                     REMAIN = LL
                     START  = 1
 
                  END DO
C
C                 If we had a part of the long word left, get set up to
C                 append more words from the error message to the output
C                 line. If we finished the word, WRDLEN .EQ. 0, then
C                 START and LINE have already been initialized.
C
                  IF ( WRDLEN .GT. 0 ) THEN
                     START = WRDLEN + 2
                     LINE  = OUTWRD
                  END IF
 
               END IF
 
            END IF
 
         END DO
C
C        We may need to write the remaining part of a line.
C
         IF ( LINE .NE. ' ' ) THEN
            CALL WRLINE  (  DEVICE, LINE )
         END IF
 
         CALL WRLINE  (  DEVICE, ' ' )
 
      END IF
 
 
      IF (  TRACE .AND. MSGSEL ( 'TRACEBACK' )  ) THEN
C
C        Extract the traceback from global storage and
C        output it:
C
         CALL TRCDEP ( DEPTH )
 
         IF ( DEPTH .GT. 0 ) THEN
C
C           We know we'll be outputting some trace information.
C           So, write a line telling the reader what's coming.
C
            CALL WRLINE ( DEVICE, 'A traceback follows.  The '  //
     .                            'name of the highest level '  //
     .                            'module is first.'          )
C
C           While there are more names in the traceback
C           representation, we stuff them into output lines and
C           write the lines out when they are full.
C
            LINE    =   ' '
            REMAIN  =   LL
 
            DO INDEX = 1, DEPTH
C
C              For each module name in the traceback representation,
C              retrieve module name and stuff it into one or more
C              lines for output.
C
C              Get a name and add the call order sign.  We
C              indicate calling order by a ' --> ' delimiter; e.g.
C              "A calls B" is indicated by 'A --> B'.
C
               CALL TRCNAM ( INDEX, NAME )
 
               LENGTH = LASTNB ( NAME )
C
C              If it's the first name, just put it into the output
C              line, otherwise, add the call order sign and put the
C              name into the output line.
C
               IF ( INDEX .EQ. 1 ) THEN
                  CALL SUFFIX ( NAME, 0,  LINE )
                  REMAIN = REMAIN - LENGTH
               ELSE
C
C                 Add the calling order indicator, if it will fit.
C                 If not, write the line and put the indicator as
C                 the first thing on the next line.
C
                  IF( REMAIN .GE. 4 ) THEN
                     CALL SUFFIX ( '-->', 1,  LINE )
                     REMAIN = REMAIN - 4
                  ELSE
                     CALL WRLINE ( DEVICE, LINE )
                     LINE = '-->'
                     REMAIN = LL - 3
                  END IF
C
C                 The name fits or it doesn't. If it does, just add
C                 it, if it doesn't, write it, then make the name
C                 the first thing on the next line.
C
                  IF( REMAIN .GE. LENGTH ) THEN
                     CALL SUFFIX ( NAME, 1,  LINE )
                     REMAIN = REMAIN - LENGTH - 1
                  ELSE
                     CALL WRLINE ( DEVICE, LINE )
                     LINE = NAME
                     REMAIN = LL - LENGTH
                  END IF
 
               END IF
 
            END DO
 
C
C           At this point, no more names are left in the
C           trace representation.  LINE may still contain
C           names, or part of a long name.  If it does,
C           we now write it out.
C
            IF ( LINE .NE. ' ' ) THEN
               CALL WRLINE ( DEVICE, LINE )
            END IF
 
            CALL WRLINE  ( DEVICE, ' '  )
 
         END IF
C
C        At this point, either we have output the trace
C        representation, or the trace representation was
C        empty.
C
      END IF
 
      IF (  DFAULT  .AND. MSGSEL ( 'DEFAULT' )  ) THEN
C
C        Output the default message:
C
         DO I = 1, DEFLEN
            CALL WRLINE  ( DEVICE, DEFMSG (I) )
         END DO
 
         CALL WRLINE  ( DEVICE, ' '  )
 
      END IF
 
C
C     At this point, we've output all of the enabled messages
C     that were specified in LIST.  At least one message that
C     was specified was enabled.
C
C     Write the ending border out:
C
      CALL WRLINE ( DEVICE, BORDER )
 
      RETURN
      END
