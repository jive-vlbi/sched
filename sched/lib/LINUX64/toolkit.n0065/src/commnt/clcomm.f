C$Procedure CLCOMM ( Command line COMMNT subroutine )
 
      SUBROUTINE CLCOMM
      IMPLICIT NONE
 
C$ Abstract
C
C     CLCOMM provides command line funcionality to the COMMNT
C     program.
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
C$ Brief_I/O
C
C     Command line options:
C 
C        -a   indicates add comments to a binary kernel.
C 
C        -e   indicates extract comments from a binary kernel.
C 
C        -r   indicates read the comments in a binary kernel.
C 
C        -d   indicates delete the comments from a binary kernel.
C 
C        -h   displaye the help message
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     Performance of the action defined by the comand line options.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The command line usage of COMMNT is shown below:
C
C     Add comments to a kernel file from a comment file.
C
C        prompt> commnt -a kernel_file comment_file
C
C     Extract comments from a kernel file to a comment file.
C
C        prompt> commnt -e kernel_file comment_file
C
C     Read the comments in a kernel file.
C
C        prompt> commnt -r kernel_file
C
C     Delete all of the comments in a kernel file.
C
C        prompt> commnt -d kernel_file
C
C$ Examples
C
C      None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     For additional information, see the COMMNT User's Guide.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIC Version 1.1.0, 04-AUG-2006 (EDW)
C
C        Replaced the DAFACU call in the add comments block with 
C        SPCAC. Use of DAFACU causes a system error under DOS from
C        CSPICE commnt for N52a and later packages.
C
C            CALL SPCAC  (  KERHAN, COMLUN, ' ', ' ' )
C
C        replaces
C
C            CALL DAFACU ( COMLUN, ' ', ' ', .TRUE., KERHAN )
C
C        Edited header.
C
C-    SPICELIB Version 1.0.1, 23-JAM-1997 (WLT)
C
C        Fixed a typo in the description of usage above.
C
C-    SPICELIB Version 1.0.0, 11-AUG-1995 (KRG)
C
C-&
 
C
C     Spicelib functions
C
      INTEGER               CARDI
      INTEGER               ISRCHC
      INTEGER               WDCNT
 
      LOGICAL               EXISTS
      LOGICAL               FAILED
 
C
C     Parameters
C
C     Set a value for the logical unit which represents the standard
C     output device, commonly a terminal. A value of 6 is widely used,
C     but the Fortran standard does not specify a value, so it may be
C     different for different fortran implementations.
C
      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 6 )
C
C     Lower bound for a SPICELIB CELL data structure.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
C
C     Maximum number of open binary files allowed.
C
      INTEGER               MAXOPN
      PARAMETER           ( MAXOPN = 1 )
C
C     Number of different options.
C
      INTEGER               NUMOPT
      PARAMETER           ( NUMOPT = 5 )
C
C     Number of different option namess.
C
      INTEGER               OPTLEN
      PARAMETER           ( OPTLEN = 16 )
C
C     Maximum length of an option flag, which may be the entire word for
C     the action: 'ADD', 'DELETE', 'EXTRACT', 'HELP', or 'READ'.
C
      INTEGER               FLGLEN
      PARAMETER           ( FLGLEN = 7 )
C
C     Number of characters that must match in the option flag.
C
      INTEGER               FLGMCH
      PARAMETER           ( FLGMCH = 2 )
C
C     Line length.
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 512 )
C
C     Output line length.
C
      INTEGER               OUTLEN
      PARAMETER           ( OUTLEN = 80 )
C
C     Length of a SPICE file architecture.
C
      INTEGER               ARCLEN
      PARAMETER           ( ARCLEN = 3 )
C
C     Length of a SPICE file type.
C
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )
C
C     Filename length.
C
      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 128 )
C
C     Number of help lines.
C
      INTEGER               HLPSIZ
      PARAMETER           ( HLPSIZ = 18 )
C
C     Variables
C
      CHARACTER*(ARCLEN)    ARCH
      CHARACTER*(FLGLEN)    CMDFLG
      CHARACTER*(FNMLEN)    COMFNM
      CHARACTER*(OUTLEN)    HLPMSG(HLPSIZ)
      CHARACTER*(LINLEN)    INPLIN
      CHARACTER*(FNMLEN)    KERFNM
      CHARACTER*(FLGMCH)    OPTFLG(NUMOPT)
      CHARACTER*(OPTLEN)    OPTNAM(NUMOPT)
      CHARACTER*(OPTLEN)    OPTION
      CHARACTER*(LINLEN)    TMPLIN
      CHARACTER*(TYPLEN)    TYPE
      CHARACTER*(OUTLEN)    USAGE
 
      INTEGER               COMLUN
      INTEGER               I
      INTEGER               IDXOPT
      INTEGER               KERHAN
      INTEGER               NUMOPN
      INTEGER               NWORDS
      INTEGER               OPNSET(LBCELL:MAXOPN)
 
      LOGICAL               GOTSOM
      SAVE
C
C     The option flags for the command line.
C
      DATA                  OPTFLG / '-A', '-D', '-E', '-H', '-R' /
C
C     The options to perform.
C
      DATA                  OPTNAM   /
     .                                 'ADD_COMMENTS    ',
     .                                 'DELETE_COMMENTS ',
     .                                 'EXTRACT_COMMENTS',
     .                                 'HELP            ',
     .                                 'READ_COMMENTS   '
     .                               /
C
C     Set up the help message.
C
      HLPMSG(  1 ) = 'Usage:'
      HLPMSG(  2 ) = ' '
      HLPMSG(  3 ) = '   commnt [<-option> [<kernel file>]] ['
     .//             '<comment file>]'
      HLPMSG(  4 ) = ' '
      HLPMSG(  5 ) = 'If no arguments are specified commnt ru'
     .//             'ns in a menu driven,'
      HLPMSG(  6 ) = 'interactive mode.'
      HLPMSG(  7 ) = ' '
      HLPMSG(  8 ) = 'Options:'
      HLPMSG(  9 ) = ' '
      HLPMSG( 10 ) = '   -a   is used to add comments to a bi'
     .//             'nary kernel.'
      HLPMSG( 11 ) = ' '
      HLPMSG( 12 ) = '   -e   is used to extract comments fro'
     .//             'm a binary kernel.'
      HLPMSG( 13 ) = ' '
      HLPMSG( 14 ) = '   -r   is used to read the comments in'
     .//             ' a binary kernel.'
      HLPMSG( 15 ) = ' '
      HLPMSG( 16 ) = '   -d   is used to delete the comments '
     .//             'from a binary kernel.'
      HLPMSG( 17 ) = ' '
      HLPMSG( 18 ) = '   -h   is used to display this message'
     .//             '.'
 
      USAGE = HLPMSG(3)
C
C     Check into the error handling.
C
      CALL CHKIN  ( 'CLCOMMNT' )
C
C     Set the error action to ABORT mode.
C
      CALL ERRACT ( 'SET', 'ABORT' )
C
C     Set the error messages that are printed.
C
      CALL ERRPRT ( 'SET', 'NONE, SHORT, LONG, TRACEBACK' )
C
C     Get the command line arguments.
C
      INPLIN = ' '
 
      CALL GETCML ( INPLIN )
C
C     Store the input command line into a temporary line that we may
C     mangle.
C
      TMPLIN = INPLIN
C
C     If the input line is blank return.
C
      IF ( TMPLIN .EQ. ' ' ) THEN
         CALL CHKOUT ( 'CLCOMMNT' )
         RETURN
      END IF
C
C     Get the option.
C
      CALL NEXTWD ( TMPLIN, CMDFLG, TMPLIN )
      CALL UCASE  ( CMDFLG, CMDFLG         )
C
C     Check to see if the option is one that we recognize. We only want
C     to match FLGMCH characters, including the dash, '-'.
C
      IDXOPT = ISRCHC ( CMDFLG (1:FLGMCH), NUMOPT, OPTFLG )
C
C     If we didn't find the option, then signal an error.
C
      IF ( IDXOPT .EQ. 0 ) THEN
 
         DO I = 1, HLPSIZ
            CALL TOSTDO ( HLPMSG(I) )
         END DO
         CALL BYEBYE('FAILURE')
      END IF

C
C     Get the number of words remaining on the command line. This
C     number should be zero, for '-h', one for '-r' and '-d', or two
C     for '-a' and '-e'.
C
      NWORDS = WDCNT ( TMPLIN )
 
      IF ( NWORDS .GT. 2 ) THEN
         DO I = 1, HLPSIZ
            CALL TOSTDO ( HLPMSG(I) )
         END DO
         CALL BYEBYE('FAILURE')
      END IF

C
C     If we have one word, it should be the name of a kernel file; if
C     two words, the first one should be the name of a kernel file.
C
      IF ( NWORDS .GT. 0 ) THEN

C
C        We should have at least the kernel file.
C
         CALL NEXTWD ( TMPLIN, KERFNM, TMPLIN )

C
C        Check to see if it exists.
C
         IF ( .NOT. EXISTS ( KERFNM ) ) THEN
            CALL SETMSG ( 'The SPICE kernel file ''#'' does'
     .      //            ' not exist.'                        )
            CALL ERRCH  ( '#', KERFNM                          )
            CALL SIGERR ( 'SPICE(FILEDOESNOTEXIST)'            )
         END IF
C
C        Get the file architecture and type, which we do not use, for
C        the kernel file.
C
         CALL GETFAT ( KERFNM, ARCH, TYPE )
 
         IF ( ARCH .EQ. '?' ) THEN
 
            CALL SETMSG ( 'The architecture of the'
     .      //            ' file ''#'' could not be'
     .      //            ' determined.'                   )
            CALL ERRCH  ( '#', KERFNM                      )
            CALL SIGERR ( 'SPICE(BADFILEFORMAT)'           )
 
         ELSE IF (       ( ARCH .NE. 'DAF' )
     .             .AND. ( ARCH .NE. 'DAS' ) ) THEN
 
            CALL SETMSG ( 'The file ''#'' was not a SPICE'
     .      //            ' kernel file. '                )
            CALL ERRCH  ( '#', KERFNM                     )
            CALL SIGERR ( 'SPICE(IMPROPERFILE)'           )
 
         END IF
      END IF

C
C     Set the option.
C
      OPTION = OPTNAM ( IDXOPT )

C
C     Perform the action based on the option selected.
C
      IF ( OPTION .EQ. 'ADD_COMMENTS' ) THEN

C
C        The number of words should be 2; the first is the name of the
C        SPICE kernel file, which we got from the command line above,
C        and the second is the name of the file containing the comments
C        to be added to the kernel file.
C
         IF ( NWORDS .NE. 2 ) THEN
            CALL SETMSG ( 'Usage is: clcommnt -a <kernel file>'
     .      //            ' <comment file>'                      )
            CALL ERRCH  ( '#', USAGE                             )
            CALL SIGERR ( 'SPICE(USAGEERROR)'                    )
         END IF

C
C        We have one word remaining, so lets assume that it is the
C        filename of the comment file.
C
         CALL NEXTWD ( TMPLIN, COMFNM, TMPLIN )

C
C        The comment file should exist.
C
         IF ( .NOT. EXISTS ( COMFNM ) ) THEN
            CALL SETMSG ( 'The file of comments ''#'' to be'
     .      //            ' added to the SPICE kernel file'
     .      //            ' ''#'' does not exist.'             )
            CALL ERRCH  ( '#', COMFNM                          )
            CALL ERRCH  ( '#', KERFNM                          )
            CALL SIGERR ( 'SPICE(FILEDOESNOTEXIST)'            )
         END IF

C
C        Now we get to work.
C
C        We open the comment file.
C
         CALL TXTOPR ( COMFNM, COMLUN )

C
C        Based on the architecture, we open the kernel file, add the
C        comments, and close the kernel file.
C
         IF ( ARCH .EQ. 'DAF' ) THEN
            CALL DAFOPW ( KERFNM, KERHAN )
            CALL SPCAC  (  KERHAN, COMLUN, ' ', ' ' )
            CALL DAFCLS ( KERHAN )
         ELSE IF ( ARCH .EQ. 'DAS' ) THEN
            CALL DASOPW ( KERFNM, KERHAN )
            CALL DASACU ( COMLUN, ' ', ' ', .TRUE., KERHAN )
            CALL DASLLC ( KERHAN )
C            CALL DASCLS ( KERHAN )
         END IF

C
C        Close the comment file.
C
         CLOSE ( COMLUN )
 
      ELSE IF ( OPTION .EQ. 'DELETE_COMMENTS ' ) THEN

C
C        The number of words should be 1. It is the name of the SPICE
C        kernel file, which we got from the command line above.
C
         IF ( NWORDS .NE. 1 ) THEN
            CALL SETMSG ( 'Usage is: clcommnt -d <kernel file>' )
            CALL ERRCH  ( '#', USAGE                            )
            CALL SIGERR ( 'SPICE(USAGEERROR)'                   )
         END IF

C
C        We simply get to work.
C
C        Based on the architecture, we open the kernel file, delete
C        all of the comments, and close the kernel file.
C
         IF ( ARCH .EQ. 'DAF' ) THEN
            CALL DAFOPW ( KERFNM, KERHAN )
            CALL DAFDC  ( KERHAN )
            CALL DAFCLS ( KERHAN )
         ELSE IF ( ARCH .EQ. 'DAS' ) THEN
            CALL DASOPW ( KERFNM, KERHAN )
            CALL DASDC  ( KERHAN )
            CALL DASLLC ( KERHAN )
C            CALL DASCLS ( KERHAN )
         END IF
 
      ELSE IF ( OPTION .EQ. 'EXTRACT_COMMENTS' ) THEN

C
C        The number of words should be 2; the first is the name of the
C        SPICE kernel file, which we got from the command line above,
C        and the second is the name of the file to which comments from
C        the kernel file are to be extracted..
C
         IF ( NWORDS .NE. 2 ) THEN
            CALL SETMSG ( 'Usage is: clcommnt -e <kernel file>'
     .      //            ' <comment file>'                      )
            CALL ERRCH  ( '#', USAGE                             )
            CALL SIGERR ( 'SPICE(USAGEERROR)'                    )
         END IF

C
C        We have one word remaining, so lets assume that it is the
C        filename of the comment file.
C
         CALL NEXTWD ( TMPLIN, COMFNM, TMPLIN )
C
C        The comment file should exist.
C
         IF ( EXISTS ( COMFNM ) ) THEN
            CALL SETMSG ( 'The file ''#'' already exists.'
     .      //            ' Please use a new filename for'
     .      //            ' the file which will contain the'
     .      //            ' comments extracted from the'
     .      //            ' ''#''.'                            )
            CALL ERRCH  ( '#', COMFNM                          )
            CALL ERRCH  ( '#', KERFNM                          )
            CALL SIGERR ( 'SPICE(FILEALREADYEXISTS)'           )
         END IF

C
C        Now we get to work.
C
C        We open the comment file.
C
         CALL TXTOPN ( COMFNM, COMLUN )
C
C        Based on the architecture, we open the kernel file, extract the
C        comments, and close the kernel file.
C
         GOTSOM = .FALSE.
         IF ( ARCH .EQ. 'DAF' ) THEN
            CALL DAFOPR ( KERFNM, KERHAN )
            CALL DAFECU ( KERHAN, COMLUN, GOTSOM )
            CALL DAFCLS ( KERHAN )
         ELSE IF ( ARCH .EQ. 'DAS' ) THEN
            CALL DASOPR ( KERFNM, KERHAN )
            CALL DASECU ( KERHAN, COMLUN, GOTSOM )
            CALL DASCLS ( KERHAN )
         END IF

C
C        If there were no comments in the file, write a message saying
C        that to the comment file and the screen.
C
         IF ( .NOT. GOTSOM ) THEN
            TMPLIN = 'There were no comments in the file ''#''.'
 
            CALL REPMC  ( TMPLIN, '#', KERFNM, TMPLIN )
 
            CALL WRITLN ( ' ',    STDOUT )
            CALL WRITLN ( TMPLIN, STDOUT )
            CALL WRITLN ( ' ',    STDOUT )
 
            CALL WRITLN ( ' ',    COMLUN )
            CALL WRITLN ( TMPLIN, COMLUN )
            CALL WRITLN ( ' ',    COMLUN )
         END IF

C
C        Close the comment file.
C
         CLOSE ( COMLUN )
 
      ELSE IF ( OPTION .EQ. 'HELP ' ) THEN
 
         CALL WRITLN ( ' ', STDOUT            )
         CALL WRITLA ( HLPSIZ, HLPMSG, STDOUT )
         CALL WRITLN ( ' ', STDOUT            )
 
      ELSE IF ( OPTION .EQ. 'READ_COMMENTS ' ) THEN

C
C        The number of words should be 1. It is the name of the SPICE
C        kernel file, which we got from the command line above.
C
         IF ( NWORDS .NE. 1 ) THEN
            CALL SETMSG ( 'Usage is: clcommnt -r <kernel file>' )
            CALL ERRCH  ( '#', USAGE                            )
            CALL SIGERR ( 'SPICE(USAGEERROR)'                   )
         END IF

C
C        We simply get to work.
C
C        Based on the architecture, we open the kernel file, read
C        (extract to standard output) all of the comments, and close the
C        kernel file.
C
         GOTSOM = .FALSE.
         CALL WRITLN ( ' ', STDOUT )
         IF ( ARCH .EQ. 'DAF' ) THEN
            CALL DAFOPR ( KERFNM, KERHAN )
            CALL DAFECU ( KERHAN, STDOUT, GOTSOM )
            CALL DAFCLS ( KERHAN )
         ELSE IF ( ARCH .EQ. 'DAS' ) THEN
            CALL DASOPR ( KERFNM, KERHAN )
            CALL DASECU ( KERHAN, STDOUT, GOTSOM )
            CALL DASCLS ( KERHAN )
         END IF
C
C        If there were no comments in the file, write a message saying
C        that to the comment file and the screen.
C
         IF ( .NOT. GOTSOM ) THEN
            TMPLIN = 'There were no comments in the file ''#''.'
 
            CALL REPMC  ( TMPLIN, '#', KERFNM, TMPLIN )
 
            CALL WRITLN ( TMPLIN, STDOUT )
         END IF
         CALL WRITLN ( ' ', STDOUT )
 
      END IF
 
      IF ( FAILED () ) THEN

C
C     If we failed, reset the error handling and delete the binary file
C     that we were creating if it exists.
C
         CALL RESET
         CALL SCARDI ( 0, OPNSET         )
         CALL CLEARI ( MAXOPN, OPNSET(1) )

C
C        Get the handles for any DAF files which may still be
C        open and close them.
C
         CALL DAFHOF    ( OPNSET )
         NUMOPN = CARDI ( OPNSET )
 
         IF ( NUMOPN .GT. 0 ) THEN
            DO I = 1, NUMOPN
               CALL DAFCLS ( OPNSET(I) )
            END DO
         END IF
         
C
C        Clear out any binary file handles in the open set,
C        OPNSET.
C
         CALL SCARDI ( 0,      OPNSET    )
         CALL CLEARI ( MAXOPN, OPNSET(1) )

C
C        Get the handles for any DAS files which may still be
C        open and close them.
C
         CALL DASHOF    ( OPNSET )
         NUMOPN = CARDI ( OPNSET )
 
         IF ( NUMOPN .GT. 0 ) THEN
 
            DO I = 1, NUMOPN
               CALL DASCLS ( OPNSET(I) )
            END DO
 
         END IF

C
C        Call RESET one more time just in case there was an
C        error closing of deleting the file.
C
         CALL RESET
 
      END IF
C
C     If we make it this far we are done....
C
      CALL CHKOUT ( 'CLCOMMNT' )
      CALL BYEBYE ( 'SUCCESS'  )
      RETURN
 
      END
