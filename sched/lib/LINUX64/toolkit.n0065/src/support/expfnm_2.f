C$Procedure      EXPFNM_2 ( Expand a filename )
 
      SUBROUTINE EXPFNM_2 ( INSTR, OUTFIL )
 
C$ Abstract
C
C     Given a character string that represents a filename, expand it
C     using a predefined environment variable or DCL symbol to a
C     complete path or to prepend path components to a partial filename.
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
C     FILES
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         INSTR
      CHARACTER*(*)         OUTFIL
 
C     Length of an environment variable or DCL symbol name.
 
      INTEGER               ENVLEN
      PARAMETER           ( ENVLEN = 32 )
 
C     Length of a filename.
 
      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 255 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INSTR      I   The character string to expand into a filename.
C     OUTFIL     O   The expanded filename.
C     ENVLEN     P   Maximum length of an environemt variable or symbol.
C     FNMLEN     P   Maximum length of a filename.
C
C$ Detailed_Input
C
C     INSTR      is the character string to be expanded.
C
C                The input character string must be either
C
C                   1) A defined environment variable having a value
C                      that is a complete path to a file.
C
C                   2) A defined environment variable, representing the
C                      leading directories in a complete path to a file,
C                      followed by a slash, '/', followed by the
C                      remainder of the complete path to a file, e.g.,
C
C                         <environment variable>/mydir1/mydir2/file.dat
C
C                      where the environment variable must begin with a
C                      dollar sign ($).
C
C                   3) A complete filename, which will not be modified.
C
C$ Detailed_Output
C
C     OUTFIL     is the expanded filename. If no expansion could be
C                done, OUTFIL will be blank. OUTFIL may not overwrite
C                INSTR.
C
C$ Parameters
C
C     ENVLEN   The maximum allowed length of an environment variable
C              or DCL symbol name.
C
C     FNMLEN   The maximum length for a filename.
C
C$ Exceptions
C
C     1) If the input string is blank, or has embedded blanks in it,
C        the error SPICE(BADFILENAME) is signalled.
C
C     2) If the expanded filename is too long to fit into the
C        output string, the error SPICE(STRINGTOOSMALL) is signalled.
C
C     3) The output string may not overwrite the input string.
C
C     4) If no expansion of the input string can be done, the
C        output filename is will be blank.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This subroutine takes as input a character string, possibly
C     containing an environment variable or DCL symbol name, that
C     represents a filename. If the character string contains an
C     environment variable or DCL symbol name, indicated by a dollar
C     sign ($) immediately preceeding the environment variable or DCL
C     symbol name, an attempt is made to obtain a value for the
C     specified environment variable or DCL symbol from the operating
C     system. If there is no dollar sign in the input character string,
C     the output filename will be assigned the value of the input
C     character string.
C
C     If successful, the original environment variable or DCL symbol
C     name, including the dollar sign, will be replaced with the value
C     that was obtained, and the resulting character string will be
C     returned as the output filename. If unsuccesful, the the output
C     filename will be blank.
C
C     Environment variable and DCL symbol names may only be used to
C     represent either a complete path to a file or the leading path
C     elements of a complete path to a file. Thus, they must appear
C     first in the input character string. See the examples.
C
C$ Examples
C
C     We provide examples using a UNIX style filename and path. For
C     other environments, the appropriate syntax for filenames and
C     paths must be used.
C
C     Example 1: Passing in a complete path to a filename.
C
C        INSTR  = 'datafile.dat'
C        OUTFIL = 'datafile.dat'
C
C     Example 2: Using an environment variable to specify the complete
C                path to a filename.
C
C        Assume that we have already defined the environment variable
C        or DCL symbol 'DATAFILE' to be 'datafile.dat'. Then we would
C        get the following:
C
C           INSTR  = '$DATAFILE'
C           OUTFIL = 'datafile.dat'
C
C     Example 3: Using an environment variable to specify the leading
C                path elements of a complete path to a filename.
C
C        Assume that we have already defined the environment variable
C        or DCL symbol 'DATAPATH' to be '/home/project/data'. Then we
C        would get the following:
C
C           INSTR  = '$DATAFILE/datafila.dat'
C           OUTFIL = '/home/project/data/datafile.dat'
C
C     Example 4: An incorrect usage of an environment variable.
C
C        Using '/home/$DATAPATH/datafile.dat' as the input string
C        would produce an error because the dollar sign is not the
C        first nonblank character in the input string. in this case,
C        OUTFIL would be blank.
C
C$ Restrictions
C
C     1) This subroutine expects environment variable and DCL symbol
C        names to begin with a dollar sign ($). Failure to do this
C        could lead to unexpected results.
C
C     2) The environment variable or DCL sumbol name must be the first
C        part of the input character string.
C
C     3) Environment variable and DCL symbol names may be at most 32
C        characters in length. Your Mileage may vary depending on the
C        particular environment. See the private subroutine ZZGETENV
C        for details.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-    Beta Version 2.0.0, 20-JAN-1999 (NJB)
C
C        No longer converts environment variables to upper case.
C
C-    Beta Version 1.0.0, 30-MAY-1996 (HAN)
C
C        This version fixes some inconsistencies in the original
C        EXPFNM_1 subroutine.
C
C-&
C
C$ Index_Entry
C
C     expand a filename
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
      INTEGER               POS
      INTEGER               RTRIM
C
C     Local variables
C
      CHARACTER*(ENVLEN)    MYENV
      CHARACTER*(FNMLEN)    MYFIL
      CHARACTER*(FNMLEN)    MYVAL
 
      INTEGER               BLANK
      INTEGER               DOLLAR
      INTEGER               INLEN
      INTEGER               NEED
      INTEGER               OUTLEN
      INTEGER               VALLEN
      INTEGER               SLASH
      INTEGER               VARLEN
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EXPFNM_2' )
      END IF
C
C     If the input filename is blank, that's an error.
C
      IF ( INSTR .EQ. ' ' ) THEN
         OUTFIL = ' '
         CALL SETMSG ( 'The input filename ''#'' was blank.' )
         CALL ERRCH  ( '#', INSTR                            )
         CALL SIGERR ( 'SPICE(BADFILENAME)'                  )
         CALL CHKOUT ( 'EXPFNM_2'                            )
         RETURN
      END IF
C
C     We know the input was not blank, so left justify it and
C     check for embedded blanks.
C
      CALL LJUST ( INSTR, MYFIL )
 
      BLANK = POS( MYFIL(:RTRIM(MYFIL)), ' ', 1 )
 
      IF ( BLANK .NE. 0 ) THEN
         OUTFIL = ' '
         CALL SETMSG ( 'The input filename ''#'' contained' //
     .                 ' embedded blanks.'                   )
         CALL ERRCH  ( '#', MYFIL                            )
         CALL SIGERR ( 'SPICE(BADFILENAME)'                  )
         CALL CHKOUT ( 'EXPFNM_2'                            )
         RETURN
      END IF
C
C     We have two cases that we need to consider:
C
C        1) The input file does not contain a dollar sign. This
C           indicates that it is a complete filename;
C
C        2) The input file has a dollar sign as the first character.
C           This indicates that the input filename has its full name,
C           or leading path components, specified by the value of an
C           environment variable. In this case, we get the environment
C           variable's value and replace the environment variable in
C           the input filename.
C
C     We deal with each of these cases, in order, below.
C
      DOLLAR = POS( MYFIL, '$', 1 )
 
      IF ( DOLLAR .EQ. 0 ) THEN
C
C        The input is assumed to be an actual filename, so set the
C        output to be the input.
C
         OUTFIL = INSTR
 
      ELSE IF ( DOLLAR .EQ. 1 ) THEN
C
C        The input is assumed to contain the name of an environment
C        variable whose value contains a complete path name to a
C        file or the leading path elements that will create a complete
C        path name to a file. To find out which, we look for a forward
C        slash. If there is one, everything between the dollar sign and
C        the first forward slash, noninclusive, is the name of the
C        environment variable. If there are no slashes, the entire
C        input name is the name of the environment variable.
C
         SLASH = POS( MYFIL, '/', 2 )
 
         IF ( SLASH .EQ. 0 ) THEN
            VARLEN = RTRIM ( MYFIL )
         ELSE
            VARLEN = SLASH - 1
         END IF
 
         IF ( VARLEN .GT. ENVLEN ) THEN
            OUTFIL = ' '
            CALL SETMSG ( 'The environment variable name ''#'' is'
     .      //            ' too long. The maximum length for an'
     .      //            ' environment variable name is #.'        )
            CALL ERRCH  ( '#', MYFIL(2:SLASH-1)                     )
            CALL ERRINT ( '#', ENVLEN                               )
            CALL SIGERR ( 'SPICE(STRINGTOOSMALL)'                   )
            CALL CHKOUT ( 'EXPFNM_2'                                )
            RETURN
         END IF
C
C        Remember to skip the dollar sign.
C
         MYENV = MYFIL(2:VARLEN)
 
C
C        Try to get the value of the environment variable. If the
C        environment variable does not exist, a blank string is
C        returned.
C
         CALL ZZGETENV ( MYENV, MYVAL )
C
C        If we got something, use it. We don't make any value
C        judgements.
C
         IF ( MYVAL .EQ. ' ' ) THEN
 
            OUTFIL = ' '
            CALL SETMSG ( 'The environment variable ''#'' was not'
     .      //            ' defined.'                               )
            CALL ERRCH  ( '#', MYENV                                )
            CALL SIGERR ( 'SPICE(NOENVVARIABLE)'                    )
            CALL CHKOUT ( 'EXPFNM_2'                                )
            RETURN
 
         END IF
 
         INLEN  = RTRIM ( MYFIL(2:) )
         VALLEN = RTRIM ( MYVAL     )
         OUTLEN = LEN   ( OUTFIL    )
         NEED   = INLEN - VARLEN + VALLEN
C
C        If the output filename length is not long enough for
C        the substitution, signal an error. Otherwise, substitute
C        in the new value.
C
         IF ( NEED .GT. OUTLEN ) THEN
 
            OUTFIL = ' '
            CALL SETMSG ( 'The expanded filename for the input '   //
     .                    'filename ''#'' exceeded the length '    //
     .                    'of the output filename. The expanded '  //
     .                    'name was # characters too long.'         )
            CALL ERRCH  ( '#', MYFIL                                )
            CALL ERRINT ( '#', NEED - OUTLEN                        )
            CALL SIGERR ( 'SPICE(STRINGTOOSMALL)'                   )
            CALL CHKOUT ( 'EXPFNM_2'                                )
            RETURN
 
         END IF
 
         CALL REPSUB ( MYFIL, 1, VARLEN, MYVAL(:VALLEN), OUTFIL )
 
      ELSE
C
C        There was a dollar sign in a position other than the first
C        nonblank position of the input filename. We do not allow
C        this. If an input filename contains a dollar sign, it must
C        be in the first nonblank position.
C
         OUTFIL = ' '
         CALL SETMSG ( 'The input filename ''#'' contained'
     .   //            ' a dollar sign ($) that was not in'
     .   //            ' the first nonblank position; this'
     .   //            ' is not allowed. See the subroutine'
     .   //            ' EXPFNM_2 for details.'              )
         CALL ERRCH  ( '#', MYFIL                            )
         CALL SIGERR ( 'SPICE(BADFILENAME)'                  )
         CALL CHKOUT ( 'EXPFNM_2'                            )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'EXPFNM_2' )
 
      RETURN
      END
