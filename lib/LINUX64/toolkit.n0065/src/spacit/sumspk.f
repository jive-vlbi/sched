C$Procedure      SUMSPK ( Summarize an SPK file )

      SUBROUTINE SUMSPK ( HANDLE, BINFNM, LPSFNM, LOGFIL, LOGLUN )

C$ Abstract
C
C     Summarize an SPK file.
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
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      CHARACTER*(*)         BINFNM
      CHARACTER*(*)         LPSFNM
      LOGICAL               LOGFIL
      INTEGER               LOGLUN

      INTEGER               NDC
      PARAMETER           ( NDC = 2 )

      INTEGER               NIC
      PARAMETER           ( NIC = 6 )

      INTEGER               NC
      PARAMETER           ( NC = NDC + ( NIC + 1 )/2 )

      INTEGER               IDSIZ
      PARAMETER           ( IDSIZ = NC * 8 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of the SPK file to be summarized.
C     LOGFIL     I   Write the summary to a log file and to screen?
C     LOGLUN     I   Logical unit connected to the log file.
C     NDC        P   Number of d.p. components in SPK descriptor.
C     NIC        P   Number of integer components in SPK descriptor.
C     NC         P   Size of packed SPK descriptor.
C     IDSIZ      P   Length of SPK segment identifier.
C     LBCELL     P   Lower bound for the SPICELIB CELL data structure.
C
C$ Detailed_Input
C
C     HANDLE     is the integer handle of the SPK file to be summarized.
C
C     LOGFIL     if TRUE means that the summary will be written to
C                a log file as well as displayed on the terminal
C                screen.  Otherwise, the summary will not be written
C                to a file.
C
C     UNIT       is the logical unit connected to a log file to which
C                the summary is to be written if LOGFIL is TRUE.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     NDC        is the number of double precision components in an
C                unpacked SPK descriptor.
C
C     NIC        is the number of integer components in an unpacked
C                SPK descriptor.
C
C     NC         is the size of a packed SPK descriptor.
C
C     IDSIZ      is the length of an SPK segment identifier.
C
C     LBCELL     is the lower bound for the SPICELIB CELL data
C                structure.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     The SPK file to be summarized is referred throughout this routine
C     by its handle. The file should already be opened for read.
C
C$ Particulars
C
C
C
C$ Restrictions
C
C
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     M.J. Spencer    (JPL)
C     J.E. McLean     (JPL)
C     R.E. Thurman    (JPL)
C
C$ Version
C
C-    Beta Version 5.0.0  21-JUL-1995 (KRG)
C
C        Added several arguments to the call of this subroutine and
C        made other modifications to allow it to perform its own 
C        formatting of the summary, including filenames and separators.
C
C-    Beta Version 4.0.0  11-APR-1994 (KRG)
C
C        Modified this routine to make use of new routines to get and
C        format and write SPK segment summaries.
C
C        Added a missing $ Index_Entries header section.
C
C        Fixed a few typos in the header.
C
C        The routine DISPSP is now obsolete. It is no longer used.
C
C-    Beta Version 3.0.0  17-MAR-1993 (KRG)
C
C        1) Changed the names of the variable TOFILE and UNIT to LOGFIL
C           and LOGLUN, respectively.
C
C        2) Updated the program to use the menuing subroutine GETOPT
C           which removes the need for the routine QSUMSP. Redesigned
C           the CASE sructure of the code to facilitate the use of the
C           menuing routine.
C
C        3) Rearranged some of thee initializations that were performed,
C           moved several calls to SCARDD outside the main loop, etc.
C
C        5) Performed some general cleanup as deemed necessary.
C
C-    Beta Version 2.1.0  20-NOV-1991 (MJS)
C
C        Checked FAILED function in main loop.
C
C-    Beta Version 2.0.0, 17-JUN-1991 (JEM)
C
C        1.  Added the arguments TOFILE and UNIT.  Previously the
C            summary was only displayed on the terminal screen.
C            Now, if requested by TOFILE, the summary is also
C            written to the file connected to UNIT.
C
C        2.  A user may cancel a task selected in QSUMC and
C            select another.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
C
C-&

C$ Index_Entries
C
C      summarize the segments in a binary spk file
C
C-&

C$ Revisions
C
C-    Beta Version 5.0.0  21-JUL-1995 (KRG)
C
C        Added several arguments to the call of this subroutine and
C        made other modifications to allow it to perform its own 
C        formatting of the summary, including filenames and separators.
C
C-    Beta Version 4.0.0  11-APR-1994 (KRG)
C
C        Modified this routine to make use of new routines to get and
C        format and write SPK segment summaries.
C
C        Added a missing $ Index_Entries header section.
C
C        Fixed a few typos in the header.
C
C        The routine DISPSP is now obsolete. It is no longer used.
C
C-    Beta Version 3.0.0  17-MAR-1993 (KRG)
C
C        1) Changed the names of the variable TOFILE and UNIT to LOGFIL
C           and LOGLUN, respectively.
C
C        2) Updated the program to use the menuing subroutine GETOPT
C           which removes the need for the routine QSUMSP. Redesigned
C           the CASE sructure of the code to facilitate the use of the
C           menuing routine.
C
C        3) Rearranged some of thee initializations that were performed,
C           moved several calls to SCARDD outside the main loop, etc.
C
C        5) Performed some general cleanup as deemed necessary.
C
C-    Beta Version 2.1.0  20-NOV-1991 (MJS)
C
C        Checked FAILED function in main loop. In the previous version,
C        if any time conversion produced an error, the summary would go
C        in an endless loop.
C
C-    Beta Version 2.0.0, 22-MAY-1991 (JEM)
C
C        1.  In addition to adding the arguments TOFILE and UNIT to
C            the calling sequence, the following code changes were
C            made.  The two new arguments were added to the calling
C            sequence of DISPSP as well.  If TOFILE is true, a
C            description of the type of summary is written to the
C            output file before calling DISPSP to write the summary.
C            If no segments are found, the message is written to the
C            output file as well as the terminal screen when
C            TOFILE is true.
C
C        2.  QSUMSP was changed.  'NONE' is now a possible task
C            returned from QSUMSP and means a task was selected,
C            then cancelled.  QSUMSP is called repeatedly until the
C            task returned is something other than NONE.  In
C            this way the user is able to select another task.
C-&



C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
      INTEGER               CARDD
      INTEGER               RTRIM
C
C     Local Parameters
C
C     Set value for a separator
C
      CHARACTER*(*)         STARS
      PARAMETER           ( STARS  = '********************' )
C
C     Set up the ID code prompt.
C
      CHARACTER*(*)         IDPRM
      PARAMETER           ( IDPRM = 'ID code? ' )
C
C     Set up the UTC time string prompt.
C
      CHARACTER*(*)         UTCPRM
      PARAMETER           ( UTCPRM = 'UTC time? ' )
C
C     Set up labels for various output things.
C
      CHARACTER*(*)         FNMLBL
      PARAMETER           ( FNMLBL = 'Summary for SPK file: #' )

      CHARACTER*(*)         LPSLBL
      PARAMETER           ( LPSLBL = 'Leapseconds File    : #' )

      CHARACTER*(*)         TYPLBL
      PARAMETER           ( TYPLBL = 'Summary Type        : #' )
C
C     Set the length for a line of text.
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
C
C     Set the length for an output line.
C
      INTEGER               OUTLEN
      PARAMETER           ( OUTLEN = 80 )
C
C     Set the length for an error message.
C
      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 320 )
C
C     Set the length for a UTC time string.
C
      INTEGER               UTCLEN
      PARAMETER           ( UTCLEN = 32 )
C
C     Set the precision for the fractional part of UTC times.
C
      INTEGER               UTCPRE
      PARAMETER           ( UTCPRE = 3 )
C
C     Set a length for the option values.
C
      INTEGER               OPTLEN
      PARAMETER           ( OPTLEN = 20 )
C
C     Set a length for the title of a menu.
C
      INTEGER               TTLLEN
      PARAMETER           ( TTLLEN = 20 )
C
C     Set the length of the text description of an option on a menu.
C
      INTEGER               OTXLEN
      PARAMETER           ( OTXLEN = 40 )
C
C     The number of options available on the main menu.
C
      INTEGER               NUMOPT
      PARAMETER           ( NUMOPT = 5 )
C
C     Parameter for the standard output unit.
C
      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 6 )
C
C     Local variables
C
      CHARACTER*(UTCLEN)    BEGUTC
      CHARACTER*(UTCLEN)    ENDUTC
      CHARACTER*(ERRLEN)    ERRMSG
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(1)         MENUNM(NUMOPT)
      CHARACTER*(TTLLEN)    MENUTL
      CHARACTER*(OTXLEN)    MENUTX(NUMOPT)
      CHARACTER*(OPTLEN)    MENUVL(NUMOPT)
      CHARACTER*(OPTLEN)    OPTION
      CHARACTER*(IDSIZ)     SEGID
      CHARACTER*(OUTLEN)    SUMSEP
      CHARACTER*(OUTLEN)    SEPAR
      CHARACTER*(OUTLEN)    TMPSTR
      CHARACTER*(LINLEN)    FNMOUT
      CHARACTER*(LINLEN)    LPSOUT
      CHARACTER*(LINLEN)    TYPOUT

      INTEGER               BODYID
      INTEGER               MENUOP
      INTEGER               SEGBAD
      INTEGER               SEGBOD
      INTEGER               SEGCEN
      INTEGER               SEGEAD
      INTEGER               SEGFRM
      INTEGER               SEGTYP

      DOUBLE PRECISION      INTRVL  ( LBCELL:2 )
      DOUBLE PRECISION      SEGINT  ( LBCELL:2 )
      DOUBLE PRECISION      INTSCT  ( LBCELL:2 )
      DOUBLE PRECISION      BEGET
      DOUBLE PRECISION      ENDET
      DOUBLE PRECISION      SEGBTM
      DOUBLE PRECISION      SEGETM

      LOGICAL               ANYSEG
      LOGICAL               CONTNU
      LOGICAL               DONE
      LOGICAL               ERROR
      LOGICAL               FOUND
      LOGICAL               HAVEIT
      LOGICAL               SEGFND
      LOGICAL               TRYAGN

C
C     Save everything to keep configuration control happy.
C
      SAVE

C
C     Initial Values
C
C
C     Define the menu title ...
C
      DATA   MENUTL    / 'SPK Summary Options' /
C
C     Define the menu option values ...
C
      DATA   MENUVL   /
     .                  'QUIT',
     .                  'Skip',
     .                  'ENTIRE_FILE',
     .                  'BY_BODY_ID',
     .                  'BY_TIME_INTERVAL'
     .                /
C
C     Define the menu descriptive text for each option ...
C
      DATA   MENUTX   /
     .                  'Quit, returning to main menu.',
     .                  'Skip',
     .                  'Summarize entire file.',
     .                  'Summarize by NAIF body ID code.',
     .                  'Summarize by time interval.'
     .                /
C
C     Define the menu option names ...
C
      DATA   MENUNM   /
     .                   'Q',
     .                   '.',
     .                   'F',
     .                   'B',
     .                   'T'
     .                /
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SUMSPK' )
      END IF
C
C     Initialize the separator.
C
      SEPAR = STARS // STARS // STARS // STARS
C
C     Initialize the segment separator.
C
      SUMSEP = '----------------------------------------' //
     .         '----------------------------------------'
C
C     Set the sizes of the window cells that we will use if the file
C     is to be summarized by time.
C
      CALL SSIZED ( 2, INTRVL )
      CALL SSIZED ( 2, SEGINT )
      CALL SSIZED ( 2, INTSCT )
C
C     Initialize a few things before we start.
C
      BODYID  = 0

      DONE = .FALSE.
      DO WHILE ( .NOT. DONE )
C
C        Initialize those things we reuse on every iteration.
C
         CONTNU = .TRUE.

         CALL WRITLN ( ' ', STDOUT )
         CALL GETOPT ( MENUTL, NUMOPT, MENUNM, MENUTX, MENUOP )

         IF ( FAILED() ) THEN
            CONTNU = .FALSE.
         END IF

         IF ( CONTNU ) THEN
C
C           Perform all of the setup necessary to perform the summary.
C           This include prompting for input values required, etc.
C
            CALL REPMC ( FNMLBL, '#', BINFNM, FNMOUT )
            CALL REPMC ( LPSLBL, '#', LPSFNM, LPSOUT )

            OPTION = MENUVL(MENUOP)

            IF ( OPTION .EQ. 'QUIT' ) THEN

               CONTNU = .FALSE.
               DONE   = .TRUE.

            ELSE IF ( OPTION .EQ. 'ENTIRE_FILE' ) THEN
C
C              Summarize the entire file.
C
               CALL REPMC ( TYPLBL, '#', 'Entire File', TYPOUT )

            ELSE IF ( OPTION .EQ. 'BY_BODY_ID' ) THEN
C
C              Summarize for a specified body.
C
C              First, we need to get the body ID code.
C
               ERRMSG = ' '
               HAVEIT = .FALSE.
               TRYAGN = .TRUE.

               DO WHILE ( TRYAGN )
                  ERROR  = .FALSE.
                  WRITE (*,*)
                  WRITE (*,*) 'Enter the desired NAIF body ID code.'
                  WRITE (*,*)
                  CALL GETINT ( IDPRM,
     .                          BODYID,
     .                          HAVEIT,
     .                          ERROR,
     .                          ERRMSG  )
                  IF ( ( .NOT. HAVEIT ) .OR. ERROR ) THEN
                     IF ( ERROR ) THEN
                        WRITE (*,*)
                        WRITE (*,*) ERRMSG(:RTRIM(ERRMSG))
                     END IF
                     IF ( .NOT. HAVEIT ) THEN
                        WRITE (*,*)
                        WRITE (*,*) 'A NAIF body ID code must be'  //
     .                              ' entered for this option.'
                        WRITE (*,*)
                     END IF
                     IF ( ( .NOT. HAVEIT ) .OR. ERROR ) THEN
                        CALL CNFIRM ( 'Try Again? (Yes/No) ', TRYAGN )
                        IF ( .NOT. TRYAGN ) THEN
                           CONTNU = .FALSE.
                        END IF
                     END IF
                  ELSE
                     TRYAGN = .FALSE.
                  END IF

               END DO
C
C              Write the type of summary to the log file if we need to.
C
               IF ( CONTNU ) THEN
                  TMPSTR = 'By segments for body: #'
                  CALL REPMC ( TYPLBL, '#', TMPSTR, TYPOUT )
                  CALL REPMI ( TYPOUT, '#', BODYID, TYPOUT )
               END IF

            ELSE IF ( OPTION .EQ. 'BY_TIME_INTERVAL' ) THEN
C
C              Summarize for given time interval.
C
C              First, we need to get the UTC time string for the
C              begin time.
C
               ERRMSG = ' '
               HAVEIT = .FALSE.
               TRYAGN = .TRUE.

               DO WHILE ( TRYAGN )
                  ERROR  = .FALSE.
                  WRITE (*,*)
                  WRITE (*,*) 'Enter the desired beginning UTC time.'
                  WRITE (*,*)
                  CALL GETCHR ( UTCPRM,
     .                          BEGUTC,
     .                          HAVEIT,
     .                          ERROR,
     .                          ERRMSG )
                  IF ( ( .NOT. HAVEIT ) .OR. ERROR ) THEN
                     IF ( ERROR ) THEN
                        WRITE (*,*)
                        WRITE (*,*) ERRMSG(:RTRIM(ERRMSG))
                     END IF
                     IF ( .NOT. HAVEIT ) THEN
                        WRITE (*,*)
                        WRITE (*,*) 'A beginning UTC time string'  //
     .                              ' must be entered for this'    //
     .                              ' option.'
                     END IF
                  ELSE
                     TRYAGN = .FALSE.
                  END IF
C
C                 We now have the beginning time in UTC, so attempt
C                 to convert it to ET. If the conversion fails, we
C                 need to immediately reset the error handling so that
C                 we can continue processing. Remember, we are in a
C                 menuing subroutine, and we are not allowed to exit
C                 on an error: we must go back to the menu. thus the
C                 need for a resetting of the error handler here. If
C                 we got to here, there were no errors, so as long as
C                 we maintain that status, everything will be hunky
C                 dory. We also convert the ET back into UTC to get
C                 a consistent format for display.
C
                  IF ( HAVEIT ) THEN
                     CALL UTC2ET ( BEGUTC, BEGET )
                     CALL ET2UTC ( BEGET, 'C', UTCPRE, BEGUTC )
                     IF ( FAILED() ) THEN
                        CALL RESET
                        ERROR = .TRUE.
                     END IF
                  END IF
C
C                 Check to see if they want to try and enter the
C                 beginning UTC time string again.
C
                  IF ( ( .NOT. HAVEIT ) .OR. ERROR ) THEN
                     WRITE (*,*)
                     CALL CNFIRM ( 'Try Again? (Yes/No) ', TRYAGN )
                     IF ( .NOT. TRYAGN ) THEN
                        CONTNU = .FALSE.
                     END IF
                  END IF
               END DO

C              Now, if we can, we need to get the UTC time string for
C              the end time.
C
               IF ( CONTNU ) THEN
                  ERRMSG = ' '
                  HAVEIT = .FALSE.
                  TRYAGN = .TRUE.

                  DO WHILE ( TRYAGN )
                     ERROR  = .FALSE.
                     WRITE (*,*)
                     WRITE (*,*) 'Enter the desired ending UTC time.'
                     WRITE (*,*)
                     CALL GETCHR ( UTCPRM,
     .                             ENDUTC,
     .                             HAVEIT,
     .                             ERROR,
     .                             ERRMSG )
                     IF ( ( .NOT. HAVEIT ) .OR. ERROR ) THEN
                        IF ( ERROR ) THEN
                           WRITE (*,*)
                           WRITE (*,*) ERRMSG(:RTRIM(ERRMSG))
                        END IF
                        IF ( .NOT. HAVEIT ) THEN
                           WRITE (*,*)
                           WRITE (*,*) 'An ending UTC time string'  //
     .                                 ' must be entered for this'    //
     .                                 ' option.'
                        END IF
                     ELSE
                        TRYAGN = .FALSE.
                     END IF
C
C                    We now have the ending time in UTC, so attempt
C                    to convert it to ET. If the conversion fails, we
C                    need to immediately reset the error handling so
C                    that we can continue processing. Remember, we are
C                    in a menuing subroutine, and we are not allowed
C                    to exit on an error: we must go back to the menu.
C                    thus the need for a resetting of the error handler
C                    here. If we got to here, there were no errors, so
C                    as long as we maintain that status, everything
C                    will be hunky dory. We also convert the ET back
C                    into UTC to get a consistent format for display.
C
                     IF ( HAVEIT ) THEN
                        CALL UTC2ET ( ENDUTC, ENDET )
                        CALL ET2UTC ( ENDET, 'C', UTCPRE, ENDUTC )
                        IF ( FAILED() ) THEN
                           CALL RESET
                           ERROR = .TRUE.
                        END IF
                     END IF
C
C                    Check to see if they want to try and enter the
C                    ending UTC time string again.
C
                     IF ( ( .NOT. HAVEIT ) .OR. ERROR ) THEN
                        WRITE (*,*)
                        CALL CNFIRM ( 'Try Again? (Yes/No) ', TRYAGN )
                        IF ( .NOT. TRYAGN ) THEN
                           CONTNU = .FALSE.
                        END IF
                     ELSE
                        TRYAGN = .FALSE.
                     END IF
                  END DO
               END IF
C
C              Create an interval out of the begin and end ET times,
C              if we can.
C
               IF ( CONTNU ) THEN
                  CALL SCARDD ( 0, INTRVL            )
                  CALL WNINSD ( BEGET, ENDET, INTRVL )
                  IF ( FAILED() ) THEN
                     CONTNU = .FALSE.
                  END IF
               END IF
C
C              Write the type of summary to the output file, if we can.
C
               IF ( CONTNU ) THEN
                  TMPSTR = 'By # Time Interval #'
                  CALL REPMC ( TYPLBL, '#', TMPSTR, TYPOUT )
                  CALL REPMC ( TYPOUT, '#', 'UTC', TYPOUT )
                  CALL REPMC ( TYPOUT, '#', '(#, #)', TYPOUT )
                  CALL REPMC ( TYPOUT, '#', BEGUTC, TYPOUT )
                  CALL REPMC ( TYPOUT, '#', ENDUTC, TYPOUT )
               END IF

            END IF
C
C           Now, if we can, search through the file from the beginning.
C           Keep track of whether or not any segments satisfy the search
C           criteria.
C
            IF ( CONTNU ) THEN

               CALL WRITLN ( ' ',   STDOUT )
               CALL WRITLN ( SEPAR, STDOUT )
               CALL WRITLN ( ' ',   STDOUT )
               CALL WRITLN ( FNMOUT, STDOUT )
               CALL WRITLN ( LPSOUT, STDOUT )
               CALL WRITLN ( TYPOUT, STDOUT )
               CALL WRITLN ( ' ',    STDOUT )

               IF ( LOGFIL ) THEN
                  CALL WRITLN ( ' ',    LOGLUN )
                  CALL WRITLN ( SEPAR,  LOGLUN )
                  CALL WRITLN ( ' ',    LOGLUN )
                  CALL WRITLN ( FNMOUT, LOGLUN )
                  CALL WRITLN ( LPSOUT, LOGLUN )
                  CALL WRITLN ( TYPOUT, LOGLUN )
                  CALL WRITLN ( ' ',    LOGLUN )
               END IF

               ANYSEG = .FALSE.
               CALL DAFBFS ( HANDLE )
               CALL DAFFNA ( FOUND  )

               DO WHILE ( FOUND .AND. CONTNU )
C
C                 On each iteration of the loop, we have not found
C                 anything initially.
C
                  SEGFND = .FALSE.
                  CALL SCARDD ( 0, INTSCT )
                  CALL SCARDD ( 0, SEGINT )
C
C                 Get the summary for the curent segment.
C
                  CALL SPKGSS ( SEGID,  SEGBOD, SEGCEN, SEGFRM,
     .                          SEGTYP, SEGBTM, SEGETM, SEGBAD,
     .                          SEGEAD          )
C
C                 Check to see if the current segment satisfies the
C                 current search criteria.
C
                  IF ( OPTION .EQ. 'ENTIRE_FILE' ) THEN
                     SEGFND = .TRUE.
                  ELSE IF ( OPTION .EQ. 'BY_BODY_ID' ) THEN
                     SEGFND = BODYID .EQ. SEGBOD
                  ELSE IF ( OPTION .EQ. 'BY_TIME_INTERVAL' ) THEN
C
C                    Create an interval out of the epochs in the
C                    segment.
C
                     CALL WNINSD ( SEGBTM , SEGETM, SEGINT )
C
C                    Intersect it with the input interval.
C
                     CALL WNINTD ( SEGINT, INTRVL, INTSCT )

                     IF ( FAILED() ) THEN
                        CALL RESET
                        CONTNU = .FALSE.
                     ELSE
                        SEGFND = CARDD( INTSCT ) .GT. 0
                     END IF

                  END IF

                  IF ( CONTNU .AND. SEGFND ) THEN

                     ANYSEG = .TRUE.
C
C                    Display the segment.
C
                     CALL WRITLN ( SUMSEP, STDOUT )
                     IF ( LOGFIL ) THEN
                        CALL WRITLN ( SUMSEP, LOGLUN )
                     END IF

                     CALL SPKWSS ( STDOUT, SEGID,  SEGBOD, 
     .                             SEGCEN, SEGFRM, SEGTYP, 
     .                             SEGBTM, SEGETM          )

                     IF ( LOGFIL ) THEN
                        CALL SPKWSS ( LOGLUN, SEGID,  SEGBOD, 
     .                                SEGCEN, SEGFRM, SEGTYP, 
     .                                SEGBTM, SEGETM          )
                     END IF

                     CALL WRITLN ( SUMSEP, STDOUT )
                     IF ( LOGFIL ) THEN
                        CALL WRITLN ( SUMSEP, LOGLUN )
                     END IF

                  END IF
C
C                 Find that next segment.
C
                  CALL DAFFNA ( FOUND  )

                  IF ( FAILED() ) THEN
                     CONTNU = .FALSE.
                  END IF

               END DO

            END IF
C
C           Better say something if no segments were matching the
C           search criteria were found.
C
            IF ( CONTNU .AND. ( .NOT. ANYSEG ) ) THEN
               LINE = 'No matching segments were found.'
               CALL WRITLN ( LINE, STDOUT )

               IF ( LOGFIL ) THEN
                  CALL WRITLN ( LINE, LOGLUN )
               END IF

            END IF

            IF ( CONTNU ) THEN
               CALL WRITLN ( ' ',   STDOUT )
               CALL WRITLN ( SEPAR, STDOUT )
               CALL WRITLN ( ' ',   STDOUT )
               IF ( LOGFIL ) THEN
                  CALL WRITLN ( ' ',   LOGLUN )
                  CALL WRITLN ( SEPAR, LOGLUN )
                  CALL WRITLN ( ' ',   LOGLUN )
               END IF
            END IF

         END IF
C
C        Reset the error handling if anything failed during the search
C        time conversions, or window building.
C
         IF ( FAILED() ) THEN
            CALL RESET
         END IF

      END DO

      CALL CHKOUT ( 'SUMSPK' )
      RETURN
      END
