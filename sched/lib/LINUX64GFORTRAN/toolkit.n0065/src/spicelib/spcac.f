 
C$Procedure SPCAC ( SPK and CK, add comments )
 
      SUBROUTINE SPCAC ( HANDLE, UNIT, BMARK, EMARK )
 
C$ Abstract
C
C     Store text from a text file in the comment area of a binary SPK
C     or CK file, appending it to whatever text may already have
C     been stored there.
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
C     SPC
C
C$ Keywords
C
C     FILES
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               UNIT
      CHARACTER*(*)         BMARK
      CHARACTER*(*)         EMARK
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle assigned to binary SPK or CK file.
C     UNIT       I   Logical unit connected to comment file.
C     BMARK      I   Beginning marker.
C     EMARK      I   Ending marker.
C
C$ Detailed_Input
C
C     HANDLE      is the handle assigned to the binary SPK or CK file
C                 which has been opened for write access.
C
C     UNIT        is the logical unit connected to the text file
C                 which contains the text to be stored in the
C                 comment area of the binary file.
C
C     BMARK,
C     EMARK       are markers that delimit a group of consecutive
C                 lines in the text file (UNIT), that get stored in the
C                 comment area of the binary file (HANDLE).
C
C                 The group of lines begins with the line that
C                 immediately follows the first line of the file
C                 equivalent to BMARK.  It ends with line that
C                 precedes the next line of the file equivalent to
C                 EMARK, including blank lines.  Leading and
C                 trailing blanks are ignored when testing for
C                 equivalence.
C
C                 By convention, if BMARK is blank, the first line of
C                 the group is the first line of the file; if EMARK is
C                 blank, the last line of the group is the last line
C                 of the file.
C
C                 If a marker is non-blank and is not found, or if
C                 non-blank markers are on successive lines in the text
C                 file, nothing gets stored in the comment area of
C                 the binary file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the specified DAF file is not open for write access, the
C        error will be diagnosed by a routine called by this routine.
C
C     2) If there is a problem reading from the comment area of the
C        binary file, the error SPICE(FILEREADFAILED) is signalled.
C
C     3) If there is a problem writing to the comment area of the
C        binary file, the error SPICE(FILEWRITEFAILED) is signalled.
C
C     4) If there is a problem reading from the text file,
C        a routine that SPCAC calls signals an error.
C
C     5) If a non-printing ASCII character is encountered in the
C        comments, a routine that SPCAC calls diagnoses and signals
C        an error.
C
C$ Files
C
C     HANDLE      is the handle assigned to the binary SPK or CK file.
C                 Use DAFOPW to open it for write access and get the
C                 handle.  Upon exit, this binary file will contain
C                 the specified text from the comment file in its
C                 comment area, appended to whatever text may already
C                 have been stored there.  SPCAC will include an extra
C                 blank line between the original text and the
C                 appended text.
C
C     UNIT        is the logical unit connected to the comment file.
C                 This file must contain only text (printable
C                 ASCII characters, namely ASCII 32-126).  Open this
C                 file with read access and get its UNIT using TXTOPR.
C
C$ Particulars
C
C     The structure of SPK and CK files accommodates comments in
C     addition to data.  The following three routines are available
C     for accessing the comment area of a binary SPK or CK file:
C
C           SPCAC           add comments
C
C           SPCEC           extract comments
C
C           SPCDC           delete comments
C
C     Note that comments must consist of only text, that is, printable
C     ASCII characters, specifically ASCII 32-126.  This excludes
C     tabs (ASCII 9) and control characters.
C
C     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and
C     SPCT2B---include these comments when converting SPK and CK
C     files between binary and text formats.
C
C$ Examples
C
C     Suppose we have a binary SPK file called A.BSP and we have
C     a text file called COMMENTS.TXT that contains comments
C     about the data in the SPK file.
C
C     The following code fragment stores the entire contents of
C     COMMENTS.TXT in the comment area of A.BSP.
C
C            CALL DAFOPW ( 'A.BSP', HANDLE )
C
C            CALL TXTOPR ( 'COMMENTS.TXT', UNIT )
C
C            BMARK = ' '
C            EMARK = ' '
C
C            CALL SPCAC  ( HANDLE, UNIT, BMARK, EMARK )
C
C            CLOSE ( UNIT )
C
C     Now suppose MORE.TXT is a text file that contains additional
C     information about the data in A.BSP, as well as information
C     about several other SPK files.  The contents of MORE.TXT are
C
C               \begin A info
C
C                 DATAFILE = A
C                 SOURCE   = JPL, 1990 September 12
C                 MISSION  = Galileo
C
C               \end A info
C
C               \begin B info
C
C                 DATAFILE = B
C                 SOURCE   = JPL, 1988 August 1
C                 MISSION  = Voyager 2
C
C               \end B info
C
C               \begin C info
C
C                 DATAFILE = C
C                 SOURCE   = JPL, 1994 January 31
C                 MISSION  = Mars Observer
C
C               \end C info
C
C     This code fragment stores only the information that pertains
C     to A.BSP, and appends it to the text from COMMENTS.TXT that
C     has already been stored in the comment area of A.BSP
C
C            CALL TXTOPR ( 'MORE.TXT', UNIT )
C
C            BMARK = '\begin A info'
C            EMARK = '\end A info'
C
C            CALL SPCAC  ( HANDLE, UNIT, BMARK, EMARK )
C
C            CLOSE ( UNIT )
C
C            CALL DAFCLS ( HANDLE )
C
C     Note that, ignoring leading and trailing blanks, BMARK and
C     EMARK are exactly equivalent to lines in the text file.
C     If the assignment had been instead BMARK = '\ begin A info',
C     with an extra space between the slash and the word begin,
C     SPCAC would not have found the marker and no comments from
C     the text file would be written to the binary file.
C
C$ Restrictions
C
C     1)  The lines in the comment file should not exceed 1000
C         characters in length.  SPCAC truncates lines longer than
C         this on the right.
C
C     2)  Use TXTOPR to open text files for read access and get
C         the logical unit.  System dependencies regarding
C         opening text files have been isolated in the routines
C         TXTOPN and TXTOPR.
C
C     3)  This routine assumes that the comment area of the binary SPK
C         or CK file contains only text stored by SPCAC.  Comments
C         written any other way may not be handled properly.
C
C     4)  The comment area of the binary SPK or CK file must contain
C         only one EOT character.  This routine seeks back from the
C         last reserved record searching for the first EOT it
C         encounters.  Thus the multiple EOT's will cause the appended
C         comments to be invisible to any reader that starts at the
C         first reserved record and reads until the first EOT present.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     J.E. McLean    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Updated this routine to utilize new handle manager
C        interfaces.
C
C-    SPICELIB Version 1.3.0, 12-FEB-1999 (FST)
C
C        Modified the EOT search code to seek back through any
C        reserved records, as opposed to just the last one.  This
C        provides the flexibility to use DAFOPN to reserve records
C        that may ultimately be used for storing comments. As a direct
C        result of these changes the SPICE(MISSINGEOT) error is no
C        longer signalled, since if no EOT is found in the reserved
C        records, they are considered available for writes.
C
C-    SPICELIB Version 1.2.0, 12-MAY-1994 (KRG)
C
C        Added an IF statement so that DAFARR is called only if new
C        reserved records need to be added to the comment area.
C
C-    SPICELIB Version 1.1.0, 09-APR-1993 (KRG)
C
C        Added code to initialize the variable LASTRR to zero. This
C        variable is used in a function call, MAX ( LASTRR-1, 1 ),
C        regardless of whether or not any reserved records are in
C        the file. Thus the need to initialize it.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
C
C-&
 
C$ Index_Entries
C
C     add comments to spk or ck file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        This routine now utilizes DAFSIH to determine if
C        HANDLE is open for WRITE access.  The call to DAFHLU
C        has been replaced with a call to ZZDDHHLU, the handle
C        manager interface for retrieving a logical unit.
C        DAFHLU is no longer used, since it locks the unit
C        returned to its HANDLE, tying up resources in the
C        handle manager.
C
C-    SPICELIB Version 1.2.0, 12-MAY-1994 (KRG)
C
C        Added an IF statement so that DAFARR is called only if new
C        reserved records need to be added to the comment area.
C
C-    SPICELIB Version 1.1.0, 09-APR-1993 (KRG)
C
C        Added code to initialize the variable LASTRR to zero. This
C        variable is used in a function call, MAX ( LASTRR-1, 1 ),
C        regardless of whether or not any reserved records are in
C        the file. Thus the need to initialize it.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               COUNTC
      INTEGER               LASTNB
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
C     IFNLEN      is the length of a DAF internal file name.
C
C     MAXCPR      is the maximum number of characters per DAF record and
C                 hence the maximum comment line length.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60     )
 
      INTEGER               MAXCPR
      PARAMETER           ( MAXCPR = 1000   )
 
C
C     Local variables
C
      CHARACTER*(MAXCPR+2)  DATA
      CHARACTER*(1)         EOT
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(MAXCPR)    LINE
      CHARACTER*(1)         NULL
      CHARACTER*(MAXCPR)    RECORD
 
      INTEGER               BLINE
      INTEGER               BWARD
      INTEGER               C
      INTEGER               CHARS
      INTEGER               DAFU
      INTEGER               EOL
      INTEGER               ELINE
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               L
      INTEGER               LASTRR
      INTEGER               LINES
      INTEGER               ND
      INTEGER               NI
      INTEGER               NR
      INTEGER               NRR
      INTEGER               POS
      INTEGER               POSEOT
      INTEGER               REC
      INTEGER               SPACE
      INTEGER               START
      INTEGER               TOTAL
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCAC' )
      END IF
 
C
C     Before doing anything, determine if the file associated with
C     HANDLE is available for WRITE access.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF
 
C
C     Rewind the comment file - we'll start the search for BMARK
C     and EMARK at the beginnning.  Once we have located the markers,
C     count the number of lines between them and the number of
C     characters in those lines, ignoring trailing blanks.
C
C     We rewind the file so that we know where the file pointer is.
C     LOCLN will compute BLINE and ELINE taking the current position
C     of the file pointer as line 1.
C
      REWIND ( UNIT )
 
      CALL LOCLN ( UNIT, BMARK, EMARK, LINE, BLINE, ELINE, FOUND )
 
C
C     If the markers are not found, or if BMARK and EMARK are on
C     successive lines, there is nothing to put in the comment area.
C
      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF
 
C
C     Adjust BLINE and ELINE so we are pointing to the group of lines
C     BETWEEN the markers.  Check and make sure there is at least one
C     line in the group.
C
      IF ( BMARK .NE. ' ' ) THEN
         BLINE = BLINE + 1
      END IF
 
      IF ( EMARK .NE. ' ' ) THEN
         ELINE = ELINE - 1
      END IF
 
      IF ( BLINE .GT. ELINE ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF
 
C
C     Calculate the number of lines and the total number of characters
C     in those lines.  The characters must all be printable, or
C     else COUNTC will signal an error.
C
      LINES = ELINE - BLINE + 1
      CHARS = COUNTC ( UNIT, BLINE, ELINE, LINE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF
 
C
C     Read the file record to find out if the DAF contains any
C     reserved records.  The reserved records in an array file
C     are stored between the first record (the file record) and
C     the first summary record.  FWARD is the record number of
C     that first summary record, and NRR is the number of reserved
C     records in the file.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
      NRR    = FWARD - 2
 
C
C     Get the logical unit for reading from and writing to the DAF.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFU )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF
 
C
C     Assign the value of NULL and EOT.  NULL gets appended to the
C     end of each line of text.  EOT gets appended to the end of
C     all the comments.  Assign initial values for SPACE, RECORD,
C     and START.
C
      NULL = CHAR( 0)
      EOT  = CHAR( 4)
      SPACE  = 0
      RECORD = ' '
      START  = 0
      LASTRR = 0
 
      IF ( NRR .NE. 0 ) THEN
C
C        At this point, we know there exist reserved records in the
C        DAF. We need to search from the last record to the first,
C        seeking for the EOT (end of transmission) character, as it
C        marks the end of the comment region.
C
         LASTRR = FWARD - 1
         I      = LASTRR + 1
         POSEOT = 0
 
         DO WHILE ( ( I .GT. 1 ) .AND. ( POSEOT .EQ. 0 ) )
C
C           Decrement the counter now.  This keeps it in
C           sync with the exit conditions.
C
            I = I - 1
            READ ( DAFU, REC=I, IOSTAT=IOSTAT ) RECORD
 
            IF ( IOSTAT .NE. 0 ) THEN
               CALL SETMSG ( 'Error reading comment area of binary '//
     .                       'file named FILE.  IOSTAT = *.'          )
               CALL ERRINT ( '*', IOSTAT                              )
               CALL ERRFNM ( 'FILE', DAFU                             )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'                  )
               CALL CHKOUT ( 'SPCAC'                                  )
               RETURN
            END IF
C
C           Call INDEX. If POSEOT is 0, then RECORD doesn't contain
C           the EOT character.
C
            POSEOT = INDEX ( RECORD, EOT )
 
         END DO
 
C
C        The amount of free space in the reserved records of the
C        files is determined by the number of empty reserved
C        records ( LASTRR - I ), and the number of characters used
C        in last record with data (MAXCPR - POSEOT).
C
         SPACE = MAXCPR*(LASTRR - I + 1) - POSEOT
C
C        Adjust the value of LASTRR to indicate the record where
C        the EOT lies.  From here on out, the purpose of this
C        variable is to indicate where to start dumping comments.
C
         LASTRR = I
C
C        If POSEOT is 0, then there are no comments in the file, but
C        there are reserved records.  Branch on this:
C
         IF ( POSEOT .EQ. 0 ) THEN
C
C           Leaving this string index at zero may be causing all sorts
C           of warning bells to go off in your head. However, before
C           this index value is used to address the contents of a
C           string it's incremented by 1.
C
            START = POSEOT
C
C        Handle the case when POSEOT is non-zero.
C
         ELSE
C
C           Replace the end-of-transmission character with a new line
C           character (we use null), so a blank line will come between
C           the old text and new text in the comment area.  START is the
C           position after which the first character of the new text
C           goes.
C
            RECORD ( POSEOT:POSEOT ) = NULL
 
            START = POSEOT
 
         END IF
 
      END IF
 
C
C     Compute the number of records (NR) needed to store all of these
C     characters.
C
C     Each line should end with a null (ASCII 0) character.  The final
C     line should also be followed by an end-of-transmission (ASCII 4)
C     character.  So the total is the number of characters, plus the
C     number of lines, plus one for the EOT.
C
C     If the TOTAL fits in the SPACE available in the last reserved
C     record, we don't need to reserve any more.  Otherwise compute
C     the number we need.
C
      TOTAL = CHARS + LINES + 1
 
      IF (  ( TOTAL - SPACE ) .GT. 0  ) THEN
 
         NR = (  ( TOTAL - SPACE ) - 1  ) / MAXCPR    +   1
 
      ELSE
 
         NR = 0
 
      END IF
 
C
C     Reserve the records to create a comment area large enough
C     to hold it all, if we need to.  If we can't do it, there's no
C     point in going on.
C
      IF ( NR .GT. 0 ) THEN
 
         CALL DAFARR ( HANDLE, NR )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPCAC' )
            RETURN
         END IF
 
      END IF
 
C
C     Load the group of lines in the comment file into the reserved
C     records. Keep adding lines to the current record until it has
C     been filled, then write it to the DAF, and begin a new record.
C
      REC    = MAX ( LASTRR-1, 1 )
      POS    = START
 
C
C     Rewind the text file then skip past the lines that we don't want
C     to position the file pointer at the correct record.
C
      REWIND ( UNIT )
 
      DO L = 1, BLINE - 1
 
         READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Error reading line # in text file named '  //
     .                    'FILE.  IOSTAT = *.'                        )
            CALL ERRINT ( '#', L                                      )
            CALL ERRINT ( '*', IOSTAT                                 )
            CALL ERRFNM ( 'FILE', UNIT                                )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'SPCAC'                                     )
            RETURN
         END IF
 
      END DO
 
C
C     Start reading the lines that we do want.  LINE is MAXCPR long
C     so that's the maximum number of characters that are read.
C
      DO L = 1, LINES
 
         READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Error reading line # in text file named '  //
     .                    'FILE.  IOSTAT = *.'                        )
            CALL ERRINT ( '#', L+BLINE-1                              )
            CALL ERRINT ( '*', IOSTAT                                 )
            CALL ERRFNM ( 'FILE', UNIT                                )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'SPCAC'                                     )
            RETURN
         END IF
 
C
C        Each line is followed by a null character.
C
         DATA          = LINE
         EOL           = LASTNB ( DATA ) + 1
         DATA(EOL:EOL) = NULL
 
C
C        The final line is followed by an additional
C        end-of-transmission character.
C
         IF ( L .EQ. LINES ) THEN
            EOL           = EOL + 1
            DATA(EOL:EOL) = EOT
         END IF
 
C
C        Moving characters one at a time is slower, but simpler,
C        than trying to move them in blocks.
C
         DO C = 1, EOL
 
C
C           If the current record is full, write it to the DAF.
C
            IF ( POS .EQ. MAXCPR ) THEN
 
               REC = REC + 1
               WRITE ( DAFU, REC=REC, IOSTAT=IOSTAT ) RECORD
 
               IF ( IOSTAT .EQ. 0 ) THEN
                  RECORD = ' '
                  POS    = 0
 
               ELSE
                  CALL SETMSG ( 'Error writing to record # of the '  //
     .                          'binary file named FILE. IOSTAT = *.' )
                  CALL ERRINT ( '#', REC                              )
                  CALL ERRINT ( '*', IOSTAT                           )
                  CALL ERRFNM ( 'FILE', DAFU                          )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'              )
                  CALL CHKOUT ( 'SPCAC'                               )
                  RETURN
               END IF
 
            END IF
 
C
C           Add the next character to the current record.
C
            POS             = POS + 1
            RECORD(POS:POS) = DATA(C:C)
 
         END DO
 
      END DO
 
C
C     Write the final record to the DAF.
C
      REC = REC + 1
      WRITE ( DAFU, REC=REC, IOSTAT=IOSTAT ) RECORD
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Error writing the final record, record #, '   //
     .                 'of the binary file named FILE.  IOSTAT = *.'  )
         CALL ERRINT ( '#', REC                                       )
         CALL ERRINT ( '*', IOSTAT                                    )
         CALL ERRFNM ( 'FILE', DAFU                                   )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                       )
         CALL CHKOUT ( 'SPCAC'                                        )
         RETURN
      END IF
 
      CALL CHKOUT ( 'SPCAC' )
      RETURN
      END
