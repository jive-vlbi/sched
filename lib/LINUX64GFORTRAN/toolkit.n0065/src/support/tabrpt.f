C$Procedure      TABRPT ( Table Format Report )
 
      SUBROUTINE TABRPT ( NITEMS, ITEM,   SIZE,
     .                            WIDTH,  JUSTR,
     .                            PRESRV, SPCIAL,
     .                            LMARGE, SPACE,
     .                            FETCH   )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine creates a tabular report using the parameters
C     supplied for the arrangement of the report and the user
C     supplied routine that fetches the items to be placed in
C     the report.
C
C$ Required_Reading
C
C     REPORTS
C
C$ Keywords
C
C     IO
C     REPORTING
C     TABLE
C
C$ Declarations
 
      INTEGER               NITEMS
      INTEGER               ITEM  ( * )
      INTEGER               SIZE  ( * )
      INTEGER               WIDTH ( * )
      LOGICAL               JUSTR ( * )
      INTEGER               SPACE
      INTEGER               LMARGE
      LOGICAL               PRESRV ( * )
      CHARACTER*(*)         SPCIAL ( * )
 
      EXTERNAL              FETCH
 
      INTEGER               MXWDTH
      PARAMETER           ( MXWDTH = 132 )
 
      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 60 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C      --------  ---  --------------------------------------------------
C     NITEMS     I   The number of columns that should appear
C     ITEM       I   An array of item codes
C     SIZE       I   The number of components associated with the items
C     WIDTH      I   The room to allow for each item
C     JUSTR      I   Justify right
C     SPACE      I   The amount of space to place between columns
C     LMARGE     I   Location of the left margin
C     PRESRV     I   Logical indicating whether to preserve components
C     SPCIAL     I   Special characters to us/recognize in a column
C     FETCH      I   Name of a routine that will fetch data for an item.
C     MAXWDTH    P   The maximum width for the report.
C     MAXCOL     P   Maximum number of columns that can be supported.
C
C$ Detailed_Input
C
C     NITEMS     The number of columns that should appear in this
C                this block of the report.
C
C     ITEM       An array of id codes that can be used to fetch
C                the data strings that will be formatted into the
C                columns of this block of the report.
C
C     SIZE       The number of components associated with each item.
C
C     WIDTH      The maximum number of characters that may appear
C                across a column
C
C     JUSTR      A logical array.  If JUSTR(I) is true, then the
C                data for a column will be right justified. Otherwise
C                it will be left justified.
C
C     SPACE      The amount of space to place between columns
C
C     LMARGE     Location of the left margin
C
C     PRESRV     Logical indicating whether to preserve components
C                by starting each new component on a new line in
C                its column.
C
C     SPCIAL     Special instructions that may be used to alter the
C                style of output in a column.  For example you might
C                want to have leaders or a trailer so that the
C                report will have vertical bars between columns.
C                Or if the column has preserved spacing you might
C                choose to use a flag with each component (especially
C                if it is likely to wrap over several lines.
C
C     FETCH      Name of a routine that will fetch data for an item.
C
C$ Detailed_Output
C
C
C$ Parameters
C
C     MXWDTH    is the maximum width page that is supported for
C               report generation.  This parameter should never
C               be larger than the same parameter that is used
C               in the PAGE MANAGER routine PAGMAN.
C
C     MAXCOL    is the maximum number of columns that can appear
C               in a report
C
C$ Exceptions
C
C     1) If NITEMS is larger than MAXCOL the error
C        SPICE(TOOMANYCOLUMNS) will be signalled.
C
C     2) If the space required implied by WIDTHS, SPACE and LMARGE
C        is greater than MXWDTH the error SPICE(REPORTTOOWIDE) will
C        be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows you to "easily" create nicely formatted
C     reports for output by your programs.  By setting the parameters
C     supplied on input together with the parameters that control
C     page layout as used by PAGMAN you can produce a wide variety of
C     report formats without having to deal with the details of
C     arranging the output on the screen.
C
C$ Examples
C
C     copy required reading examples here.
C
C$ Restrictions
C
C     This routine works in conjunction with the routine PAGMAN
C     and its entry points.  You need to be sure that PAGMAN has
C     been properly initialized before you begin using this routine.
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
C-    Beta Version 3.0.0, 2-OCT-1996 (WLT)
C
C        Increased the internal buffer sizes and modified
C        the fetching logic so that the buffer will not fill
C        up and inadvertantly cut off data with no warning.
C
C-    Beta Version 2.0.0, 9-Aug-1995 (WLT)
C
C        Increased several buffer parameters and put in a check
C        for FAILED so that we can quit this thing if we need to.
C
C-    Beta Version 1.0.0, 1-JAN-1994 (WLT)
C
C-&
 
C$ Index_Entries
C
C
C     Arrange data in columns
C
C-&
 
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME = 'TABRPT' )
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
 
      LOGICAL               RETURN
      LOGICAL               FAILED
      LOGICAL               EQSTR
 
C
C     Other functions
C
      INTEGER               QLSTNB
C
C     Local parameters
C
 
      INTEGER               STLSIZ
      PARAMETER           ( STLSIZ  = 80 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = 1024 )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               HLFHLD
      PARAMETER           ( HLFHLD = 130  )
 
      INTEGER               THSHLD
      PARAMETER           ( THSHLD = 2*HLFHLD )
 
      INTEGER               PAGESZ
      PARAMETER           ( PAGESZ = 2*THSHLD )
C
C     The arrays below are used to store attributes on a column
C     by column basis.
C
C     STYLE  is the style to be used when formating text for an
C            individual column
C
C     COUNT  is a counter that is used to indicate how many components
C            have been processed for an individual column
C
C     ROW    keeps track of the last row in the local page where
C            formatted text was placed.
C
C     DONE   is a logical that indicates whether we have formatted
C            all of the data for a column.
C
      CHARACTER*(STLSIZ)    STYLE ( MAXCOL )
      INTEGER               COUNT ( MAXCOL )
      INTEGER               ROW   ( MAXCOL )
      LOGICAL               DONE  ( MAXCOL )
 
C
C     Local variables
C
 
      CHARACTER*(LNGSIZ)    LONG
      CHARACTER*(LNGSIZ)    GETSTR
      CHARACTER*(MXWDTH)    BUFFER ( LBCELL : THSHLD )
      CHARACTER*(MXWDTH)    PAGE   (          PAGESZ )
      CHARACTER*(WDSIZE)    VALUE
      CHARACTER*(WDSIZE)    KEY
      CHARACTER*(MAXCOL)    HRD
 
      INTEGER               DID
      INTEGER               I
      INTEGER               ID
      INTEGER               J
      INTEGER               L
      INTEGER               LAST
      INTEGER               LEFT
      INTEGER               MAXROW
      INTEGER               NROWS
      INTEGER               PUTAT
      INTEGER               R
      INTEGER               RIGHT
      INTEGER               ROOM
      INTEGER               TOSHIP
      INTEGER               WDTH
 
      LOGICAL               DOHRD
      LOGICAL               FILLED
      LOGICAL               FINISH
      LOGICAL               FULL
      LOGICAL               NOROOM
 
 
C
C     Saved variables
C
      SAVE
      DATA                  KEY   / 'abort' /
      DATA                  HRD   / ' ' /
      DATA                  DOHRD / .FALSE. /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'TABRPT' )
C
C     Initialize the cell that is used by NICEBT and make sure
C     the page is completely blank
C
      DO I = 1, PAGESZ
         PAGE(I) = ' '
      END DO
C
C     Initialize the local page and set the column parameters.
C
 
      DO I = 1, NITEMS
         DONE (I) = .FALSE.
         COUNT(I) =  0
         ROW  (I) =  0
         CALL REPMI  ( 'LEFT 1 RIGHT #', '#', WIDTH(I), STYLE(I) )
         CALL SUFFIX (  SPCIAL(I),        1,            STYLE(I) )
      END DO
C
C     The logical FINISH is used to keep track of whether or not
C     we have finished processing all items.  Certainly we haven't
C     done so yet.  It will be the value of the expression given
C     by DONE(1) .AND. DONE(2) .AND. ... .AND. DONE(NITEMS)
C
      FINISH = .FALSE.
 
      DO WHILE ( .NOT. FINISH )
C
C        We need to reset the left margin of the page.
C
         LEFT  =  LMARGE
 
         DO ID = 1, NITEMS
C
C           We are going to format items for output one at a time.
C           We will either fetch all of the components, or we
C           will fill up the room allotted for this item in the
C           buffer that will hold the data.
C
C           Thus at the end of this loop, we will have filled
C           up as much room as there is for this part of the
C           report and be ready to send that stuff to the
C           printer.
C
C           Set the right margin and determine whether or not
C           the  COLUMN that holds the text to be formatted is
C           already filled up.
C
            FILLED =      ROW  (ID) .GE. THSHLD
     .               .OR. DONE (ID)
 
            RIGHT  = LEFT + WIDTH(ID) - 1
 
            DO WHILE ( .NOT. FILLED )
C
C              Put data into the long string for output until
C              it becomes full or it is appropriate to stop doing
C              so (there's no more data, or the PRESRV flag tells
C              us to stop).
               PUTAT = 1
               FULL  = .FALSE.
               ROOM  =  MIN  ( LNGSIZ, WIDTH(ID) * HLFHLD )
               LONG  = ' '
 
               DO WHILE (      ( .NOT. DONE(ID) )
     .                   .AND. ( .NOT. FULL     ) )
C
C                 Increment COUNT so that we can fetch the next
C                 component of this item.
C
                  COUNT(ID) = COUNT(ID) + 1
 
                  CALL FETCH( ITEM(ID), COUNT(ID), GETSTR, WDTH )
 
                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'TABRPT' )
                     RETURN
                  END IF
 
C
C                 Determine the next place to add on to this string
C                 and see if adding on at that point would fill up
C                 the available space in our string.
C
                  L    = QLSTNB( GETSTR )
                  LAST = MAX( L, 1 )
 
                  IF ( PUTAT + L .LT. ROOM ) THEN
 
                     LONG(PUTAT:) = GETSTR(1:LAST)
                     PUTAT        = MIN ( PUTAT + L + 2, LNGSIZ )
 
C
C                    If the input was a blank, we step back to
C                    the beginning of the string.
C
                     IF ( PUTAT .EQ. 2 ) THEN
                        PUTAT  = 1
                     END IF
 
                     NOROOM = PUTAT + WIDTH(ID) .GE. ROOM
 
                  ELSE IF ( PUTAT .EQ. 1 ) THEN
C
C                    This case is very funky.  We are at the very
C                    beginning of the output buffer, but there still
C                    isn't room.  This means the user requested
C                    a width such that HLFHLD * WIDTH(ID)  is smaller
C                    than the size of the data in the column.
C                    In other words, the width must be less than
C                    the value DATA_LENGTH/HLFHLD.  Since the
C                    maximum data length is 1024 and HLFHLD is
C                    at last look 130, this means they have asked
C                    to fit data that is very long into a very
C                    column that is less than 8 characters wide.
C                    Sorry but there doesn't seem to be a morally
C                    compelling reason to handle this case
C                    robustly.  We just put some dots at the end
C                    of the output to indicate there's more stuff
C                    that can't be printed.
C
                     LONG              = GETSTR
                     NOROOM            = .TRUE.
                     LONG(ROOM-7:ROOM) = '........'
                     PUTAT             = ROOM
 
                  ELSE
C
C                    There isn't room to append GETSTR to the end
C                    of LONG.  Adjust the counter back by 1 and
C                    set NOROOM to .TRUE.
C
                     COUNT(ID) = COUNT(ID) - 1
                     NOROOM    = .TRUE.
 
                  END IF
 
                  DONE (ID) = COUNT (ID)  .GE. SIZE (ID)
                  FULL      = PRESRV(ID)  .OR. NOROOM
 
               END DO
 
C
C              Format the string into the holding buffer.
C
               CALL SSIZEC   ( THSHLD,                   BUFFER )
               CALL NICEBT_1 ( LONG(1:PUTAT), STYLE(ID), BUFFER )
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TABRPT' )
                  RETURN
               END IF
 
               NROWS = CARDC(BUFFER)
C
C              Transfer the data from the holding buffer
C              to the page layout buffer.
C
               DO J = 1, NROWS
 
                  ROW(ID) = ROW(ID) + 1
                  R       = ROW(ID)
 
                  PAGE(R)(LEFT:RIGHT) = BUFFER(J)
 
                  IF ( JUSTR(ID) ) THEN
                     CALL RJUST ( PAGE(R)(LEFT:RIGHT),
     .                            PAGE(R)(LEFT:RIGHT)  )
                  END IF
C
C                 Replace any "hardspaces" by blanks.
C
                  IF ( DOHRD ) THEN
                     IF ( HRD(ID:ID) .NE. ' ' ) THEN
                        CALL REPLCH ( PAGE(R)(LEFT:RIGHT),
     .                                HRD(ID:ID),  ' ',
     .                                PAGE(R)(LEFT:RIGHT) )
                     END IF
                  END IF
 
               END DO
 
C
C              Determine whether this column has been sufficiently
C              filled up.
C
               DONE(ID) =  COUNT(ID) .GE.   SIZE(ID)
               FILLED   =  DONE (ID) .OR. ( ROW (ID) .GE. THSHLD )
 
            END DO
 
C
C           Once you get to this point, the current column has
C           been filled as much as is possible.   We need to
C           Set the left margin for the next item to process
C
            LEFT     = RIGHT + SPACE   +    1
 
 
         END DO
 
 
C
C        By the time you get to this point, every column has either
C        filled up or there's nothing left to print.
C
C        In either case we need to ship out the rows from
C        1 to MIN ( MAX{ROW(1) ... ROW(NITEMS)}, THRSHOLD )
C        and shift the rest of the stuff up in the buffer.
C
         MAXROW = 0
 
         DO I = 1, NITEMS
            MAXROW = MAX( MAXROW, ROW(I) )
         END DO
 
         TOSHIP    = MIN ( MAXROW, THSHLD )
C
C        Ship out the rows that are ready to go.
C
         DO R = 1, TOSHIP
            CALL PAGPUT ( PAGE(R) )
 
            CALL PAGPMT ( DID, VALUE )
 
            IF ( DID .NE. 0 ) THEN
               IF ( EQSTR(VALUE, KEY ) ) THEN
                  CALL CHKOUT ( 'TABRPT' )
                  RETURN
               END IF
            END IF
 
         END DO
 
C
C        Shift the remaining rows up to the top of the page
C
         DO R = TOSHIP + 1, PAGESZ
            PAGE(R-TOSHIP) = PAGE(R)
         END DO
 
C
C        Blank out the last TOSHIP rows.
C
         DO R = PAGESZ - TOSHIP + 1, PAGESZ
            PAGE(R) = ' '
         END DO
C
C        Finally adjust the positions where each column should begin
C        filling in more data.
C
         DO J = 1, NITEMS
            ROW(J) = MAX( ROW(J) - TOSHIP, 0 )
         END DO
 
C
C        Now examine each of the ID's to see if we are done
C        processing all items.
C
         FINISH = .TRUE.
 
         DO ID = 1, NITEMS
            FINISH   = FINISH .AND. DONE(ID)
         END DO
 
 
      END DO
C
C     Send any remaining rows out to the page manager.
C
      MAXROW = 0
 
      DO I = 1, NITEMS
         MAXROW = MAX( MAXROW, ROW(I) )
      END DO
 
      DO R = 1, MAXROW
         CALL PAGPUT ( PAGE(R) )
         PAGE(R) = ' '
 
         CALL PAGPMT ( DID, VALUE )
 
         IF ( DID .NE. 0 ) THEN
            IF ( EQSTR(VALUE, KEY ) ) THEN
               CALL CHKOUT ( 'TABRPT' )
               RETURN
            END IF
         END IF
 
      END DO
 
      CALL CHKOUT ( 'TABRPT' )
      RETURN
 
C$Procedure      TABABT ( Tabular Report Abort Key )
 
      ENTRY TABABT ( SPCIAL )
 
C$ Abstract
C
C     Set the abort string to use if the page manager prompt has
C     been set.
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
C      REPORTS
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         SPCIAL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SPCIAL     I   String used to indicate report should be aborted.
C
C$ Detailed_Input
C
C     SPCIAL     is an array of strings.  Only the first entry is used.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
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
C     This entry point is used to set the KEY that is used to
C     determine whether or not a report should be aborted
C
C$ Examples
C
C     Suppose that you plan to ask the user whether or not
C     a report should be continued. And that the user should
C     type 'N' if the report should not be continued.
C
C     CALL TABABT ( 'N' )
C
C     DO WHILE ( MOREDATA )
C
C        CALL TABRPT ( .... )
C
C        CALL PAGPMT ( DIDPMT, RESPNS )
C        IF ( DIDPMT .EQ. 1 ) THEN
C           QUIT = EQSTR( RESPNS, 'N' )
C        END IF
C
C        IF ( .NOT. QUIT ) THEN
C
C           see if there is more data
C
C        ELSE
C
C           MOREDATA = .FALSE.
C
C        END IF
C
C     END DO
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 10-SEP-1998 (WLT)
C
C
C-&
 
 
      KEY = SPCIAL(1)
      RETURN
 
 
 
 
C$Procedure      TABHRD ( Tabular Report Hard Space )
 
      ENTRY TABHRD ( NITEMS, SPCIAL )
 
C$ Abstract
C
C    Set the hard space to be used in reports.
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
C     REPORTS
C
C$ Declarations
C
C     IMPLICIT NONE
C     INTEGER               NITEMS
C     CHARACTER*(*)         SPCIAL ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NITEMS     I   Number of items to check in a report.
C     SPCIAL     I   SPCIAL(I)(1:1) contains that hardspace character
C
C     The function returns
C
C$ Detailed_Input
C
C     NITEMS      Number of items to appear in a report.
C
C     SPCIAL      The string SPCIAL(I) contains the character that
C                 should be filtered from the Ith entry and converted
C                 to a space after all justifications and formatting
C                 have been performed.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This entry point allows you to specify some character that
C     should be converted to a blank character after all column
C     settings and justifications have been performed.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 23-SEP-1998 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     set a hard space character
C
C-&
 
      HRD   = ' '
      DOHRD = .FALSE.
 
      DO I = 1, NITEMS
         HRD(I:I) = SPCIAL(I)
         DOHRD    = DOHRD .OR. HRD(I:I) .NE. ' '
      END DO
 
      RETURN
 
 
 
      END
