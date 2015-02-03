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
      SUBROUTINE NSPTAB ( IDLIST, N, FROM, TO, EVERY, FORMAT )
C
C$ Version
C
C      Inspekt Routine Version 1.3.0 22-APR-1997 (WLT)
C
C         Changed call to SETERR regarding unknown FORMAT so that
C         the value of format is set using ERRCH instead of 
C         concatenation.
C
C      Inspekt Routine Version 1.1.0  4-AUG-1995 (WLT)
C
C       Added a blank line to the header of a report and
C       removed one from the bottom of the title of the
C       report
C
C
C
       IMPLICIT NONE
 
      INTEGER               IDLIST ( 0 : * )
      INTEGER               N
      INTEGER               FROM
      INTEGER               TO
      INTEGER               EVERY
      CHARACTER*(*)         FORMAT
 
 
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME = 'NSPTAB' )
 
 
 
C
C     Functions to pass on to others.
C
      EXTERNAL              FETCHA
      EXTERNAL              NSPTV
      EXTERNAL              NSPWLN
 
C
C     Spicelib Functions
C
 
      LOGICAL               RETURN
      INTEGER               RTRIM
 
      LOGICAL               BATCH
C
C     Local Parameters
C
      CHARACTER*(170)       LINE
 
      INTEGER               MOST
      PARAMETER           ( MOST   = 60 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 132 )
 
      INTEGER               OFF
      PARAMETER           ( OFF    = 0  )
 
      INTEGER               ASK
      PARAMETER           ( ASK    = 1  )
 
      INTEGER               ON
      PARAMETER           ( ON     = 2  )
 
 
C
C     Local Variables
C
 
      CHARACTER*(MAXLEN)    MYLINE
      CHARACTER*(MAXLEN)    TITLE
      CHARACTER*(MAXLEN)    STYLE
      CHARACTER*(WDSIZE)    LR
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    SPCIAL( 0 : MOST )
      CHARACTER*(WDSIZE)    TYPE
      CHARACTER*(1920)      TEXT
 
      INTEGER               ADJVAL
      INTEGER               COUNT
      INTEGER               FREQF
      INTEGER               FREQH
      INTEGER               FREQT
      INTEGER               I
      INTEGER               ID
      INTEGER               MYID
      INTEGER               IORDER (    MOST )
      INTEGER               K
      INTEGER               LAST
      INTEGER               M
      INTEGER               LMARGE
      INTEGER               MWIDTH (    MOST )
      INTEGER               NITEMS
      INTEGER               PAGEHT
      INTEGER               PAGEWD
      INTEGER               R
      INTEGER               REQW  (     3    )
      INTEGER               SHIFT
      INTEGER               SIZE  ( 0 : MOST )
      INTEGER               RSCALE
      INTEGER               SKIP
      INTEGER               SPACE
      INTEGER               START
      INTEGER               TOTAL
      INTEGER               W
      INTEGER               W1
      INTEGER               W2
      INTEGER               W3
      INTEGER               WIDTH ( 0 : MOST )
      INTEGER               WMODS (     MOST )
 
      LOGICAL               ADJUST
      LOGICAL               DOTITL
      LOGICAL               FOUND
      LOGICAL               JUSTR ( 0 : MOST )
      LOGICAL               LSTAT  (    3    )
      LOGICAL               MARKED
      LOGICAL               PRESRV( 0 : MOST )
      LOGICAL               SPACED
      LOGICAL               SSTAT (     3   )
C
C     The function RSCALE is used to adjust the widths of columns
C     in cases where they will not all fit on the output page.  This
C     is strictly a hueristic function.  It just seems to look right
C     The scaling is linear as a function of the width of the column
C     Columns 8 wide will be scaled to a width of 6 (a factor of .75)
C     Columns that are 80 wide will be scaled to a width of 36 ( a
C     factor of 0.45 )  The max and min are just there to make
C     sure that things don't get out of control if repeated scalings
C     are applied).
C
      RSCALE(W) = MAX ( 6,
     .            MIN ( 40,
     .                  IDNINT (  ( 0.78333334D0 - (DBLE(W)/240.0D0) )
     .                          * DBLE(W) )))
 
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( RNAME )
 
       LINE = '========================'
     .//      '========================'
     .//      '========================'
     .//      '========================'
     .//      '========================'
     .//      '========================'
     .//      '========================'
 
      IF ( N .GE. MOST ) THEN
 
         CALL SETMSG ( 'You have requested # or more columns for '
     .   //            'in the current report. You must keep the '
     .   //            'total number of columns requested to less '
     .   //            'than # columns. ' )
         CALL ERRINT ( '#', MOST )
         CALL ERRINT ( '#', MOST )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
C
C     We shall leave two spaces between adjacent columns in the
C     table.
C
      SPACE = 2
C
C     The id list we pass to the report generator will
C     begin either at 0 (marked reports) or 1 (unmarked).
C     We will adjust the start point if we find out we have
C     a marked report.
C
      START = 1
 
 
C
C     Fetch the widths of all of the columns
C
      WIDTH (0) =  1
      JUSTR (0) = .FALSE.
      SIZE  (0) =  1
      SPCIAL(0) = ' '
 
      DO I = 1, N
 
         CALL CLQ2ID ( IDLIST(I), MYID )
 
         CALL CLGAI  ( MYID, 'WIDTH', M,      WIDTH(I)   )
         CALL CLGAC  ( MYID, 'JUSTIFICATION', LR         )
 
         JUSTR (I) =  LR   .EQ.  'RIGHT'
         SIZE  (I) =  1
         SPCIAL(I) = ' '
 
      END DO
C
C     Although it should never happen, it is easier to figure it
C     out here than somewhere else.  See if any of the WIDTHS
C     are non-positive
C
      DO I = 1, N
 
         CALL CLQ2ID ( IDLIST(I), MYID )
 
         IF ( WIDTH(I) .LE. 0 ) THEN
            CALL SETMSG ( 'The column, #, has a non-positive width. '
     .      //            'The width reported by the column '
     .      //            'manager was: #' )
 
            CALL CLGAC ( MYID, 'NAME', NAME )
 
            CALL ERRCH  ( '#', NAME     )
            CALL ERRINT ( '#', WIDTH(I) )
            CALL SIGERR ( 'INSPEKT(BADWIDTH)' )
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
 
      END DO
 
      IF      ( FORMAT .EQ. 'MARKED TABULAR'           ) THEN
 
         MARKED    = .TRUE.
         START     =  0
         IDLIST(0) =  0
         NITEMS    =  N + 1
         SPACED    = .FALSE.
         LMARGE    =  4
 
         DO I = START, N
            PRESRV(I) = .FALSE.
         END DO
 
 
      ELSE IF ( FORMAT .EQ. 'MARKED TABULAR PRESERVED' ) THEN
 
         MARKED    = .TRUE.
         START     =  0
         IDLIST(0) =  0
         NITEMS    =  N + 1
         SPACED    = .FALSE.
         LMARGE    =  4
 
         DO I = START, N
            PRESRV(I) = .TRUE.
         END DO
 
      ELSE IF ( FORMAT .EQ. 'TABULAR PRESERVED' ) THEN
 
         MARKED    = .FALSE.
         START     =  1
         IDLIST(0) =  0
         NITEMS    =  N
         SPACED    = .FALSE.
         LMARGE    =  1
 
         DO I = START, N
            PRESRV(I) = .TRUE.
         END DO
 
      ELSE IF ( FORMAT .EQ. 'TABULAR' ) THEN
 
         MARKED    = .FALSE.
         START     =  1
         IDLIST(0) =  0
         NITEMS    =  N
         SPACED    = .FALSE.
         LMARGE    =  1
 
         DO I = START, N
            PRESRV(I) = .FALSE.
         END DO
 
      ELSE IF ( FORMAT .EQ. 'SPACED TABULAR'           ) THEN
 
         MARKED    = .FALSE.
         START     =  1
         IDLIST(0) =  0
         NITEMS    =  N
         SPACED    = .TRUE.
         LMARGE    =  1
 
         DO I = START, N
            PRESRV(I) = .FALSE.
         END DO
 
 
      ELSE IF ( FORMAT .EQ. 'SPACED TABULAR PRESERVED' ) THEN
 
         MARKED    = .FALSE.
         START     =  1
         IDLIST(0) =  0
         NITEMS    =  N
         SPACED    = .TRUE.
         LMARGE    =  1
 
         DO I = START, N
            PRESRV(I) = .TRUE.
         END DO
 
      ELSE
 
         CALL SETMSG ('The format supplied to NSPTAB was not '
     .   //           'one that it is prepared to accept. The '
     .   //           'format requested was: "#".'            )
         CALL ERRCH  ( '#', FORMAT                            )
         CALL SIGERR ('INSPEKT(UNKNOWNFORMAT)'                )
         CALL CHKOUT ( RNAME )
         RETURN
 
 
      END IF
 
C
C     See if the page is wide enough to hold all of the columns.
C
      CALL BBGETI_1 ( 'COPY', 'PAGEWIDTH', K, PAGEWD )
 
      TOTAL = -SPACE
 
      DO I = START, N
         TOTAL = TOTAL + WIDTH(I) + SPACE
      END DO
 
      IF ( TOTAL .GT. PAGEWD ) THEN
 
         CALL BBGETI_1 ( 'COPY', 'AUTOADJUST', K, ADJVAL )
 
C
C        First see if we can adjust anything so that everything
C        will fit on the screen. We shall adjust character column
C        widths by scaling them.  The scale facter shall be given
C        by (-1/240)*WIDTH + .783333334
C
C        This function Scales 8 to 6 and  80 to 36
C
         REQW(1) = -SPACE
         REQW(2) = -SPACE
         REQW(3) = -SPACE
 
         IF ( START .LE. 0 ) THEN
 
            REQW(1) = WIDTH(0) + SPACE
            REQW(2) = WIDTH(0) + SPACE
            REQW(3) = WIDTH(0) + SPACE
 
         END IF
 
         DO I = 1, N
 
            REQW(1) = REQW(1) + SPACE
            REQW(2) = REQW(2) + SPACE
            REQW(3) = REQW(3) + SPACE
 
            CALL CLQ2ID ( IDLIST(I),    MYID )
 
            CALL CLGAC  ( MYID, 'TYPE', TYPE )
 
            IF ( TYPE(1:4) .NE. 'CHAR' ) THEN
               REQW(1) = REQW(1) + WIDTH(I)
               REQW(2) = REQW(2) + WIDTH(I)
               REQW(3) = REQW(3) + WIDTH(I)
            ELSE
               W       = WIDTH(I)
               W1      = RSCALE(W )
               W2      = RSCALE(W1)
               W3      = RSCALE(W2)
 
               REQW(1) = REQW(1)  + W1
               REQW(2) = REQW(2)  + W2
               REQW(3) = REQW(3)  + W3
            END IF
 
         END DO
 
         IF (      REQW(3) .GT. PAGEWD
     .        .OR. ADJVAL .EQ. OFF
     .        .OR. ( BATCH() .AND. ADJVAL .EQ. ASK ) ) THEN
 
            CALL SETMSG ( 'The data requested will not fit within '
     .      //            'the space available on a page.  The '
     .      //            'page width is set at #.  The report '
     .      //            'specified would require a width of #. '
     .      //            'You will need to adjust some combination '
     .      //            'of column selection, column widths, and '
     .      //            'page width.  Alternatively, you can set '
     .      //            'report format to FLAGGED or VERBATIM. '  )
 
 
 
            CALL ERRINT ( '#', PAGEWD )
            CALL ERRINT ( '#', TOTAL  )
            CALL SIGERR ( 'INSPEKT(REPORTTOOWIDE)' )
            CALL CHKOUT ( RNAME )
            RETURN
 
         ELSE IF ( ADJVAL .EQ. ASK  ) THEN
 
 
            CALL NSPGST ( 'SAVE', SSTAT )
            CALL NSPGST ( 'LOG',  LSTAT )
            CALL NSPIOH ( 'SAVE'        )
            CALL NSPIOH ( 'LOG'         )
 
            TEXT = 'The data requested will not fit within the '
     .      //     'current page width. '
     .      //     'The page width is set at #.  The report '
     .      //     'specified would require a width of #. '
     .      //     '/cr/cr However, by temporarily '
     .      //     'adjusting '
     .      //     'the column widths for character columns '
     .      //     '(for this report only) I can '
     .      //     'fit all of the data on the page. /cr/cr'
 
            CALL REPMI ( TEXT, '#', PAGEWD, TEXT )
            CALL REPMI ( TEXT, '#', TOTAL,  TEXT )
 
            CALL NSPMRG   (                   STYLE  )
            CALL SUFFIX   ( 'NEWLINE /cr', 1, STYLE )
 
            CALL NSPWLN   ( ' '                 )
            CALL NICEPR_1 ( TEXT, STYLE, NSPWLN )
            CALL NSPWLN   ( ' '                 )
            CALL CNFIRM_1 ( 'Should I adjust columns widths '
     .      //              '(Y/N)? :', ADJUST )
 
            CALL NSPPST ( 'SAVE', SSTAT )
            CALL NSPPST ( 'LOG',  LSTAT )
 
            IF ( .NOT. ADJUST ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF
 
 
 
         END IF
C
C        We are going to adjust the widths of columns but we are going
C        to attempt to do this in a way that will favor reducing wide
C        columns instead of those that are already narrow.  (Sort of a
C        progressive tax system). However, once a column is adjusted we
C        are not going to adjust it again until we've determined that
C        it still has more than its fair share of the screen.  The
C        adjustment is done using the RSCALE function.  So when a
C        column ID is adjusted its width, W,  is replaced by RSCALE(W).
C        But, the modified width is replaced by
C        RSCALE(RSCALE(W))-4*MODS that way we wont be adjusting it for
C        a while unless it is so wide that even with a second
C        adjustment it would end up being the widest column in the
C        table.  This way once a column has been adjusted twice every
C        other column will have to be adjusted at least once before it
C        will be adjusted a third time.  No column will be adjusted
C        more than three times.
C
         DO I = 1, N
            WMODS (I) = 0
            MWIDTH(I) = WIDTH(I)
         END DO
 
 
         DO WHILE ( TOTAL .GT. PAGEWD )
 
            CALL ORDERI ( MWIDTH, N, IORDER )
 
            LAST = N
            ID   = IORDER(LAST)
 
            CALL CLQ2ID( IDLIST(ID),  MYID        )
            CALL CLGAC ( MYID,       'TYPE', TYPE )
 
 
            DO WHILE (       TYPE(1:4) .NE. 'CHAR'
     .                  .OR. WMODS(ID) .GE.  3   )
 
               LAST = LAST - 1
               ID   = IORDER(LAST)
               CALL CLQ2ID ( IDLIST(ID), MYID        )
               CALL CLGAC  ( MYID,      'TYPE', TYPE )
 
            END DO
 
            W          = WIDTH(ID)
            W          = RSCALE(W)
            TOTAL      = TOTAL + W - WIDTH(ID)
            WIDTH (ID) = W
            WMODS (ID) = WMODS(ID) + 1
            MWIDTH(ID) = RSCALE(W)  - 4*WMODS(ID)
 
            CALL ORDERI ( MWIDTH, N, IORDER )
 
         END DO
 
 
      END IF
 
C
C     If still, here, the page is wide enough to hold everything.
C     It is time to fetch and set all of the page attributes.
C
      CALL BBGETI_1 ( 'COPY', 'PAGEHEIGHT',      K, PAGEHT )
      CALL BBGETI_1 ( 'COPY', 'TITLEFREQUENCY',  K, FREQT  )
      CALL BBGETI_1 ( 'COPY', 'HEADERFREQUENCY', K, FREQH  )
 
      FREQF = -1
 
 
      CALL BBGETC_1 ( 'COPY', 'PAGETITLE',          K, TITLE )
      CALL BBGETC_1 ( 'COPY', 'TITLEJUSTIFICATION', K, LR    )
 
      R      = RTRIM(TITLE)
      DOTITL = TITLE .NE. ' '
 
      IF      ( LR .EQ. 'LEFT'   ) THEN
 
         SHIFT = 0
 
      ELSE IF ( LR .EQ. 'RIGHT'  ) THEN
 
         SHIFT = TOTAL - R
 
      ELSE IF ( LR .EQ. 'CENTER' ) THEN
 
         SHIFT = (TOTAL - R)/2
 
      END IF
 
      CALL SHIFTR ( TITLE, SHIFT, ' ', TITLE )
 
 
      CALL PAGRST
 
      CALL PAGSET ( 'TITLEFREQUENCY',  FREQT  )
      CALL PAGSET ( 'HEADERFREQUENCY', FREQH  )
      CALL PAGSET ( 'FOOTERFREQUENCY', FREQT  )
      CALL PAGSET ( 'PAGEWIDTH',       PAGEWD )
      CALL PAGSET ( 'PAGEHEIGHT',      PAGEHT )
 
      IF ( DOTITL ) THEN
 
         CALL PAGSCN ( 'TITLE' )
         CALL PAGPUT ( ' '   )
         CALL PAGPUT ( ' '   )
         CALL PAGPUT ( TITLE )
 
      END IF
C
C     Now set up the header.
C
      CALL PAGSCN('HEADER')
      CALL PAGPUT(' ')
      CALL TABRPT( N,      IDLIST(1),  SIZE  (1),
     .                     WIDTH (1),  JUSTR (1),
     .                     PRESRV(1),  SPCIAL(1),
     .                     LMARGE,     SPACE,
     .                     FETCHA  )
 
      MYLINE(1:TOTAL ) = LINE
 
      IF ( TOTAL .LT. MAXLEN ) THEN
         MYLINE(1+TOTAL:) = ' '
      END IF
 
      CALL PAGPUT( MYLINE   )
C
C     Now reset the left margin to 1 since reports begin in
C     column 1 regardless of where their headers start.
C
      LMARGE = 1
C
C     Finally, begin fetching and printing output from the
C     E-kernel.
C
      CALL PAGSCN('BODY' )
 
      DO I = 1, FROM
         CALL CLADV ( FOUND )
      END DO
 
      COUNT = FROM
 
      DO WHILE ( FOUND )
C
C        We need to get the size of each of the columns for this
C        row. (Note if IDLIST(0) is active we don't need to adjust
C        it since it is used only for the report marker and has
C        a fixed size of 1.)
C
         DO I = 1, N
            CALL CLNCMP( IDLIST(I), SIZE(I) )
         END DO
 
         CALL TABRPT ( NITEMS, IDLIST(START), SIZE  (START),
     .                         WIDTH (START), JUSTR (START),
     .                         PRESRV(START), SPCIAL(START),
     .                         LMARGE,        SPACE,
     .                         NSPTV )
 
         IF ( SPACED ) THEN
            CALL PAGPUT ( ' ' )
         END IF
 
C
C        Advance to the next row of the query.
C
         SKIP = 0
         DO WHILE ( FOUND .AND. SKIP .LT. EVERY )
            CALL CLADV ( FOUND )
            SKIP  = SKIP  + 1
            COUNT = COUNT + 1
         END DO
 
         IF ( COUNT .GT. TO ) THEN
            FOUND = .FALSE.
         END IF
 
      END DO
C
C     Finally fill out the rest of the page.
C
      CALL PAGSET ( 'TITLEFREQUENCY',  0  )
      CALL PAGSET ( 'HEADERFREQUENCY', 0  )
 
      CALL PAGPUT ( ' ' )
      CALL PAGPUT ( ' ' )
 
      CALL CHKOUT ( RNAME )
      RETURN
      END
 
 
 
 
 
 
