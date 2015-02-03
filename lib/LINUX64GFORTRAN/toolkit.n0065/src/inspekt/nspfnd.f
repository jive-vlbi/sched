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
 
      SUBROUTINE NSPFND ( COMMND, ERROR )
C
C     This routine handles the task of setting up the information
C     needed to format the results of an E-kernel query and
C     passes this information on to the appropriate formatting
C     routines.
C
C$ Version
C
C     Inspekt Routine Version 4.1.0 21-Nov-1995 (WLT)
C
C       Fixed bug in table name expansion.
C
C     Inspekt Routine Version 4.0.0 2-NOV-1995 (WLT)
C
C       The ability to recognized patterns for column names
C       and tables was added.  Patterns when encountered are
C       now replaced by the matching name.  If a pattern occurs
C       that is not uniquely matched an error is set and the
C       routine returns.
C
C     Inspekt Routine Version 3.1.0 4-AUG-1995 (WLT)
C
C       Fixed a counting problem in the sample counting
C       and select subsample so that it counts correctly
C       on output.
C
C     Inspekt Routine Version 3.0.0 3-AUG-1995 (WLT)
C
C       Modified what is sent to CLMGR to reflect change
C       in the EK interface routines.  Actually, Nat is
C       responsible for this change.
C
C     Inspekt Routine Version 2.0.0 7-APR-1995 (WLT)
C
C        The unused variable RNAMEC was removed.
C
      IMPLICIT NONE
 
      CHARACTER*(*)         COMMND
      CHARACTER*(*)         ERROR   ( 2 )
 
 
 
 
 
 
      CHARACTER*(6)         RNAME
 
 
C
C     Inspekt's output function
C
      EXTERNAL              NSPWLN
C
C     SPICELIB functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN
C
C     Meta/2 Functions
C
      INTEGER               M2HAVE
      LOGICAL               M2XIST
      LOGICAL               BATCH
C
C     Interface to the SPICELIB error handling.
C
      LOGICAL               HAVE
 
C
C     Meta/2 syntax definition variables.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE  = 80 )
 
      INTEGER               NUPPER
      PARAMETER           ( NUPPER = 5 )
 
      INTEGER               NSYN
      PARAMETER           ( NSYN   = NUPPER )
 
      INTEGER               SYNLEN
      PARAMETER           ( SYNLEN = 700 )
 
      CHARACTER*(WDSIZE)    SYNKEY ( LBCELL : NSYN )
      INTEGER               SYNPTR ( LBCELL : NSYN )
      CHARACTER*(SYNLEN)    SYNVAL ( LBCELL : NSYN )
 
 
C
C     SELWLD
C     SELCOL
C     SAMFLL
C     SAMFOL
C     SAMPRC
C
 
      INTEGER               SELWLD
      PARAMETER           ( SELWLD = 1 )
 
      INTEGER               SELCOL
      PARAMETER           ( SELCOL = SELWLD + 1 )
 
      INTEGER               SAMFLL
      PARAMETER           ( SAMFLL = SELCOL + 1 )
 
      INTEGER               SAMFOL
      PARAMETER           ( SAMFOL = SAMFLL + 1 )
 
      INTEGER               SAMPRC
      PARAMETER           ( SAMPRC = SAMFOL + 1 )
 
 
 
 
 
 
 
 
C
C     Local Variables
C
      INTEGER               MXCOLS
      PARAMETER           ( MXCOLS = 100 )
 
      INTEGER               QLEN
      PARAMETER           ( QLEN   = 1024 )
 
      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = 800 )
 
 
 
 
 
      CHARACTER*(1)         BS
      CHARACTER*(1)         OPTNAM (3)
      CHARACTER*(QLEN)      COPY
      CHARACTER*(LNGSIZ)    MESSGE
      CHARACTER*(LNSIZE)    OPTTXT (3)
      CHARACTER*(LNSIZE)    STYLE
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(QLEN)      COLS
      CHARACTER*(QLEN)      QUERY
      CHARACTER*(WDSIZE)    FORMAT
 
 
      INTEGER               B
      INTEGER               CENTER
      INTEGER               COUNT
      INTEGER               E
      INTEGER               EVERY
      INTEGER               FROM
      INTEGER               I
      INTEGER               IDLIST ( 0 : MXCOLS )
      INTEGER               K
      INTEGER               LAST
      INTEGER               LRADUS
      INTEGER               N
      INTEGER               NOPT
      INTEGER               NUM
      INTEGER               OINDNT
      INTEGER               OPTION
      INTEGER               R
      INTEGER               THSHLD
      INTEGER               TINDNT
      INTEGER               TO
      INTEGER               URADUS
 
 
      LOGICAL               DISPLY
      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               LSTAT  ( 3 )
      LOGICAL               SAMPLE
      LOGICAL               SELECT
      LOGICAL               SSTAT  ( 3 )
      LOGICAL               SUMMRZ
C
C     Save everything.
C
      SAVE
 
      DATA      FIRST
     .       / .TRUE. /
 
      DATA      THSHLD
     .       /  200    /
 
      DATA      RNAME   / 'NSPFND'  /
 
      DATA   (  SYNVAL(I), I=LBCELL,0)
     .       / ' ', ' ', ' ', ' ', ' ',  ' ' /
 
 
 
C
C     Standard Spicelib error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( RNAME )
C
C     On the first pass establish the syntax that this routine
C     is responsible for recognizing.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
         BS    =  '@'
 
         SYNVAL  ( SELWLD ) = 'SELECT[select] *[wildcard] '
     .                      //       'FROM[f] (1:)#word[query] '
     .                      //       '(0:2){ ORDER BY '
     .                      //       '(1:100)@word[by] '
     .                      //       '| WHERE (1:)#word[where] } '
 
 
         SYNVAL  ( SELCOL ) = 'SELECT[select] (1:100)#word[columns] '
     .                      //       'FROM[f] (1:)#word[query] '
     .                      //       '(0:2){ ORDER BY '
     .                      //       '(1:100)@word[by]   '
     .                      //       '| WHERE (1:)#word[where] } '
 
 
 
C
C
C        Below are the full set of SAMPLE commands.
C
C        SAMPLE[sample] @int(1:)[count]
C        (1:1){ SELECT[select]              (1:100)@word[columns]
C             | SELECT[select] *[wildcard]
C             }
C        FROM[f] (1:)@word[query]
C        (0:1){ ORDER BY (1:100)@word[by] }
C
C
C
C        SAMPLE[sample]
C        (1:1){ FIRST[first] @int(1:)[count]
C             | LAST[last]   @int(1:)[count]
C             }
C        (1:1){ SELECT[select]              (1:100)@word[columns]
C             | SELECT[select] *[wildcard]
C             }
C        FROM[f]    (1:)@word[query]
C        (0:1){ ORDER BY (1:100)@word[by] }
C
C
C
C        SAMPLE[sample] @int(1:)[count]
C        (1:1){ UP TO       @int(0:100)[upto]     EVERY @int(1:)[every]
C             | UP TO       @int(0:100)[upto]
C             | STARTING AT @int(0:100)[starting] EVERY @int(1:)[every]
C             | STARTING AT @int(0:100)[starting]
C             | CENTER   AT @int(0:100)[center]   EVERY @int(1:)[every]
C             | CENTER   AT @int(0:100)[center]
C             | FROM @int(0:100)[from] TO @int(0:100)[to]
C             }
C        (1:1){ SELECT[select]              (1:100)@word[columns]
C             | SELECT[select] *[wildcard]
C             }
C        FROM[f]    (1:)@word[query]
C        (0:1){ ORDER BY (1:100)@word[by] }
C
C
C
C
 
         SYNVAL ( SAMFLL ) = 'SAMPLE[sample] @int(1:)[count] '
     .   //                  '(1:1){ SELECT[select] '
     .   //                  '(1:100)@word[columns] | '
     .   //                  'SELECT[select] *[wildcard] } '
     .   //                  'FROM[f] (1:)@word[query] '
     .   //                  '(0:2){ ORDER BY (1:100)@word[by] '
     .   //                  '| WHERE (1:)#word[where] } '
 
 
         SYNVAL ( SAMFOL ) = 'SAMPLE[sample] (1:1){ FIRST[first] '
     .   //                  '@int(1:)[count] | LAST[last] '
     .   //                  '@int(1:)[count] } (1:1){ '
     .   //                  'SELECT[select] '
     .   //                  '(1:100)@word[columns] | '
     .   //                  'SELECT[select] *[wildcard] } '
     .   //                  'FROM[f] (1:)@word[query] '
     .   //                  '(0:2){ ORDER BY (1:100)@word[by] '
     .   //                  '| WHERE (1:)#word[where] } '
 
 
         SYNVAL ( SAMPRC ) = 'SAMPLE[sample] @int(1:)[count] '
     .   //                  '(1:1){ UP TO @int(0:100)[upto] '
     .   //                  'EVERY @int(1:)[every] | UP TO '
     .   //                  '@int(0:100)[upto] | STARTING AT '
     .   //                  '@int(0:100)[starting] EVERY '
     .   //                  '@int(1:)[every] | STARTING AT '
     .   //                  '@int(0:100)[starting] | CENTER AT '
     .   //                  '@int(0:100)[center] EVERY '
     .   //                  '@int(1:)[every] | CENTER AT '
     .   //                  '@int(0:100)[center] | FROM '
     .   //                  '@int(0:100)[from] TO '
     .   //                  '@int(0:100)[to] } (1:1){ '
     .   //                  'SELECT[select] '
     .   //                  '(1:100)@word[columns] | '
     .   //                  'SELECT[select] *[wildcard] } '
     .   //                  'FROM[f] (1:)@word[query] '
     .   //                  '(0:2){ ORDER BY (1:100)@word[by] '
     .   //                  '| WHERE (1:)#word[where] } '
 
         DO I = 1, NUPPER
            CALL REPLCH ( SYNVAL(I), '#', BS, SYNVAL(I)        )
         END DO
 
         CALL M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL )
 
      END IF
 
 
 
C
C     See if this command matches a known syntax.  If it doesn't
C     there is no point in hanging around.
C
      CALL M2CHCK ( COMMND, SYNKEY, SYNPTR, SYNVAL, ERROR )
 
      IF ( HAVE(ERROR) ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
C
C     Now expand the various parts of the command.  Table
C     templates first. Get the portion of the input command
C     that corresponds to the "query" portion.
C
      LAST = M2HAVE ( 'query' )
 
      CALL M2VGET   ( 'query', 1,    FOUND, B, I )
      CALL M2VGET   ( 'query', LAST, FOUND, I, E )
 
      COPY  = COMMND
      QUERY = COMMND(B:E)
 
      CALL NAMXPN ( QUERY, 'TABLE', ERROR )
 
      IF ( HAVE(ERROR) ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
C
C     Put the resolved version of QUERY back into the command.
C     Next expand column names.
C
      R = RTRIM(QUERY)
 
      CALL REPSUB ( COMMND, B, E, QUERY(1:R), COMMND )
      CALL NAMXPN ( COMMND,       'COLUMN',   ERROR  )
 
      IF ( HAVE(ERROR) ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
C
C     If we ended up changing the command as a result of
C     name expansion, we log that in the log file.  Also
C     we re-check the syntax of the command.  The syntax
C     should still be ok.  But parts of the commmand have
C     probably moved so that we need to re-determine where
C     all the parts are located.
C
      IF ( COPY .NE. COMMND ) THEN
 
         CALL NSPLOG ( COMMND, .TRUE. )
         CALL M2CHCK ( COMMND, SYNKEY, SYNPTR, SYNVAL, ERROR )
 
         IF ( HAVE(ERROR) ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
 
      END IF
 
 
 
 
C
C     If we get here, we have what may pass for a legitimate command.
C     We are going to need to construct the list of columns that
C     will be output in the report.
C
C     The wildcard case is the easiest.  Just fetch all of the
C     column names from the column manager.  If columns have
C     actually been specified, we are going check that the columns
C     actually exist before going on.  If a column doesn't exist
C     we'll try to correct the spelling.  If all that fails, we
C     let the user know that a column isn't on the list of known
C     columns.  NOTE: we do not check to see if the columns in
C     the query are recognized.  It would be friendlier to do
C     so, but not in the prototype.
C
C
C     Next look up the report format since it will decide where
C     things are going to go from here.
C
 
 
      CALL BBGETC_1 ( 'COPY', 'FORMAT', N, FORMAT )
 
 
      IF      ( M2XIST( 'wildcard' ) ) THEN
 
         COLS = '*'
 
         CALL M2VGET ( 'query', 1, FOUND, B, E )
         QUERY  = COMMND(B:E)
 
 
 
      ELSE
 
         CALL M2GETA ( 'columns', COMMND, FOUND, COLS )
         CALL M2VGET ( 'query', 1, FOUND, B, E )
         QUERY = COMMND(B:)
 
      END IF
C
C     Issue the query. Keep in mind that IDLIST has some extra
C     room at the beginning for "special columns" needed by the
C     various reporting functions.
C
      CALL CLSCOP ( COLS, COMMND, N, IDLIST(1), NUM, ERROR )
 
      IF ( HAVE(ERROR) ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
      IF (  NUM .EQ. 0 ) THEN
 
 
         CALL NSPWLN ( ' ' )
         CALL NSPWLN ( '  There were no matches to the supplied query.')
         CALL NSPWLN ( ' ' )
 
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
C
C     Determine what kind of query we have.  Is it a subsample,
C     summarize or straigh selection.
C
      SAMPLE = M2XIST ( 'sample'    )
      SUMMRZ = M2XIST ( 'summarize' )
 
      SELECT = .NOT. ( SAMPLE .OR. SUMMRZ )
 
      IF ( SAMPLE ) THEN
C
C        If it is a subsample, we have a lot of options to check.
C        All subsamples require the user to supply a number of
C        items to choose from the selection match.
C
         CALL M2GETI ( 'count', COMMND,  FOUND, COUNT )
 
         IF ( M2XIST('last') ) THEN
C
C           In this case we show the last count items.
C
            EVERY = 1
            FROM  = MAX ( NUM - COUNT + 1, 1 )
            TO    = NUM
 
         ELSE IF ( M2XIST('first') ) THEN
C
C           Here we show the first count.
C
            EVERY = 1
            FROM  = 1
            TO    = MIN ( COUNT, NUM )
 
         ELSE IF ( M2XIST('from') ) THEN
C
C           In this case we use the percentiles specified
C           by from and to determine the bounds and compute
C           the skip factor using the count supplied.
C
            CALL M2GETI ( 'from', COMMND, FOUND, FROM )
            CALL M2GETI ( 'to',   COMMND, FOUND, TO   )
 
            FROM  = 1 + ((NUM-1)*FROM)/100
            TO    = 1 + ((NUM-1)*TO  )/100
            EVERY = MAX( 1, (TO - FROM)/COUNT )
 
         ELSE IF ( M2XIST('upto') )THEN
C
C           In this case, we have want to show some subset
C           up to a specified percentile.
C
            EVERY = 1
 
            CALL M2GETI ( 'upto',  COMMND, FOUND, TO    )
            CALL M2GETI ( 'every', COMMND, FOUND, EVERY )
 
            TO   = 1 + ((NUM-1)*TO)/100
            FROM = MAX ( 1,  TO - (COUNT-1)*EVERY )
 
         ELSE IF ( M2XIST('starting') ) THEN
C
C           Here we want to show samples starting at a specified
C           percentile.
C
            EVERY = 1
            CALL M2GETI ( 'starting', COMMND, FOUND, FROM )
            CALL M2GETI ( 'every',    COMMND, FOUND, EVERY )
 
            FROM = 1 + ((NUM-1)*FROM)/100
            TO   = MIN ( NUM,   FROM + (COUNT-1)*EVERY )
 
 
         ELSE IF ( M2XIST('center')   ) THEN
 
C
C           In this case we want to show samples centered about
C           a user specified percentile.
C
            EVERY  = 1
 
            CALL M2GETI ( 'center', COMMND, FOUND, CENTER )
            CALL M2GETI ( 'every',  COMMND, FOUND, EVERY  )
 
            CENTER = 1 + ((NUM-1)*CENTER)/100
 
            LRADUS = (COUNT - 1)/2
            URADUS =  COUNT - LRADUS
 
            FROM   = CENTER - LRADUS*EVERY
            TO     = CENTER + URADUS*EVERY
 
            FROM   = MAX(   1, FROM )
            TO     = MIN( NUM, TO   )
 
         ELSE
C
C           This is the simplest to specify.  Start at the beginning
C           and go to the end skipping just enough in between to
C           get the number of samples the user requested.
C
            EVERY = MAX ( 1, NUM/COUNT )
            FROM  = 1
            TO    = 1 + EVERY*(COUNT - 1 )
 
         END IF
 
      ELSE IF ( SELECT ) THEN
C
C        This is a straight selection.  We need to see if we
C        are going to exceed the deluge warning limit.
C
         CALL BBGETI_1 ( 'COPY', 'REPORTLIMIT', K, THSHLD )
C
C        And until we have other information, we are going to
C        just assume we are printing everything.
C
         FROM  = 1
         TO    = NUM
         EVERY = 1
 
      END IF
 
 
 
 
      IF ( NUM .GT. THSHLD .AND. SELECT ) THEN
C
C        We are going to give the user the option to turn this
C        selection request into a SAMPLE DELUGE LIMIT SELECT ...
C        request.
C
         MESSGE = 'There were # matches to the supplied query. '
     .   //       'The threshold for automatic display is #. '
 
 
         CALL REPMI   ( MESSGE, '#', NUM,    MESSGE )
         CALL REPMI   ( MESSGE, '#', THSHLD, MESSGE )
 
         CALL NSPGST   ( 'LOG',  LSTAT )
         CALL NSPIOH   ( 'LOG'         )
 
         CALL NSPGST   ( 'SAVE', SSTAT )
         CALL NSPIOH   ( 'SAVE'        )
 
         CALL NSPMRG   (                   STYLE  )
         CALL NICEPR_1 ( MESSGE, STYLE,    NSPWLN )
 
         TITLE     = 'Do you want to display all, some or none?'
         TINDNT    =  2
         NOPT      =  3
         OINDNT    =  5
         OPTNAM(1) = 'A'
         OPTTXT(1) = 'All.  Display all requested items'
         OPTNAM(2) = 'S'
         OPTTXT(2) = 'Some. Display a sub-sample of #.'
         OPTNAM(3) = 'N'
         OPTTXT(3) = 'None. Discard this query.'
         CALL REPMI ( OPTTXT(2), '#', THSHLD, OPTTXT(2) )
 
         IF ( .NOT. BATCH() ) THEN
            CALL GETOPT_2 ( TITLE,  TINDNT, NOPT, OPTNAM, OPTTXT,
     .                      OINDNT, OPTION )
            DISPLY = OPTION .NE. 3
            SAMPLE = OPTION .EQ. 2
            SELECT = OPTION .EQ. 1
         ELSE
            DISPLY = .TRUE.
            SAMPLE = .FALSE.
            SELECT = .TRUE.
         END IF
 
 
         IF ( SAMPLE ) THEN
            EVERY = MAX ( 1, NUM/THSHLD )
            FROM  = 1
            TO    = 1 + EVERY*(THSHLD - 1)
         END IF
 
         CALL NSPPST   ('LOG',  LSTAT )
         CALL NSPPST   ('SAVE', SSTAT )
 
         IF ( .NOT. DISPLY ) THEN
 
            CALL SUFFIX   ( 'No matching items were displayed. ',
     .                       1, MESSGE      )
            CALL NSPGST   ( 'SCREEN', SSTAT )
            CALL NSPIOH   ( 'SCREEN'        )
            CALL NSPWLN   ( ' ' )
            CALL NSPMRG   (         STYLE         )
            CALL NICEPR_1 ( MESSGE, STYLE, NSPWLN )
            CALL NSPPST   ('SCREEN',    SSTAT )
            CALL CHKOUT   ( RNAME )
            RETURN
 
         END IF
 
      END IF
 
 
 
      IF ( FORMAT .EQ. 'DELIMITED' ) THEN

         CALL NSPDEL( IDLIST, N, FROM, TO, EVERY )
         
      ELSE IF ( FORMAT .EQ. 'DELIMITED PRESERVED' ) THEN

         CALL NSPDEL( IDLIST, N, FROM, TO, EVERY )
         
      ELSE IF (   FORMAT .EQ. 'FLAGGED' ) THEN
 
         CALL NSPFLG ( IDLIST, N, FROM, TO, EVERY, FORMAT )
 
      ELSE IF ( FORMAT .EQ. 'FLAGGED PRESERVED' ) THEN
 
         CALL NSPFLG ( IDLIST, N, FROM, TO, EVERY, FORMAT )
 
      ELSE IF ( FORMAT .EQ. 'VERBATIM' ) THEN
 
         CALL NSPVRB ( IDLIST, N, FROM, TO, EVERY )
 
      ELSE
 
         CALL NSPTAB ( IDLIST, N, FROM, TO, EVERY,FORMAT )
 
      END IF
 
 
      CALL CHKOUT ( RNAME )
      RETURN
 
      END
