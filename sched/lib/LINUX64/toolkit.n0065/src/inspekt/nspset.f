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
 
      SUBROUTINE NSPSET ( COMMND, ERROR )
 
      IMPLICIT NONE

C     March 26, 2003
C
C        Added the SET FORMAT DELIMITED command
C
C     August 29, 1996
C
C        Increase the size of a word from 32 to 80 characters.
C
C     Nov 21, 1995
C
C        Removed the SET EDITOR command since its already supported
C        by the built in command editor code.
C
C     Nov 2, 1995
C
C        Added the ability to use templates instead of full
C        names when requesting information about a column.
C
C     Sep 7, 1995
C
C        Added    DEFAULT INTEGER FORMAT
C                 DEFAULT FLOATING FORMAT
C
C        Synstax and actions to the routine.
C
C     Sep 6, 1995
C
C       Changed the syntax
C
C       from SET TIME FORMAT
C       to   SET DEFAULT TIME FORMAT
C
C     Aug 29, 1995
C
C       Added the syntax SET HELP NO WAIT (two words) in addition to
C       the original SET HELP NOWAIT.
C
C     Aug 15, 1995
C
C     Increase the declared length of syntax templates from 120 to 150
C
C
C     This routine allows users to set parameters that will be used
C     to establish the appearance of reports.
C
 
      CHARACTER*(*)         COMMND
      CHARACTER*(*)         ERROR   ( 2 )
 
 
 
 
 
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'NSPSET' )
 
      CHARACTER*(*)         RNAMEC
      PARAMETER           ( RNAMEC = 'NSPSET:' )
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               EQSTR
 
C
C     Interface to the SPICELIB error handling.
C
      LOGICAL               HAVE
 
C
C     Meta/2 functions
C
      LOGICAL               M2XIST
 
C
C     Parameters used in parsing.
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               STRING
      PARAMETER           ( STRING = 1 )
 
      INTEGER               INT
      PARAMETER           ( INT    = 2 )
 
      INTEGER               OFF
      PARAMETER           ( OFF    = 0 )
 
      INTEGER               ASK
      PARAMETER           ( ASK    = 1 )
 
      INTEGER               ON
      PARAMETER           ( ON     = 2 )
 
 
C
C     Meta/2 syntax definition variables.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 80 )
 
      INTEGER               NUPPER
      PARAMETER           ( NUPPER = 26 )
 
      INTEGER               NSYN
      PARAMETER           ( NSYN   = NUPPER )
 
      INTEGER               SYNLEN
      PARAMETER           ( SYNLEN = 150 )
 
      CHARACTER*(WDSIZE)    SYNKEY ( LBCELL : NSYN )
      INTEGER               SYNPTR ( LBCELL : NSYN )
      CHARACTER*(SYNLEN)    SYNVAL ( LBCELL : NSYN )
 
 
 
C
C     DICTNY
C     FLAGGD
C     VRBATM
C     SPACED
C     MARKED
C     PLAIN
C     FRMMRK
C     COLWID
C     COLJST
C     ALIAS
C     COLFMT
C     TIMFMT
C     PAGEHT
C     PAGEWD
C     PAGETL
C     TLFREQ
C     TLJUST
C     HDFREQ
C     RPTLIM
C     AUTOAD
C
 
      INTEGER               DICTNY
      PARAMETER           ( DICTNY =  1 )
 
      INTEGER               FLAGGD
      PARAMETER           ( FLAGGD = DICTNY + 1 )
 
      INTEGER               VRBATM
      PARAMETER           ( VRBATM = FLAGGD + 1 )
 
      INTEGER               SPACED
      PARAMETER           ( SPACED = VRBATM + 1 )
 
      INTEGER               MARKED
      PARAMETER           ( MARKED = SPACED + 1 )
 
      INTEGER               PLAIN
      PARAMETER           ( PLAIN  = MARKED + 1 )

      INTEGER               DELMTD
      PARAMETER           ( DELMTD = PLAIN  + 1 )

      INTEGER               DELMTS
      PARAMETER           ( DELMTS = DELMTD + 1 )
 
      INTEGER               FRMMRK
      PARAMETER           ( FRMMRK = DELMTS  + 1 )
 
      INTEGER               COLWID
      PARAMETER           ( COLWID = FRMMRK + 1 )
 
      INTEGER               COLJST
      PARAMETER           ( COLJST = COLWID + 1 )
 
      INTEGER               ALIAS
      PARAMETER           ( ALIAS  = COLJST + 1 )
 
      INTEGER               COLFMT
      PARAMETER           ( COLFMT = ALIAS  + 1 )
 
      INTEGER               TIMFMT
      PARAMETER           ( TIMFMT = COLFMT + 1 )
 
      INTEGER               PAGEHT
      PARAMETER           ( PAGEHT = TIMFMT + 1 )
 
      INTEGER               PAGEWD
      PARAMETER           ( PAGEWD = PAGEHT + 1 )
 
      INTEGER               PAGETL
      PARAMETER           ( PAGETL = PAGEWD + 1 )
 
      INTEGER               TLFREQ
      PARAMETER           ( TLFREQ = PAGETL + 1 )
 
      INTEGER               TLJUST
      PARAMETER           ( TLJUST = TLFREQ + 1 )
 
      INTEGER               HDFREQ
      PARAMETER           ( HDFREQ = TLJUST + 1 )
 
      INTEGER               RPTLIM
      PARAMETER           ( RPTLIM = HDFREQ + 1 )
 
      INTEGER               AUTOAD
      PARAMETER           ( AUTOAD = RPTLIM + 1 )
 
      INTEGER               HPAUSE
      PARAMETER           ( HPAUSE = AUTOAD + 1 )
 
      INTEGER               INTFMT
      PARAMETER           ( INTFMT = HPAUSE  + 1 )
 
      INTEGER               DPFMT
      PARAMETER           ( DPFMT  = INTFMT + 1 )
 
 
 
 
 
C
C     There are seven different formats supported.  The names
C     associated with these formats shall be regarded as
C     global variables.  We need arrays to hold these names.
C
      INTEGER               NFMT
      PARAMETER           ( NFMT = 8 )
 
      CHARACTER*(WDSIZE)    FMT  ( NFMT )
      CHARACTER*(WDSIZE)    FORM ( NFMT )
 
C
C     Local Variables
C
      CHARACTER*(1)         BS
      CHARACTER*(WDSIZE)    ATTR
      CHARACTER*(WDSIZE)    COLNAM
      CHARACTER*(WDSIZE)    LITNAM
      CHARACTER*(WDSIZE)    ITEM
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    TEMP
      CHARACTER*(WDSIZE)    QUOTE
      CHARACTER*(WDSIZE)    DELIM
      CHARACTER*(LNSIZE)    SVALUE
      CHARACTER*(LNSIZE)    MARGIN
 
 
      INTEGER               ATYPE
      INTEGER               I
      INTEGER               ID
      INTEGER               IVALUE
      INTEGER               W
      INTEGER               N
      INTEGER               NUMFND
      INTEGER               P
      INTEGER               USEID
 
      LOGICAL               COLCOM
      LOGICAL               FIRST
      LOGICAL               FOUND
C
C     Save everything.
C
      SAVE
 
      DATA        FIRST / .TRUE. /
 
      DATA      (  SYNVAL(I), I = -5, 0 )
     .          /  ' ',  ' ', ' ', ' ', ' ', ' ' /
 
      DATA      ( FMT(I), FORM(I), I = 1,NFMT )
     .          / 'dict',      'FLAGGED',
     .            'flag',      'FLAGGED PRESERVED',
     .            'verbat',    'VERBATIM',
     .            'spaced',    'SPACED TABULAR ',
     .            'marked',    'MARKED TABULAR ',
     .            'plain',     'TABULAR',
     .            'delimited', 'DELIMITED',
     .            'delimited', 'DELIMITED' /
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
 
C
C     The syntax definitions follow.
C
         SYNVAL ( DICTNY ) = 'FORMAT[fmt] '
     .                     //       'FLAGGED[dict] '
 
         SYNVAL ( FLAGGD ) = 'FORMAT[fmt] FLAGGED '
     .                      //      'PRESERVED[flag] '
 
         SYNVAL ( VRBATM ) = 'FORMAT[fmt] VERBATIM[verbat]'
 
         SYNVAL ( SPACED ) = 'FORMAT[fmt] SPACED[spaced] '
     .                     //       'TABULAR '
     .                     //'(0:1){ PRESERVED[preserved] } '
 
         SYNVAL ( MARKED ) = 'FORMAT[fmt] MARKED[marked] '
     .                     //       'TABULAR '
     .                     //'(0:1){ PRESERVED[preserved] } '
 
         SYNVAL ( PLAIN ) = 'FORMAT[fmt] TABULAR[plain] '
     .                     //'(0:1){ PRESERVED[preserved] } '

         SYNVAL ( DELMTD ) = 'FORMAT[fmt] DELIMITED[delimited] '
     .                     //'(0:3){ DELIMITER #word(%)[delimiter]'
     .                     //'     | QUOTE #word(%)[quote] '
     .                     //'     | PRESERVED[preserved] } '

         SYNVAL ( DELMTS ) = 'FORMAT[fmt] DELIMITED[delimited] '
     .                     //'(0:3){ DELIMITER SPACE[delimiter] '
     .                     //'     | QUOTE #word(%)[quote] '
     .                     //'     | PRESERVED[preserved] } '
         
C
C     The syntax below allows the user to change the leadoff
C     character that is used for the MARKED TABULAR reports.
C
         SYNVAL ( FRMMRK ) = 'FORMAT MARK '
     .                     //       '#word(%)[fmtmark] '
 
C
C     Below is the syntax for column attributes that the use
C     can control in reports.
C
         SYNVAL ( COLWID ) = 'COLUMN #word[colnam] '
     .                     //       'WIDTH #int(8:)[columnwdth] '
 
         SYNVAL ( COLJST ) = 'COLUMN #word[colnam] '
     .                     //       'JUSTIFICATION '
     .                     //       '(1:1){ LEFT[left] '
     .                     //       '     | RIGHT[right] } '
 
         SYNVAL ( ALIAS  ) = 'COLUMN #word[colnam] '
     .                     //       'HEADING (1:)#word[alias]'
 
 
         SYNVAL ( COLFMT) = 'COLUMN #word[colnam] '
     .                     //       'FORMAT (1:)#word[colfmt]'
 
 
C
C     The TIME column is always present and has special formatting
C     requirements.  Users may set these using the command below.
C
         SYNVAL ( TIMFMT ) = 'DEFAULT TIME FORMAT '
     .                     //       '(1:)#word[timefmt] '
 
 
 
C
C     The next set of command syntax definitions give the user
C     control over the dimensions of the output.
C
 
         SYNVAL ( PAGEHT ) = 'PAGE HEIGHT '
     .                     //       '#int(20:)[pageht] '
 
         SYNVAL ( PAGEWD ) = 'PAGE WIDTH '
     .                     //       '#int(40:132)[pagewdth] '
 
         SYNVAL ( PAGETL ) = 'PAGE '
     .                     //'(1:1){ TITLE NONE[notitle] '
     .                     //     '| TITLE (1:)#word[pagetitle] }'
C
C     The next set of syntax definitions allow the user control
C     over titles and their positions within reports.
C
         SYNVAL ( TLFREQ ) = 'TITLE FREQUENCY[titlefreq] '
     .                     //'(1:1){ 0[zero] '
     .                     //     '| 1ST[first] '
     .                     //     '| FIRST[first] '
     .                     //     '| ALL[all] '
     .                     //     '| EVERY #int(1:)[every] } '
 
         SYNVAL ( TLJUST ) = 'TITLE '
     .                     //       'JUSTIFICATION[titlejustify] '
     .                     //'(1:1){ LEFT[left] '
     .                     //     '| RIGHT[right] '
     .                     //     '| CENTER[center] } '
 
C
C     The user may use the syntax given below to control the
C     frequency with which headers are printed with tabular
C     format reports.
C
         SYNVAL ( HDFREQ ) = 'HEADER FREQUENCY[headerfreq]'
     .                     //' (1:1){ 0[zero] '
     .                     //      '| 1ST[first] '
     .                     //      '| FIRST[first] '
     .                     //      '| ALL[all] '
     .                     //      '| EVERY #int(1:)[every] } '
 
 
         SYNVAL ( RPTLIM ) = 'DELUGE WARNING #int(1:)[limit]'
 
         SYNVAL ( AUTOAD ) = 'AUTOADJUST[auto] '
     .                     //       '(1:1){ OFF[off] '
     .                     //             ' | ASK[ask] '
     .                     //             ' | ON[on] } '
 
 
         SYNVAL ( HPAUSE ) = 'HELP[help] '
     .                     // '(1:1){ WAIT[wait] '
     .                     // '     | NO WAIT[nowait]  '
     .                     // '     | NOWAIT[nowait]  } '
 
 
         SYNVAL ( INTFMT ) = 'DEFAULT INTEGER FORMAT '
     .                     //       '#word[intfmt] '
 
         SYNVAL ( DPFMT ) = 'DEFAULT FLOATING FORMAT '
     .                     //       '#word[dpfmt] '
 
 
         BS    =  '@'
 
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
         CALL PREFIX ( RNAMEC, 1, ERROR(2) )
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
C
C     If we get to this point, we have a legitimate command.
C     See if the user is trying to set a report attribute or a column
C     attribute.  For column commands, we extract the column name
C     and the attribute of that column that the user is allowed to
C     set.  The variables used for this are COLNAM, ATTR, and SVALUE
C     or IVALUE depending upon whether the attribute is represented
C     as a string or integer.
C
C     The other attributes control shape and other global
C     characteristics of reports.  In these cases, we extract the
C     characteristics name and value.  The variables used here
C     are ITEM to hold the name of the control item and SVALUE or
C     IVALUE to contain the value of the control item.
C
      COLCOM = M2XIST('colnam')
 
      IF      ( COLCOM ) THEN
 
         CALL M2GETC ( 'colnam', COMMND, FOUND, LITNAM )
         CALL UCASE  (  LITNAM,                 COLNAM )
 
         CALL NAMXPN (  COLNAM, 'COLUMN',       ERROR  )
 
         IF ( HAVE(ERROR) ) THEN
            CALL PREFIX ( RNAMEC, 1, ERROR(2) )
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
 
         IF      ( M2XIST('columnwdth') ) THEN
 
            ATTR  = 'WIDTH'
            ATYPE =  INT
            CALL M2GETI ( 'columnwdth', COMMND, FOUND, IVALUE )
 
         ELSE IF ( M2XIST('left') ) THEN
 
            ATTR   = 'JUSTIFICATION'
            ATYPE  =  STRING
            SVALUE = 'LEFT'
 
         ELSE IF ( M2XIST('right') ) THEN
 
            ATTR   = 'JUSTIFICATION'
            ATYPE  =  STRING
            SVALUE = 'RIGHT'
 
         ELSE IF ( M2XIST('alias') ) THEN
 
            ATTR  = 'ALIAS'
            ATYPE =  STRING
            CALL M2GETA ( 'alias', COMMND, FOUND, SVALUE )
 
         ELSE IF ( M2XIST('colfmt') ) THEN
 
            ATTR  = 'FORMAT'
            ATYPE =  STRING
            CALL M2GETA ( 'colfmt', COMMND, FOUND, SVALUE )
 
         END IF
 
      ELSE IF ( M2XIST('help') ) THEN
 
         ITEM = 'HELPPROMPT'
         ATYPE = INT
 
         IF ( M2XIST('wait') ) THEN
            IVALUE = 1
         ELSE
            IVALUE = 0
         END IF
 
      ELSE IF ( M2XIST('fmt')  ) THEN
 
         ITEM  = 'FORMAT'
         ATYPE =  STRING
         I     =  1
 
         DO WHILE (        ( I .LE. NFMT            )
     .              .AND.  ( .NOT. M2XIST( FMT(I) ) )  )
            I = I + 1
         END DO
 
         SVALUE = FORM(I)
         
         IF ( M2XIST('preserved') ) THEN
            CALL SUFFIX ( 'PRESERVED', 1, SVALUE )
         END IF

         IF ( M2XIST( 'delimited' ) ) THEN

            DELIM = 'TAB'
            QUOTE = '"'

            CALL M2GETC ( 'delimiter', COMMND, FOUND, DELIM  )
            CALL M2GETC ( 'quote',     COMMND, FOUND, QUOTE  )

            IF ( EQSTR( 'SPACE', DELIM ) ) THEN
               DELIM = 'SPACE'
            END IF

            CALL BBPUTC_1 ('POST', 'QUOTE',     1, QUOTE )
            CALL BBPUTC_1 ('POST', 'DELIMITER', 1, DELIM )
         END IF
 
 
      ELSE IF ( M2XIST('fmtmark')   ) THEN
 
         ITEM  =       'FMTMARK'
         ATYPE =        STRING
         CALL M2GETC ( 'fmtmark', COMMND, FOUND, SVALUE  )
 
      ELSE IF ( M2XIST('timefmt')   ) THEN
 
         ITEM   = 'TIMEFMT'
         ATYPE  =  STRING
 
         CALL M2GETA ( 'timefmt', COMMND, FOUND, SVALUE  )
C
C        If the format entered is one of the SCLK formats we need
C        to make sure that the SCLK kernel is loaded for
C        that SCLK.
C
         CALL NSPCHT ( SVALUE, W )
 
         IF ( HAVE(ERROR) ) THEN
            CALL PREFIX ( RNAMEC, 1, ERROR(2) )
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
 
      ELSE IF ( M2XIST('intfmt')   ) THEN
 
         ITEM   = 'INTFMT'
         ATYPE  =  STRING
 
         CALL M2GETA ( 'intfmt', COMMND, FOUND, SVALUE  )
 
         CALL UCASE( SVALUE, TEMP )
 
         IF ( TEMP .EQ. 'DEFAULT' ) THEN
            SVALUE = TEMP
         END IF
 
      ELSE IF ( M2XIST('dpfmt')   ) THEN
 
         ITEM   = 'DPFMT'
         ATYPE  =  STRING
 
         CALL M2GETA ( 'dpfmt', COMMND, FOUND, SVALUE  )
 
         CALL UCASE( SVALUE, TEMP )
 
         IF ( TEMP .EQ. 'DEFAULT' ) THEN
            SVALUE = TEMP
         END IF
 
      ELSE IF ( M2XIST('pageht')   ) THEN
 
         ITEM  =       'PAGEHEIGHT'
         ATYPE =        INT
         CALL M2GETI ( 'pageht', COMMND, FOUND, IVALUE  )
 
      ELSE IF ( M2XIST('pagewdth')   ) THEN
 
         ITEM  =       'PAGEWIDTH'
         ATYPE =        INT
         CALL M2GETI ( 'pagewdth', COMMND, FOUND, IVALUE  )
C
C        We need to notify the command loop "page margins" routine
C        that the pagewidth has been modified.
C
         CALL NSPSLR ( 1, IVALUE )
 
      ELSE IF ( M2XIST('pagetitle')   ) THEN
 
         ITEM  =       'PAGETITLE'
         ATYPE =        STRING
         CALL M2GETA ( 'pagetitle', COMMND, FOUND, SVALUE  )
 
      ELSE IF ( M2XIST('notitle')   ) THEN
 
         ITEM   = 'PAGETITLE'
         ATYPE  =  STRING
         SVALUE = ' '
 
      ELSE IF ( M2XIST('titlefreq')   ) THEN
 
         ITEM  = 'TITLEFREQUENCY'
         ATYPE =  INT
 
         IF ( M2XIST('zero') ) THEN
            IVALUE = -1
         ELSE IF ( M2XIST('first') ) THEN
            IVALUE =  0
         ELSE IF ( M2XIST('all') ) THEN
            IVALUE =  1
         ELSE IF ( M2XIST('every') ) THEN
            CALL M2GETI ( 'every', COMMND, FOUND, IVALUE )
         END IF
 
      ELSE IF ( M2XIST('limit')   ) THEN
 
         ITEM  = 'REPORTLIMIT'
         ATYPE =  INT
 
         CALL M2GETI ( 'limit', COMMND, FOUND, IVALUE  )
 
 
      ELSE IF ( M2XIST('titlejustify')   ) THEN
 
         ITEM  = 'TITLEJUSTIFICATION'
         ATYPE =  STRING
 
         IF ( M2XIST('left') ) THEN
            SVALUE = 'LEFT'
         ELSE IF ( M2XIST('right') ) THEN
            SVALUE =  'RIGHT'
         ELSE IF ( M2XIST('center') ) THEN
            SVALUE =  'CENTER'
         END IF
 
      ELSE IF ( M2XIST('headerfreq') ) THEN
 
         ITEM  = 'HEADERFREQUENCY'
         ATYPE =  INT
 
         IF ( M2XIST('zero') ) THEN
            IVALUE = -1
         ELSE IF ( M2XIST('first') ) THEN
            IVALUE =  0
         ELSE IF ( M2XIST('all') ) THEN
            IVALUE =  1
         ELSE
            CALL M2GETI ( 'every', COMMND, FOUND, IVALUE )
         END IF
 
      ELSE IF ( M2XIST('auto') ) THEN
         ITEM  = 'AUTOADJUST'
         ATYPE = INT
 
         IF      ( M2XIST('off') ) THEN
            IVALUE = OFF
         ELSE IF ( M2XIST('ask') ) THEN
            IVALUE = ASK
         ELSE IF ( M2XIST('on' ) ) THEN
            IVALUE = ON
         END IF
 
      END IF
C
C     Now depending upon the type of object we just snagged,
C     hand it to the appropriate buffering routine.
C
 
 
      IF ( COLCOM ) THEN
 
         CALL CLN2ID ( COLNAM, ID, FOUND  )
 
         IF ( FOUND ) THEN
C
C           Yes.  The requested column is recognized so we just
C           need to set a couple of values before further processing.
C
            USEID  = ID
            NUMFND = 1
 
         ELSE
C
C           Ooops.  The column wasn't qualified with a table name.
C           See if the column is in some table.  If it's in just
C           one table, we will show it.  If it's in more than one
C           say so.  If it's not in any of them say so.
C
            NUMFND = 0
 
            CALL CLNUM ( N )
 
            DO I = 1, N
 
               CALL CLNID ( I, ID, FOUND )
 
               IF ( FOUND ) THEN
 
                  CALL CLGAC ( ID, 'COLNAM', NAME )
                  P = MAX( 1, INDEX( NAME, '.' ) )
 
                  IF ( NAME(P:) .EQ. COLNAM ) THEN
                     USEID  = ID
                     NUMFND = NUMFND + 1
                  END IF
 
               END IF
 
            END DO
 
         END IF
 
 
         IF ( NUMFND .EQ. 0 ) THEN
 
            ERROR(1) =  'There is currently no column having the '
     .      //          'name ''#''.  To obtain a list of the columns '
     .      //          'that are available you can use the either of'
     .      //          'the commands: SHOW SUMMARY or SHOW KERNELS'
            CALL REPMC ( ERROR(1), '#', LITNAM, ERROR(1) )
            CALL CHKOUT(     RNAME  )
            RETURN
 
         ELSE IF ( NUMFND .GT. 1 ) THEN
 
            ERROR(1) = 'The column requested, ''#'', appears in '
     .      //         'more than one table. To specify the '
     .      //         'table and column of interest supply both '
     .      //         'the table and column names separated by '
     .      //         'a period as in ''TABLE.COLUMN''. '
 
 
            CALL REPMC ( ERROR(1), '#', LITNAM, ERROR(1) )
            CALL CHKOUT ( RNAME )
            RETURN
 
         ELSE
 
            ID = USEID
 
         END IF
 
         IF ( ATYPE .EQ. INT ) THEN
            CALL CLPAI ( ID, ATTR, IVALUE )
         ELSE
            CALL CLPAC ( ID, ATTR, SVALUE )
         END IF
 
      ELSE
 
         IF ( ATYPE .EQ. INT ) THEN
            CALL BBPUTI_1 ( 'POST', ITEM, 1, IVALUE )
         ELSE
            CALL BBPUTC_1 ( 'POST', ITEM, 1, SVALUE )
         END IF
 
      END IF
C
C     If we adjusted the page width, we shall want to adjust the
C     margins used by META/2 for reporting spelling errors.  Every
C     place else in Inspekt, looks this up directly.  Meta/2 on the
C     other hand doesn't know about Inspekt and thus needs to be
C     told directly what the margins are.
C
      IF ( M2XIST('pagewdth')   ) THEN
         CALL NSPMRG ( MARGIN )
         CALL M2MARG ( MARGIN )
      END IF
 
 
C
C     One final error check, and then we are all done.
C
      IF ( HAVE(ERROR) ) THEN
         CALL PREFIX ( RNAMEC, 1, ERROR(2) )
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
 
 
      CALL CHKOUT ( RNAME )
      RETURN
 
      END
