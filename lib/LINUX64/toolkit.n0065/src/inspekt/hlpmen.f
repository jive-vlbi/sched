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
      SUBROUTINE HLPMEN  ( TOPIC )
      IMPLICIT NONE
      CHARACTER*(*)         TOPIC
 
C
C     Version 2.0 September 8, 1995
C
C        Increased MAXLIN from 200 to 500.
C
 
      EXTERNAL NSPWLN
C
C     Spicelib Functions
C
      INTEGER               LTRIM
      INTEGER               CARDC
      INTEGER               FRSTNB
      LOGICAL               ELEMC
      LOGICAL               RETURN
      LOGICAL               MATCH
      
      LOGICAL               BATCH
 
 
 
      INTEGER               MAXTXT
      PARAMETER           ( MAXTXT = 100 )
 
      INTEGER               MAXMEN
      PARAMETER           ( MAXMEN = MAXTXT )
 
      INTEGER               AVESIZ
      PARAMETER           ( AVESIZ = 8 )
C
C     We are going to keep track of the text via pointers in a
C     symbol table. We shall use parameters to point to the various
C     components
C
      INTEGER               TXTMEN
      PARAMETER           ( TXTMEN = 1 )
 
      INTEGER               TXTAT
      PARAMETER           ( TXTAT = TXTMEN + 1 )
 
      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = TXTAT + 1 )
 
      INTEGER               TXTQTY
      PARAMETER           ( TXTQTY = TXTSIZ )
 
      INTEGER               LSTDAT
      PARAMETER           ( LSTDAT = TXTSIZ )
 
      INTEGER               NDAT
      PARAMETER           ( NDAT   = LSTDAT )
 
 
      INTEGER               MOST
      PARAMETER           ( MOST    = 19 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE  = 80 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE  = 32 )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
 
      INTEGER               B
      INTEGER               I
      INTEGER               INDX
      INTEGER               LAST
      INTEGER               N
      INTEGER               NOPT
      INTEGER               NTXT
      INTEGER               OPTION
      INTEGER               OSTART
      INTEGER               SIZE
 
      LOGICAL               FOUND
 
      INTEGER               MAXLIN
      PARAMETER           ( MAXLIN = 500 )
 
      CHARACTER*(80)        HELP ( LBCELL : MAXLIN )
 
      INTEGER               MAXVST
      PARAMETER           ( MAXVST = 20 )
 
 
      CHARACTER*(WDSIZE)    TXTNAM ( LBCELL : MAXMEN )
      INTEGER               TXTPTR ( LBCELL : MAXMEN )
      INTEGER               TXTDAT ( LBCELL : MAXMEN * NDAT )
      CHARACTER*(WDSIZE)    VISITD ( MAXVST )
 
      INTEGER               TXINFO  ( LSTDAT )
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    LNAME
      CHARACTER*(WDSIZE)    ORDNL
      CHARACTER*(WDSIZE)    RESPNS
      CHARACTER*(WDSIZE)    OPTTXT  ( MOST )
      CHARACTER*(2)         OPTNAM  ( MOST )
 
      INTEGER               LONGSZ
      PARAMETER           ( LONGSZ = 200 )
 
      CHARACTER*(LONGSZ)    MESSGE
 
 
      LOGICAL               FTITLE
      LOGICAL               DOPMT
      LOGICAL               DIDPMT
 
      LOGICAL               LSTAT ( 3 )
      LOGICAL               SSTAT ( 3 )
 
      INTEGER               DOTHEM
      INTEGER               PAGEWD
      INTEGER               PAGEHT
      INTEGER               OPAGWD
      INTEGER               OLSKIP
      INTEGER               OLNDNT
      INTEGER               OINDNT
      INTEGER               OISKIP
      INTEGER               MENBEG
      INTEGER               ITEM
 
 
      INTEGER               J
 
      CHARACTER*(LNSIZE)    STYLE
 
      SAVE
 
      DATA OPTNAM / 'P', 'Q', '1', '2', '3', '4', '5', '6', '7',
     .              '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
     .              'H' /
 
      RETURN 
 
      ENTRY HLPINT ()
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'HLPINT' )
      CALL SSIZEC ( MAXMEN,        TXTNAM )
      CALL SSIZEI ( MAXMEN,        TXTPTR )
      CALL SSIZEI ( MAXMEN * NDAT, TXTDAT )
 
      CALL HLPSIZ ( NTXT )
 
      DO I = 1, NTXT
C
C        For each help screen determine it's menu name, the location
C        of the @@ characters, and the location of the associated menu.
C
C        Fetch the I'th block of help text.
C
         CALL SSIZEC ( MAXLIN, HELP )
         CALL HLPTXT ( I,      HELP )
 
C
C        We have not located the title yet and we don't know
C        the number of lines of text present so far.  Note that
C        if we don't find a title, this item will become
C        invisible from the point of view of the help system.
C
         FTITLE = .FALSE.
         J      =  1
 
         DO WHILE ( J .LE. CARDC(HELP) )
 
            B = LTRIM ( HELP(J) )
 
            IF ( HELP(J)(B:B+1) .EQ. '@@'  ) THEN
 
               FTITLE = .TRUE.
               CALL CMPRSS ( ' ', 1, HELP(J)(B+2:), NAME )
               CALL UCASE  ( NAME, NAME )
               CALL LJUST  ( NAME, NAME )
 
               TXINFO( TXTAT  ) = I
               TXINFO( TXTSIZ ) = J - 1
               TXINFO( TXTMEN ) = J + 1
 
C
C              Store the index and the length of this text in
C              the integer text data table.
C
 
               CALL SYPUTI ( NAME, TXINFO, TXTQTY, TXTNAM,
     .                                             TXTPTR,
     .                                             TXTDAT )
C
C              Set J to point past the end of the text so that
C              we can finish the loop now.
C
               J = CARDC(HELP) + 1
 
            END IF
 
            J = J + 1
 
         END DO
C
C        The following block is simply a courtesy to developers
C        so they can determine easily if at least every text
C        block has a title.  Once a text system has been "debugged"
C        this block should never get exercised.
C
         IF ( .NOT. FTITLE ) THEN
 
            CALL INTORD ( I,     ORDNL )
            CALL LCASE  ( ORDNL, ORDNL )
            CALL SETMSG ( 'The # block of help text does not '
     .      //            'have a title. ' )
            CALL ERRCH  ( '#', ORDNL )
            CALL SIGERR ( 'SPICE(UNTITLEDHELP)' )
            CALL CHKOUT ( 'HLPINT' )
            RETURN
 
         END IF
 
 
      END DO
C
C     Now we perform another courtesy check for the developer
C     to make sure every menu item points to something in
C     the help list.
C
      DO I = 1, CARDC(TXTNAM)
 
         NAME = TXTNAM(I)
         CALL SYGETI ( NAME ,  TXTNAM,
     .                         TXTPTR,
     .                         TXTDAT, N,  TXINFO, FOUND )
 
         ITEM   = TXINFO ( TXTAT  )
         MENBEG = TXINFO ( TXTMEN )
 
         CALL SCARDC ( MAXLIN, HELP )
         CALL HLPTXT ( ITEM,   HELP )
 
         DO J = MENBEG, CARDC(HELP)
 
            CALL UCASE ( HELP(J),       LNAME )
            CALL LJUST ( LNAME,         LNAME )
            CALL CMPRSS( ' ', 1, LNAME, LNAME )
 
            IF ( .NOT. ELEMC(LNAME,TXTNAM)
     .           .AND. LNAME .NE. ' '
     .           .AND. LNAME .NE. 'QUIT HELP' ) THEN
               MESSGE = 'Under the topic ''#'', (the #''th text '
     .         //       'block) the link menu item # does not '
     .         //       'point to a recognized help section. '
               CALL REPMC ( MESSGE, '#', NAME,   MESSGE )
               CALL REPMI ( MESSGE, '#', ITEM,   MESSGE )
               CALL REPMC ( MESSGE, '#', LNAME,  MESSGE )
 
               CALL NSPMRG   ( STYLE )
               CALL NICEPR_1 ( MESSGE, STYLE, NSPWLN )
            END IF
 
         END DO
 
      END DO
 
      CALL CHKOUT ( 'HLPINT' )
      RETURN
 
C
C     The routine DOHELP does the real work of displaying menus and
C     help information.
C
      ENTRY DOHELP ( TOPIC )
C
C     Fetch the margins to be used for displaying text
C
      LAST         = 1
      VISITD(LAST) = 'HELP'
 
      IF ( TOPIC .EQ. ' ' ) THEN
         NAME = 'HELP'
      ELSE
         CALL CMPRSS ( ' ', 1, TOPIC, NAME )
         CALL LJUST  ( NAME ,         NAME )
         CALL UCASE  ( NAME,          NAME )
C
C        If we get an exact match, we don't spend any more time
C        on this item.
C
         IF ( ELEMC( NAME, TXTNAM ) ) THEN
            I     = CARDC(TXTNAM) + 1
            FOUND = .TRUE.
         ELSE
C
C           Hmmm. No exact match.  Append a wild card to the end
C           of name and look for a pattern match.
C
            I = 1
            CALL SUFFIX( '*', 0, NAME )
         END IF
 
 
         DO WHILE ( I .LE. CARDC(TXTNAM) )
            IF ( MATCH( TXTNAM(I), NAME ) )THEN
               NAME  = TXTNAM(I)
               I     = CARDC(TXTNAM)
               FOUND = .TRUE.
            END IF
            I = I + 1
         END DO
 
         IF ( .NOT. FOUND ) THEN
C
C           Still no match.  Put a wild card on the front of name
C           and look for a match.
C
            CALL PREFIX ( '*', 0, NAME )
            I = 1
 
            DO WHILE ( I .LE. CARDC(TXTNAM) )
               IF ( MATCH( TXTNAM(I), NAME ) ) THEN
                  NAME  = TXTNAM(I)
                  I     = CARDC(TXTNAM)
                  FOUND = .TRUE.
               END IF
               I = I + 1
            END DO
 
         END IF
 
      END IF
 
      IF ( NAME .NE. 'HELP' ) THEN
         LAST         = LAST + 1
         VISITD(LAST) = NAME
      END IF
 
C
C     We begin help by asking for the top level menu HELP.
C
      CALL SYGETI ( NAME ,  TXTNAM,
     .                      TXTPTR,
     .                      TXTDAT, N,  TXINFO, FOUND )
 
C
C     Get the current page settings.
C
      DOTHEM = 0
      CALL BBGETI_1 ( 'COPY', 'PAGEWIDTH',  N, PAGEWD )
      CALL BBGETI_1 ( 'COPY', 'PAGEHEIGHT', N, PAGEHT )
      CALL BBGETI_1 ( 'COPY', 'HELPPROMPT', N, DOTHEM )
      DOPMT = DOTHEM .NE. 0
 
 
      CALL PARAMS ( 'GET',  'PAGEWIDTH',    OPAGWD )
      CALL PARAMS ( 'GET',  'LEFTSKIP',     OLSKIP )
      CALL PARAMS ( 'GET',  'LITERALINDENT',OLNDNT )
      CALL PARAMS ( 'GET',  'ITEMINDENT',   OINDNT )
      CALL PARAMS ( 'GET',  'ITEMSKIP',     OISKIP )
 
      CALL PARAMS ( 'SET',  'PAGEWIDTH',    PAGEWD )
      CALL PARAMS ( 'SET',  'LEFTSKIP',     3      )
      CALL PARAMS ( 'SET',  'LITERALINDENT',2      )
      CALL PARAMS ( 'SET',  'ITEMINDENT',   4      )
      CALL PARAMS ( 'SET',  'ITEMSKIP',     2      )
 
 
 
 
      DO WHILE ( FOUND )
C
C        Get the various attributes associated with this help
C        item.
C
         INDX   =  TXINFO( TXTAT  )
         SIZE   =  TXINFO( TXTSIZ )
         MENBEG =  TXINFO( TXTMEN )
C
C        Fetch the text associated with this item.
C
         CALL SSIZEC ( MAXLIN, HELP )
         CALL HLPTXT ( INDX,   HELP )
 
C
C        Extract the links menu from the end of the help text.
C
C        Note that if we have previouly buffered topics we
C        add a n option to the menu to return to the previous
C        topic.  We also need to adjust where the OPTNAMs begin
C        when we create our menu.  We use OSTART to do this.
C
         IF ( LAST .GT. 1 ) THEN
            NOPT      = 1
            OSTART    = 1
            OPTTXT(1) = 'Previous Topic'
         ELSE
            OSTART    = 2
            NOPT      = 0
         END IF
 
         DO J = MENBEG, CARDC(HELP)
 
            IF ( HELP(J) .NE. ' ' ) THEN
               NOPT         = NOPT + 1
               OPTTXT(NOPT) = HELP(J)
            END IF
 
         END DO
 
C
C        If there is some text associated with this item, now
C        is the time to display it.
C
         IF ( SIZE .GT. 0 ) THEN
C
C           Set up the page manager for receiving output.
C
            CALL PAGRST
            CALL PAGSET ( 'TITLEFREQUENCY', 0       )
            CALL PAGSET ( 'PAGEWIDTH',      PAGEWD  )
            CALL PAGSET ( 'PAGEHEIGHT',     PAGEHT  )
            CALL PAGSET ( 'NOSPACETITLE',   0       )
            CALL PAGSET ( 'NOSPACEHEADER',  0       )
            CALL PAGSET ( 'NOSPACEFOOTER',  0       )
 
            IF ( DOPMT ) THEN
               CALL PAGSET ( 'PROMPT', 1 )
               CALL PAGSCN ( 'PROMPT' )
               CALL PAGPUT ( '---' )
            END IF
C
C           The title for this menu is the topic listing
C           that appears in the text.  This starts with
C           the characters '@@' which we don't want to display
C
            B = FRSTNB  ( HELP(MENBEG-1) )
            CALL PAGSCN ( 'TITLE' )
            CALL PAGPUT ( ' '     )
            CALL PAGPUT ( HELP(MENBEG-1)(B+2:) )
            CALL PAGPUT ( ' '     )
 
C
C           That's it.  The page settings are all in order
C           Set the page section to BODY so that the stuff
C           created by SUBTEX will be displayed appropriately.
C
            CALL PAGSCN ( 'BODY' )
C
C           Set the cardinality of the HELP buffer so that
C           stuff starting with the title is ignored.
C
            CALL SCARDC ( SIZE, HELP )
            CALL SUBTEX (       HELP )
 
         END IF
C
C        See if the last line sent caused a prompt to be issued.
C        If so, we won't have to issue one.  Also we reset the
C        page so that others won't have to deal with any debris
C        left around by this routine.
C
         CALL PAGPMT ( DIDPMT, RESPNS )
         CALL PAGRST
 
         IF (       SIZE .GT. 0
     .        .AND. DOPMT
     .        .AND. .NOT. DIDPMT ) THEN
C
C           Issue our own prompt since we didn't do one last
C           time. (This gives the user a chance to read the
C           output before going on to the next menu.)
C
            CALL NSPGST ( 'LOG',  LSTAT )
            CALL NSPGST ( 'SAVE', SSTAT )
            CALL NSPIOH ( 'LOG' )
            CALL NSPIOH ( 'SAVE' )
 
            CALL PROMPT ( '---', RESPNS )
 
            CALL NSPPST ( 'LOG',  LSTAT )
            CALL NSPPST ( 'SAVE', SSTAT )
 
         END IF
 
C
C        If there is any menu to display showing related options,
C        now is the time to display it.
C
         IF ( NOPT .GT. 1 ) THEN
 
            IF ( BATCH() ) THEN
               RETURN
            END IF
 
            CALL GETOPT_2 ( ' ', 1, NOPT,  OPTNAM(OSTART), OPTTXT, 3,
     .                                             OPTION   )
C
C           Fetch the help data for the user's selection.
C           First normalize the topic name.
C
            CALL UCASE  ( OPTTXT(OPTION), NAME )
            CALL LJUST  ( NAME,           NAME )
            CALL CMPRSS ( ' ', 1, NAME,   NAME )
C
C           We keep a buffer of previouly visited topics.  If
C           the user requests the previous topic we need to
C           get it and look up the text information for it.
C
C           Also the number of previous topics is now smaller.
C
            IF ( NAME .EQ. 'PREVIOUS TOPIC' ) THEN
 
               LAST = MAX(1, LAST - 1 )
               NAME = VISITD(LAST)
 
               CALL SYGETI ( NAME, TXTNAM,
     .                             TXTPTR,
     .                             TXTDAT, N,    TXINFO, FOUND )
 
            ELSE
C
C              This is some other topic, look it up and put it
C              on the buffer of visited topics.
C
               CALL SYGETI ( NAME, TXTNAM,
     .                             TXTPTR,
     .                             TXTDAT, N,    TXINFO, FOUND )
               LAST = LAST + 1
 
               IF ( LAST .GT. MAXVST ) THEN
 
                  DO I = 3, MAXVST
                     VISITD(I-1) = VISITD(I)
                  END DO
 
                  LAST = MAXVST
 
               END IF
 
               VISITD ( LAST ) = NAME
 
            END IF
 
 
         ELSE
 
            NAME  = 'QUIT HELP'
            FOUND = .FALSE.
 
         END IF
 
 
      END DO
 
      IF ( NAME .NE. 'QUIT HELP' ) THEN
 
         MESSGE = 'Sorry, but no help is available for ' // NAME
         CALL NSPMRG   ( STYLE )
         CALL NICEPR_1 ( MESSGE, STYLE, NSPWLN )
 
      END IF
 
      CALL PARAMS ( 'SET',  'PAGEWIDTH',    OPAGWD )
      CALL PARAMS ( 'SET',  'LEFTSKIP',     OLSKIP )
      CALL PARAMS ( 'SET',  'LITERALINDENT',OLNDNT )
      CALL PARAMS ( 'SET',  'ITEMINDENT',   OINDNT )
      CALL PARAMS ( 'SET',  'ITEMSKIP',     OISKIP )
 
      RETURN
 
      END
 
 
 
 
 
 
 
