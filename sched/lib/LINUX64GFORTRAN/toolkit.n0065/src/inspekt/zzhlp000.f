C$Procedure      ZZHLP000 ( private help text )
 
      SUBROUTINE ZZHLP000 ( BEGIN, FINISH, TEXT )
 
C$ Abstract
C
C     Fill out a portion of the help text needed by percy.
C
C     Private routine intended solely for the support of Inspekt
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
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               BEGIN ( * )
      INTEGER               FINISH( * )
      CHARACTER*(*)         TEXT  ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BEGIN      O   Indexes of begins of text help
C     FINISH     O   Indexes of ends of text help
C     TEXT       O   A block of text help.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine simply fills begin and end markers as well
C     as actual text for a block of help text for percy.
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
C-    Inspekt Version 1.0.0, 1-AUG-1997 (WLT)
C
C
C-&
 
      INTEGER               I
      INTEGER               J
 
      J = FINISH ( 1 )
      I = BEGIN  ( 1 )
 
      FINISH(1) = J
      BEGIN (1) = I
 
      CALL REPMC ( TEXT(1), '*', '*', TEXT(1) )
 
 
      BEGIN ( 1 ) = 1
      TEXT(  1 ) = 'You''ve already discovered the basics of '
     .//           'Inspekt''s Help system. When'
      TEXT(  2 ) = 'you type "help;" (or "HELP;") a menu is  '
     .//           'displayed such as the one'
      TEXT(  3 ) = 'below:'
      TEXT(  4 ) = '@literal'
      TEXT(  5 ) = '(Q) Quit Help'
      TEXT(  6 ) = '(1) About Help'
      TEXT(  7 ) = '      ...'
      TEXT(  8 ) = 'Option: _'
      TEXT(  9 ) = '|endliteral'
      TEXT( 10 ) = 'At the prompt (Option: ) type the number '
     .//           'or letter shown in'
      TEXT( 11 ) = 'parentheses to the left of the topic of i'
     .//           'nterest and hit RETURN.'
      TEXT( 12 ) = 'You  will be presented with a screen of t'
     .//           'ext that describes the'
      TEXT( 13 ) = 'topic.'
      TEXT( 14 ) = ' '
      TEXT( 15 ) = 'After a full screen of text has been disp'
     .//           'layed, a left justified'
      TEXT( 16 ) = 'short vertical line will be displayed. Ju'
     .//           'st to the right of this'
      TEXT( 17 ) = 'line will be the cursor.  This indicates '
     .//           'there is more text or'
      TEXT( 18 ) = 'related topics associated with the curren'
     .//           't topic.  Hit a carriage'
      TEXT( 19 ) = 'return to display the additional informat'
     .//           'ion.   If there is more'
      TEXT( 20 ) = 'text on the topic, you will see another s'
     .//           'creen of text followed by'
      TEXT( 21 ) = 'the same short line and prompt.  If there'
     .//           ' is no more text for the'
      TEXT( 22 ) = 'current topic, another menu of options wi'
     .//           'll be displayed allowing'
      TEXT( 23 ) = 'you to exit the help system; return to th'
     .//           'e main menu; or move on'
      TEXT( 24 ) = 'to topics related to the current topic.'
      TEXT( 25 ) = ' '
      TEXT( 26 ) = '@@About Help'
      TEXT( 27 ) = 'Quit Help'
      TEXT( 28 ) = 'Help'
      TEXT( 29 ) = 'Short Cut to Topics'
      TEXT( 30 ) = 'Making Help Wait'
      FINISH( 1 ) = 30
 
      BEGIN ( 2 ) = 31
      TEXT( 31 ) = 'The "page" on which reports are displayed'
     .//           ' is often too narrow to'
      TEXT( 32 ) = 'hold a tabular report.  However, by adjus'
     .//           'ting the width of some columns'
      TEXT( 33 ) = 'it may be possible to fit the report in t'
     .//           'he available space.  If'
      TEXT( 34 ) = 'this is true, Inspekt will ask whether or'
     .//           ' not it should temporarily'
      TEXT( 35 ) = '(for the duration of the report) adjust c'
     .//           'olumn widths so that the'
      TEXT( 36 ) = 'report may be printed. This feature is ca'
     .//           'lled "AUTOADJUST".  By'
      TEXT( 37 ) = 'default, AUTOADJUST is set up so that Ins'
     .//           'pekt asks you whether or'
      TEXT( 38 ) = 'not it should adjust some column widths t'
     .//           'o fit a report on the current'
      TEXT( 39 ) = 'page.  This default action can be modifie'
     .//           'd by using the SET AUTOADJUST'
      TEXT( 40 ) = 'command.  You can set the automatic actio'
     .//           'n to'
      TEXT( 41 ) = '@newlist'
      TEXT( 42 ) = '@numitem Prohibit display of reports that'
     .//           ' are too wide.'
      TEXT( 43 ) = ' '
      TEXT( 44 ) = '@numitem Ask whether or not a columns sho'
     .//           'uld be narrowed to'
      TEXT( 45 ) = '         accommodate a wide  report (defa'
     .//           'ult)'
      TEXT( 46 ) = ' '
      TEXT( 47 ) = '@numitem Automatically adjust column widt'
     .//           'hs and display reports.'
      TEXT( 48 ) = ' '
      TEXT( 49 ) = 'The syntax for this command is:'
      TEXT( 50 ) = '@literal'
      TEXT( 51 ) = '   SET AUTOADJUST (1:1){ OFF | ASK | ON }'
      TEXT( 52 ) = '|endliteral'
      TEXT( 53 ) = ' '
      TEXT( 54 ) = 'To see the current status of AUTOADJUST t'
     .//           'ype the command'
      TEXT( 55 ) = '@literal'
      TEXT( 56 ) = 'SHOW FORMAT'
      TEXT( 57 ) = '|endliteral'
      TEXT( 58 ) = ' '
      TEXT( 59 ) = 'Note: AUTOADJUST does not affect the widt'
     .//           'hs of numeric or time columns.'
      TEXT( 60 ) = 'Only character columns are affected by AU'
     .//           'TOADJUST.'
      TEXT( 61 ) = ' '
      TEXT( 62 ) = ' '
      TEXT( 63 ) = '@@Autoadjust'
      TEXT( 64 ) = 'Quit Help'
      TEXT( 65 ) = 'Help'
      TEXT( 66 ) = ' '
      FINISH( 2 ) = 66
 
      BEGIN ( 3 ) = 67
      TEXT( 67 ) = 'Inspekt allows you to collect frequently '
     .//           'executed sequences'
      TEXT( 68 ) = 'of command in files called Inspekt Proced'
     .//           'ure Files.'
      TEXT( 69 ) = ' '
      TEXT( 70 ) = 'To create a procedure file,  simply type '
     .//           'in a sequence of'
      TEXT( 71 ) = 'commands as you would type them when ente'
     .//           'ring them'
      TEXT( 72 ) = 'in Inspekt.'
      TEXT( 73 ) = 'End each command with a semi-colon.  Star'
     .//           't every new command'
      TEXT( 74 ) = 'on a new line.'
      TEXT( 75 ) = ' '
      TEXT( 76 ) = 'You can insert a "comment" line in the pr'
     .//           'ocedure file by starting'
      TEXT( 77 ) = 'the line with a semi-colon (;).'
      TEXT( 78 ) = ' '
      TEXT( 79 ) = 'To start a procedure file enter the comma'
     .//           'nd:'
      TEXT( 80 ) = '@literal'
      TEXT( 81 ) = 'START filename;'
      TEXT( 82 ) = '|endliteral'
      TEXT( 83 ) = 'where "filename" above is replaced by the'
     .//           ' name of your procedure file.'
      TEXT( 84 ) = ' '
      TEXT( 85 ) = 'Procedure files may start other procedure'
     .//           ' files.  Procedure commands'
      TEXT( 86 ) = 'may not use any of the following commands'
     .//           ':'
      TEXT( 87 ) = '@literal'
      TEXT( 88 ) = 'EDIT'
      TEXT( 89 ) = 'DO'
      TEXT( 90 ) = 'RECALL'
      TEXT( 91 ) = '|endliteral'
 
      RETURN
      END
