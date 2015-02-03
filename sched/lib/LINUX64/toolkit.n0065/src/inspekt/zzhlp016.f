C$Procedure      ZZHLP016 ( private help text )
 
      SUBROUTINE ZZHLP016 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1490 ) = ' '
      TEXT( 1491 ) = 'Patterns are useful only when they are '
     .//             'used to select matching'
      TEXT( 1492 ) = 'strings.'
      TEXT( 1493 ) = ' '
      TEXT( 1494 ) = 'Pattern matches in Inspekt are case ins'
     .//             'ensitive: ''a'' matches'
      TEXT( 1495 ) = 'both ''a'' and ''A''.  Upper and lower '
     .//             'case letters are said to'
      TEXT( 1496 ) = 'be equivalent.'
      TEXT( 1497 ) = ' '
      TEXT( 1498 ) = 'If a pattern does not begin with a wild'
     .//             ' card, a matching string'
      TEXT( 1499 ) = 'must begin with the a character that is'
     .//             ' equivalent to the beginning'
      TEXT( 1500 ) = 'character of the pattern.  If a pattern'
     .//             ' does not end with a wild'
      TEXT( 1501 ) = 'card, a matching string must end with a'
     .//             ' character that is equivalent'
      TEXT( 1502 ) = 'to the ending character of the pattern.'
      TEXT( 1503 ) = '@@Patterns'
      TEXT( 1504 ) = 'Quit Help'
      TEXT( 1505 ) = 'Help'
      FINISH( 37 ) = 1505
 
      BEGIN ( 38 ) = 1506
      TEXT( 1506 ) = 'If you have a problem that you can''t f'
     .//             'igure out on'
      TEXT( 1507 ) = 'your own, you can send it to NAIF at th'
     .//             'e e-mail address'
      TEXT( 1508 ) = 'given below.  However, please send Insp'
     .//             'ekt''s log file'
      TEXT( 1509 ) = 'along with a description of the problem'
     .//             '.  The log file'
      TEXT( 1510 ) = 'allows us to reproduce the problem.  We'
     .//             ' may also need'
      TEXT( 1511 ) = 'to request a copy of the E-kernels you '
     .//             'were using'
      TEXT( 1512 ) = 'at the time you experienced the problem'
     .//             '.'
      TEXT( 1513 ) = ' '
      TEXT( 1514 ) = 'If you have comments or suggestions'
      TEXT( 1515 ) = 'regarding Inspekt, send them to'
      TEXT( 1516 ) = '@literal'
      TEXT( 1517 ) = 'btaber@spice.jpl.nasa.gov'
      TEXT( 1518 ) = '|endliteral'
      TEXT( 1519 ) = 'Within the limits of time and money, we'
     .//             ' will do our best'
      TEXT( 1520 ) = 'to respond to your correspondence.'
      TEXT( 1521 ) = '@literal'
      TEXT( 1522 ) = '      ---Bill Taber'
      TEXT( 1523 ) = '|endliteral'
      TEXT( 1524 ) = ' '
      TEXT( 1525 ) = '@@Problems, Suggestions'
      TEXT( 1526 ) = 'Quit Help'
      TEXT( 1527 ) = 'Help'
      FINISH( 38 ) = 1527
 
      BEGIN ( 39 ) = 1528
      TEXT( 1528 ) = 'The word query is used in two ways in I'
     .//             'nspekt.'
      TEXT( 1529 ) = ' '
      TEXT( 1530 ) = '@newlist'
      TEXT( 1531 ) = '@numitem A query is a SELECT command is'
     .//             'sued to Inspekt to search'
      TEXT( 1532 ) = '         loaded E-kernels for data that'
     .//             ' satisfies a user specified'
      TEXT( 1533 ) = '         selection criteria'
      TEXT( 1534 ) = ' '
      TEXT( 1535 ) = '@numitem A query is a word in a command'
     .//             ', or symbol that ends in'
      TEXT( 1536 ) = '         a question mark.  When Inspekt'
     .//             ' encounters such a word'
      TEXT( 1537 ) = '         in a command, it prompts the u'
     .//             'ser for a value to replace'
      TEXT( 1538 ) = '         the "query"'
      TEXT( 1539 ) = ' '
      TEXT( 1540 ) = '@paritem For example suppose you enter '
     .//             'the command'
      TEXT( 1541 ) = ' '
      TEXT( 1542 ) = '@literalitem'
      TEXT( 1543 ) = 'Inspekt> SHOW COLUMN NAME?;'
      TEXT( 1544 ) = '|endliteral'
      TEXT( 1545 ) = ' '
      TEXT( 1546 ) = '@paritem Inspekt will ask you to supply'
     .//             ' a value for NAME?  and then'
      TEXT( 1547 ) = '         continue acting upon the resul'
     .//             'ting command. Normally you will'
      TEXT( 1548 ) = '         not type queries directly when'
     .//             ' issuing a command. Instead,'
      TEXT( 1549 ) = '         you will define symbols that c'
     .//             'ontain queries or place'
      TEXT( 1550 ) = '         queries in commands that are c'
     .//             'ollected in Inspekt procedure files.'
      TEXT( 1551 ) = ' '
      TEXT( 1552 ) = '@@Query'
      TEXT( 1553 ) = 'Quit Help'
      TEXT( 1554 ) = 'Help'
      TEXT( 1555 ) = ' '
      FINISH( 39 ) = 1555
 
      BEGIN ( 40 ) = 1556
      TEXT( 1556 ) = 'After you issue a SELECT command to Ins'
     .//             'pekt, the program presents'
      TEXT( 1557 ) = 'a display of the items that match the s'
     .//             'election criteria. The items'
      TEXT( 1558 ) = 'displayed together with the method in w'
     .//             'hich they are displayed'
      TEXT( 1559 ) = 'is called a "report".'
      TEXT( 1560 ) = ' '
      TEXT( 1561 ) = 'A report has several attributes that ar'
     .//             'e user adjustable.'
      TEXT( 1562 ) = ' '
      TEXT( 1563 ) = 'Reports are presented in a region that '
     .//             'models a physical page as you'
      TEXT( 1564 ) = 'find in a loose leaf binder.  You may a'
     .//             'djust the height and width'
      TEXT( 1565 ) = 'of these pages.'
      TEXT( 1566 ) = ' '
      TEXT( 1567 ) = 'A report is presented in a particular f'
     .//             'ormat.   There are four basic'
      TEXT( 1568 ) = 'formats: tabular, flagged, verbatim and'
     .//             ' delimited.  You may select the format'
      TEXT( 1569 ) = 'you prefer for reports the program crea'
     .//             'tes.  Tabular formats are'
      TEXT( 1570 ) = 'usually the easiest to read.  However, '
     .//             'if events have a large number'
      TEXT( 1571 ) = 'of attributes and you want to see many '
     .//             'of these attributes, you may'
      TEXT( 1572 ) = 'find the flagged format better suited t'
     .//             'o your needs.  Verbatim format'
      TEXT( 1573 ) = 'allows you to see most directly the act'
     .//             'ual contents of an event'
      TEXT( 1574 ) = 'without any of Inspekt''s formatting to'
     .//             'ols modifying line breaks'
      TEXT( 1575 ) = 'in the original data.  However, this fo'
     .//             'rmat is difficult to read'
      TEXT( 1576 ) = 'and suitable only when you need to see '
     .//             'directly how a particular'
      TEXT( 1577 ) = 'event was stored in the E-kernel.  Fina'
     .//             'lly, the delimited format creates'
      TEXT( 1578 ) = 'a format suitable for import into commo'
     .//             'n spreadsheet programs.'
      TEXT( 1579 ) = ' '
      TEXT( 1580 ) = 'Every report has a title. You can remov'
     .//             'e the title, change it and'
 
      RETURN
      END
