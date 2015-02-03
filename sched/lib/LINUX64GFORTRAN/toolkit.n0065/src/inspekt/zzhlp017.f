C$Procedure      ZZHLP017 ( private help text )
 
      SUBROUTINE ZZHLP017 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1581 ) = 'alter the frequency with which it is di'
     .//             'splayed (only at the'
      TEXT( 1582 ) = 'beginning of the report, on every page,'
     .//             ' or every n''th page). You'
      TEXT( 1583 ) = 'can also control whether the title is l'
     .//             'eft justified, right justified'
      TEXT( 1584 ) = 'or centered on the report page.'
      TEXT( 1585 ) = ' '
      TEXT( 1586 ) = 'Reports in tabular format, have a heade'
     .//             'r as well as a title.  The'
      TEXT( 1587 ) = 'header names the various columns that a'
     .//             'ppear in the report.  As with'
      TEXT( 1588 ) = 'the report title you can adjust the fre'
     .//             'quency with which the header'
      TEXT( 1589 ) = 'is displayed in the report.'
      TEXT( 1590 ) = ' '
      TEXT( 1591 ) = '@@Reports'
      TEXT( 1592 ) = 'Quit Help'
      TEXT( 1593 ) = 'Help'
      TEXT( 1594 ) = 'Tabular Format'
      TEXT( 1595 ) = 'Flagged Format'
      TEXT( 1596 ) = 'Verbatim Format'
      TEXT( 1597 ) = 'Delimited Format'
      TEXT( 1598 ) = 'SET FORMAT ...'
      TEXT( 1599 ) = 'SET HEADER ...'
      TEXT( 1600 ) = 'SET TITLE  ...'
      TEXT( 1601 ) = 'SET PAGE   ...'
      FINISH( 40 ) = 1601
 
      BEGIN ( 41 ) = 1602
      TEXT( 1602 ) = 'When you first open an E-kernel, it is '
     .//             'often not clear what range'
      TEXT( 1603 ) = 'of events are present in the kernel.  I'
     .//             'n such situations, simply'
      TEXT( 1604 ) = 'selecting events may result in much mor'
     .//             'e data that you can easily'
      TEXT( 1605 ) = 'view.  For this reason you may preface '
     .//             'your selection command with'
      TEXT( 1606 ) = 'a "SAMPLE" clause. By doing so, you can'
     .//             ' view a reasonable subset'
      TEXT( 1607 ) = 'of the events that match your selection'
     .//             ' criteria.  There are several'
      TEXT( 1608 ) = 'forms of the SAMPLE clause.  However, a'
     .//             'll sample commands are formed'
      TEXT( 1609 ) = 'the same way: type the SAMPLE clause an'
     .//             'd follow it by the SELECT'
      TEXT( 1610 ) = 'statement you would issue if you wanted'
     .//             ' to view all of the matching'
      TEXT( 1611 ) = 'events of a selection.'
      TEXT( 1612 ) = ' '
      TEXT( 1613 ) = 'The SAMPLE clauses that can prefixed to'
     .//             ' a SELECT command'
      TEXT( 1614 ) = 'are shown below:'
      TEXT( 1615 ) = '@literal'
      TEXT( 1616 ) = 'SAMPLE number'
      TEXT( 1617 ) = 'SAMPLE FIRST number'
      TEXT( 1618 ) = 'SAMPLE LAST  number'
      TEXT( 1619 ) = 'SAMPLE number FROM  percentile TO perce'
     .//             'ntile'
      TEXT( 1620 ) = 'SAMPLE number UP TO       percentile [E'
     .//             'VERY number]'
      TEXT( 1621 ) = 'SAMPLE number STARTING AT percentile [E'
     .//             'VERY number]'
      TEXT( 1622 ) = 'SAMPLE number CENTER   AT percentile [E'
     .//             'VERY number]'
      TEXT( 1623 ) = '|endliteral'
      TEXT( 1624 ) = 'The first "number" in each clause is th'
     .//             'e number of samples to'
      TEXT( 1625 ) = 'extract from a selection. Percentiles m'
     .//             'ust be integers from 0 to'
      TEXT( 1626 ) = '100. The 0th percentile corresponds to '
     .//             'the first event of a selection;'
      TEXT( 1627 ) = 'the 100th percentile to the last event.'
     .//             ' In those clauses with an'
      TEXT( 1628 ) = 'optional "EVERY" subclause, the "EVERY-'
     .//             'number" refers to how many'
      TEXT( 1629 ) = 'events to skip between displayed events'
     .//             '. If not supplied the'
      TEXT( 1630 ) = '"EVERY-number" value is assumed to be o'
     .//             'ne (1).'
      TEXT( 1631 ) = '@@Sampling Data'
      TEXT( 1632 ) = 'Quit Help'
      TEXT( 1633 ) = 'Help'
      TEXT( 1634 ) = 'Looking at Data    --- SELECT'
      FINISH( 41 ) = 1634
 
      BEGIN ( 42 ) = 1635
      TEXT( 1635 ) = 'To save output that is printed on your '
     .//             'terminal (or in your terminal window)'
      TEXT( 1636 ) = 'issue the command:'
      TEXT( 1637 ) = '@literal'
      TEXT( 1638 ) = 'SAVE TO filename;'
      TEXT( 1639 ) = '|endliteral'
      TEXT( 1640 ) = 'All subsequent screen output will be sa'
     .//             'ved the the file specified'
      TEXT( 1641 ) = 'in the command. To stop saving to the S'
     .//             'AVE file, issue the command'
      TEXT( 1642 ) = '@literal'
      TEXT( 1643 ) = 'DISCARD;'
      TEXT( 1644 ) = '|endliteral'
      TEXT( 1645 ) = 'You may SAVE and DISCARD output as freq'
     .//             'uently as you wish.  Moreover,'
      TEXT( 1646 ) = 'a different file may be used with each '
     .//             'SAVE command. However, to'
      TEXT( 1647 ) = 'begin saving output to a new file you m'
     .//             'ust first issue a DISCARD'
      TEXT( 1648 ) = 'command.  If you use the same filename '
     .//             'you may (depending upon your'
      TEXT( 1649 ) = 'system) overwrite the previous file.'
      TEXT( 1650 ) = ' '
      TEXT( 1651 ) = 'Regardless of whether you save any scre'
     .//             'en output, Inspekt automatically'
      TEXT( 1652 ) = 'saves every command you type at the "In'
     .//             'spekt>" prompt.  These commands'
      TEXT( 1653 ) = 'are collected in the Inspekt Log File. '
     .//             ' It has a name of the form'
      TEXT( 1654 ) = '"nsp#####.log" where each ''#'' is a de'
     .//             'cimal digit. In addition to'
      TEXT( 1655 ) = 'commands you type, the text of any erro'
     .//             'r message that Inspekt produces'
      TEXT( 1656 ) = 'are recorded in the log file.'
      TEXT( 1657 ) = ' '
      TEXT( 1658 ) = 'The log file is a crucial aid in determ'
     .//             'ining why Inspekt sometimes'
      TEXT( 1659 ) = 'behaves in unexpected ways. Each log fi'
     .//             'le is in fact an Inspekt'
      TEXT( 1660 ) = 'procedure that you can execute to exact'
     .//             'ly the sequence of commands'
      TEXT( 1661 ) = 'you entered in some previous Inspekt wo'
     .//             'rk session.  If you have'
      TEXT( 1662 ) = 'a problem with Inspekt, and need our as'
     .//             'sistance, be sure to save the'
      TEXT( 1663 ) = 'log file so that we can duplicate your '
     .//             'problems.'
      TEXT( 1664 ) = ' '
      TEXT( 1665 ) = '@@Saving Work        --- SAVE TO'
      TEXT( 1666 ) = 'Quit Help'
      TEXT( 1667 ) = 'Help'
      FINISH( 42 ) = 1667
 
      BEGIN ( 43 ) = 1668
      TEXT( 1668 ) = 'When you issue a select command, you ar'
     .//             'e telling Inspekt to find'
      TEXT( 1669 ) = 'rows from a table (or join of tables) a'
     .//             's specified by a FROM clause.'
      TEXT( 1670 ) = 'The rows match some criterion specified'
     .//             ' in a "WHERE" clause. (If'
      TEXT( 1671 ) = 'no "WHERE" clause is specified, all eve'
     .//             'nts are considered to be of'
 
      RETURN
      END
