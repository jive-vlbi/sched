C$Procedure      ZZHLP018 ( private help text )
 
      SUBROUTINE ZZHLP018 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1672 ) = 'interest.) To request columns in a repo'
     .//             'rt you should type the first'
      TEXT( 1673 ) = 'portion of the SELECT command as shown '
     .//             'below.'
      TEXT( 1674 ) = '@literal'
      TEXT( 1675 ) = 'SELECT col_1, col_2, ... , col_n'
      TEXT( 1676 ) = 'FROM table ...'
      TEXT( 1677 ) = 'WHERE condition ...'
      TEXT( 1678 ) = 'ORDER BY ...'
      TEXT( 1679 ) = '|endliteral'
      TEXT( 1680 ) = 'Note that commas (,) are required betwe'
     .//             'en column names.  If you'
      TEXT( 1681 ) = 'select rows from a single table (i.e. t'
     .//             'he FROM clause lists a single'
      TEXT( 1682 ) = 'table) you can just use the column name'
     .//             's from that table.  However,'
      TEXT( 1683 ) = 'if you select columns from a "join" of '
     .//             'two or more tables and two'
      TEXT( 1684 ) = 'or more of the tables possess the colum'
     .//             'n you want to see you need'
      TEXT( 1685 ) = 'to make sure you tell Inspekt which tab'
     .//             'le the column belongs to.'
      TEXT( 1686 ) = 'To do this you attach the name of the t'
     .//             'able (or an alias for that'
      TEXT( 1687 ) = 'table) to the front of the column name '
     .//             'as in'
      TEXT( 1688 ) = '@literal'
      TEXT( 1689 ) = '   table.column'
      TEXT( 1690 ) = 'or'
      TEXT( 1691 ) = '   alias.column'
      TEXT( 1692 ) = '|endliteral'
      TEXT( 1693 ) = 'The columns will be displayed in the re'
     .//             'port in the same order as you list'
      TEXT( 1694 ) = 'them in the select clause.'
      TEXT( 1695 ) = ' '
      TEXT( 1696 ) = '@@Select Clause'
      TEXT( 1697 ) = 'Quit Help'
      TEXT( 1698 ) = 'Help'
      TEXT( 1699 ) = 'Looking at Data    --- SELECT'
      FINISH( 43 ) = 1699
 
      BEGIN ( 44 ) = 1700
      TEXT( 1700 ) = 'To adjust the display attributes of a c'
     .//             'olumn use the SET COLUMN'
      TEXT( 1701 ) = 'command.  You may adjust the width of a'
     .//             ' column, the heading that'
      TEXT( 1702 ) = 'will appear in  tabular reports, whethe'
     .//             'r or not columns will be'
      TEXT( 1703 ) = 'left or right justified  in reports, an'
     .//             'd the format of the column'
      TEXT( 1704 ) = 'for time and numeric columns.  The synt'
     .//             'ax of the SET COLUMN command'
      TEXT( 1705 ) = 'is:'
      TEXT( 1706 ) = '@literal'
      TEXT( 1707 ) = 'SET COLUMN @name'
      TEXT( 1708 ) = ' (1:1){ WIDTH @int(8:80)'
      TEXT( 1709 ) = '      | FORMAT    (1:)@word'
      TEXT( 1710 ) = '      | HEADING   (1:)@word'
      TEXT( 1711 ) = '      | JUSTIFICATION RIGHT'
      TEXT( 1712 ) = '      | JUSTIFICATION LEFT }'
      TEXT( 1713 ) = '|endliteral'
      TEXT( 1714 ) = 'NOTE: If you modify the format of a num'
     .//             'eric or time column, the'
      TEXT( 1715 ) = 'width of the column is automatically ad'
     .//             'justed to match the'
      TEXT( 1716 ) = 'width of the new format.  You must re-a'
     .//             'djust the width if you'
      TEXT( 1717 ) = 'want it to be something other than the '
     .//             'width of the format.'
      TEXT( 1718 ) = '@@SET COLUMN ...'
      TEXT( 1719 ) = 'Quit Help'
      TEXT( 1720 ) = 'Help'
      TEXT( 1721 ) = 'Time Formats'
      TEXT( 1722 ) = 'Numeric Formats'
      TEXT( 1723 ) = 'SHOW COLUMN   ...'
      FINISH( 44 ) = 1723
 
      BEGIN ( 45 ) = 1724
      TEXT( 1724 ) = 'Inspekt supports 4 basic report formats'
     .//             ': TABULAR, FLAGGED, VERBATIM and'
      TEXT( 1725 ) = 'DELIMITED. These are set using the SET '
     .//             'FORMAT command. The syntax for'
      TEXT( 1726 ) = 'this command is:'
      TEXT( 1727 ) = ' '
      TEXT( 1728 ) = '@literal'
      TEXT( 1729 ) = ' '
      TEXT( 1730 ) = 'SET FORMAT (0:1){ MARKED | SPACED }'
      TEXT( 1731 ) = '   TABULAR (0:1){ PRESERVED }'
      TEXT( 1732 ) = ' '
      TEXT( 1733 ) = 'SET FORMAT FLAGGED (0:1){ PRESERVED }'
      TEXT( 1734 ) = ' '
      TEXT( 1735 ) = 'SET FORMAT VERBATIM'
      TEXT( 1736 ) = ' '
      TEXT( 1737 ) = 'SET FORMAT DELIMITED (0:1){ PRESERVED }'
      TEXT( 1738 ) = '                     (0:2){ DELIMITER @'
     .//             'word(SPACE|%)'
      TEXT( 1739 ) = '                          | QUOTE @word'
     .//             '(%) }'
      TEXT( 1740 ) = '|endliteral'
      TEXT( 1741 ) = ' '
      TEXT( 1742 ) = 'TABULAR reports show each event as a ro'
     .//             'w of a table with the various'
      TEXT( 1743 ) = 'event attributes appearing in fixed col'
     .//             'umns.'
      TEXT( 1744 ) = ' '
      TEXT( 1745 ) = 'FLAGGED reports show events as a vertic'
     .//             'al list of name-value paragraphs'
      TEXT( 1746 ) = 'with each complete event separated from'
     .//             ' the next by a blank line.'
      TEXT( 1747 ) = ' '
      TEXT( 1748 ) = 'VERBATIM reports list events as a verti'
     .//             'cal list as well. However the'
      TEXT( 1749 ) = 'column name is listed on one line and i'
     .//             'ts value on the next line(s).'
      TEXT( 1750 ) = 'This  presents the components of each c'
     .//             'olumn without line breaks.'
      TEXT( 1751 ) = ' '
      TEXT( 1752 ) = 'DELIMITED reports are used to export an'
     .//             ' Inspekt report into a tab (or'
      TEXT( 1753 ) = 'otherwise delimited) format suitable fo'
     .//             'r importing into spreadsheet'
      TEXT( 1754 ) = 'programs such as Microsoft Excel.'
      TEXT( 1755 ) = ' '
      TEXT( 1756 ) = '@@SET FORMAT ...'
      TEXT( 1757 ) = 'Quit Help'
      TEXT( 1758 ) = 'Help'
      TEXT( 1759 ) = 'Reports'
      TEXT( 1760 ) = 'SET FORMAT MARK ...'
      TEXT( 1761 ) = 'SHOW FORMAT     ...'
      FINISH( 45 ) = 1761
 
      BEGIN ( 46 ) = 1762
      TEXT( 1762 ) = 'When you use  MARKED TABULAR formats fo'
     .//             'r reports, Inspekt places'
 
      RETURN
      END
