C$Procedure      ZZHLP019 ( private help text )
 
      SUBROUTINE ZZHLP019 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1763 ) = 'the character ">" in the leftmost colum'
     .//             'n of the report.  You can'
      TEXT( 1764 ) = 'modify this FORMAT MARK by issuing a SE'
     .//             'T FORMAT MARK command.'
      TEXT( 1765 ) = ' '
      TEXT( 1766 ) = 'Decide what character you want to appea'
     .//             'r in the leftmost column'
      TEXT( 1767 ) = '(remember you cannot use the semi-colon'
     .//             ' ";"). For the purpose of'
      TEXT( 1768 ) = 'an example lets use "=". Then issue the'
     .//             ' command:'
      TEXT( 1769 ) = '@literal'
      TEXT( 1770 ) = 'SET FORMAT MARK =;'
      TEXT( 1771 ) = '|endliteral'
      TEXT( 1772 ) = 'From this point forward MARKED TABULAR '
     .//             'reports will place the character'
      TEXT( 1773 ) = '"=" in the left most column of reports.'
      TEXT( 1774 ) = '@@SET FORMAT MARK ...'
      TEXT( 1775 ) = 'Quit Help'
      TEXT( 1776 ) = 'Help'
      FINISH( 46 ) = 1776
 
      BEGIN ( 47 ) = 1777
      TEXT( 1777 ) = 'When Inspekt produces a report as the r'
     .//             'esult of a SELECT command,'
      TEXT( 1778 ) = 'and the reporting format is some form o'
     .//             'f tabular format, Inspekt'
      TEXT( 1779 ) = 'can place a header at the beginning of '
     .//             'the displayed output.  This'
      TEXT( 1780 ) = 'header displays the names of the variou'
     .//             's columns that appear in the'
      TEXT( 1781 ) = 'report.  (If a column has an alias, the'
     .//             ' alias will be used instead of'
      TEXT( 1782 ) = 'the column name.)'
      TEXT( 1783 ) = ' '
      TEXT( 1784 ) = 'You can control how often these items a'
     .//             're placed in your report  by'
      TEXT( 1785 ) = 'issuing a SET HEADER FREQUENCY command.'
     .//             '  The syntax for this is:'
      TEXT( 1786 ) = '@literal'
      TEXT( 1787 ) = '@exliteral'
      TEXT( 1788 ) = 'SET HEADER FREQUENCY (1:1){ 0'
      TEXT( 1789 ) = '                          | 1ST'
      TEXT( 1790 ) = '                          | FIRST'
      TEXT( 1791 ) = '                          | ALL'
      TEXT( 1792 ) = '                          | EVERY @int('
     .//             '1:)'
      TEXT( 1793 ) = '                          }'
      TEXT( 1794 ) = '!endliteral'
      TEXT( 1795 ) = '|endliteral'
      TEXT( 1796 ) = 'The values mean respectively: on none o'
     .//             'f the pages; on the first'
      TEXT( 1797 ) = 'page only, on every page, and on the fi'
     .//             'rst page and every nth page'
      TEXT( 1798 ) = 'following the first page.'
      TEXT( 1799 ) = ' '
      TEXT( 1800 ) = '@@SET HEADER ...'
      TEXT( 1801 ) = 'Quit Help'
      TEXT( 1802 ) = 'Help'
      TEXT( 1803 ) = 'SET TITLE  ...'
      FINISH( 47 ) = 1803
 
      BEGIN ( 48 ) = 1804
      TEXT( 1804 ) = 'Reports written by Inspekt are modelled'
     .//             ' as if they were being written to a pag'
     .//             'e'
      TEXT( 1805 ) = 'of fixed height and width.  This is con'
     .//             'venient if you plan to save  the output'
      TEXT( 1806 ) = 'of a file (using the SAVE TO command) a'
     .//             'nd then print the the  resulting file.'
      TEXT( 1807 ) = 'Moreover, if you have a long report, it'
     .//             ' allows you to have  header and title'
      TEXT( 1808 ) = 'information appear on your screen at re'
     .//             'gular intervals.'
      TEXT( 1809 ) = ' '
      TEXT( 1810 ) = 'The default height and width of this pa'
     .//             'ge are 20 and 80 characters respectivel'
     .//             'y.'
      TEXT( 1811 ) = 'To adjust these use the following comma'
     .//             'nds.'
      TEXT( 1812 ) = '@literal'
      TEXT( 1813 ) = 'SET PAGE HEIGHT @int(20:)'
      TEXT( 1814 ) = 'SET PAGE WIDTH  @int(40:132)'
      TEXT( 1815 ) = '|endliteral'
      TEXT( 1816 ) = 'You may also set the title that appears'
     .//             ' on reports with the command'
      TEXT( 1817 ) = '@literal'
      TEXT( 1818 ) = 'SET PAGE TITLE (1:)@word'
      TEXT( 1819 ) = '|endliteral'
      TEXT( 1820 ) = 'Use the title "NONE" if you want no tit'
     .//             'le to appear on report pages.'
      TEXT( 1821 ) = ' '
      TEXT( 1822 ) = '@@SET PAGE   ...'
      TEXT( 1823 ) = 'Quit Help'
      TEXT( 1824 ) = 'Help'
      TEXT( 1825 ) = 'SHOW PAGE        ...'
      FINISH( 48 ) = 1825
 
      BEGIN ( 49 ) = 1826
      TEXT( 1826 ) = 'There are two commands available for ad'
     .//             'justing the format of time strings when'
      TEXT( 1827 ) = 'they are output in reports.  They are:'
      TEXT( 1828 ) = '@literal'
      TEXT( 1829 ) = 'SET COLUMN column_name FORMAT format;'
      TEXT( 1830 ) = ' '
      TEXT( 1831 ) = 'SET DEFAULT TIME FORMAT format;'
      TEXT( 1832 ) = '|endliteral'
      TEXT( 1833 ) = ' '
      TEXT( 1834 ) = '@@SET TIME ...'
      TEXT( 1835 ) = 'Quit Help'
      TEXT( 1836 ) = 'Help'
      TEXT( 1837 ) = 'Time Formats'
      TEXT( 1838 ) = 'Default Time Format'
      TEXT( 1839 ) = 'Custom Formats'
      FINISH( 49 ) = 1839
 
      BEGIN ( 50 ) = 1840
      TEXT( 1840 ) = 'When Inspekt produces a report as the r'
     .//             'esult of a SELECT command, it can place'
      TEXT( 1841 ) = 'a title and a header on the report.'
      TEXT( 1842 ) = 'You set a report title that appears on '
     .//             'pages of a report by'
      TEXT( 1843 ) = 'issuing the command:'
      TEXT( 1844 ) = '@literal'
      TEXT( 1845 ) = 'SET PAGE TITLE title'
      TEXT( 1846 ) = '|endliteral'
      TEXT( 1847 ) = 'Note that the TITLE is an attribute of '
     .//             'the display page.  Whenever, a report'
      TEXT( 1848 ) = 'is displayed in which a title is allowe'
     .//             'd, the title you''ve set with the'
      TEXT( 1849 ) = 'SET PAGE TITLE command will be the titl'
     .//             'e used.'
      TEXT( 1850 ) = ' '
 
      RETURN
      END
