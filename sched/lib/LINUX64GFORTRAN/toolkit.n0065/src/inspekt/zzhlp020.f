C$Procedure      ZZHLP020 ( private help text )
 
      SUBROUTINE ZZHLP020 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1851 ) = 'You may also control the placement of t'
     .//             'he report title using the command:'
      TEXT( 1852 ) = '@literal'
      TEXT( 1853 ) = 'SET TITLE JUSTIFICATION (1:1){ LEFT | C'
     .//             'ENTER | RIGHT }'
      TEXT( 1854 ) = '|endliteral'
      TEXT( 1855 ) = ' '
      TEXT( 1856 ) = 'You can control how often the title are'
     .//             ' placed in your report  by'
      TEXT( 1857 ) = 'issuing a SET TITLE/HEADER FREQUENCY co'
     .//             'mmand.  The syntax for this is:'
      TEXT( 1858 ) = '@exliteral'
      TEXT( 1859 ) = 'SET TITLE  FREQUENCY (1:1){ 0'
      TEXT( 1860 ) = '                          | 1ST'
      TEXT( 1861 ) = '                          | FIRST'
      TEXT( 1862 ) = '                          | ALL'
      TEXT( 1863 ) = '                          | EVERY @int('
     .//             '1:)'
      TEXT( 1864 ) = '                          }'
      TEXT( 1865 ) = '!endliteral'
      TEXT( 1866 ) = 'The values mean respectively: on none o'
     .//             'f the pages; on the first page only; on'
      TEXT( 1867 ) = 'every page, and on the first page and e'
     .//             'very nth page following the first  page'
     .//             '.'
      TEXT( 1868 ) = '(See SET PAGE ...for a description of t'
     .//             'he output page ).'
      TEXT( 1869 ) = ' '
      TEXT( 1870 ) = '@@SET TITLE  ...'
      TEXT( 1871 ) = 'Quit Help'
      TEXT( 1872 ) = 'Help'
      TEXT( 1873 ) = 'SET HEADER  ...'
      FINISH( 50 ) = 1873
 
      BEGIN ( 51 ) = 1874
      TEXT( 1874 ) = 'When you type'
      TEXT( 1875 ) = '@literal'
      TEXT( 1876 ) = 'EDIT command'
      TEXT( 1877 ) = '|endliteral'
      TEXT( 1878 ) = 'Inspekt writes the specified command to'
     .//             ' a file, starts up some'
      TEXT( 1879 ) = 'text editor on your system and loads th'
     .//             'e file into the editor'
      TEXT( 1880 ) = 'window.  However, the editor selected m'
     .//             'ight not be one you are'
      TEXT( 1881 ) = 'familiar with.  This can be terribly fr'
     .//             'ustrating because you may'
      TEXT( 1882 ) = 'not know how to get the editor to carry'
     .//             ' out any familiar task.'
      TEXT( 1883 ) = ' '
      TEXT( 1884 ) = 'You can determine the default editor by'
     .//             ' typing'
      TEXT( 1885 ) = '@literal'
      TEXT( 1886 ) = 'SHOW ENVIRONMENT'
      TEXT( 1887 ) = '|endliteral'
      TEXT( 1888 ) = 'If the editor listed there is not the o'
     .//             'ne you prefer you can'
      TEXT( 1889 ) = 'change to some other editor by typing'
      TEXT( 1890 ) = '@literal'
      TEXT( 1891 ) = 'SET EDITOR edit_launching_command'
      TEXT( 1892 ) = '|endliteral'
      TEXT( 1893 ) = 'where "edit_launching_command" is the c'
     .//             'ommand you type to'
      TEXT( 1894 ) = 'invoke your favorite editor on your com'
     .//             'puter.  If you use an alias'
      TEXT( 1895 ) = 'for this command, you should look up wh'
     .//             'at this alias translates'
      TEXT( 1896 ) = 'to and use that translation.'
      TEXT( 1897 ) = ' '
      TEXT( 1898 ) = '@@Setting The Editor'
      TEXT( 1899 ) = 'Quit Help'
      TEXT( 1900 ) = 'Help'
      FINISH( 51 ) = 1900
 
      BEGIN ( 52 ) = 1901
      TEXT( 1901 ) = 'Inspekt allows you to adjust the workin'
     .//             'g session environment'
      TEXT( 1902 ) = 'as well as the appearance of its output'
     .//             's to suit your needs.'
      TEXT( 1903 ) = 'You may control:'
      TEXT( 1904 ) = '@newlist'
      TEXT( 1905 ) = '@numitem the format of reports that res'
     .//             'ult from the SELECT command;'
      TEXT( 1906 ) = ' '
      TEXT( 1907 ) = '@numitem the format used when printing '
     .//             'time strings;'
      TEXT( 1908 ) = ' '
      TEXT( 1909 ) = '@numitem the width and justification(ri'
     .//             'ght/left) of columns that'
      TEXT( 1910 ) = 'appear in reports;'
      TEXT( 1911 ) = ' '
      TEXT( 1912 ) = '@numitem the size of the "page" on whic'
     .//             'h reports appear;'
      TEXT( 1913 ) = ' '
      TEXT( 1914 ) = '@numitem the frequency with which repor'
     .//             't titles and report headers appear.'
      TEXT( 1915 ) = ' '
      TEXT( 1916 ) = 'To change current settings use the "SET'
     .//             '" command. To examine current'
      TEXT( 1917 ) = 'settings use the "SHOW" command.'
      TEXT( 1918 ) = '@@Setting up Inspekt --- SET'
      TEXT( 1919 ) = 'Quit Help'
      TEXT( 1920 ) = 'Help'
      TEXT( 1921 ) = 'Columns'
      TEXT( 1922 ) = 'Display Area'
      TEXT( 1923 ) = 'Reports'
      TEXT( 1924 ) = 'SET COLUMN ...'
      TEXT( 1925 ) = 'SET FORMAT ...'
      TEXT( 1926 ) = 'SET HEADER ...'
      TEXT( 1927 ) = 'SET PAGE   ...'
      TEXT( 1928 ) = 'SET TIME   ...'
      TEXT( 1929 ) = 'SET TITLE  ...'
      TEXT( 1930 ) = 'Time Formats'
      TEXT( 1931 ) = 'Numeric Formats'
      TEXT( 1932 ) = 'Other Settings'
      TEXT( 1933 ) = 'Current Settings   --- SHOW'
      TEXT( 1934 ) = ' '
      FINISH( 52 ) = 1934
 
      BEGIN ( 53 ) = 1935
      TEXT( 1935 ) = 'If you already know the title of a help'
     .//             ' topic you''d like to see, you don''t'
      TEXT( 1936 ) = 'have to navigate through the help syste'
     .//             'ms series of menus.  Instead, you'
      TEXT( 1937 ) = 'can bring up the help topic immediately'
     .//             '.  To do this you add the topic'
      TEXT( 1938 ) = 'title to the HELP command.'
      TEXT( 1939 ) = '@literal'
      TEXT( 1940 ) = 'Inspekt> HELP topic title;'
      TEXT( 1941 ) = '|endliteral'
 
      RETURN
      END
