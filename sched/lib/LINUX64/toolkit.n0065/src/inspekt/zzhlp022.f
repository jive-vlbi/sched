C$Procedure      ZZHLP022 ( private help text )
 
      SUBROUTINE ZZHLP022 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 2033 ) = ' '
      TEXT( 2034 ) = '@numitem What leapsecond and SCLK kerne'
     .//             'ls are loaded'
      TEXT( 2035 ) = ' '
      TEXT( 2036 ) = 'To see the current session environment,'
     .//             ' type the command'
      TEXT( 2037 ) = '@literal'
      TEXT( 2038 ) = 'SHOW ENVIRONMENT'
      TEXT( 2039 ) = '|endliteral'
      TEXT( 2040 ) = ' '
      TEXT( 2041 ) = '@@SHOW ENVIRONMENT ...'
      TEXT( 2042 ) = 'Quit Help'
      TEXT( 2043 ) = 'Help'
      TEXT( 2044 ) = 'Making Help Wait'
      TEXT( 2045 ) = 'Echoing Translated Commands'
      TEXT( 2046 ) = 'Kernels            --- LOAD'
      FINISH( 56 ) = 2046
 
      BEGIN ( 57 ) = 2047
      TEXT( 2047 ) = 'You can see what report format is curre'
     .//             'ntly active by typing the'
      TEXT( 2048 ) = 'command:'
      TEXT( 2049 ) = '@literal'
      TEXT( 2050 ) = 'SHOW FORMAT;'
      TEXT( 2051 ) = '|endliteral'
      TEXT( 2052 ) = 'In addition to showing you the format, '
     .//             'it will show you the current'
      TEXT( 2053 ) = 'format being used for presenting time a'
     .//             'nd in the case of MARKED'
      TEXT( 2054 ) = 'TABULAR format the current format mark.'
     .//             '  You will also be given'
      TEXT( 2055 ) = 'the current value for triggering a data'
     .//             ' DELUGE WARNING.  An example'
      TEXT( 2056 ) = 'result is given here.'
      TEXT( 2057 ) = '@literal'
      TEXT( 2058 ) = 'Report Format          :  MARKED TABULA'
     .//             'R'
      TEXT( 2059 ) = 'Report Mark            :  ''>'''
      TEXT( 2060 ) = 'Default Time     Format:  YYYY MON DD H'
     .//             'R:MN:SC::UTC::RND'
      TEXT( 2061 ) = 'Default Integer  Format:  ###########'
      TEXT( 2062 ) = 'Default Floating Format:  #########.###'
     .//             '#'
      TEXT( 2063 ) = 'Deluge Warning         :  100'
      TEXT( 2064 ) = 'Auto Adjust            :  ASK (applies '
     .//             'only to tabular formats)'
      TEXT( 2065 ) = '|endliteral'
      TEXT( 2066 ) = '@@SHOW FORMAT   ...'
      TEXT( 2067 ) = 'Quit Help'
      TEXT( 2068 ) = 'Help'
      TEXT( 2069 ) = 'Default Floating Format'
      TEXT( 2070 ) = 'Default Integer Format'
      TEXT( 2071 ) = 'Default Time Format'
      FINISH( 57 ) = 2071
 
      BEGIN ( 58 ) = 2072
      TEXT( 2072 ) = 'When you issue a select command to Insp'
     .//             'ekt, the speed'
      TEXT( 2073 ) = 'with which it is executed may depend up'
     .//             'on whether the'
      TEXT( 2074 ) = 'columns referenced in the select comman'
     .//             'd are indexed.'
      TEXT( 2075 ) = ' '
      TEXT( 2076 ) = 'To get a list of all indexed columns, t'
     .//             'ype the command'
      TEXT( 2077 ) = ' '
      TEXT( 2078 ) = '@literal'
      TEXT( 2079 ) = 'SHOW INDEXED'
      TEXT( 2080 ) = '|endliteral'
      TEXT( 2081 ) = ' '
      TEXT( 2082 ) = '@@SHOW INDEXED  ...'
      TEXT( 2083 ) = 'Quit Help'
      TEXT( 2084 ) = 'Help'
      FINISH( 58 ) = 2084
 
      BEGIN ( 59 ) = 2085
      TEXT( 2085 ) = 'You can create a summary of the loaded '
     .//             'E-kernels by typing the'
      TEXT( 2086 ) = 'command'
      TEXT( 2087 ) = '@literal'
      TEXT( 2088 ) = 'SHOW KERNELS;'
      TEXT( 2089 ) = '|endliteral'
      TEXT( 2090 ) = ' '
      TEXT( 2091 ) = 'There are two main reasons for issuing '
     .//             'this command:'
      TEXT( 2092 ) = '@newlist'
      TEXT( 2093 ) = '@numitem Obtaining a quick summary of l'
     .//             'oaded kernels'
      TEXT( 2094 ) = ' '
      TEXT( 2095 ) = '@numitem Finding out whether the tables'
     .//             ' and kernels you thought you'
      TEXT( 2096 ) = '         loaded were in fact loaded by '
     .//             'Inspekt.'
      TEXT( 2097 ) = ' '
      TEXT( 2098 ) = '@@SHOW KERNELS     ...'
      TEXT( 2099 ) = 'Quit Help'
      TEXT( 2100 ) = 'Help'
      TEXT( 2101 ) = 'SHOW COMMENTS ...'
      FINISH( 59 ) = 2101
 
      BEGIN ( 60 ) = 2102
      TEXT( 2102 ) = 'You can see the current page settings ('
     .//             'including the report title'
      TEXT( 2103 ) = 'and header attributes) by typing the co'
     .//             'mmand:'
      TEXT( 2104 ) = '@literal'
      TEXT( 2105 ) = 'SHOW PAGE;'
      TEXT( 2106 ) = '|endliteral'
      TEXT( 2107 ) = 'A sample output is given below. (Note: '
     .//             'the Page width refers to'
      TEXT( 2108 ) = 'number of columns one character wide wi'
     .//             'll fit on the page.)'
      TEXT( 2109 ) = '@literal'
      TEXT( 2110 ) = 'Page height (rows)   :  20'
      TEXT( 2111 ) = 'Page width  (columns):  80'
      TEXT( 2112 ) = ' '
      TEXT( 2113 ) = 'Page Title           :  Inspekt Report'
      TEXT( 2114 ) = 'Title Justification  :  LEFT'
      TEXT( 2115 ) = 'Title Appears on     :  First page only'
      TEXT( 2116 ) = ' '
      TEXT( 2117 ) = 'Header Appears on    :  First page only'
      TEXT( 2118 ) = '@@SHOW PAGE        ...'
      TEXT( 2119 ) = 'Quit Help'
      TEXT( 2120 ) = 'Help'
 
      RETURN
      END
