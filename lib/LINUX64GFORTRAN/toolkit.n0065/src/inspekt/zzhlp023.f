C$Procedure      ZZHLP023 ( private help text )
 
      SUBROUTINE ZZHLP023 ( BEGIN, FINISH, TEXT )
 
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
      FINISH( 60 ) = 2120
 
      BEGIN ( 61 ) = 2121
      TEXT( 2121 ) = 'To see a summary of the characteristics'
     .//             ' of all columns for all loaded events k'
     .//             'er'
      TEXT( 2122 ) = '@literal'
      TEXT( 2123 ) = 'SHOW SUMMARY;'
      TEXT( 2124 ) = '|endliteral'
      TEXT( 2125 ) = 'Inspekt will display an alphabetical li'
     .//             'sting of all columns'
      TEXT( 2126 ) = ' '
      TEXT( 2127 ) = '@@SHOW SUMMARY     ...'
      TEXT( 2128 ) = 'Quit Help'
      TEXT( 2129 ) = 'Help'
      FINISH( 61 ) = 2129
 
      BEGIN ( 62 ) = 2130
      TEXT( 2130 ) = 'In addition to symbols you define, ther'
     .//             'e are special symbols called "queries"'
      TEXT( 2131 ) = 'that have no permanent value.  You supp'
     .//             'ly the value of a query when a command'
      TEXT( 2132 ) = 'containing it is issued.  A query is an'
     .//             'y word beginning with a letter'
      TEXT( 2133 ) = 'and ending with a question mark (?).  W'
     .//             'henever, such a word is encountered in'
      TEXT( 2134 ) = 'a command, Inspekt asks you to supply a'
     .//             ' value for the query.  You should enter'
      TEXT( 2135 ) = 'the value followed by a semicolon (;).'
      TEXT( 2136 ) = ' '
      TEXT( 2137 ) = 'Here''s an example.  Suppose you routin'
     .//             'ely issue some select command and that '
     .//             'the'
      TEXT( 2138 ) = 'only portion that changes is the constr'
     .//             'aint upon the column time.'
      TEXT( 2139 ) = '@literal'
      TEXT( 2140 ) = 'SELECT TIME, COMMENTS'
      TEXT( 2141 ) = 'FROM EVENTLIST'
      TEXT( 2142 ) = 'WHERE TIME variable condition'
      TEXT( 2143 ) = 'ORDER BY TIME;'
      TEXT( 2144 ) = '|endliteral'
      TEXT( 2145 ) = 'Define the following symbols'
      TEXT( 2146 ) = '@literal'
      TEXT( 2147 ) = 'DEFINE STDSEL SELECT TIME, COMMENTS FOR'
     .//             'M EVENTLIST WHERE TIME'
      TEXT( 2148 ) = '       After_Before_Tween? ORDER BY TIM'
     .//             'E;'
      TEXT( 2149 ) = 'DEFINE AFTER  GT TIME?'
      TEXT( 2150 ) = 'DEFINE BEFORE LT TIME?'
      TEXT( 2151 ) = 'DEFINE TWEEN  BETWEEN FIRST_TIME? AND L'
     .//             'AST_TIME?'
      TEXT( 2152 ) = '|endliteral'
      TEXT( 2153 ) = ' '
      TEXT( 2154 ) = 'Here''s what happens when we now type S'
     .//             'TDSEL;'
      TEXT( 2155 ) = '@literal'
      TEXT( 2156 ) = 'Inspekt> STDSEL;'
      TEXT( 2157 ) = '|endliteral'
      TEXT( 2158 ) = 'Inspekt responds with the following pro'
     .//             'mpt.'
      TEXT( 2159 ) = '@literal'
      TEXT( 2160 ) = 'Enter value for After_Before_Tween >'
      TEXT( 2161 ) = '|endliteral'
      TEXT( 2162 ) = 'You can type anything at this prompt. H'
     .//             'owever the prompt suggests you should'
      TEXT( 2163 ) = 'pick one of the symbols you define earl'
     .//             'ier.  Type "before" followed by a semi-'
     .//             'co'
      TEXT( 2164 ) = '@literal'
      TEXT( 2165 ) = 'Enter value for After_Before_Tween > be'
     .//             'fore;'
      TEXT( 2166 ) = 'Enter value for TIME >'
      TEXT( 2167 ) = '|endliteral'
      TEXT( 2168 ) = 'Now enter some time (be sure to put it '
     .//             'in quotes) and follow this with a semi-'
     .//             'co'
      TEXT( 2169 ) = '@literal'
      TEXT( 2170 ) = 'Enter value for After_Before_Tween > be'
     .//             'fore;'
      TEXT( 2171 ) = 'Enter value for TIME > "1 jan 1995";'
      TEXT( 2172 ) = '|endliteral'
      TEXT( 2173 ) = 'The effect of these three entries is'
      TEXT( 2174 ) = '@literal'
      TEXT( 2175 ) = 'SELECT TIME, COMMENTS FROM EVENTLIST'
      TEXT( 2176 ) = 'WHERE TIME LT "1 jan 1995"'
      TEXT( 2177 ) = 'ORDER BY TIME;'
      TEXT( 2178 ) = '|endliteral'
      TEXT( 2179 ) = 'By creative use of symbols and queries '
     .//             'you can greatly alter'
      TEXT( 2180 ) = 'your view of Inspekt and greatly ease t'
     .//             'he entry of frequently'
      TEXT( 2181 ) = 'typed commands.'
      TEXT( 2182 ) = '@@Special Symbols --- Queries'
      TEXT( 2183 ) = 'Quit Help'
      TEXT( 2184 ) = 'Help'
      FINISH( 62 ) = 2184
 
      BEGIN ( 63 ) = 2185
      TEXT( 2185 ) = 'When you create SELECT commands you wil'
     .//             'l often need to'
      TEXT( 2186 ) = 'compare a character column to some spec'
     .//             'ified string. To'
      TEXT( 2187 ) = 'specify a string you must enclose it in'
     .//             ' either single ('') or'
      TEXT( 2188 ) = 'double (") quotes.  (Note that these ar'
     .//             'e both single characters.'
      TEXT( 2189 ) = 'You cannot use '''' in place of ".) For'
     .//             ' example, you might want to'
      TEXT( 2190 ) = 'find all of the rows for which a charac'
     .//             'ter column begins with'
      TEXT( 2191 ) = 'the letter ''A''.  To specify this cond'
     .//             'ition in a query you'
      TEXT( 2192 ) = 'would type:'
      TEXT( 2193 ) = '@literal'
      TEXT( 2194 ) = 'column GE ''A'' AND column LE ''B'''
      TEXT( 2195 ) = '|endliteral'
      TEXT( 2196 ) = 'If you need to put a quote into a strin'
     .//             'g, you type double the'
      TEXT( 2197 ) = 'quote as you would in a FORTRAN string.'
     .//             '  For example the word'
      TEXT( 2198 ) = 'DOESN''T would be typed as'
      TEXT( 2199 ) = '@literal'
      TEXT( 2200 ) = '''DOESN''''T'''
      TEXT( 2201 ) = '|endliteral'
      TEXT( 2202 ) = ' '
      TEXT( 2203 ) = '@@Specifying Strings'
      TEXT( 2204 ) = 'Quit Help'
      TEXT( 2205 ) = 'Help'
      TEXT( 2206 ) = 'Specifying Times'
      FINISH( 63 ) = 2206
 
      BEGIN ( 64 ) = 2207
      TEXT( 2207 ) = 'Although Inspekt can display times in a'
     .//             'lmost any format, the set of'
      TEXT( 2208 ) = 'inputs is necessarily more restrictive.'
 
      RETURN
      END
