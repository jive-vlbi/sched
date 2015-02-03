C$Procedure      ZZHLP024 ( private help text )
 
      SUBROUTINE ZZHLP024 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 2209 ) = ' '
      TEXT( 2210 ) = 'First, time strings need to be input as'
     .//             ' strings.'
      TEXT( 2211 ) = 'Strings must be enclosed'
      TEXT( 2212 ) = 'in quotes. The condition'
      TEXT( 2213 ) = '@literal'
      TEXT( 2214 ) = 'TIME LT 1 JAN 1995'
      TEXT( 2215 ) = '|endliteral'
      TEXT( 2216 ) = 'will not be recognized if it is part of'
     .//             ' a SELECT command.  However,'
      TEXT( 2217 ) = 'once you place quotes around the time, '
     .//             'the time string will be'
      TEXT( 2218 ) = 'recognized'
      TEXT( 2219 ) = '@literal'
      TEXT( 2220 ) = 'TIME LT "1 JAN 1995"'
      TEXT( 2221 ) = '|endliteral'
      TEXT( 2222 ) = ' '
      TEXT( 2223 ) = 'A wide variety of time formats are allo'
     .//             'wed as input to Inspekt.'
      TEXT( 2224 ) = 'These formats are listed below.'
      TEXT( 2225 ) = ' '
      TEXT( 2226 ) = '@subsection Spacecraft Clock'
      TEXT( 2227 ) = '@literal'
      TEXT( 2228 ) = 'MO   SCLK mars observer spacecraft cloc'
     .//             'k string'
      TEXT( 2229 ) = 'GLL  SCLK galileo spacecraft clock stri'
     .//             'ng'
      TEXT( 2230 ) = 'VGR1 SCLK voyager 1 spacecraft clock st'
     .//             'ring'
      TEXT( 2231 ) = 'VGR2 SCLK voyager 2 spacecraft clock st'
     .//             'ring'
      TEXT( 2232 ) = '|endliteral'
      TEXT( 2233 ) = ' '
      TEXT( 2234 ) = 'To use these formats you must have eith'
     .//             'er specified SCLK to be an'
      TEXT( 2235 ) = 'appropriate  kernel (see Getting Starte'
     .//             'd) or have loaded an appropriate'
      TEXT( 2236 ) = 'kernel via the LOAD SCLK KERNEL command'
     .//             '.'
      TEXT( 2237 ) = ' '
      TEXT( 2238 ) = ' '
      TEXT( 2239 ) = '@subsection ISO Formats'
      TEXT( 2240 ) = ' '
      TEXT( 2241 ) = 'The International Standards Organizatio'
     .//             'n (ISO) time format is used  by'
      TEXT( 2242 ) = 'many NASA flight projects.'
      TEXT( 2243 ) = '@literal'
      TEXT( 2244 ) = 'YYYY-MM-DDTHR:MN:SC    ISO UTC Calendar'
     .//             ' format'
      TEXT( 2245 ) = 'YYYY-DDDTHR:MN:SC      ISO UTC Day of y'
     .//             'ear format'
      TEXT( 2246 ) = '|endliteral'
      TEXT( 2247 ) = ' '
      TEXT( 2248 ) = '@subsection Generic'
      TEXT( 2249 ) = ' '
      TEXT( 2250 ) = 'In these formats Month stands for the m'
     .//             'onth spelled out to 3 or more'
      TEXT( 2251 ) = 'letters, e.g. Jan, Janu, Janua, etc.  A'
     .//             'lso note that where spaces have'
      TEXT( 2252 ) = 'been used to separate the components of'
     .//             ' the date you may also use a'
      TEXT( 2253 ) = 'comma or a slash (i.e. the solidus (/) '
     .//             ').  All times are UTC.'
      TEXT( 2254 ) = '@literal'
      TEXT( 2255 ) = 'MONTH DD YYYY  HR:MN:SC.##...#'
      TEXT( 2256 ) = 'DD MONTH YYYY  HR:MN:SC.##...#'
      TEXT( 2257 ) = 'YYYY DD MONTH  HR:MN:SC.##...#'
      TEXT( 2258 ) = 'YYYY MONTH DD  HR:MN:SC.##...#'
      TEXT( 2259 ) = ' '
      TEXT( 2260 ) = 'YYYY MM DD HR:MN:SC.##...#'
      TEXT( 2261 ) = 'MM DD YYYY HR:MN:SC.##...#'
      TEXT( 2262 ) = ' '
      TEXT( 2263 ) = 'JD244xxxx.xx...x'
      TEXT( 2264 ) = '|endliteral'
      TEXT( 2265 ) = '@@Specifying Times'
      TEXT( 2266 ) = 'Quit Help'
      TEXT( 2267 ) = 'Help'
      TEXT( 2268 ) = 'Where Clause'
      FINISH( 64 ) = 2268
 
      BEGIN ( 65 ) = 2269
      TEXT( 2269 ) = 'A symbol is a word that begins with a l'
     .//             'etter of the alphabet'
      TEXT( 2270 ) = 'and does not end with a question mark. '
     .//             ' It must be 32 or'
      TEXT( 2271 ) = 'fewer characters in length.  Moreover, '
     .//             'you must specifically'
      TEXT( 2272 ) = 'designate this word to be a symbol via '
     .//             'the DEFINE command.'
      TEXT( 2273 ) = 'The define command associates a value w'
     .//             'ith the symbol.'
      TEXT( 2274 ) = ' '
      TEXT( 2275 ) = 'When you type a command in Inspekt that'
     .//             ' contains a symbol,'
      TEXT( 2276 ) = 'the symbol is replaced by its associate'
     .//             'd value.  The command'
      TEXT( 2277 ) = 'is then re-examined and any remaining s'
     .//             'ymbols are replaced'
      TEXT( 2278 ) = 'by their associated values.'
      TEXT( 2279 ) = ' '
      TEXT( 2280 ) = 'Symbols allow you to customize your Ins'
     .//             'pekt environment.  In'
      TEXT( 2281 ) = 'addition they allow you to greatly redu'
     .//             'ce the amount of typing'
      TEXT( 2282 ) = 'you need to do in order to issue freque'
     .//             'ntly occurring groups'
      TEXT( 2283 ) = 'of words.'
      TEXT( 2284 ) = ' '
      TEXT( 2285 ) = 'Words that are surrounded by any of the'
     .//             ' characters ("''@) are not'
      TEXT( 2286 ) = 'regarded as symbols and are processed a'
     .//             's they appear.'
      TEXT( 2287 ) = ' '
      TEXT( 2288 ) = 'Symbols are case insensitive in Inspekt'
     .//             '.  If you define "SPUD"'
      TEXT( 2289 ) = 'to be a symbol then "spud", "Spud", "sP'
     .//             'ud", etc. will all be'
      TEXT( 2290 ) = 'interpreted as SPUD.'
      TEXT( 2291 ) = ' '
      TEXT( 2292 ) = ' '
      TEXT( 2293 ) = 'You may not define EDIT, DO, RECALL, ST'
     .//             'ART, STOP, DEFINE, ECHO'
      TEXT( 2294 ) = '@@Symbol'
      TEXT( 2295 ) = 'Quit Help'
      TEXT( 2296 ) = 'Help'
      FINISH( 65 ) = 2296
 
      BEGIN ( 66 ) = 2297
      TEXT( 2297 ) = 'The language you use to communicate wit'
     .//             'h Inspekt is a word oriented'
      TEXT( 2298 ) = 'language.  With one exception the small'
     .//             'est significant component of a'
      TEXT( 2299 ) = 'command is a word.  The words in a comm'
     .//             'and must match a pattern that is'
      TEXT( 2300 ) = 'called the syntax of the command. The s'
     .//             'yntax of the commands you type'
      TEXT( 2301 ) = 'at the prompt Inspekt> can be expressed'
     .//             ' in a language called Meta/2.'
      TEXT( 2302 ) = 'The sections below describe the various'
     .//             ' constructs that make up a Meta/2'
 
      RETURN
      END
