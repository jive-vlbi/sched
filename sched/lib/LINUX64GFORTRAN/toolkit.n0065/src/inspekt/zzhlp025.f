C$Procedure      ZZHLP025 ( private help text )
 
      SUBROUTINE ZZHLP025 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 2303 ) = 'specification.  After reading these sec'
     .//             'tions you should be able to make'
      TEXT( 2304 ) = 'sense of any syntax specification prese'
     .//             'nted later in this User''s Guide.'
      TEXT( 2305 ) = ' '
      TEXT( 2306 ) = 'Inspekt''s command language is quite si'
     .//             'mple;  it does not require all of'
      TEXT( 2307 ) = 'the features of Meta/2.   But since Ins'
     .//             'pekt is expected to grow, we''ve'
      TEXT( 2308 ) = 'included a complete description of Meta'
     .//             '/2 so that you''ll have a handy'
      TEXT( 2309 ) = 'reference as this growth occurs.'
      TEXT( 2310 ) = ' '
      TEXT( 2311 ) = '@section Keywords'
      TEXT( 2312 ) = ' '
      TEXT( 2313 ) = 'Each command you type at the prompt Ins'
     .//             'pekt> must begin with a keyword.'
      TEXT( 2314 ) = 'Keywords define the structure of a comm'
     .//             'and: for the most part, a command'
      TEXT( 2315 ) = 'is a collection of keywords, some of wh'
     .//             'ich are modified by non-keyword'
      TEXT( 2316 ) = 'arguments.  The keywords of a Meta/2 sy'
     .//             'ntax description are the words'
      TEXT( 2317 ) = 'that do not begin with one of the speci'
     .//             'al characters in the list below:'
      TEXT( 2318 ) = '@exliteral'
      TEXT( 2319 ) = '"("   the left parenthesis'
      TEXT( 2320 ) = '"@"   the "at" sign'
      TEXT( 2321 ) = '"|"   the vertical bar'
      TEXT( 2322 ) = '"}"    the right  brace'
      TEXT( 2323 ) = '!endliteral'
      TEXT( 2324 ) = ' '
      TEXT( 2325 ) = 'In Inspekt all of the keywords are engl'
     .//             'ish words.'
      TEXT( 2326 ) = ' '
      TEXT( 2327 ) = 'The simplest Meta/2 expressions consist'
     .//             ' entirely of keywords.  Some of'
      TEXT( 2328 ) = 'Inspekt''s commands that fall into this'
     .//             ' category are shown below.'
      TEXT( 2329 ) = '@literal'
      TEXT( 2330 ) = 'SHOW KERNELS'
      TEXT( 2331 ) = ' '
      TEXT( 2332 ) = 'SHOW SUMMARY'
      TEXT( 2333 ) = ' '
      TEXT( 2334 ) = 'SHOW INDEXES'
      TEXT( 2335 ) = ' '
      TEXT( 2336 ) = 'SET AUTOADJUST ON'
      TEXT( 2337 ) = ' '
      TEXT( 2338 ) = 'SET FORMAT TABULAR'
      TEXT( 2339 ) = ' '
      TEXT( 2340 ) = 'SET FORMAT FLAGGED PRESERVED'
      TEXT( 2341 ) = ' '
      TEXT( 2342 ) = 'SET FORMAT VERBATIM'
      TEXT( 2343 ) = ' '
      TEXT( 2344 ) = 'SET TITLE FREQUENCY ALL'
      TEXT( 2345 ) = ' '
      TEXT( 2346 ) = 'SET HEADER FREQUENCY FIRST'
      TEXT( 2347 ) = '|endliteral'
      TEXT( 2348 ) = 'When you type a command, you may enter '
     .//             'the keywords in upper, lower or'
      TEXT( 2349 ) = 'mixed case.'
      TEXT( 2350 ) = ' '
      TEXT( 2351 ) = '@section Class Templates'
      TEXT( 2352 ) = ' '
      TEXT( 2353 ) = 'Keywords can be modified by non-keyword'
     .//             ' values.  The values associated'
      TEXT( 2354 ) = 'with a keyword always immediately follo'
     .//             'w that keyword.  A collection of'
      TEXT( 2355 ) = 'values is terminated by another keyword'
     .//             ', or by the end of the command.'
      TEXT( 2356 ) = ' '
      TEXT( 2357 ) = 'Class templates are used to indicate th'
     .//             'at the values associated with a'
      TEXT( 2358 ) = 'particular keyword belong to a class of'
     .//             ' values.  For example the Meta/2'
      TEXT( 2359 ) = 'expression of the rule that the keyword'
     .//             's SHOW COLUMN are to be followed'
      TEXT( 2360 ) = 'by the name of a column looks like this'
     .//             '.'
      TEXT( 2361 ) = '@literal'
      TEXT( 2362 ) = 'SHOW COLUMN @name ...'
      TEXT( 2363 ) = '|endliteral'
      TEXT( 2364 ) = 'The word, @name, indicates that the nex'
     .//             't word in the command following'
      TEXT( 2365 ) = 'COLUMN should begin with a letter follo'
     .//             'wed by characters from the'
      TEXT( 2366 ) = 'collection:  upper and lower case lette'
     .//             'rs, digits, the hyphen and the'
      TEXT( 2367 ) = 'underscore.  Given this rule, a command'
     .//             ' that begins'
      TEXT( 2368 ) = ' '
      TEXT( 2369 ) = '@literal'
      TEXT( 2370 ) = 'SHOW COLUMN 13 ...'
      TEXT( 2371 ) = '|endliteral'
      TEXT( 2372 ) = ' '
      TEXT( 2373 ) = 'is clearly in error.'
      TEXT( 2374 ) = ' '
      TEXT( 2375 ) = 'Class templates are very much like the '
     .//             'wild card templates used in'
      TEXT( 2376 ) = 'operating systems like Unix and VMS, ex'
     .//             'cept they are a little more'
      TEXT( 2377 ) = 'specialized.  The class templates recog'
     .//             'nized by Meta/2 are listed below'
      TEXT( 2378 ) = 'along with descriptions of the words th'
     .//             'at match them.  Class templates'
      TEXT( 2379 ) = 'in a Meta/2 description begin with the '
     .//             '"at" character (@).'
      TEXT( 2380 ) = ' '
      TEXT( 2381 ) = '@subsection number'
      TEXT( 2382 ) = ' '
      TEXT( 2383 ) = 'This template matches any number.  Exam'
     .//             'ples are'
      TEXT( 2384 ) = '@literal'
      TEXT( 2385 ) = '1'
      TEXT( 2386 ) = '3.14'
      TEXT( 2387 ) = '0.07281D-10'
      TEXT( 2388 ) = '|endliteral'
      TEXT( 2389 ) = ' '
      TEXT( 2390 ) = '@subsection int'
      TEXT( 2391 ) = ' '
      TEXT( 2392 ) = 'This template matches only integer valu'
     .//             'es.  Examples are'
      TEXT( 2393 ) = '@literal'
      TEXT( 2394 ) = '-3'
      TEXT( 2395 ) = '26172771'
      TEXT( 2396 ) = '0.24E6'
      TEXT( 2397 ) = '|endliteral'
      TEXT( 2398 ) = ' '
      TEXT( 2399 ) = '@subsection word'
      TEXT( 2400 ) = ' '
      TEXT( 2401 ) = 'This template matches any string of con'
     .//             'tiguous, non-blank, printing'
      TEXT( 2402 ) = 'characters.  Examples are'
 
      RETURN
      END
