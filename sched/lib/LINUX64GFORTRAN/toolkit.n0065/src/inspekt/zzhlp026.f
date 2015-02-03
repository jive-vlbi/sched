C$Procedure      ZZHLP026 ( private help text )
 
      SUBROUTINE ZZHLP026 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 2403 ) = '@literal'
      TEXT( 2404 ) = 'alpine'
      TEXT( 2405 ) = '/u/user/naif/etc/data/spam'
      TEXT( 2406 ) = '^&HANNk228***(JASNSK'
      TEXT( 2407 ) = '|endliteral'
      TEXT( 2408 ) = ' '
      TEXT( 2409 ) = '@subsection name'
      TEXT( 2410 ) = ' '
      TEXT( 2411 ) = 'This template matches any word that beg'
     .//             'ins with a letter and contains'
      TEXT( 2412 ) = 'from one  to 32 letters, numbers, under'
     .//             'scores, and hyphens.  Examples'
      TEXT( 2413 ) = 'are'
      TEXT( 2414 ) = '@literal'
      TEXT( 2415 ) = 'Andrea'
      TEXT( 2416 ) = 'BRORSEN-METCALF'
      TEXT( 2417 ) = 'COMMAND_STEM'
      TEXT( 2418 ) = 'X11J9'
      TEXT( 2419 ) = '|endliteral'
      TEXT( 2420 ) = ' '
      TEXT( 2421 ) = '@subsection calendar'
      TEXT( 2422 ) = ' '
      TEXT( 2423 ) = 'This template matches a sequence of wor'
     .//             'ds that make up a calendar date.'
      TEXT( 2424 ) = 'The rules for matching this template ar'
     .//             'e somewhat complicated:  for the'
      TEXT( 2425 ) = 'most part, any unambiguous format will '
     .//             'be accepted.  Examples are'
      TEXT( 2426 ) = '@literal'
      TEXT( 2427 ) = 'JAN 1, 1992 12:28:28'
      TEXT( 2428 ) = '1992-3-18 18:28'
      TEXT( 2429 ) = '5 APR 1993 18:00:00.289'
      TEXT( 2430 ) = '12-1-1995'
      TEXT( 2431 ) = '|endliteral'
      TEXT( 2432 ) = ' '
      TEXT( 2433 ) = '@subsection Template Quantifiers'
      TEXT( 2434 ) = ' '
      TEXT( 2435 ) = 'The construct'
      TEXT( 2436 ) = '@literal'
      TEXT( 2437 ) = 'FORMAT (1:8)@word'
      TEXT( 2438 ) = '|endliteral'
      TEXT( 2439 ) = ' '
      TEXT( 2440 ) = 'matches the keyword FORMAT followed by '
     .//             'between one and eight words.'
      TEXT( 2441 ) = 'The quantifier'
      TEXT( 2442 ) = '@literal'
      TEXT( 2443 ) = '(n:m)'
      TEXT( 2444 ) = '|endliteral'
      TEXT( 2445 ) = 'when prefixed to any class template has'
     .//             ' similar meaning.  The lower'
      TEXT( 2446 ) = 'bound is always present, and is always '
     .//             'positive.  The upper bound is'
      TEXT( 2447 ) = 'optional:  for example, template'
      TEXT( 2448 ) = '@literal'
      TEXT( 2449 ) = '(2:)@int'
      TEXT( 2450 ) = '|endliteral'
      TEXT( 2451 ) = ' '
      TEXT( 2452 ) = 'matches any sequence of two or more int'
     .//             'eger words.'
      TEXT( 2453 ) = ' '
      TEXT( 2454 ) = '@subsection Numeric Qualifiers'
      TEXT( 2455 ) = ' '
      TEXT( 2456 ) = 'The construct'
      TEXT( 2457 ) = '@literal'
      TEXT( 2458 ) = 'WIDTH @int(8:80)'
      TEXT( 2459 ) = '|endliteral'
      TEXT( 2460 ) = 'matches the keyword WIDTH followed by a'
     .//             'n integer between 8 and 80'
      TEXT( 2461 ) = 'inclusive.  The qualifier'
      TEXT( 2462 ) = '@literal'
      TEXT( 2463 ) = '(n:m)'
      TEXT( 2464 ) = '|endliteral'
      TEXT( 2465 ) = 'when suffixed to any numeric class temp'
     .//             'late has similar meaning.  Both'
      TEXT( 2466 ) = 'bounds are optional:  for example the t'
     .//             'emplates'
      TEXT( 2467 ) = '@literal'
      TEXT( 2468 ) = '@number(2:)'
      TEXT( 2469 ) = '@number(:1000)'
      TEXT( 2470 ) = '|endliteral'
      TEXT( 2471 ) = 'are matched by  numeric words whose val'
     .//             'ues are greater than or equal to'
      TEXT( 2472 ) = 'two and less than or equal to 1000 resp'
     .//             'ectively.'
      TEXT( 2473 ) = ' '
      TEXT( 2474 ) = '@subsection Character Qualifiers'
      TEXT( 2475 ) = ' '
      TEXT( 2476 ) = 'The construct'
      TEXT( 2477 ) = '@literal'
      TEXT( 2478 ) = 'DIRECTORY @word([*]|*%:[*]|*:)'
      TEXT( 2479 ) = '|endliteral'
      TEXT( 2480 ) = 'matches any word that matches one of th'
     .//             'e individual wildcard templates'
      TEXT( 2481 ) = '@literal'
      TEXT( 2482 ) = '[*]'
      TEXT( 2483 ) = ' '
      TEXT( 2484 ) = '*%:[*]'
      TEXT( 2485 ) = ' '
      TEXT( 2486 ) = '*:'
      TEXT( 2487 ) = '|endliteral'
      TEXT( 2488 ) = 'The wildcard characters (asterisk and p'
     .//             'ercent sign)  match any substring'
      TEXT( 2489 ) = 'and any character respectively.  The qu'
     .//             'alifier'
      TEXT( 2490 ) = '@exliteral'
      TEXT( 2491 ) = '(t1|...|tn)'
      TEXT( 2492 ) = '!endliteral'
      TEXT( 2493 ) = 'when suffixed to any character class te'
     .//             'mplate has a similar meaning.'
      TEXT( 2494 ) = ' '
      TEXT( 2495 ) = '@section Combining Quantifiers with Qua'
     .//             'lifiers'
      TEXT( 2496 ) = ' '
      TEXT( 2497 ) = 'Quantifiers and qualifiers may be combi'
     .//             'ned in any combination.  The'
      TEXT( 2498 ) = 'following are all valid class templates'
     .//             '.'
      TEXT( 2499 ) = '@exliteral'
      TEXT( 2500 ) = '@int(-5:5)'
      TEXT( 2501 ) = ' '
      TEXT( 2502 ) = '(3:)@name(A*|B*|*X)'
 
      RETURN
      END
