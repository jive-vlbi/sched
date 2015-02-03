C$Procedure      ZZHLP027 ( private help text )
 
      SUBROUTINE ZZHLP027 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 2503 ) = ' '
      TEXT( 2504 ) = '(1:3)@name(John|Bobby|Teddy)'
      TEXT( 2505 ) = '!endliteral'
      TEXT( 2506 ) = ' '
      TEXT( 2507 ) = '@section Switches'
      TEXT( 2508 ) = ' '
      TEXT( 2509 ) = ' '
      TEXT( 2510 ) = 'The construct'
      TEXT( 2511 ) = '@exliteral'
      TEXT( 2512 ) = '    (1:1){ NONE'
      TEXT( 2513 ) = '         | FIRST'
      TEXT( 2514 ) = '         | 1ST'
      TEXT( 2515 ) = '         | ALL'
      TEXT( 2516 ) = '         | EVERY @int(2:) }'
      TEXT( 2517 ) = '!endliteral'
      TEXT( 2518 ) = 'is called a switch.  It is the final co'
     .//             'nstruct that you need to know.'
      TEXT( 2519 ) = 'Although it looks forbidding, it is rea'
     .//             'lly quite simple.  A switch is'
      TEXT( 2520 ) = 'a list of keyword-template phrases, sep'
     .//             'arated by vertical bars, and'
      TEXT( 2521 ) = 'surrounded by braces.  The left brace i'
     .//             's prefixed with a quantifier,'
      TEXT( 2522 ) = '@literal'
      TEXT( 2523 ) = '   (n:m){ ... }'
      TEXT( 2524 ) = '|endliteral'
      TEXT( 2525 ) = 'Whenever you see a switch,  it means th'
     .//             'at at least n and not more than'
      TEXT( 2526 ) = 'm of the phrases separated by vertical '
     .//             'bars must appear;  however, they'
      TEXT( 2527 ) = 'may appear in any order.  Thus, the syn'
     .//             'tax'
      TEXT( 2528 ) = '@exliteral'
      TEXT( 2529 ) = 'SET TITLE FREQUENCY (1:1){ NONE'
      TEXT( 2530 ) = '                         | FIRST'
      TEXT( 2531 ) = '                         | 1ST'
      TEXT( 2532 ) = '                         | ALL'
      TEXT( 2533 ) = '                         | EVERY @int(2'
     .//             ':) }'
      TEXT( 2534 ) = '!endliteral'
      TEXT( 2535 ) = 'matches all of the following commands.'
      TEXT( 2536 ) = '@literal'
      TEXT( 2537 ) = 'SET TITLE FREQUENCY FIRST'
      TEXT( 2538 ) = ' '
      TEXT( 2539 ) = 'SET TITLE FREQUENCY EVERY 3'
      TEXT( 2540 ) = ' '
      TEXT( 2541 ) = 'SET TITLE FREQUENCY ALL'
      TEXT( 2542 ) = '|endliteral'
      TEXT( 2543 ) = 'but does not match'
      TEXT( 2544 ) = '@literal'
      TEXT( 2545 ) = 'SET TITLE FREQUENCY NONE FIRST ALL EVER'
     .//             'Y @int(2:)'
      TEXT( 2546 ) = '|endliteral'
      TEXT( 2547 ) = ' '
      TEXT( 2548 ) = 'When you see the special word'
      TEXT( 2549 ) = '@literal'
      TEXT( 2550 ) = '@options'
      TEXT( 2551 ) = '|endliteral'
      TEXT( 2552 ) = 'within a switch, it means that the phra'
     .//             'ses following the token are'
      TEXT( 2553 ) = 'optional, whereas the phrases preceding'
     .//             ' the token are required (again,'
      TEXT( 2554 ) = 'the phrases may appear in any order).  '
     .//             'For example the construct'
      TEXT( 2555 ) = '@exliteral'
      TEXT( 2556 ) = '   (2:3){         WIDTH  @int(40:132)'
      TEXT( 2557 ) = '        |         HEIGHT @int(22:)'
      TEXT( 2558 ) = '        | @options'
      TEXT( 2559 ) = '        |         TITLE (1:3)@word }'
      TEXT( 2560 ) = '!endliteral'
      TEXT( 2561 ) = 'means that the phrases beginning with t'
     .//             'he keywords WIDTH and HEIGHT must'
      TEXT( 2562 ) = 'appear, while the phrase beginning with'
     .//             ' TITLE is optional.'
      TEXT( 2563 ) = ' '
      TEXT( 2564 ) = '@subsection Nesting Switches'
      TEXT( 2565 ) = ' '
      TEXT( 2566 ) = 'Switches cannot be nested.  The constru'
     .//             'ct'
      TEXT( 2567 ) = '@exliteral'
      TEXT( 2568 ) = '   (a:b){ ...'
      TEXT( 2569 ) = '        | (c:d){ ...  }'
      TEXT( 2570 ) = '        }'
      TEXT( 2571 ) = '!endliteral'
      TEXT( 2572 ) = 'is illegal.'
      TEXT( 2573 ) = ' '
      TEXT( 2574 ) = '@section Examples'
      TEXT( 2575 ) = ' '
      TEXT( 2576 ) = 'Given the syntax description'
      TEXT( 2577 ) = '@exliteral'
      TEXT( 2578 ) = '   SET FORMAT (0:1){ SPACED  | MARKED }'
     .//             ' TABULAR'
      TEXT( 2579 ) = '              (0:1){ PRESERVED }'
      TEXT( 2580 ) = '!endliteral'
      TEXT( 2581 ) = 'Convince yourself that the following ar'
     .//             'e all valid  commands.'
      TEXT( 2582 ) = '@literal'
      TEXT( 2583 ) = 'SET FORMAT TABULAR;'
      TEXT( 2584 ) = ' '
      TEXT( 2585 ) = 'SET FORMAT SPACED TABULAR;'
      TEXT( 2586 ) = ' '
      TEXT( 2587 ) = 'SET FORMAT MARKED TABULAR PRESERVED;'
      TEXT( 2588 ) = ' '
      TEXT( 2589 ) = 'SET FORMAT TABULAR PRESERVED;'
      TEXT( 2590 ) = ' '
      TEXT( 2591 ) = 'SET FORMAT SPACE TABULAR PRESERVED;'
      TEXT( 2592 ) = '|endliteral'
      TEXT( 2593 ) = ' '
      TEXT( 2594 ) = 'Convince yourself that the following ar'
     .//             'e not valid commands.'
      TEXT( 2595 ) = '@literal'
      TEXT( 2596 ) = 'SET FORMAT SPACED;'
      TEXT( 2597 ) = ' '
      TEXT( 2598 ) = 'SET FORMAT PRESERVED TABULAR;'
      TEXT( 2599 ) = ' '
      TEXT( 2600 ) = 'SET FORMAT MARKED PRESERVED;'
      TEXT( 2601 ) = '|endliteral'
      TEXT( 2602 ) = '@@Syntax Description Language'
 
      RETURN
      END
