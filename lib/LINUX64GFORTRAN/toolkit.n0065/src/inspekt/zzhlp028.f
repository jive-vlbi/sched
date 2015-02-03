C$Procedure      ZZHLP028 ( private help text )
 
      SUBROUTINE ZZHLP028 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 2603 ) = 'Quit Help'
      TEXT( 2604 ) = 'Help'
      FINISH( 66 ) = 2604
 
      BEGIN ( 67 ) = 2605
      TEXT( 2605 ) = 'Listed here is the formal syntax for ev'
     .//             'ery'
      TEXT( 2606 ) = 'command recognized by Inspekt.'
      TEXT( 2607 ) = ' '
      TEXT( 2608 ) = '@exliteral'
      TEXT( 2609 ) = 'DEFINE   @name'
      TEXT( 2610 ) = 'DEFINE   @name (1:)@word'
      TEXT( 2611 ) = 'UNDEFINE @name'
      TEXT( 2612 ) = ' '
      TEXT( 2613 ) = 'DISCARD'
      TEXT( 2614 ) = ' '
      TEXT( 2615 ) = 'DO @word'
      TEXT( 2616 ) = ' '
      TEXT( 2617 ) = 'ECHO'
      TEXT( 2618 ) = 'NO ECHO'
      TEXT( 2619 ) = ' '
      TEXT( 2620 ) = 'EDIT @word'
      TEXT( 2621 ) = ' '
      TEXT( 2622 ) = 'EXIT'
      TEXT( 2623 ) = 'HELP'
      TEXT( 2624 ) = 'LOAD EK @word'
      TEXT( 2625 ) = 'LOAD LEAPSECONDS @word'
      TEXT( 2626 ) = 'LOAD SCLK KERNEL @word'
      TEXT( 2627 ) = ' '
      TEXT( 2628 ) = 'RECALL'
      TEXT( 2629 ) = 'RECALL @word'
      TEXT( 2630 ) = ' '
      TEXT( 2631 ) = 'SAVE TO @word'
      TEXT( 2632 ) = ' '
      TEXT( 2633 ) = 'SAMPLE @int(1:)  SELECT (1:100)@word'
      TEXT( 2634 ) = '                 FROM   (1:)@word'
      TEXT( 2635 ) = '                (0:1){ WHERE    (1:)@wo'
     .//             'rd    }'
      TEXT( 2636 ) = '                (0:1){ ORDER BY (1:100)'
     .//             '@word }'
      TEXT( 2637 ) = ' '
      TEXT( 2638 ) = ' '
      TEXT( 2639 ) = 'SAMPLE          (1:1){ FIRST @int(1:)'
      TEXT( 2640 ) = '                     | LAST  @int(1:)'
      TEXT( 2641 ) = '                     }'
      TEXT( 2642 ) = '                 SELECT (1:100)@word'
      TEXT( 2643 ) = '                 FROM   (1:)@word'
      TEXT( 2644 ) = '                (0:1){ WHERE    (1:)@wo'
     .//             'rd    }'
      TEXT( 2645 ) = '                (0:1){ ORDER BY (1:100)'
     .//             '@word }'
      TEXT( 2646 ) = ' '
      TEXT( 2647 ) = 'SAMPLE @int(1:) (1:1){ UP TO       @int'
     .//             '(0:100) EVERY @int(1:)'
      TEXT( 2648 ) = '                     | UP TO       @int'
     .//             '(0:100)'
      TEXT( 2649 ) = '                     | STARTING AT @int'
     .//             '(0:100) EVERY @int(1:)'
      TEXT( 2650 ) = '                     | STARTING AT @int'
     .//             '(0:100)'
      TEXT( 2651 ) = '                     | CENTER   AT @int'
     .//             '(0:100) EVERY @int(1:)'
      TEXT( 2652 ) = '                     | CENTER   AT @int'
     .//             '(0:100)'
      TEXT( 2653 ) = '                     | FROM @int(0:100)'
     .//             ' TO   @int(0:100)'
      TEXT( 2654 ) = '                     }'
      TEXT( 2655 ) = '                 SELECT (1:100)@word'
      TEXT( 2656 ) = '                 FROM   (1:)@word'
      TEXT( 2657 ) = '                (0:1){ WHERE    (1:)@wo'
     .//             'rd    }'
      TEXT( 2658 ) = '                (0:1){ ORDER BY (1:100)'
     .//             '@word }'
      TEXT( 2659 ) = ' '
      TEXT( 2660 ) = 'SELECT (1:100)@word FROM (1:)@word'
      TEXT( 2661 ) = '                    (0:1){ WHERE    (1:'
     .//             ')@word    }'
      TEXT( 2662 ) = '                    (0:1){ ORDER BY (1:'
     .//             '100)@word }'
      TEXT( 2663 ) = ' '
      TEXT( 2664 ) = 'SET AUTOADJUST (1:1){ OFF | ASK | ON }'
      TEXT( 2665 ) = ' '
      TEXT( 2666 ) = 'SET COLUMN @word FORMAT (1:)@word }'
      TEXT( 2667 ) = 'SET COLUMN @word HEADING (1:)@word'
      TEXT( 2668 ) = 'SET COLUMN @word JUSTIFICATION (1:1){ L'
     .//             'EFT  | JUSTIFICATION }'
      TEXT( 2669 ) = 'SET COLUMN @word WIDTH @int'
      TEXT( 2670 ) = ' '
      TEXT( 2671 ) = 'SET DEFAULT TIME     FORMAT (1:)@word'
      TEXT( 2672 ) = 'SET DEFAULT INTEGER  FORMAT @word'
      TEXT( 2673 ) = 'SET DEFAULT FLOATING FORMAT @word'
      TEXT( 2674 ) = ' '
      TEXT( 2675 ) = 'SET DELUGE WARNING @int(1:)'
      TEXT( 2676 ) = ' '
      TEXT( 2677 ) = 'SET EDITOR (1:)@word'
      TEXT( 2678 ) = ' '
      TEXT( 2679 ) = 'SET FORMAT (0:1){ SPACED | MARKED } TAB'
     .//             'ULAR (0:1){ PRESERVED }'
      TEXT( 2680 ) = 'SET FORMAT FLAGGED (0:1){ PRESERVED }'
      TEXT( 2681 ) = 'SET FORMAT MARK @word(%)'
      TEXT( 2682 ) = 'SET FORMAT VERBATIM'
      TEXT( 2683 ) = ' '
      TEXT( 2684 ) = 'SET FORMAT DELIMITED (0:1){ PRESERVED }'
      TEXT( 2685 ) = '                     (0:2){ DELIMITER @'
     .//             'word(SPACE|%)'
      TEXT( 2686 ) = '                          | QUOTE @word'
     .//             '(%) }'
      TEXT( 2687 ) = ' '
      TEXT( 2688 ) = 'SET HEADER FREQUENCY (1:1){ 0'
      TEXT( 2689 ) = '                          | FIRST | 1ST'
      TEXT( 2690 ) = '                          | ALL'
      TEXT( 2691 ) = '                          | EVERY @int('
     .//             '2:) }'
      TEXT( 2692 ) = ' '
      TEXT( 2693 ) = 'SET HELP WAIT'
      TEXT( 2694 ) = 'SET HELP NO WAIT'
      TEXT( 2695 ) = ' '
      TEXT( 2696 ) = 'SET PAGE HEIGHT @int'
      TEXT( 2697 ) = 'SET PAGE TITLE (1:)@word'
      TEXT( 2698 ) = 'SET PAGE WIDTH @int'
      TEXT( 2699 ) = ' '
 
      RETURN
      END
