C$Procedure      ZZHLP029 ( private help text )
 
      SUBROUTINE ZZHLP029 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 2700 ) = 'SET TITLE FREQUENCY (1:1){ 0'
      TEXT( 2701 ) = '                         | FIRST | 1ST'
      TEXT( 2702 ) = '                         | ALL'
      TEXT( 2703 ) = '                         | EVERY @int(2'
     .//             ':) }'
      TEXT( 2704 ) = ' '
      TEXT( 2705 ) = 'SET TITLE JUSTIFICATION (1:1){ LEFT | C'
     .//             'ENTER | RIGHT }'
      TEXT( 2706 ) = ' '
      TEXT( 2707 ) = 'SHOW COLUMN @word'
      TEXT( 2708 ) = 'SHOW COMMENTS'
      TEXT( 2709 ) = 'SHOW COMMENTS @word'
      TEXT( 2710 ) = 'SHOW ENVIRONMENT'
      TEXT( 2711 ) = 'SHOW FORMAT'
      TEXT( 2712 ) = 'SHOW INDEXES'
      TEXT( 2713 ) = 'SHOW KERNELS'
      TEXT( 2714 ) = 'SHOW PAGE'
      TEXT( 2715 ) = 'SHOW SYMBOL @word'
      TEXT( 2716 ) = 'SHOW SUMMARY'
      TEXT( 2717 ) = ' '
      TEXT( 2718 ) = 'START @word'
      TEXT( 2719 ) = ' '
      TEXT( 2720 ) = 'UNLOAD @word'
      TEXT( 2721 ) = '!endliteral'
      TEXT( 2722 ) = ' '
      TEXT( 2723 ) = '@@Syntax Summaries'
      TEXT( 2724 ) = 'Quit Help'
      TEXT( 2725 ) = 'Help'
      TEXT( 2726 ) = 'Syntax Description Language'
      FINISH( 67 ) = 2726
 
      BEGIN ( 68 ) = 2727
      TEXT( 2727 ) = 'A "table" is a named grouping of events'
     .//             ' (or other objects) all of'
      TEXT( 2728 ) = 'which have the same named attributes.'
      TEXT( 2729 ) = ' '
      TEXT( 2730 ) = 'The designation "table" comes from the '
     .//             'appearance of listings of the'
      TEXT( 2731 ) = 'attributes of events.  If we list all o'
     .//             'f the events we have'
      TEXT( 2732 ) = 'recorded on a sheet of paper (or a term'
     .//             'inal) so that the attributes'
      TEXT( 2733 ) = 'for each event are always listed in the'
     .//             ' same order from left to right,'
      TEXT( 2734 ) = 'the resulting page will look like a "ta'
     .//             'ble" of values similar to'
      TEXT( 2735 ) = 'a table of sines and cosines or a table'
     .//             ' of prices.'
      TEXT( 2736 ) = '@@Table'
      TEXT( 2737 ) = 'Quit Help'
      TEXT( 2738 ) = 'Help'
      FINISH( 68 ) = 2738
 
      BEGIN ( 69 ) = 2739
      TEXT( 2739 ) = 'Inspekt provides a number of different '
     .//             'Tabular formats.'
      TEXT( 2740 ) = ' '
      TEXT( 2741 ) = '@subsection TABULAR'
      TEXT( 2742 ) = '@literal'
      TEXT( 2743 ) = 'JAN 25 07:15:00  OBSERVATION   WEATHER '
     .//             '  Sunny and cool (about 55 F)'
      TEXT( 2744 ) = '                                       '
     .//             '  Air Quality good.'
      TEXT( 2745 ) = 'JAN 26 07:15:00  OBSERVATION   WEATHER '
     .//             '  Sunny and cool (about 52 F)'
      TEXT( 2746 ) = '|endliteral'
      TEXT( 2747 ) = '@subsection MARKED TABULAR'
      TEXT( 2748 ) = '@literal'
      TEXT( 2749 ) = '> JAN 25 07:15:00  OBSERVATION WEATHER '
     .//             '  Sunny and cool (about 55 F)'
      TEXT( 2750 ) = '                                       '
     .//             '  Air Quality good.'
      TEXT( 2751 ) = '> JAN 26 07:15:00  OBSERVATION WEATHER '
     .//             '  Sunny and cool (about 52 F)'
      TEXT( 2752 ) = ' '
      TEXT( 2753 ) = '|endliteral'
      TEXT( 2754 ) = '@subsection SPACED TABULAR'
      TEXT( 2755 ) = '@literal'
      TEXT( 2756 ) = 'JAN 25 07:15:00  OBSERVATION   WEATHER '
     .//             '  Sunny and cool (about 55 F)'
      TEXT( 2757 ) = '                                       '
     .//             '  Air Quality good.'
      TEXT( 2758 ) = ' '
      TEXT( 2759 ) = 'AN 26 07:15:00  OBSERVATION    WEATHER '
     .//             '  Sunny and cool (about 52 F)'
      TEXT( 2760 ) = '|endliteral'
      TEXT( 2761 ) = '@@Tabular Format'
      TEXT( 2762 ) = 'Quit Help'
      TEXT( 2763 ) = 'Help'
      TEXT( 2764 ) = 'Reports'
      TEXT( 2765 ) = 'Tabular Format Mark'
      FINISH( 69 ) = 2765
 
      BEGIN ( 70 ) = 2766
      TEXT( 2766 ) = 'The format of a marked tabular report i'
     .//             's similar to that of a'
      TEXT( 2767 ) = 'tabular report.  However, an extra colu'
     .//             'mn is inserted at the left'
      TEXT( 2768 ) = 'side of the report.  A format mark appe'
     .//             'ars'
      TEXT( 2769 ) = 'in this column when each new row is beg'
     .//             'un.'
      TEXT( 2770 ) = 'You can quickly scan the left most colu'
     .//             'mn of the'
      TEXT( 2771 ) = 'report and determine where each new row'
     .//             ' begins.   The default format'
      TEXT( 2772 ) = 'mark is the "greater than" symbol (>). '
     .//             ' To change this default to'
      TEXT( 2773 ) = 'some other character, use the SET FORMA'
     .//             'T MARK command.'
      TEXT( 2774 ) = '@literal'
      TEXT( 2775 ) = 'SET FORMAT MARK character'
      TEXT( 2776 ) = '|endliteral'
      TEXT( 2777 ) = 'where "character" is the character you '
     .//             'would like to replace ''>''.'
      TEXT( 2778 ) = ' '
      TEXT( 2779 ) = 'NOTE: the format character cannot be a '
     .//             'semi-colon.'
      TEXT( 2780 ) = ' '
      TEXT( 2781 ) = 'For example suppose that you would like'
     .//             ' to change the format mark'
      TEXT( 2782 ) = 'from ''>'' to ''=''.  Type the command'
      TEXT( 2783 ) = '@literal'
      TEXT( 2784 ) = 'SET FORMAT MARK =;'
      TEXT( 2785 ) = '|endliteral'
      TEXT( 2786 ) = ' '
      TEXT( 2787 ) = '@@Tabular Format Mark'
      TEXT( 2788 ) = 'Quit Help'
      TEXT( 2789 ) = 'Help'
      TEXT( 2790 ) = 'Tabular Format'
 
      RETURN
      END
