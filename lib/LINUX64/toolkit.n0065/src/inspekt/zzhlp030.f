C$Procedure      ZZHLP030 ( private help text )
 
      SUBROUTINE ZZHLP030 ( BEGIN, FINISH, TEXT )
 
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
      FINISH( 70 ) = 2790
 
      BEGIN ( 71 ) = 2791
      TEXT( 2791 ) = 'You may request time to be displayed in'
     .//             ' an almost limitless variety of formats'
     .//             '.'
      TEXT( 2792 ) = 'The default format is UTC calendar form'
     .//             'at. Other standard formats may be  set '
     .//             'by'
      TEXT( 2793 ) = 'using either of the commands:'
      TEXT( 2794 ) = '@literal'
      TEXT( 2795 ) = 'SET COLUMN column_name FORMAT format;'
      TEXT( 2796 ) = 'SET DEFAULT TIME FORMAT format;'
      TEXT( 2797 ) = '|endliteral'
      TEXT( 2798 ) = 'A number of standard formats may be spe'
     .//             'cified:'
      TEXT( 2799 ) = ' '
      TEXT( 2800 ) = '@setparamsize{GLLSCLK}'
      TEXT( 2801 ) = ' '
      TEXT( 2802 ) = '@param  UTC.'
      TEXT( 2803 ) = 'Default format: YYYY-MON-DD HR:MN:SC'
      TEXT( 2804 ) = ' '
      TEXT( 2805 ) = '@param ISO.'
      TEXT( 2806 ) = 'International Standards format: YYYY-MM'
     .//             '-DDTHR:MN:SC'
      TEXT( 2807 ) = ' '
      TEXT( 2808 ) = '@param ISODOY.'
      TEXT( 2809 ) = 'International Standards day of year: YY'
     .//             'YY-DOYTHR:MN:SC'
      TEXT( 2810 ) = ' '
      TEXT( 2811 ) = '@param  JED.'
      TEXT( 2812 ) = 'Julian Ephemeris date to 5 decimal plac'
     .//             'es'
      TEXT( 2813 ) = ' '
      TEXT( 2814 ) = '@param MOSCLK.'
      TEXT( 2815 ) = 'Mars Observer Spacecraft Clock format'
      TEXT( 2816 ) = ' '
      TEXT( 2817 ) = '@param GLLSCLK.'
      TEXT( 2818 ) = 'Galileo Spacecraft Clock format'
      TEXT( 2819 ) = ' '
      TEXT( 2820 ) = 'These formats will be recognized regard'
     .//             'less of the case of the letters use to'
      TEXT( 2821 ) = 'specify them.  The UTC, ISO and ISODY f'
     .//             'ormats are all UTC times.  You may also'
      TEXT( 2822 ) = 'create a custom format (see Custom Form'
     .//             'ats).'
      TEXT( 2823 ) = '@@Time Formats'
      TEXT( 2824 ) = 'Quit Help'
      TEXT( 2825 ) = 'Help'
      TEXT( 2826 ) = 'Custom Formats'
      TEXT( 2827 ) = 'Default Time Format'
      FINISH( 71 ) = 2827
 
      BEGIN ( 72 ) = 2828
      TEXT( 2828 ) = 'Titles are single lines of text that ap'
     .//             'pear at the beginning'
      TEXT( 2829 ) = 'of Inspekt reports.  You may adjust the'
     .//             ' text of the title,'
      TEXT( 2830 ) = 'its positioning (left justified, center'
     .//             'ed, or right justified), and'
      TEXT( 2831 ) = 'how often it is shown as a report is di'
     .//             'splayed.  To see the current'
      TEXT( 2832 ) = 'attributes of the report title, type'
      TEXT( 2833 ) = '@literal'
      TEXT( 2834 ) = 'SHOW PAGE'
      TEXT( 2835 ) = '|endliteral'
      TEXT( 2836 ) = ' '
      TEXT( 2837 ) = '@@Titles'
      TEXT( 2838 ) = 'Quit Help'
      TEXT( 2839 ) = 'Help'
      FINISH( 72 ) = 2839
 
      BEGIN ( 73 ) = 2840
      TEXT( 2840 ) = 'Inspekt is a command driven program.  Y'
     .//             'ou type a command at the'
      TEXT( 2841 ) = 'prompt'
      TEXT( 2842 ) = '@literal'
      TEXT( 2843 ) = 'Inspekt>'
      TEXT( 2844 ) = '|endliteral'
      TEXT( 2845 ) = 'The program performs some action based '
     .//             'upon the typed command.  If'
      TEXT( 2846 ) = 'a command is too long to fit on a singl'
     .//             'e line, hit the return key'
      TEXT( 2847 ) = 'when you get to the end of the line and'
     .//             ' continue typing on the next'
      TEXT( 2848 ) = 'line. When you are finished typing the '
     .//             'command, type a semi-colon'
      TEXT( 2849 ) = '(";").  The semi-colon is required at t'
     .//             'he end of all commands typed'
      TEXT( 2850 ) = 'in response to a prompt ending with ">"'
     .//             '.  It is needed even if the'
      TEXT( 2851 ) = 'line will fit on a single line. Occasio'
     .//             'nally, Inspekt may prompt'
      TEXT( 2852 ) = 'you to supply inputs other than command'
     .//             's (such as in the Help'
      TEXT( 2853 ) = 'system).  In such cases the prompt will'
     .//             ' not end in a greater than'
      TEXT( 2854 ) = 'sign ">" and the semi-colon is not need'
     .//             'ed.'
      TEXT( 2855 ) = ' '
      TEXT( 2856 ) = 'The commands you type may be in either '
     .//             'upper or lower case.'
      TEXT( 2857 ) = ' '
      TEXT( 2858 ) = 'If you begin typing a command and reali'
     .//             'ze you''ve made an error or'
      TEXT( 2859 ) = 'wish to start again, add a blank line t'
     .//             'o the command entered so'
      TEXT( 2860 ) = 'far. Inspekt will ignore what you''ve t'
     .//             'yped and prompt you for a'
      TEXT( 2861 ) = 'new command.'
      TEXT( 2862 ) = '@@Typing Commands'
      TEXT( 2863 ) = 'Quit Help'
      TEXT( 2864 ) = 'Help'
      TEXT( 2865 ) = 'Editing Commands'
      TEXT( 2866 ) = 'Column and Table Abbreviations'
      TEXT( 2867 ) = 'Using Symbols'
      TEXT( 2868 ) = 'Special Symbols --- Queries'
      TEXT( 2869 ) = 'Collecting Commands In Files'
      TEXT( 2870 ) = ' '
      FINISH( 73 ) = 2870
 
      BEGIN ( 74 ) = 2871
      TEXT( 2871 ) = '@subsection An Example'
      TEXT( 2872 ) = ' '
      TEXT( 2873 ) = 'Inspekt allows you to create special wo'
     .//             'rds that when'
      TEXT( 2874 ) = 'encountered in a command are translated'
     .//             ' into different words.'
      TEXT( 2875 ) = 'These special words are called ''symbol'
     .//             's''.'
      TEXT( 2876 ) = ' '
      TEXT( 2877 ) = 'For example suppose that you frequently'
     .//             ' want to edit your last'
      TEXT( 2878 ) = '"Select" command.  You could make up th'
     .//             'e symbol ES as shown below:'
 
      RETURN
      END
