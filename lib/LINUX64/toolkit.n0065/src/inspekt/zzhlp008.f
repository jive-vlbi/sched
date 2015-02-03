C$Procedure      ZZHLP008 ( private help text )
 
      SUBROUTINE ZZHLP008 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 768 ) = 'the format used when presenting its valu'
     .//            'es in a report will'
      TEXT( 769 ) = 'be the default floating format.'
      TEXT( 770 ) = ' '
      TEXT( 771 ) = '@@Default Floating Format'
      TEXT( 772 ) = 'Quit Help'
      TEXT( 773 ) = 'Help'
      TEXT( 774 ) = 'Numeric Formats'
      TEXT( 775 ) = 'Default Integer Format'
      TEXT( 776 ) = 'Default Time Format'
      TEXT( 777 ) = 'SHOW FORMAT   ...'
      TEXT( 778 ) = ' '
      FINISH( 11 ) = 778
 
      BEGIN ( 12 ) = 779
      TEXT( 779 ) = 'When printing an integer in a report, In'
     .//            'spekt first examines'
      TEXT( 780 ) = 'the column attributes to determine if yo'
     .//            'u have specified'
      TEXT( 781 ) = 'a particular format for that column.  If'
     .//            ' you have that format'
      TEXT( 782 ) = 'is used to create the text that is prese'
     .//            'nted in the report.'
      TEXT( 783 ) = 'If you have not specified a particular f'
     .//            'ormat, Inspekt looks'
      TEXT( 784 ) = 'up the "default integer format" and uses'
     .//            ' this to create text'
      TEXT( 785 ) = 'to be used in the report.  You may adjus'
     .//            't the default integer'
      TEXT( 786 ) = 'format.  To do this issue the command'
      TEXT( 787 ) = ' '
      TEXT( 788 ) = '@literal'
      TEXT( 789 ) = 'SET DEFAULT INTEGER FORMAT format;'
      TEXT( 790 ) = '|endliteral'
      TEXT( 791 ) = ' '
      TEXT( 792 ) = 'where "format" is the format you''d like'
     .//            ' Inspekt to use'
      TEXT( 793 ) = 'when you have not specified a particular'
     .//            ' format for an integer'
      TEXT( 794 ) = 'column.'
      TEXT( 795 ) = ' '
      TEXT( 796 ) = 'If you''ve specified a format for an int'
     .//            'eger column, and would'
      TEXT( 797 ) = 'like to return to using the default inte'
     .//            'ger format issue the'
      TEXT( 798 ) = 'command'
      TEXT( 799 ) = '@literal'
      TEXT( 800 ) = 'SET COLUMN column_name FORMAT DEFAULT;'
      TEXT( 801 ) = '|endliteral'
      TEXT( 802 ) = 'Until you change the format for the spec'
     .//            'ified column again,'
      TEXT( 803 ) = 'the format used when presenting its valu'
     .//            'es in a report will'
      TEXT( 804 ) = 'be the default integer format.'
      TEXT( 805 ) = ' '
      TEXT( 806 ) = '@@Default Integer Format'
      TEXT( 807 ) = 'Quit Help'
      TEXT( 808 ) = 'Help'
      TEXT( 809 ) = 'Numeric Formats'
      TEXT( 810 ) = 'Default Floating Format'
      TEXT( 811 ) = 'Default Time Format'
      TEXT( 812 ) = 'SHOW FORMAT   ...'
      FINISH( 12 ) = 812
 
      BEGIN ( 13 ) = 813
      TEXT( 813 ) = 'When printing a time in a report, Inspek'
     .//            't first examines'
      TEXT( 814 ) = 'the column attributes to determine if yo'
     .//            'u have specified'
      TEXT( 815 ) = 'a particular format for that column.  If'
     .//            ' you have that format'
      TEXT( 816 ) = 'is used to create the text that is prese'
     .//            'nted in the report.'
      TEXT( 817 ) = 'If you have not specified a particular f'
     .//            'ormat, Inspekt looks'
      TEXT( 818 ) = 'up the "default time format" and uses th'
     .//            'is to create text'
      TEXT( 819 ) = 'to be used in the report.  You may adjus'
     .//            't the default time'
      TEXT( 820 ) = 'format.  To do this issue the command'
      TEXT( 821 ) = ' '
      TEXT( 822 ) = '@literal'
      TEXT( 823 ) = 'SET DEFAULT TIME FORMAT format;'
      TEXT( 824 ) = '|endliteral'
      TEXT( 825 ) = ' '
      TEXT( 826 ) = 'where "format" is the format you''d like'
     .//            ' Inspekt to use'
      TEXT( 827 ) = 'when you have not specified a particular'
     .//            ' format for a column.'
      TEXT( 828 ) = ' '
      TEXT( 829 ) = 'If you''ve specified a format for a time'
     .//            ' column, and would'
      TEXT( 830 ) = 'like to return to using the default time'
     .//            ' format issue the'
      TEXT( 831 ) = 'command'
      TEXT( 832 ) = '@literal'
      TEXT( 833 ) = 'SET COLUMN column_name FORMAT DEFAULT;'
      TEXT( 834 ) = '|endliteral'
      TEXT( 835 ) = 'Until you change the format for the spec'
     .//            'ified column again,'
      TEXT( 836 ) = 'the format used when presenting its valu'
     .//            'es in a report will'
      TEXT( 837 ) = 'be the default time format.'
      TEXT( 838 ) = ' '
      TEXT( 839 ) = '@@Default Time Format'
      TEXT( 840 ) = 'Quit Help'
      TEXT( 841 ) = 'Help'
      TEXT( 842 ) = 'Custom Formats'
      TEXT( 843 ) = 'Default Floating Format'
      TEXT( 844 ) = 'Default Integer Format'
      TEXT( 845 ) = 'SHOW FORMAT   ...'
      FINISH( 13 ) = 845
 
      BEGIN ( 14 ) = 846
      TEXT( 846 ) = 'The Delimited formats provide a mechanis'
     .//            'm for producing a report that'
      TEXT( 847 ) = 'is suitable for import into Microsoft Ex'
     .//            'cel or other spreadsheet'
      TEXT( 848 ) = 'programs.'
      TEXT( 849 ) = ' '
      TEXT( 850 ) = 'When the delimited format is used, the r'
     .//            'esults of queries are written'
      TEXT( 851 ) = 'out without formatting. The values for t'
     .//            'he columns are quoted if they'
      TEXT( 852 ) = 'are not numeric and the values are separ'
     .//            'ated by a tab character (or any'
      TEXT( 853 ) = 'other character you choose other than '''
     .//            '@'').'
      TEXT( 854 ) = ' '
      TEXT( 855 ) = 'The default character used to quote stri'
     .//            'ngs is ''"''. You may choose any'
      TEXT( 856 ) = 'other character (except ''@'') as the qu'
     .//            'ote character when you set the'
      TEXT( 857 ) = 'format to DELIMITED.'
      TEXT( 858 ) = ' '
 
      RETURN
      END
