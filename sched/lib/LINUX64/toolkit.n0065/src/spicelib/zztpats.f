C$Procedure ZZTPATS (Private, Time --- Time Patterns)

      LOGICAL FUNCTION ZZTPATS ( ROOM, NKNOWN, KNOWN, MEANNG )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize the built-in time patterns.
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
C      None.
C
C$ Keywords
C
C      PRIVATE
C
C$ Declarations

      IMPLICIT NONE
      INTEGER               ROOM
      INTEGER               NKNOWN
      CHARACTER*(*)         KNOWN  ( * )
      CHARACTER*(*)         MEANNG ( * )

      INTEGER               COUNT
      PARAMETER           ( COUNT = 203 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ROOM       I   The declared space available for patterns
C     KNOWN      O   The patterns that are automatically recognized
C     MEANNG     O   The meaning associated with the patterns.
C     COUNT      P   The number of patterns built in to this routine.
C     The function returns .TRUE. if the initialization is successful.
C
C$ Detailed_Input
C
C     ROOM       an integer giving the room available for known patterns
C                and their meanings.
C
C                If ROOM does not equal the number of built-in patterns
C                the function returns only those patterns that will fit
C                and returns the value FALSE.
C
C$ Detailed_Output
C
C     NKNOWN     is the number of patterns/meanings returned in the
C                arrays KNOWN and MEANNG
C
C     KNOWN      is the array of automatically recognized calendar
C                date patterns.  KNOWN will be sorted according to
C                the FORTRAN collating sequence.
C
C     MEANNG     is the array of "meanings" associated with the built-in
C                patterns returned in the array KNOWN. MEANNG(I) is
C                the "meaning" associated with known pattern KNOWN(I).
C
C     The function returns TRUE if the arrays, KNOWN and MEANNG are
C     successfully initialized.  Otherwise it returns FALSE.
C
C$ Parameters
C
C     COUNT      is the number of patterns/meanings that are
C                returned by this routine.
C
C$ Exceptions
C
C     Error Free.
C
C     1) If ROOM is less than count, the function returns FALSE.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a utility routine that supports the SPICE routine
C     TPARTV that parses time strings.  This routine initializes
C     the set of built-in time patterns for use by TPARTV
C
C$ Examples
C
C     See TPARTV
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 11-DEC-2013 (EDW)
C
C        Corrected typo which showed MEANNG(203) assigned
C        the value 'm*D*YH*M' rather than the correct assignment
C        to MYMNNG( 203 ). The assignment error often prevented
C        the Time subsystem from parsing time strings with
C        the format 'i-i-Yi:i'.
C
C        Corrected header section ordering to meet SPICE requirements.
C
C-    SPICELIB Version 3.0.0, 16-AUG-2002 (WLT)
C
C        The interface of the routine was changed from
C
C           ZZTPATS( ROOM, KNOWN, MEANNG )
C
C        to
C
C           ZZTPATS( ROOM, NKNOWN, KNOWN, MEANNG )
C
C        and made error free.
C
C-    SPICELIB Version 2.0.0, 16-APR-1997 (WLT)
C
C        The collection of recognized built in patterns was
C        increased from 185 to 203 patterns.  The new patterns
C        begin at KNOWN(186) below.
C
C-    SPICELIB Version 1.0.0, 02-APR-1996 (WLT)
C
C-&

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               I
      INTEGER               ORDVEC ( COUNT )
      CHARACTER*(WDSIZE)    MYKNWN ( COUNT )
      CHARACTER*(WDSIZE)    MYMNNG ( COUNT )

      MYKNWN(   1 ) = 'Y-i-it'
      MYMNNG(   1 ) = 'Y*m*D*'


      MYKNWN(   2 ) = 'Y-i-iti:i'
      MYMNNG(   2 ) = 'Y*m*D*H*M'


      MYKNWN(   3 ) = 'Y-i-iti:i:i'
      MYMNNG(   3 ) = 'Y*m*D*H*M*S'


      MYKNWN(   4 ) = 'Y-i-iti:i:n'
      MYMNNG(   4 ) = 'Y*m*D*H*M*S'


      MYKNWN(   5 ) = 'Y-i-iti:n'
      MYMNNG(   5 ) = 'Y*m*D*H*M'


      MYKNWN(   6 ) = 'Y-i/'
      MYMNNG(   6 ) = 'Y*y*'


      MYKNWN(   7 ) = 'Y-i/i:i'
      MYMNNG(   7 ) = 'Y*y*H*M'


      MYKNWN(   8 ) = 'Y-i/i:i:i'
      MYMNNG(   8 ) = 'Y*y*H*M*S'


      MYKNWN(   9 ) = 'Y-i/i:i:n'
      MYMNNG(   9 ) = 'Y*y*H*M*S'


      MYKNWN(  10 ) = 'Y-i/i:n'
      MYMNNG(  10 ) = 'Y*y*H*M'


      MYKNWN(  11 ) = 'Y-id'
      MYMNNG(  11 ) = 'Y*y*'


      MYKNWN(  12 ) = 'Y-idi:i'
      MYMNNG(  12 ) = 'Y*y*H*M'


      MYKNWN(  13 ) = 'Y-idi:i:i'
      MYMNNG(  13 ) = 'Y*y*H*M*S'


      MYKNWN(  14 ) = 'Y-idi:i:n'
      MYMNNG(  14 ) = 'Y*y*H*M*S'


      MYKNWN(  15 ) = 'Y-idi:n'
      MYMNNG(  15 ) = 'Y*y*H*M'


      MYKNWN(  16 ) = 'Y-it'
      MYMNNG(  16 ) = 'Y*y*'


      MYKNWN(  17 ) = 'Y-iti:i'
      MYMNNG(  17 ) = 'Y*y*H*M'


      MYKNWN(  18 ) = 'Y-iti:i:i'
      MYMNNG(  18 ) = 'Y*y*H*M*S'


      MYKNWN(  19 ) = 'Y-iti:i:n'
      MYMNNG(  19 ) = 'Y*y*H*M*S'


      MYKNWN(  20 ) = 'Y-iti:n'
      MYMNNG(  20 ) = 'Y*y*H*M'


      MYKNWN(  21 ) = 'Yid'
      MYMNNG(  21 ) = 'Yy*'


      MYKNWN(  22 ) = 'Yidi:i'
      MYMNNG(  22 ) = 'Yy*H*M'


      MYKNWN(  23 ) = 'Yidi:i:i'
      MYMNNG(  23 ) = 'Yy*H*M*S'


      MYKNWN(  24 ) = 'Yidi:i:n'
      MYMNNG(  24 ) = 'Yy*H*M*S'


      MYKNWN(  25 ) = 'Yidi:n'
      MYMNNG(  25 ) = 'Yy*H*M'


      MYKNWN(  26 ) = 'Yii'
      MYMNNG(  26 ) = 'YmD'


      MYKNWN(  27 ) = 'Yiii'
      MYMNNG(  27 ) = 'YmDH'


      MYKNWN(  28 ) = 'Yiii:i'
      MYMNNG(  28 ) = 'YmDH*M'


      MYKNWN(  29 ) = 'Yiii:i:i'
      MYMNNG(  29 ) = 'YmDH*M*S'


      MYKNWN(  30 ) = 'Yiii:i:n'
      MYMNNG(  30 ) = 'YmDH*M*S'


      MYKNWN(  31 ) = 'Yiii:n'
      MYMNNG(  31 ) = 'YmDH*M'


      MYKNWN(  32 ) = 'Yiiii'
      MYMNNG(  32 ) = 'YmDHM'


      MYKNWN(  33 ) = 'Yiiiii'
      MYMNNG(  33 ) = 'YmDHMS'


      MYKNWN(  34 ) = 'Yiiiin'
      MYMNNG(  34 ) = 'YmDHMS'


      MYKNWN(  35 ) = 'Yiiin'
      MYMNNG(  35 ) = 'YmDHM'


      MYKNWN(  36 ) = 'Yiin'
      MYMNNG(  36 ) = 'YmDH'


      MYKNWN(  37 ) = 'Yim'
      MYMNNG(  37 ) = 'YDm'


      MYKNWN(  38 ) = 'Yimi'
      MYMNNG(  38 ) = 'YDmH'


      MYKNWN(  39 ) = 'Yimi:i'
      MYMNNG(  39 ) = 'YDmH*M'


      MYKNWN(  40 ) = 'Yimi:i:i'
      MYMNNG(  40 ) = 'YDmH*M*S'


      MYKNWN(  41 ) = 'Yimi:i:n'
      MYMNNG(  41 ) = 'YDmH*M*S'


      MYKNWN(  42 ) = 'Yimi:n'
      MYMNNG(  42 ) = 'YDmH*M'


      MYKNWN(  43 ) = 'Yimn'
      MYMNNG(  43 ) = 'YDmH'


      MYKNWN(  44 ) = 'Yin'
      MYMNNG(  44 ) = 'YmD'


      MYKNWN(  45 ) = 'Ymi'
      MYMNNG(  45 ) = 'YmD'


      MYKNWN(  46 ) = 'Ymii'
      MYMNNG(  46 ) = 'YmDH'


      MYKNWN(  47 ) = 'Ymii:i'
      MYMNNG(  47 ) = 'YmDH*M'


      MYKNWN(  48 ) = 'Ymii:i:i'
      MYMNNG(  48 ) = 'YmDH*M*S'


      MYKNWN(  49 ) = 'Ymii:i:n'
      MYMNNG(  49 ) = 'YmDH*M*S'


      MYKNWN(  50 ) = 'Ymii:n'
      MYMNNG(  50 ) = 'YmDH*M'


      MYKNWN(  51 ) = 'Ymin'
      MYMNNG(  51 ) = 'YmDH'


      MYKNWN(  52 ) = 'Ymn'
      MYMNNG(  52 ) = 'YmD'


      MYKNWN(  53 ) = 'Ynm'
      MYMNNG(  53 ) = 'YDm'


      MYKNWN(  54 ) = 'i-Y/'
      MYMNNG(  54 ) = 'y*Y*'


      MYKNWN(  55 ) = 'i-Y/i:i'
      MYMNNG(  55 ) = 'y*Y*H*M'


      MYKNWN(  56 ) = 'i-Y/i:i:i'
      MYMNNG(  56 ) = 'y*Y*H*M*S'


      MYKNWN(  57 ) = 'i-Y/i:i:n'
      MYMNNG(  57 ) = 'y*Y*H*M*S'


      MYKNWN(  58 ) = 'i-Y/i:n'
      MYMNNG(  58 ) = 'y*Y*H*M'


      MYKNWN(  59 ) = 'i-Yd'
      MYMNNG(  59 ) = 'y*Y*'


      MYKNWN(  60 ) = 'i-Ydi:i'
      MYMNNG(  60 ) = 'y*Y*H*M'


      MYKNWN(  61 ) = 'i-Ydi:i:i'
      MYMNNG(  61 ) = 'y*Y*H*M*S'


      MYKNWN(  62 ) = 'i-Ydi:i:n'
      MYMNNG(  62 ) = 'y*Y*H*M*S'


      MYKNWN(  63 ) = 'i-Ydi:n'
      MYMNNG(  63 ) = 'y*Y*H*M'


      MYKNWN(  64 ) = 'i-i-it'
      MYMNNG(  64 ) = 'Y*m*D*'


      MYKNWN(  65 ) = 'i-i-iti:i'
      MYMNNG(  65 ) = 'Y*m*D*H*M'


      MYKNWN(  66 ) = 'i-i-iti:i:i'
      MYMNNG(  66 ) = 'Y*m*D*H*M*S'


      MYKNWN(  67 ) = 'i-i-iti:i:n'
      MYMNNG(  67 ) = 'Y*m*D*H*M*S'


      MYKNWN(  68 ) = 'i-i-iti:n'
      MYMNNG(  68 ) = 'Y*m*D*H*M'


      MYKNWN(  69 ) = 'i-i/i:i'
      MYMNNG(  69 ) = 'Y*y*H*M'


      MYKNWN(  70 ) = 'i-i/i:i:i'
      MYMNNG(  70 ) = 'Y*y*H*M*S'


      MYKNWN(  71 ) = 'i-i/i:i:n'
      MYMNNG(  71 ) = 'Y*y*H*M*S'


      MYKNWN(  72 ) = 'i-i/i:n'
      MYMNNG(  72 ) = 'Y*y*H*M'


      MYKNWN(  73 ) = 'i-idi:i'
      MYMNNG(  73 ) = 'Y*y*H*M'


      MYKNWN(  74 ) = 'i-idi:i:i'
      MYMNNG(  74 ) = 'Y*y*H*M*S'


      MYKNWN(  75 ) = 'i-idi:i:n'
      MYMNNG(  75 ) = 'Y*y*H*M*S'


      MYKNWN(  76 ) = 'i-idi:n'
      MYMNNG(  76 ) = 'Y*y*H*M'


      MYKNWN(  77 ) = 'i-it'
      MYMNNG(  77 ) = 'Y*y*'


      MYKNWN(  78 ) = 'i-iti:i'
      MYMNNG(  78 ) = 'Y*y*H*M'


      MYKNWN(  79 ) = 'i-iti:i:i'
      MYMNNG(  79 ) = 'Y*y*H*M*S'


      MYKNWN(  80 ) = 'i-iti:i:n'
      MYMNNG(  80 ) = 'Y*y*H*M*S'


      MYKNWN(  81 ) = 'i-iti:n'
      MYMNNG(  81 ) = 'Y*y*H*M'


      MYKNWN(  82 ) = 'i:i:iimY'
      MYMNNG(  82 ) = 'H*M*SDmY'


      MYKNWN(  83 ) = 'i:i:imiY'
      MYMNNG(  83 ) = 'H*M*SmDY'


      MYKNWN(  84 ) = 'i:i:nimY'
      MYMNNG(  84 ) = 'H*M*SDmY'


      MYKNWN(  85 ) = 'i:i:nmiY'
      MYMNNG(  85 ) = 'H*M*SmDY'


      MYKNWN(  86 ) = 'i:iimY'
      MYMNNG(  86 ) = 'H*MDmY'


      MYKNWN(  87 ) = 'i:imiY'
      MYMNNG(  87 ) = 'H*MmDY'


      MYKNWN(  88 ) = 'i:nimY'
      MYMNNG(  88 ) = 'H*MDmY'


      MYKNWN(  89 ) = 'i:nmiY'
      MYMNNG(  89 ) = 'H*MmDY'


      MYKNWN(  90 ) = 'iYd'
      MYMNNG(  90 ) = 'yY*'


      MYKNWN(  91 ) = 'iYdi:i'
      MYMNNG(  91 ) = 'yY*H*M'


      MYKNWN(  92 ) = 'iYdi:i:i'
      MYMNNG(  92 ) = 'yY*H*M*S'


      MYKNWN(  93 ) = 'iYdi:i:n'
      MYMNNG(  93 ) = 'yY*H*M*S'


      MYKNWN(  94 ) = 'iYdi:n'
      MYMNNG(  94 ) = 'yY*H*M'


      MYKNWN(  95 ) = 'iiY'
      MYMNNG(  95 ) = 'mDY'


      MYKNWN(  96 ) = 'iiYi'
      MYMNNG(  96 ) = 'mDYH'


      MYKNWN(  97 ) = 'iiYi:i'
      MYMNNG(  97 ) = 'mDYH*M'


      MYKNWN(  98 ) = 'iiYi:i:i'
      MYMNNG(  98 ) = 'mDYH*M*S'


      MYKNWN(  99 ) = 'iiYi:i:n'
      MYMNNG(  99 ) = 'mDYH*M*S'


      MYKNWN( 100 ) = 'iiYi:n'
      MYMNNG( 100 ) = 'mDYH*M'


      MYKNWN( 101 ) = 'iiYn'
      MYMNNG( 101 ) = 'mDYH'


      MYKNWN( 102 ) = 'iid'
      MYMNNG( 102 ) = 'Yy*'


      MYKNWN( 103 ) = 'iidi:i'
      MYMNNG( 103 ) = 'Yy*H*M'


      MYKNWN( 104 ) = 'iidi:i:i'
      MYMNNG( 104 ) = 'Yy*H*M*S'


      MYKNWN( 105 ) = 'iidi:i:n'
      MYMNNG( 105 ) = 'Yy*H*M*S'


      MYKNWN( 106 ) = 'iidi:n'
      MYMNNG( 106 ) = 'Yy*H*M'


      MYKNWN( 107 ) = 'iim'
      MYMNNG( 107 ) = 'YDm'


      MYKNWN( 108 ) = 'iimi'
      MYMNNG( 108 ) = 'YDmH'


      MYKNWN( 109 ) = 'iimi:i'
      MYMNNG( 109 ) = 'YDmH*M'


      MYKNWN( 110 ) = 'iimi:i:i'
      MYMNNG( 110 ) = 'YDmH*M*S'


      MYKNWN( 111 ) = 'iimi:i:n'
      MYMNNG( 111 ) = 'YDmH*M*S'


      MYKNWN( 112 ) = 'iimi:n'
      MYMNNG( 112 ) = 'YDmH*M'


      MYKNWN( 113 ) = 'iimii'
      MYMNNG( 113 ) = 'YDmHM'


      MYKNWN( 114 ) = 'iimiii'
      MYMNNG( 114 ) = 'YDmHMS'


      MYKNWN( 115 ) = 'iimiin'
      MYMNNG( 115 ) = 'YDmHMS'


      MYKNWN( 116 ) = 'iimin'
      MYMNNG( 116 ) = 'YDmHM'


      MYKNWN( 117 ) = 'iimn'
      MYMNNG( 117 ) = 'YDmH'


      MYKNWN( 118 ) = 'imY'
      MYMNNG( 118 ) = 'DmY'


      MYKNWN( 119 ) = 'imYi'
      MYMNNG( 119 ) = 'DmYH'


      MYKNWN( 120 ) = 'imYi:i'
      MYMNNG( 120 ) = 'DmYH*M'


      MYKNWN( 121 ) = 'imYi:i:i'
      MYMNNG( 121 ) = 'DmYH*M*S'


      MYKNWN( 122 ) = 'imYi:i:n'
      MYMNNG( 122 ) = 'DmYH*M*S'


      MYKNWN( 123 ) = 'imYi:n'
      MYMNNG( 123 ) = 'DmYH*M'


      MYKNWN( 124 ) = 'imYn'
      MYMNNG( 124 ) = 'DmYH'


      MYKNWN( 125 ) = 'imi'
      MYMNNG( 125 ) = 'YmD'


      MYKNWN( 126 ) = 'imi:i:iY'
      MYMNNG( 126 ) = 'DmH*M*SY'


      MYKNWN( 127 ) = 'imi:i:nY'
      MYMNNG( 127 ) = 'DmH*M*SY'


      MYKNWN( 128 ) = 'imi:iY'
      MYMNNG( 128 ) = 'DmH*MY'


      MYKNWN( 129 ) = 'imi:nY'
      MYMNNG( 129 ) = 'DmH*MY'


      MYKNWN( 130 ) = 'imii'
      MYMNNG( 130 ) = 'YmDH'


      MYKNWN( 131 ) = 'imii:i'
      MYMNNG( 131 ) = 'YmDH*M'


      MYKNWN( 132 ) = 'imii:i:i'
      MYMNNG( 132 ) = 'YmDH*M*S'


      MYKNWN( 133 ) = 'imii:i:n'
      MYMNNG( 133 ) = 'YmDH*M*S'


      MYKNWN( 134 ) = 'imii:n'
      MYMNNG( 134 ) = 'YmDH*M'


      MYKNWN( 135 ) = 'imiii'
      MYMNNG( 135 ) = 'YmDHM'


      MYKNWN( 136 ) = 'imiiii'
      MYMNNG( 136 ) = 'YmDHMS'


      MYKNWN( 137 ) = 'imiiin'
      MYMNNG( 137 ) = 'YmDHMS'


      MYKNWN( 138 ) = 'imiin'
      MYMNNG( 138 ) = 'YmDHM'


      MYKNWN( 139 ) = 'imin'
      MYMNNG( 139 ) = 'YmDH'


      MYKNWN( 140 ) = 'imn'
      MYMNNG( 140 ) = 'YmD'


      MYKNWN( 141 ) = 'inY'
      MYMNNG( 141 ) = 'mDY'


      MYKNWN( 142 ) = 'inm'
      MYMNNG( 142 ) = 'YDm'


      MYKNWN( 143 ) = 'miY'
      MYMNNG( 143 ) = 'mDY'


      MYKNWN( 144 ) = 'miYi'
      MYMNNG( 144 ) = 'mDYH'


      MYKNWN( 145 ) = 'miYi:i'
      MYMNNG( 145 ) = 'mDYH*M'


      MYKNWN( 146 ) = 'miYi:i:i'
      MYMNNG( 146 ) = 'mDYH*M*S'


      MYKNWN( 147 ) = 'miYi:i:n'
      MYMNNG( 147 ) = 'mDYH*M*S'


      MYKNWN( 148 ) = 'miYi:n'
      MYMNNG( 148 ) = 'mDYH*M'


      MYKNWN( 149 ) = 'miYn'
      MYMNNG( 149 ) = 'mDYH'


      MYKNWN( 150 ) = 'mii'
      MYMNNG( 150 ) = 'mDY'


      MYKNWN( 151 ) = 'mii:i:iY'
      MYMNNG( 151 ) = 'mDH*M*SY'


      MYKNWN( 152 ) = 'mii:i:nY'
      MYMNNG( 152 ) = 'mDH*M*SY'


      MYKNWN( 153 ) = 'mii:iY'
      MYMNNG( 153 ) = 'mDH*MY'


      MYKNWN( 154 ) = 'mii:nY'
      MYMNNG( 154 ) = 'mDH*MY'


      MYKNWN( 155 ) = 'miii'
      MYMNNG( 155 ) = 'mDYH'


      MYKNWN( 156 ) = 'miii:i'
      MYMNNG( 156 ) = 'mDYH*M'


      MYKNWN( 157 ) = 'miii:i:i'
      MYMNNG( 157 ) = 'mDYH*M*S'


      MYKNWN( 158 ) = 'miii:i:n'
      MYMNNG( 158 ) = 'mDYH*M*S'


      MYKNWN( 159 ) = 'miii:n'
      MYMNNG( 159 ) = 'mDYH*M'


      MYKNWN( 160 ) = 'miiii'
      MYMNNG( 160 ) = 'mDYHM'


      MYKNWN( 161 ) = 'miiiii'
      MYMNNG( 161 ) = 'mDYHMS'


      MYKNWN( 162 ) = 'miiiin'
      MYMNNG( 162 ) = 'mDYHMS'


      MYKNWN( 163 ) = 'miiin'
      MYMNNG( 163 ) = 'mDYHM'


      MYKNWN( 164 ) = 'miin'
      MYMNNG( 164 ) = 'mDYH'


      MYKNWN( 165 ) = 'mnY'
      MYMNNG( 165 ) = 'mDY'


      MYKNWN( 166 ) = 'mni'
      MYMNNG( 166 ) = 'mDY'


      MYKNWN( 167 ) = 'nmY'
      MYMNNG( 167 ) = 'DmY'


      MYKNWN( 168 ) = 'i/i/i'
      MYMNNG( 168 ) = 'm*D*Y'

      MYKNWN( 169 ) = 'i/i/ii:i'
      MYMNNG( 169 ) = 'm*D*YH*M'


      MYKNWN( 170 ) = 'i/i/ii:n'
      MYMNNG( 170 ) = 'm*D*YH*M'

      MYKNWN( 171 ) = 'i/i/ii:i:n'
      MYMNNG( 171 ) = 'm*D*YH*M*S'


      MYKNWN( 172 ) = 'i/i/ii:i:i'
      MYMNNG( 172 ) = 'm*D*YH*M*S'


      MYKNWN( 173 ) = 'i/i/Y'
      MYMNNG( 173 ) = 'm*D*Y'


      MYKNWN( 174 ) = 'i/i/Yi:i'
      MYMNNG( 174 ) = 'm*D*YH*M'


      MYKNWN( 175 ) = 'i/i/ii:n'
      MYMNNG( 175 ) = 'm*D*YH*M'


      MYKNWN( 176 ) = 'i/i/Yi:i:n'
      MYMNNG( 176 ) = 'm*D*YH*M*S'

      MYKNWN( 177 ) = 'i/i/Yi:i:i'
      MYMNNG( 177 ) = 'm*D*YH*M*S'


      MYKNWN( 178 ) = 'Y-i-iti'
      MYMNNG( 178 ) = 'Y*m*D*H'


      MYKNWN( 179 ) = 'Y-iti'
      MYMNNG( 179 ) = 'Y*y*H'


      MYKNWN( 180 ) = 'Y-i-itn'
      MYMNNG( 180 ) = 'Y*m*D*H'


      MYKNWN( 181 ) = 'Y-itn'
      MYMNNG( 181 ) = 'Y*y*H'


      MYKNWN( 182 ) = 'i-i-iti'
      MYMNNG( 182 ) = 'Y*m*D*H'


      MYKNWN( 183 ) = 'i-i-itn'
      MYMNNG( 183 ) = 'Y*m*D*H'


      MYKNWN( 184 ) = 'i-iti'
      MYMNNG( 184 ) = 'Y*y*H'


      MYKNWN( 185 ) = 'i-itn'
      MYMNNG( 185 ) = 'Y*y*H'


      MYKNWN( 186 ) = 'i:ii/i/i'
      MYMNNG( 186 ) = 'H*Mm*D*Y'


      MYKNWN( 187 ) = 'i:ni/i/i'
      MYMNNG( 187 ) = 'H*Mm*D*Y'


      MYKNWN( 188 ) = 'i:i:ii/i/i'
      MYMNNG( 188 ) = 'H*M*Sm*D*Y'


      MYKNWN( 189 ) = 'i:i:ni/i/i'
      MYMNNG( 189 ) = 'H*M*Sm*D*Y'


      MYKNWN( 190 ) = 'i:ii/i/Y'
      MYMNNG( 190 ) = 'H*Mm*D*Y'


      MYKNWN( 191 ) = 'i:ni/i/Y'
      MYMNNG( 191 ) = 'H*Mm*D*Y'


      MYKNWN( 192 ) = 'i:i:ii/i/Y'
      MYMNNG( 192 ) = 'H*M*Sm*D*Y'


      MYKNWN( 193 ) = 'i:i:ni/i/Y'
      MYMNNG( 193 ) = 'H*M*Sm*D*Y'


      MYKNWN( 194 ) = 'i:ii-i-Y'
      MYMNNG( 194 ) = 'H*Mm*D*Y'


      MYKNWN( 195 ) = 'i:ni-i-Y'
      MYMNNG( 195 ) = 'H*Mm*D*Y'


      MYKNWN( 196 ) = 'i:i:ii-i-Y'
      MYMNNG( 196 ) = 'H*M*Sm*D*Y'


      MYKNWN( 197 ) = 'i:i:ni-i-Y'
      MYMNNG( 197 ) = 'H*M*Sm*D*Y'


      MYKNWN( 198 ) = 'i/i/Y/i:n'
      MYMNNG( 198 ) = 'm*D*Y*H*M'


      MYKNWN( 199 ) = 'i-i-Y'
      MYMNNG( 199 ) = 'm*D*Y'


      MYKNWN( 200 ) = 'i-i-Yi:n'
      MYMNNG( 200 ) = 'm*D*YH*M'


      MYKNWN( 201 ) = 'i-i-Yi:i:n'
      MYMNNG( 201 ) = 'm*D*YH*M*S'


      MYKNWN( 202 ) = 'i-i-Yi:i:i'
      MYMNNG( 202 ) = 'm*D*YH*M*S'


      MYKNWN( 203 ) = 'i-i-Yi:i'
      MYMNNG( 203 ) = 'm*D*YH*M'

C
C     Copy as many patterns and meanings as the input arrays allow.
C
      NKNOWN = MIN ( COUNT, ROOM )
      DO I = 1, NKNOWN
         KNOWN (I) = MYKNWN(I)
         MEANNG(I) = MYMNNG(I)
      END DO

C
C     Make sure everything is in the proper order.
C
      CALL ORDERC ( KNOWN,  NKNOWN, ORDVEC )
      CALL REORDC ( ORDVEC, NKNOWN, KNOWN  )
      CALL REORDC ( ORDVEC, NKNOWN, MEANNG )

C
C     If there wasn't sufficient room to get all of the patterns
C     and meanings, return FALSE.
C
      IF ( COUNT .GT. ROOM ) THEN
         ZZTPATS = .FALSE.
         RETURN
      END IF

      ZZTPATS = .TRUE.

      RETURN
      END
