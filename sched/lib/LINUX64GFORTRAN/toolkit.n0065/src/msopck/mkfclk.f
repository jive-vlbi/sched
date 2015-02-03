C$Procedure      MKFCLK ( MaKe a Fake sCLK kernel )
 
      SUBROUTINE MKFCLK ( SCLK, SCID, REFET, MADEBY )
 
C$ Abstract
C
C     This routine creates a simple SCLK kernel for those times when a
C     real SCLK is not available but is needed for producing CKs.
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
C      UTILITY
C
C$ Declarations
 
      IMPLICIT NONE

      CHARACTER*(*)         SCLK
      INTEGER               SCID 
      DOUBLE PRECISION      REFET
      CHARACTER*(*)         MADEBY
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SCLK       I   Name of the SCLK file to make.
C     SCID       I   Spacecraft ID associated with the clock.
C     REFET      I   Reference ET (seconds past J2000).
C     MADEBY     I   String identifying the file producer.
C
C$ Detailed_Input
C
C     SCLK     Name of the SCLK file to make.
C
C     SCID     Spacecraft ID associated with the clock. This is the ID
C              that will be used as the spacecraft ID argument in the
C              calls to SCLK routines to provide conversion for this
C              clock.
C
C     REFET    Reference ET (seconds past J2000). The time at which the
C              clock will start.
C
C     MADEBY   String identifying the file producer. This string will
C              be included 'as is' in the sentence "This file was
C              created by [MADEBY] on [DATE]." in the file comments.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     This routine creates an SCLK kernel with the specified name for
C     the specified spacecraft ID that simply models TDB seconds past
C     the input epoch REFET.  The granularity of this clock is 1
C     millisecond.
C
C$ Exceptions
C
C     1) If an error occurs while attempting to write to the SCLK
C        file, a routine called by this routine will detect and signal
C        the error.
C
C$ Particulars
C
C     This routine allows anyone creating C-kernels to have a readily
C     available SCLK kernel that simply follows ET.
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
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 24-AUG-2006 (BVS)
C
C        Initial version heavily based on Bill's mkclck from prediCkt.
C
C-&
 
C$ Index_Entries
C
C     Create an SCLK kernel that follows ET
C
C-&

C
C     SPICELIB Functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN

C
C     Local parameters.
C
      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 33 )

C
C     Local variables.
C
      CHARACTER*(LNSIZE)    CURTIM
      CHARACTER*(LNSIZE)    START
      CHARACTER*(LNSIZE)    TEXT  ( TXTSIZ )
 
      DOUBLE PRECISION      TVEC    ( 6 )
 
      INTEGER               I
      INTEGER               UNIT

C
C     Save everything to prevent potential memory problems in f2c'ed
C     version.
C
      SAVE

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'MKFCLK' )
      END IF
 
C
C     SCLK template. 
C
      TEXT(  1 ) = 'KPL/SCLK'
      TEXT(  2 ) = ' '
      TEXT(  3 ) = 'Fake SCLK Kernel for Spacecraft with ID %'
      TEXT(  4 ) = '-----------------------------------------'
     .//           '---------------'
      TEXT(  5 ) = ' '
      TEXT(  6 ) = '   This SCLK kernel contains the data nec'
     .//           'essary for converting from'
      TEXT(  7 ) = '   ephemeris time (ET) to ticks for a fak'
     .//           'e clock associated with the'
      TEXT(  8 ) = '   spacecraft with ID code %. This fake c'
     .//           'lock runs at the same rate'
      TEXT(  9 ) = '   as ET, starting at ET ! and going for 100'
      TEXT( 10 ) = '   years. It has two fields -- second'
     .//           's and milliseconds. The clock''s '
      TEXT( 11 ) = '   granularity is 1 millisecond.'
      TEXT( 12 ) = ' '
      TEXT( 13 ) = '   This file was created by ^'
      TEXT( 14 ) = '   on $.'
      TEXT( 15 ) = ' '
      TEXT( 16 ) = 'Kernel data'
      TEXT( 17 ) = '-----------------------------------------'
     .//           '---------------'
      TEXT( 18 ) = ' '
      TEXT( 19 ) = '   ' // CHAR(BSLASH) // 'begindata'
      TEXT( 20 ) = ' '
      TEXT( 21 ) = '      SCLK_KERNEL_ID = ( @$ )'
      TEXT( 22 ) = '      SCLK_DATA_TYPE_#       = ( 1 )'
      TEXT( 23 ) = '      SCLK01_TIME_SYSTEM_#   = ( 1 )'
      TEXT( 24 ) = '      SCLK01_N_FIELDS_#      = ( 2 )'
      TEXT( 25 ) = '      SCLK01_MODULI_#        = ( 3155760000 1000 )'
      TEXT( 26 ) = '      SCLK01_OFFSETS_#       = ( 0 0 )'
      TEXT( 27 ) = '      SCLK01_OUTPUT_DELIM_#  = ( 1 )'
      TEXT( 28 ) = '      SCLK_PARTITION_START_# = ( 0.00000E+00 )'
      TEXT( 29 ) = '      SCLK_PARTITION_END_#   = ( 3.15576E+12 )'
      TEXT( 30 ) = '      SCLK01_COEFFICIENTS_#  = ( 0.0 @! 1.0 )'
      TEXT( 31 ) = ' '
      TEXT( 32 ) = '   ' // CHAR(BSLASH) // 'begintext'
      TEXT( 33 ) = ' '

C
C     Convert reference ET to calendar format.
C
      CALL ETCAL  ( REFET, START )
      CALL CMPRSS ( ' ', 1, START, START )
      I = RTRIM ( START )
      CALL REPLCH ( START(1:I), ' ', '-', START(1:I) )

C
C     Get CPU time and package it into a string.
C
      CURTIM = 'YYYY-MM-DD/HR:MN:SC'
      CALL CPUTIM ( TVEC )
      CALL DPFMT  ( TVEC(1), '0YYY', CURTIM(1:4)   )
      CALL DPFMT  ( TVEC(2), '0M',   CURTIM(6:7)   )
      CALL DPFMT  ( TVEC(3), '0D',   CURTIM(9:10)  )
      CALL DPFMT  ( TVEC(4), '0h',   CURTIM(12:13) )
      CALL DPFMT  ( TVEC(5), '0m',   CURTIM(15:16) )
      CALL DPFMT  ( TVEC(6), '0s',   CURTIM(18:19) )
 
C
C     Replace markers in template with IDs, times, etc.
C
      DO I = 1, TXTSIZ
         CALL REPMI ( TEXT(I), '#', -SCID,  TEXT(I) )
         CALL REPMI ( TEXT(I), '%', SCID,   TEXT(I) )
         CALL REPMC ( TEXT(I), '!', START,  TEXT(I) )
         CALL REPMC ( TEXT(I), '$', CURTIM, TEXT(I) )
         CALL REPMC ( TEXT(I), '^', MADEBY, TEXT(I) )
      END DO
 
C
C     Open output SCLK file, write SCLK contents and close the file.
C
      CALL TXTOPN ( SCLK, UNIT )
      CALL WRITLA ( TXTSIZ, TEXT, UNIT )
      CLOSE ( UNIT = UNIT )

C
C     All done.
C
      CALL CHKOUT( 'MKFCLK' )

      RETURN
      END
