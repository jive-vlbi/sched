C$ Procedure TIMECN (Convert and round times)

      SUBROUTINE TIMECN ( TCONV, IDS, TOUT, LINET )
      
C$ Abstract
C
C     This is internal subroutine for CKBRIEF program. It converts
C     time between encoded SCLK, SCLK string, ET, UTC or UTC/DOY. 
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
C$ Keywords
C
C     SUMMARY
C     C KERNEL
C      
C$ Declarations

      IMPLICIT NONE

      INCLUDE           'ckbrief.inc'
      
      DOUBLE PRECISION  TCONV
      INTEGER           IDS
      CHARACTER*(*)     TOUT
      CHARACTER*(*)     LINET
      
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TCONV      I   Encoded SCLK time
C     IDS        I   NAIF ID code of object 
C     TOUT       I   Form of time representation on output
C     LINET      O   Text presentation of time
C
C$ Detailed Input
C
C     TCONV          Encoded SCLK time to be converted, rounded
C                    and decoded to character string
C
C     IDS            Integer NAIF ID code found in summary from which
C                    TCONV was obtained.
C
C     TOUT           Key specifying time presentation on output: 
C                    SCLK string, encoded SCLK, ET, UTC or DOY UTC.
C
C$ Detailed Output
C
C     LINET          Character string which contains time converted 
C                    to requested representation or NOTIME flag if
C                    conversion was not possible.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     None.
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
C     Y.K. Zaiko      (BERC)
C     B.V. Semenov    (NAIF)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    CKBRIEF Beta Version 1.0.0, 17-FEB-1999 (YKZ)(BVS)
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      
C
C     Local variables
C      
      DOUBLE PRECISION      ETTIME
      
      INTEGER               SC
      LOGICAL               OK

C
C     Reset output time string.     
C      
      LINET  = ' '
            
C
C     It is necessary to use real spacecraft ID in SCLK<->ET
C     conversion routines. CKMETA is providing it.
C
      CALL CKMETA ( IDS, 'SCLK', SC )
      
C
C     TIMECN is the special routine to be used in CKBRIEF
C     utility to convert times in accordance to user request. If user
C     haven't provided ancillary files to perform this conversion, the
C     program shouldn't stop. To achieve this we'll forbid TIMECN to
C     be aborted by SPICELIB standard error processing if it can't
C     convert times. On the exit from TIMECN, SPICE error handling 
C     is restored to its original state.
C
      CALL ERRACT ( 'SET', 'RETURN' )
      CALL ERRPRT ( 'SET', 'NONE' )
           
C
C     We do appropriate conversion depending on the requested output
C     time representation. If SCLK for the s/c of interest and(!) 
C     LSK file weren't loaded, conversions to string SCLK, ET, UTC 
C     and UTC/DOY are not possible. The output time set to NOTIME
C     flag.
C     
      IF ( TOUT .EQ. DPSCLK ) THEN
      
C  
C        DP SLCKs should be simply converted to string.
C
         CALL DPFMT ( TCONV, PICTDP, LINET )
         
         
      ELSE IF ( TOUT .EQ. SCLK ) THEN
      
C
C        SCLK string is computed from DP SCLK if it's possible.
C         
         CALL SCDECD ( SC, TCONV, LINET )       
         IF ( FAILED () ) THEN
            LINET = NOTIME
         END IF
         
         
      ELSE IF ( TOUT .EQ. ET ) THEN
      
C
C        Calendar ET is computed by converting DP SCLK to ET seconds 
C        and converting them further to ET calendar string
C         
         CALL SCT2E ( SC, TCONV, ETTIME )
         IF ( .NOT. FAILED() ) THEN
            CALL TIMOUT ( ETTIME, PICTET, LINET )
            IF ( FAILED() ) THEN
               LINET = NOTIME
            END IF
         ELSE
            LINET = NOTIME
         END IF


      ELSE IF ( TOUT .EQ. UTC ) THEN
      
C
C        UTC time is computed by converting DP SCLK to ET seconds, 
C        which after that converted to UTC string.
C      
         CALL SCT2E ( SC, TCONV, ETTIME )
         IF ( .NOT. FAILED() ) THEN
            CALL TIMOUT ( ETTIME, PICTUT, LINET )
            IF ( FAILED() ) THEN
               LINET = NOTIME
            END IF
         ELSE
            LINET = NOTIME
         END IF
         
         
      ELSE IF ( TOUT .EQ. DOY ) THEN
      
C
C        UTCDOY time is computed by converting DP SCLK to ET seconds, 
C        which after that converted to UTC string.
C      
         CALL SCT2E ( SC, TCONV, ETTIME )
         IF ( .NOT. FAILED() ) THEN
            CALL TIMOUT ( ETTIME, PICTDY, LINET )
            IF ( FAILED() ) THEN
               LINET = NOTIME
            END IF
         ELSE
            LINET = NOTIME
         END IF
         
      END IF
      
      OK = .NOT. FAILED()

C
C     Now we can reset SPICE error handling mechanism back to its 
C     original state.
C 
      CALL RESET
      CALL ERRACT ( 'SET', 'ABORT' )
      CALL ERRPRT ( 'SET', 'DEFAULT' )

C
C     There is a bug in UNITIM (trace: SCT2E --> SCTE01 --> UNITIM)
C     that has to be temporarily fixed before UNITIM officially fixed 
C     in N0049 delivery. Call to a specially written routine FIXUNI
C     does that.
C
      IF ( .NOT. OK ) THEN     
         CALL FIXUNI
      END IF
           
      RETURN
      END
