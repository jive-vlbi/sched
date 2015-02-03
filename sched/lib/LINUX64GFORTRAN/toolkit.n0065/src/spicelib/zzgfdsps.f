C$Procedure   ZZGFDSPS  ( GF, display string  )
 
      SUBROUTINE ZZGFDSPS ( NLEAD, STRING, FMT, NTRAIL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Display a character string at a position at the first column on
C     the previous line on the screen.
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
C     STRING
C     DISPLAY
C     CURSOR
C     POSITION
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               NLEAD
      CHARACTER*(*)         STRING
      CHARACTER*(*)         FMT
      INTEGER               NTRAIL
 
C$ Brief_I/O
C
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NLEAD      I   Number of leading blank lines to write.
C     STRING     I   The string to display.
C     FMT        I   Format in which the string is to be written.
C     NTRAIL     I   Number of trailing blank lines to write.
C
C$ Detailed_Input
C
C     NLEAD          is the number of blank lines to write before
C                    writing the output text string.
C
C     STRING         is a message to be displayed on the standard
C                    output stream.
C
C     FMT            is a Fortran format specification used to write
C                    the output string.
C
C                    FMT may be left to default ('A'), or may be used
C                    to control the length of the string ('A10').
C
C     NTRAIL         is the number of blank lines to write after
C                    writing the output text string.
C
C$ Detailed_Output
C
C     None. This program has no output arguments but writes to the
C     standard output stream.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If an error occurs when this routine attempts to
C        write its output, the message SPICE(WRITEERROR)
C        will be signaled.
C
C     2) If the either of the input arguments NLEAD or NTRAIL
C        is non-positive, then no leading or trailing blank
C        lines will be written, respectively. This case is not
C        considered an error.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine supports the default GF progress report display.
C     Output is written to the standard output stream; normally this
C     results in output on a terminal window.
C
C     After the output line is written, this routine moves the cursor
C     up and to the first column, so a subsequent call will overwrite
C     output from the current call.
C
C$ Examples
C
C     See calls made to this routine by the entry points of
C     ZZGFRPWK.
C
C$ Restrictions
C
C     This routine relies on platform support for ANSI cursor control
C     character sequences.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C     L.S. Elson       (JPL)
C     I.M. Underwood   (JPL)
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.15.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 1.14.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 1.13.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 1.12.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.11.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 1.10.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 1.9.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 1.8.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 1.7.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 1.6.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 1.5.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.4.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 1.3.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 1.2.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 1.1.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.0.0, 15-APR-2009 (NJB)
C
C-&
 
C$ Index_Entries
C
C     GF output progress report string
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
 
C
C     Local parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
C
C     Local Variables
C
      CHARACTER*(LNSIZE)    TMPSTR
 
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               L
 
C
C     Use discovery check-in.
C
 
C
C     Get a local copy of the input string.
C
      TMPSTR = STRING
      L      = RTRIM(TMPSTR)
C
C     Write leading blank lines, if any. We use cursor control
C     here so as to avoid overwriting the last status line.
C
      DO I = 1, NLEAD
         WRITE (*, '(A)' ) CHAR(27)//'[B'
      END DO
 
C
C     Write the status line, then move the cursor up one line.
C     This places the cursor at the start of the line that
C     was just written.
C
      CALL SUFFIX (  CHAR(27)//'[A', 0, TMPSTR )
 
      WRITE ( *, '(A)', IOSTAT = IOSTAT ) TMPSTR( : RTRIM(TMPSTR) )
 
      IF ( IOSTAT .NE. 0  ) THEN
 
         CALL CHKIN  ( 'ZZGFDSPS'                                      )
         CALL SETMSG ( 'Error writing to standard output: IOSTAT = #.' )
         CALL ERRINT ( '#', IOSTAT                                     )
         CALL SIGERR ( 'SPICE(WRITEFAILED)'                            )
         CALL CHKOUT ( 'ZZGFDSPS'                                      )
         RETURN
 
      END IF
 
C
C     Write trailing blank lines, if any. We use cursor control
C     here so as to avoid overwriting the last status line.
C
      DO I = 1, NTRAIL
         WRITE (*, '(A)' ) CHAR(27)//'[B'
      END DO
 
      RETURN
      END
