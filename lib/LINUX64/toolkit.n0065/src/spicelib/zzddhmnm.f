C$Procedure ZZDDHMNM ( Return unique enough DP number for a file )
 
      DOUBLE PRECISION FUNCTION ZZDDHMNM ( UNIT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return a unique enough DP number ("Magic NuMber") computed
C     using the contents of the file attached to the specified unit.
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

      INCLUDE              'zzddhman.inc'

      INTEGER               UNIT
 
      INTEGER               IDLEN
      PARAMETER           ( IDLEN  = 8 )

      INTEGER               NINTS
      PARAMETER           ( NINTS  = 20 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit attached to a file.
C
C     The function returns a unique enough DP number computed
C     using the contents of the file attached to the UNIT.
C
C$ Detailed_Input
C
C     UNIT        is the logical unit attached to a file opened for
C                 direct access prior to calling this routine.
C
C$ Detailed_Output
C
C     The function returns a DP number, computed using a few integers
C     read from the file attached to the UNIT, that is unique enough
C     for the handle manager to check if two files opened for READ
C     access are not the same file.
C
C     If reading the first record of the file or any of the lower level
C     routines called by this function fail for any reason, the 
C     returned value is set to 0.D0.
C
C$ Parameters
C
C     IDLEN       is the length of the ID word.
C
C     NINTS       is the number of integers that will be read from
C                 a particular record of the file. NINTS must be 
C                 big enough to make sure that unique pointers from
C                 the file record of DAF and DAS files are read and
C                 used to compute the output value.
C
C     The function also uses some DDH general parameters from 
C     zzddhman.inc.
C
C$ Exceptions
C
C     Error free.
C
C     This routine and routines in its call tree signal several
C     SPICE(BUG) exceptions. They are signaled if the module or modules
C     in its calling tree are improperly configured to run on this
C     platform.
C
C$ Files
C
C     The input UNIT must be attached to a file opened for direct 
C     access prior to calling this function.
C
C$ Particulars
C
C     This function reads the first IDLEN characters (assumed to be the
C     ID word) followed by NINTS integers from the first record of the
C     direct access file attached to the input UNIT.
C
C     If successful, it examines the ID word to determine the file
C     architecture.
C
C     For DAF files it then tries to determine the binary format.
C
C     For DAF files in the native binary format, it adds up NINTS
C     integers read from the first record to get initial output value.
C     Then it reads additional NINTS integers from the first descriptor
C     record and, if the second read is successful, it adds these
C     additional integers to the output value.
C
C     For DAF files in a non-native binary format supported by run-time
C     translation, it re-read NINTS integers from the first record as
C     characters, converts them to NINTS integers using ZZXLATEI, and
C     adds them up to get initial output value. Then it reads as
C     characters NINTS integers from the first descriptor record of the
C     file, converts them to NINTS integers using ZZXLATEI and ,if the
C     second read is successful, it adds these additional integers to
C     the output value.
C
C     For DAS files, text kernels and unrecognized files, it simply
C     adds up NINTS integers to get the output value.
C
C     If the initial read is not successful, the output value is set to
C     zero.
C
C$ Examples
C
C     See the caller routine, ZZDDHF2H.
C
C$ Restrictions
C
C     The input UNIT must be attached to a file opened for direct
C     access prior to calling this function.
C
C$ Literature_References
C
C     None.
C 
C$ Author_and_Institution
C
C     B.B. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 26-APR-2012 (BVS)
C
C-&

C
C     SPICELIB Functions
C
      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters.
C
C     Character buffer size consistent with the number of integers
C     that will be read from the file.
C
      INTEGER               NCHARS
      PARAMETER           ( NCHARS = NINTS * 4 )

C
C     Minimum and maximum values for the range of ASCII printing
C     characters.
C
      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )
 
      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )

C
C     Local variables.
C      
      CHARACTER*(IDLEN)     ARCH
      CHARACTER*(IDLEN)     IDWORD
      CHARACTER*(IDLEN)     TYPE
      CHARACTER*(NCHARS)    STRBUF

      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )

      DOUBLE PRECISION      MNM

      INTEGER               BFF
      INTEGER               I
      INTEGER               INTARR ( NINTS )
      INTEGER               IOSTAT

      INTEGER               NATBFF
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               NUMSUP
      INTEGER               SUPIDX

      LOGICAL               FIRST

C
C     Saved variables.
C
      SAVE                  FIRST

      SAVE                  NATBFF
      SAVE                  SUPBFF
      SAVE                  NUMSUP
 
C
C     Data statements.
C
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /

C
C     Set default output value to zero.
C
      MNM      = 0.D0
      ZZDDHMNM = 0.D0

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHMNM' )
      END IF

C
C     Perform some initialization tasks.
C
      IF ( FIRST ) THEN
 
         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP,
     .                   STRAMH, STRARC, STRBFF  )
 
C
C        Check FAILED() to handle the unlikely event that
C        ZZDDHINI signaled SPICE(BUG).
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHMNM' )
            RETURN
         END IF
 
C
C        Do not perform initialization tasks again.
C
         FIRST = .FALSE.
 
      END IF

C
C     Read ID word string followed by NINTS integers from the first
C     record of the file.
C
      READ ( UNIT=UNIT, REC=1, IOSTAT=IOSTAT ) IDWORD, INTARR

      IF ( IOSTAT .EQ. 0 ) THEN

C
C        Read succeeded. Try to determine the file architecture and
C        type from the ID word. To do this, mimic the part of GETFAT
C        that deals only with the ID word. First replace any non
C        printing ASCII characters in the ID word with blanks, then use
C        IDW2AT on the "cleaned" ID word to get architecture and type.
C
         DO I = 1, IDLEN
            IF (      ( ICHAR(IDWORD(I:I)) .LT. MINPCH )
     .           .OR. ( ICHAR(IDWORD(I:I)) .GT. MAXPCH ) ) THEN
               IDWORD(I:I) = ' '
            END IF
         END DO

         CALL IDW2AT ( IDWORD, ARCH, TYPE )

C
C        Compute the output value based on the file architecture.
C
         IF   ( ARCH .EQ. 'DAF' ) THEN

C
C           For DAF files, try to get the file's binary format.
C
            CALL ZZDDHPPF ( UNIT, DAF, BFF )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZDDHMNM' )
               RETURN
            END IF

C
C           If the file is in a non-native format, we will need to read
C           the first record again, now directly as characters, and 
C           translate these character to native integers.
C
            IF ( BFF .NE. NATBFF ) THEN

C           
C              First, check if run-time translation is supported for
C              this BFF. This check, stolen from ZZDDHMAN, is needed
C              because ZZXLATEI accepts only BFFs for which translation
C              is guaranteed to be supported on this platform. If it
C              is not supported, simply get out (note that the default 
C              return value was set to zero at the start.) 
C
               SUPIDX = ISRCHI ( BFF, NUMSUP, SUPBFF )
 
               IF ( SUPIDX .EQ. 0 ) THEN
                  CALL CHKOUT ( 'ZZDDHMNM' )
                  RETURN
               END IF
 
C
C              Read the first record as characters and do translation.
C
               READ ( UNIT=UNIT, REC=1, IOSTAT=IOSTAT ) IDWORD, STRBUF

               CALL ZZXLATEI ( BFF, STRBUF, NINTS, INTARR )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZDDHMNM' )
                  RETURN
               END IF

            END IF

C           
C           Add integers from the file record to the output value.
C
            DO I = 1, NINTS
               MNM = MNM + INTARR(I)
            END DO

C
C           Read more integers from the start of the first descriptor
C           record without regard to the file's binary format and, 
C           if successful, add them to the total.
C
            READ ( UNIT=UNIT, REC=INTARR(18), IOSTAT=IOSTAT ) INTARR

            IF ( IOSTAT .EQ. 0 ) THEN

               DO I = 1, NINTS
                  MNM = MNM + INTARR(I)
               END DO

            END IF

         ELSE IF ( ARCH .EQ. 'DAS' ) THEN

C
C           For DAS files, for now, add up integers from the first
C           record to get the output value.
C
            DO I = 1, NINTS
               MNM = MNM + INTARR(I)
            END DO

         ELSE

C
C           For all other files, add up integers from the first record
C           to get the output value.
C
            DO I = 1, NINTS
               MNM = MNM + INTARR(I)
            END DO

         END IF

      ELSE

C
C        The read of the file record failed. Do nothing as the output
C        value has already been set at the start of the function.
C        

      END IF

      ZZDDHMNM = MNM

      CALL CHKOUT ( 'ZZDDHMNM' )

      RETURN
      END
