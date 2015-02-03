C$Procedure      SCPART ( Spacecraft Clock Partition Information )
 
      SUBROUTINE SCPART ( SC, NPARTS, PSTART, PSTOP )
 
C$ Abstract
C
C     Get spacecraft clock partition information from a spacecraft
C     clock kernel file.
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
C     SCLK
C
C$ Keywords
C
C     TIME
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'sclk.inc'
      INCLUDE               'zzctr.inc'

      INTEGER               SC
      INTEGER               NPARTS
      DOUBLE PRECISION      PSTART  ( * )
      DOUBLE PRECISION      PSTOP   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft identification code.
C     NPARTS     O   The number of spacecraft clock partitions.
C     PSTART     O   Array of partition start times.
C     PSTOP      O   Array of partition stop times.
C     MXPART     P   Maximum number of partitions.
C
C$ Detailed_Input
C
C     SC         is the NAIF ID for the spacecraft whose clock partition
C                information is being requested.
C
C$ Detailed_Output
C
C     NPARTS     is the number of spacecraft clock time partitions
C                described in the kernel file for spacecraft SC.
C
C     PSTART     is an array containing NPARTS partition start times
C                represented as double precision, encoded SCLK
C                ("ticks"). The values contained in PSTART are whole
C                numbers.
C
C     PSTOP      is an array containing NPARTS partition end times
C                represented as double precision, encoded SCLK
C                ("ticks"). The values contained in PSTOP are whole
C                numbers.
C
C$ Parameters
C
C     MXPART     is the maximum number of partitions for any spacecraft
C                clock. SCLK kernels contain start and stop times for
C                each partition. See the INCLUDE file sclk.inc for this
C                parameter's value.
C
C$ Exceptions
C
C     1)  If the kernel variables containing the spacecraft clock
C         partition start and stop times have not been loaded in the
C         kernel pool, the error will be diagnosed by routines called
C         by this routine.
C
C     2)  If the number of start and stop times are different then
C         the error SPICE(NUMPARTSUNEQUAL) is signaled.
C
C$ Files
C
C     An SCLK kernel containing spacecraft clock partition start
C     and stop times for the spacecraft clock indicated by SC must
C     be loaded into the kernel pool.
C
C$ Particulars
C
C     SCPART looks for two variables in the kernel pool for each
C     spacecraft's partition information. If SC = -nn, then the names of
C     the variables are
C
C         'SCLK_PARTITION_START_nn' and
C         'SCLK_PARTITION_END_nn'.
C
C     The start and stop times returned are in units of "ticks".
C
C$ Examples
C
C     1)  The following program fragment finds and prints out partition
C         start and stop times in clock format for the Galileo mission.
C         In this example, Galileo partition times are assumed to be
C         in the kernel file SCLK.KER.
C
C            CHARACTER*(30)        START
C            CHARACTER*(30)        STOP
C
C            CALL FURNSH ( 'SCLK.KER' )
C
C            SC = -77
C
C            CALL SCPART ( SC, NPARTS, PSTART, PSTOP )
C
C            DO I = 1, NPARTS
C
C               CALL SCFMT ( SC, PSTART( I ), START )
C               CALL SCFMT ( SC, PSTOP ( I ), STOP  )
C
C               WRITE (*,*)
C               WRITE (*,*) 'Partition ', I, ':'
C               WRITE (*,*) 'Start = ', START
C               WRITE (*,*) 'Stop  = ', STOP
C
C            END DO
C
C$ Restrictions
C
C     1) This routine assumes that an SCLK kernel appropriate to the
C        spacecraft identified by SC has been loaded into the kernel
C        pool.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     J.M. Lynch     (JPL)
C     B.V. Semenov   (JPL)
C     R.E. Thurman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.3.1, 19-MAR-2014 (NJB)
C
C        Minor header comment updates were made.
C
C-    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 2.1.0, 05-FEB-2008 (NJB)
C
C        The values of the parameter MXPART is now
C        provided by the INCLUDE file sclk.inc.
C
C-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.1.0, 22-MAR-1993 (JML)
C
C        The routine now uses the kernel pool watch capability.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB) (JML) (RET)
C
C-&
 
C$ Index_Entries
C
C     spacecraft_clock partition information
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         NMPSTR
      PARAMETER           ( NMPSTR = 'SCLK_PARTITION_START' )
 
      CHARACTER*(*)         NMPSTP
      PARAMETER           ( NMPSTP = 'SCLK_PARTITION_END'   )
 
C
C     Local variables
C
      CHARACTER*(60)        KVNAME  ( 2 )
 
      DOUBLE PRECISION      PRTSA   ( MXPART )
      DOUBLE PRECISION      PRTSO   ( MXPART )
 
      INTEGER               I
      INTEGER               LSTPRT
      INTEGER               NPRTSA
      INTEGER               NPRTSO
      INTEGER               OLDSC
      INTEGER               USRCTR ( CTRSIZ ) 
 
      LOGICAL               FIRST
      LOGICAL               NODATA
      LOGICAL               UPDATE
 
C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  LSTPRT
      SAVE                  NODATA
      SAVE                  OLDSC
      SAVE                  PRTSA
      SAVE                  PRTSO
      SAVE                  USRCTR
 
C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /
      DATA                  NODATA  / .TRUE. / 
      DATA                  OLDSC   / 0      /
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SCPART' )
 
C
C     On the first pass through the subroutine, or if the
C     spacecraft code changes, set watches on the SCLK kernel
C     variables for the current clock.
C
      IF (  FIRST  .OR.  ( SC .NE. OLDSC )  ) THEN
C
C        Make up a list of names of kernel variables that we'll use.
C
         KVNAME(1) = NMPSTR
         KVNAME(2) = NMPSTP
 
         DO I = 1, 2
            CALL SUFFIX ( '_#',        0,         KVNAME(I) )
            CALL REPMI  ( KVNAME(I),  '#',  -SC,  KVNAME(I) )
         END DO
C
C        Set a watch on all of the kernel variables used.
C
         CALL SWPOOL ( 'SCPART', 2, KVNAME )
C
C        Keep track of the last spacecraft ID encountered.
C
         OLDSC =  SC
 
C
C        Initialize the local POOL counter to user value.
C
         CALL ZZCTRUIN( USRCTR )

         FIRST = .FALSE.
 
      END IF
 
C
C     If any of the kernel pool variables that this routine uses
C     have been updated, or if the spacecraft ID changes, look up
C     the new values from the kernel pool.
C
      CALL ZZCVPOOL ( 'SCPART', USRCTR, UPDATE )
 
      IF ( UPDATE .OR. NODATA ) THEN
C
C        Read the values from the kernel pool.
C
         CALL SCLD01 ( NMPSTR, SC, MXPART, NPRTSA, PRTSA )
         CALL SCLD01 ( NMPSTP, SC, MXPART, NPRTSO, PRTSO )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT ( 'SCPART' )
            RETURN

         END IF
        
C
C        Error checking.
C
         IF ( NPRTSA .NE. NPRTSO  ) THEN

            NODATA = .TRUE.

            CALL SETMSG ( 'The number of partition start and stop '  //
     .                    'times are unequal for spacecraft #.    '    )
            CALL ERRINT ( '#', SC                                      )
            CALL SIGERR ( 'SPICE(NUMPARTSUNEQUAL)'                     )
            CALL CHKOUT ( 'SCPART'                                     )
            RETURN

         END IF
 
C
C        At this point we have the data we sought. We need not
C        perform another kernel pool look-up unless there's 
C        a kernel pool update or change in the SCLK ID.
C
         NODATA = .FALSE.

C
C        Buffer the number of partitions and the partition start
C        and stop times.
C
         LSTPRT = NPRTSA
C
C        The partition start and stop times must be whole numbers.
C
         DO I = 1, LSTPRT
            PRTSA(I) = ANINT ( PRTSA(I)  )
            PRTSO(I) = ANINT ( PRTSO(I)  )
         END DO
 
      END IF
 
C
C     Copy the values in local buffers to the output arguments.
C
      NPARTS = LSTPRT
 
      DO I = 1, NPARTS
         PSTART(I) = PRTSA(I)
         PSTOP(I)  = PRTSO(I)
      END DO
 
      CALL CHKOUT ( 'SCPART' )
      RETURN
      END
