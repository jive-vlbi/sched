C$Procedure TLE2SPK ( Read two-line element set and create type 10 SPK )

      SUBROUTINE TLE2SPK( INPFN, OBIDVL, CNIDVL, FRNMVL, SGIDVL, HANDLE,
     .                    COVVAL, COVTYP)

C$ Abstract
C
C     This routine is a module of the MKSPK program. It creates an SPK
C     file from a file containing the NORAD "two-line element sets".
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
C     MKSPK User's Guide
C
C$ Keywords
C
C     None.
C
C$ Declarations

      IMPLICIT       NONE

      INCLUDE               'mkspk.inc'

      CHARACTER*(*)         INPFN
      INTEGER               OBIDVL ( 2 )
      INTEGER               CNIDVL
      CHARACTER*(*)         FRNMVL
      CHARACTER*(*)         SGIDVL
      INTEGER               HANDLE
      DOUBLE PRECISION      COVVAL ( 2 )
      CHARACTER*(*)         COVTYP ( 2 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INPFN      I   Input file name
C     OBIDVL     I   Input TLE object ID and output SPK object ID
C     CNIDVL     I   Center ID NAIF code
C     FRNMVL     I   Reference frame name of output SPK
C     SGIDVL     I   Segment identifier
C     HANDLE     I   Handle of an SPK file open for writing.
C     COVVAL     I   Coverage start and stop times or pads.
C     COVTYP     I   Coverage value types ('TIME' or 'PAD').
C
C$ Detailed_Input
C
C     INPFN       is the name of input file containing the NORAD
C                 "two-line element sets"
C
C     FRNMVL      is the reference frame that output states are
C                 referenced to. It must be 'J2000'.
C
C     OBIDVL      is a two-element array containing TLE object ID and
C                 SPK object ID. The first element is the TLE object ID
C                 -- object ID to look for in the input TLE file. The
C                 second element is the SPK object ID -- the NAIF ID to
C                 use in the output SPK file.
C
C     CNIDVL      is the NAIF ID for the center body. It must be 399
C                 corresponding to Earth.
C
C     SGIDVL      is identifier of segment stored in output file.
C
C     HANDLE      is the file handle of an SPK file that has been
C                 opened for writing.
C
C     COVVAL      is a two-element array specifying coverage start and
C                 stop times or pads. The first element is either the
C                 start time or start pad. The second element is either
C                 the stop time or stop pad.
C
C                 The start and stop times are given as TDB seconds
C                 past J2000. If specified, they are used as the start
C                 and stop times of the output SPK.
C
C                 The start and stop pad are given durations in TDB
C                 seconds. If specified, they are applied to the
C                 earliest and latest input TLE times to extend or
C                 contract the output SPK coverage. Positive pad values
C                 extend coverage outwards; negative pad values
C                 contract coverage inwards.
C
C                 Combinations of the start time and stop pad and
C                 the start pad and stop time are permitted.
C
C     COVTYP      is a two-element array specifying whether the
C                 corresponding elements of COVVAL are times of pads.
C
C                 The first element specifies whether COVVAL(1) is the
C                 start time (COVTYP(1)='TIME') or start pad
C                 (COVTYP(1)='PAD').
C
C                 The second element specifies whether COVVAL(2) is
C                 the stop time (COVTYP(2)='TIME') or stop pad
C                 (COVTYP(2)='PAD')
C
C$ Detailed_Output
C
C     None.       The data input is stored in an SPK segment in the
C                 DAF connected to the input HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the center body of the motion is not the Earth, then
C        the error SPICE(INCONSISTCENTERID) will signal.
C
C     2) If the reference frame is not 'J2000', then the error
C        SPICE(INCONSISTFRAME) will be signaled.
C
C     3) If code of requested space object is not found in the
C        input file, then the error SPICE(NOTLEDATAFOROBJECT)
C        will signal.
C
C     4) If second line of two-line element records does not exist,
C        then the error SPICE(NOSECONDLINE) will signal.
C
C     5) If second line of two-line element records does not exist at
C        the end of the file, then the error SPICE(NOSECONDLINE2)
C        will signal.
C
C     6) If any one of the required geophysical constants was not
C        found in the POOL, then the error SPICE(MISSINGGEOCONSTS)
C        will signal.
C
C     7) The error SPICE(INVALIDTLEORDER) signals if the epoch used
C        to provide coverage across a segment boundary has a date-time
C        before (earlier than) the epochs of the previous segment.
C
C     8) The error SPICE(INVALIDVALUE) signals if a time pad value is
C        not positive definite.
C
C     9) SPICE(BADTLE) if TLE set has incorrect format.
C
C     10) SPICE(BADTLEPADS) if both pads are zero for single TLE.
C
C     11) SPICE(BADSTOPTIME) if specified stop time before computed 
C         start time.
C
C     12) SPICE(BADSTARTTIME) if specified start time after computed 
C         stop time.
C
C     13) SPICE(INVALIDVALUE1) if specified start pad is negative.
C
C     14) SPICE(INVALIDINPUT1) if first coverage type is not 'TIME' 
C         or 'PAD'.
C
C     15) SPICE(INVALIDVALUE2) if specified stop pad is negative.
C
C     16) SPICE(INVALIDINPUT2) if second coverage type is not 'TIME' 
C         or 'PAD'.
C
C$ Files
C
C     This routine read text data from the input data file INPFN
C     containing two-line element set file in standard text format.
C
C     Leapsecond Kernel (LSK) file must be loaded before running
C     this routine.
C
C     A geophysical constants file for the Earth must be loaded
C     before running this routine.
C
C        The geophysical constants kernel should contain
C        the following variables:
C
C        BODY399_J2 --- J2 gravitational harmonic for earth.
C        BODY399_J3 --- J3 gravitational harmonic for earth.
C        BODY399_J4 --- J4 gravitational harmonic for earth.
C        BODY399_KE --- Square root of the GM for earth where GM
C                       is expressed in earth radii cubed
C                       per minutes squared.
C        BODY399_ER --- Equatorial radius of the earth in km.
C        BODY399_S0 --- Low altitude bound for atmospheric model in km.
C        BODY399_Q0 --- High altitude bound for atmospheric model in km.
C        BODY399_AE --- Distance units/earth radius (normally 1).
C
C
C     The program creates SPK file connected to HANDLE.
C     This file must be opened for writing before calling this
C     routine.
C
C$ Particulars
C
C     1) S_i > S_i+1 check:
C
C     The TLE2SPK logic can process a data file of arbitrary size
C     (within limits of memory) in a read-sort-write loop procedure
C     with each read-sort-write iteration acting on MAXEPC records
C     or less.
C
C     Given a TLE data file S, comprised of N sets of MAXEPC records,
C     S_1, S_2, .., S_N, (S_N may have less than MAXEPC records)
C     TLE2SPK reads S_i, sorts the records in ascending order by epoch,
C     then writes a type 10 segment to the SPK. Therefor, all sorting
C     applies locally to S_i not globally to S. The logic implicitly
C     assumes all epochs in S_i < all epochs in S_i+1. To ensure no
C     gaps between SPK segments written from S_i and S_i+1, the routine
C     adds the final record of S_i to the S_i+1 record set. If
C     S_i < S_i+1, that record is written as the first record in the
C     sorted S_i+1, if S_i > S_i+1, that record is written as the final
C     record in the sorted S_i+1. This second condition is considered
C     an error (SPICE(INVALIDTLEORDER)).
C
C
C     2) SPK coverage pads:
C
C     The pad values have only a positive sense. Nominal use of pads
C     expands the SPK coverage time interval based on the epoch of the
C     first TLE, EPOCH(1), and the epoch of the final TLE, EPOCH(N):
C
C                  EPOCH(1)                          EPOCH(N)
C            ------|---------------------------------|----------------
C
C            --|(--)---------------------------------(--)|------------
C              start pad                                 end pad
C
C     The effect of the pads, start and end, are independent of the
C     other. Use of only a start pad on the SPK coverage interval:
C
C
C                  EPOCH(1)                          EPOCH(N)
C            ------|---------------------------------|----------------
C
C            --|(--)---------------------------------|----------------
C              start pad
C
C     Use of only an end pad on the SPK coverage interval:
C
C                  EPOCH(1)                          EPOCH(N)
C            ------|---------------------------------|----------------
C
C            ------|---------------------------------(--)|------------
C                                                        end pad
C
C$ Examples
C
C     None.
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
C     N.G. Khavenson (IKI RAS, Russia)
C     B.V. Semenov   (JPL)
C     W.L. Taber     (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    Version 3.0.0, 01-MAY-2014 (EDW)(BVS)
C
C        Changed calling sequence to pass in both TLE and SPK IDs and
C        start and stop times or pads. Updated to produce output SPK
C        with coverage consistent with the start and stop times or pads
C        provided as inputs.
C
C        Increase to TLE buffer length defined by parameter MAXEPC
C        (maximum number of records in a type 10 segment) from 5000 to
C        50000. Addition of error checks that signal if the epochs of
C        a segment buffer represent dates-times before those in the
C        previous segment.
C
C        Implemented a set container for record epochs to eliminate
C        duplicate records, as defined by duplicate epochs, prior
C        to processing records.
C
C        Header and comment edits to describe new functionality
C        and to correspond to NAIF header standards.
C
C        Eliminated use of COVPAD parameter and removed dead code.
C
C        BUG FIX: fixed to process TLE lines with spacecraft code
C        left-padded with zeros to five digits.  
C
C-    Version 2.2.0, 06-MAR-2009 (BVS).
C
C        Encapsulated forward/backward propagation period in COVPAD
C        variable.
C
C-    Version 2.1.0, 18-MAR-2005 (EDW)
C
C        Corrected a logic error that prevent processing of TLEs
C        for vehicles with ID codes four characters or shorter.
C
C-    Version 2.0.0, 06-APR-2004 (EDW)
C
C        Modified algorithm to call ZZGETELM, a modified version of
C        GETELM. ZZGETELM returns a flag and explanation string
C        for any TLE processing error.
C
C        Correct a typo:
C
C           ENDINT   = EPOCHS(I)
C
C        to
C
C           ENDINT   = EPOCHS(PUT)
C
C        This typo could cause a TLE segment summary to report the
C        wrong end time for the segment data.
C
C-    Version 1.0.1, 27-JAN-2000 (BVS).
C
C        Added a little better error message for the case when
C        geophysical constants weren't loaded.
C
C-    Version 1.0.0, 22-NOV-1999 (NGK).
C
C        Initial release based on the  MKSPK10 utility program
C        Version 1.0.0, 18-JUL-1997 (WLT)
C
C-&

C$ Index_Entries
C
C     Creates an SPK file from a file containing the NORAD
C     "two-line element sets.
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN

      INTEGER               RTRIM
      LOGICAL               EQSTR
      LOGICAL               RETURN
      LOGICAL               ELEMD



C
C     Local variables
C

C
C     Size WDSIZE, LINLEN, FILSIZE are defined in include file.
C

      CHARACTER*( WDSIZE )  HWORD
      CHARACTER*( WDSIZE )  CODE
      CHARACTER*( WDSIZE )  CHOSE1
      CHARACTER*( WDSIZE )  CHOSE2
      CHARACTER*( LINLEN )  LINES  ( 2 )
      CHARACTER*( LINLEN )  ERROR  ( 2 )
      CHARACTER*( FILSIZ )  OUTFN

      CHARACTER *(*)        FRAME
      PARAMETER           ( FRAME = 'J2000' )

      INTEGER               CENTER
      PARAMETER           ( CENTER = 399 )

      INTEGER               FRCODE
      INTEGER               FRAMID

      LOGICAL               OK

C
C
C     The following integers are used to mark the various
C     slots in the array for geophysical constants.
C
C        J2
C        J3
C        J4
C        KE
C        QO
C        SO
C        ER
C        AE
C
      INTEGER               START
      PARAMETER           ( START  = 0 )

      INTEGER               J2
      PARAMETER           ( J2     = START  + 1 )

      INTEGER               J3
      PARAMETER           ( J3     = J2     + 1 )

      INTEGER               J4
      PARAMETER           ( J4     = J3     + 1 )

      INTEGER               KE
      PARAMETER           ( KE     = J4     + 1 )

      INTEGER               QO
      PARAMETER           ( QO     = KE     + 1 )

      INTEGER               SO
      PARAMETER           ( SO     = QO     + 1 )

      INTEGER               ER
      PARAMETER           ( ER     = SO     + 1 )

      INTEGER               AE
      PARAMETER           ( AE     = ER     + 1 )

      INTEGER               NGEOCN
      PARAMETER           ( NGEOCN = AE )

C
C     An enumeration of the various components of the
C     elements array---ELEMS
C
C        KNDT20
C        KNDD60
C        KBSTAR
C        KINCL
C        KNODE0
C        KECC
C        KOMEGA
C        KMO
C        KNO
C
      INTEGER               KNDT20
      PARAMETER           ( KNDT20 = START  + 1 )

      INTEGER               KNDD60
      PARAMETER           ( KNDD60 = KNDT20 + 1 )

      INTEGER               KBSTAR
      PARAMETER           ( KBSTAR = KNDD60 + 1 )

      INTEGER               KINCL
      PARAMETER           ( KINCL  = KBSTAR + 1 )

      INTEGER               KNODE0
      PARAMETER           ( KNODE0 = KINCL  + 1 )

      INTEGER               KECC
      PARAMETER           ( KECC   = KNODE0 + 1 )

      INTEGER               KOMEGA
      PARAMETER           ( KOMEGA = KECC   + 1 )

      INTEGER               KMO
      PARAMETER           ( KMO    = KOMEGA + 1 )

      INTEGER               KNO
      PARAMETER           ( KNO    = KMO    + 1 )

      INTEGER               KEPOCH
      PARAMETER           ( KEPOCH = KNO    + 1 )

      INTEGER               NELEMS
      PARAMETER           ( NELEMS = KEPOCH )

C
C     The next set of parameters govern how many items will
C     go into a segment.
C
      INTEGER               MAXEPC
      PARAMETER           ( MAXEPC = 50000 )

      INTEGER               MAXELM
      PARAMETER           ( MAXELM = NELEMS * MAXEPC )

      INTEGER               EPCRM
      PARAMETER           ( EPCRM  = MAXEPC + 1      )

      INTEGER               ELMRM
      PARAMETER           ( ELMRM  = MAXELM + NELEMS )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      DUPCHK ( LBCELL:MAXEPC )
      DOUBLE PRECISION      ELEMS  ( ELMRM  )
      DOUBLE PRECISION      EPOCHS ( EPCRM  )
      DOUBLE PRECISION      GEOPHS ( NGEOCN )
      DOUBLE PRECISION      BEGINT
      DOUBLE PRECISION      BEGPAD
      DOUBLE PRECISION      ENDPAD
      DOUBLE PRECISION      ENDINT
      DOUBLE PRECISION      SVEPCH
      DOUBLE PRECISION      BSAVEP
      DOUBLE PRECISION      BSAVEL ( NELEMS )
      DOUBLE PRECISION      ESAVEP
      DOUBLE PRECISION      ESAVEL ( NELEMS )

      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               TOTAL

      LOGICAL               EOF
      LOGICAL               FMODEL
      LOGICAL               FOUND
      LOGICAL               PRIORS
      LOGICAL               BEGBOL
      LOGICAL               ENDBOL

      INTEGER               IORDER ( MAXEPC )

      CHARACTER*(FILSIZ)    GEOLST
      CHARACTER*(1)         TYPE

      LOGICAL               DONE
      LOGICAL               BSAVFL
      LOGICAL               ESAVFL

C
C     Save all.
C
      SAVE

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TLE2SPK' )
      END IF


C
C     Assign the size for the set used to check for duplicate records.
C
      CALL SSIZED ( MAXEPC, DUPCHK )
      CALL SCARDD ( 0,      DUPCHK )

C
C     Initialize the Boolean set to true when processing a record set
C     to multiple segments. If true, the code logic performs the S_i >
C     S_i+1 check. Also initialize the saved epoch to DPMIN to avoid
C     compiler errors.
C
      PRIORS = .FALSE.
      SVEPCH = DPMIN()

C
C     Set flags and initial values for saved padding records.
C
      BSAVFL = .FALSE.
      BSAVEP = DPMIN()
      ESAVFL = .FALSE.
      ESAVEP = DPMAX()

C
C     Set times and pads based on COVTYP values. If 'TIME', zero for
C     pads.
C
      BEGBOL = .FALSE.
      ENDBOL = .FALSE.

      IF      ( EQSTR( COVTYP(1), 'TIME' ) ) THEN

         BEGBOL = .TRUE.
         BEGPAD = 0.D0

      ELSE IF ( EQSTR( COVTYP(1), 'PAD' ) ) THEN

         IF ( COVVAL(1) .LT. 0.D0 ) THEN
            CALL SETMSG ( 'Time pads must have positive definite '//
     .                    'value. Begin time pad has value #.'    )
            CALL ERRDP  ( '#', COVVAL(1)                          )
            CALL SIGERR ( 'SPICE(INVALIDVALUE1)'                  )
         END IF

         BEGPAD = COVVAL(1)

      ELSE
         CALL SETMSG ( 'COVTYP(1) must be set to either ''TIME'' ' //
     .                 'or ''PAD''. It was set to ''#''.'          )
         CALL ERRCH  ( '#', COVTYP(1)                              )
         CALL SIGERR ( 'SPICE(INVALIDINPUT1)'                      )
      END IF


      IF      ( EQSTR( COVTYP(2), 'TIME' ) ) THEN

         ENDBOL = .TRUE.
         ENDPAD = 0.D0

      ELSE IF ( EQSTR( COVTYP(2), 'PAD' ) ) THEN

         IF ( COVVAL(2) .LT. 0.D0 ) THEN
            CALL SETMSG ( 'Time pads must have positive definite '//
     .                    'value. End time pad has value #.'      )
            CALL ERRDP  ( '#', COVVAL(2)                          )
            CALL SIGERR ( 'SPICE(INVALIDVALUE2)'                  )
         END IF

         ENDPAD = COVVAL(2)

      ELSE
         CALL SETMSG ( 'COVTYP(2) must be set to either ''TIME'' ' //
     .                 'or ''PAD''. It was set to ''#''.'          )
         CALL ERRCH  ( '#', COVTYP(2)                              )
         CALL SIGERR ( 'SPICE(INVALIDINPUT2)'                      )
      END IF

C
C     Get file name of output file.
C
      CALL DAFHFN ( HANDLE, OUTFN )

C
C     Check center ID.
C
      IF ( CNIDVL .NE. CENTER ) THEN

C
C        The center body is not the Earth; TLE data applies only to
C        earth orbiters. Signal an error then return. KCENID is defined
C        in include file.
C
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN )
         CALL SETMSG ( 'Processing of two-line element data '     //
     .                 'requires the setup file keyword ''#'' to '//
     .                 'be set to #.'                             )
         CALL ERRCH  ( '#', KCENID                                )
         CALL ERRINT ( '#', CENTER                                )
         CALL SIGERR ( 'SPICE(INCONSISTCENTERID)'                 )

      END IF

C
C     Check reference frame.
C
      CALL NAMFRM ( FRNMVL, FRCODE )
      CALL NAMFRM ( FRAME,  FRAMID )

      IF ( FRCODE .NE. FRAMID )  THEN

C
C        The frame is not J2000. Complain. KRFRNM is defined in include
C        file.
C
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN )
         CALL SETMSG ( 'Processing of two-line element data '     //
     .                 'requires the setup file keyword ''#'' to '//
     .                 'be set to ''#''.'                         )
         CALL ERRCH ( '#', KRFRNM                                 )
         CALL ERRCH ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INCONSISTFRAME)'                    )

      END IF

C
C     Convert object ID to string code that allows to chose the object
C     data from input file.
C
      CALL INTSTR ( OBIDVL(1), CODE )

C
C     Initialize CHOSE1 to seven characters, with the TLE line ID as
C     the first character.
C
      CHOSE1 = '1'

C
C     Write the ID string CODE to CHOSE1 so that the last RTRIM(CODE)
C     characters of the CHOSE1(1:7) contain the code.
C
      CHOSE1( 7 - RTRIM(CODE) + 1: 7 ) =  CODE(1:RTRIM(CODE))

C
C     Make another version of this lookup string with the code zero
C     padded on left to five digits because according the TLE spec:
C
C        The catalog number assigned to the object by the US Air Force.
C        The numbers are assigned sequentially as objects are
C        cataloged. Object numbers less then 10000 are always aligned
C        to the right, and padded with zeros or spaces to the left.
C
C     (Note that Ed says that it's not always the case.)
C
      CHOSE2 = CHOSE1
      CALL REPLCH ( CHOSE2(3:7), ' ', '0', CHOSE2(3:7) )

C
C     Read first line from TLE data file
C
      CALL RDTEXT ( INPFN, LINES(1), EOF )

C
C     Read the file until we find the first TLE line for this ID.
C
      DO WHILE ( LINES(1)(1:7) .NE. CHOSE1(1:7) .AND. 
     .           LINES(1)(1:7) .NE. CHOSE2(1:7) .AND. 
     .           .NOT. EOF )

         CALL RDTEXT ( INPFN, LINES(1), EOF )

      END DO

      IF ( EOF ) THEN

C
C        Requested data was not found. Complain.
C
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN )
         CALL SETMSG ( 'No data for the object with NORAD ID # '     //
     .                 'were found in the input two-line element '   //
     .                 'file.'                                       )
         CALL ERRINT ( '#', OBIDVL(1)                                )
         CALL SIGERR ( 'SPICE(NOTLEDATAFOROBJECT)'                   )

      END IF

C
C     Check whether all geophysical constants needed for the SGP4
C     propagation model are present in the kernel pool.
C
      GEOLST = ' '

      CALL DTPOOL ( 'BODY399_J2', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_J2,', 0, GEOLST )
      END IF

      CALL DTPOOL ( 'BODY399_J3', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_J3,', 0, GEOLST )
      END IF

      CALL DTPOOL ( 'BODY399_J4', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_J4,', 0, GEOLST )
      END IF

      CALL DTPOOL ( 'BODY399_KE', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_KE,', 0, GEOLST )
      END IF

      CALL DTPOOL ( 'BODY399_QO', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_QO,', 0, GEOLST )
      END IF

      CALL DTPOOL ( 'BODY399_SO', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_SO,', 0, GEOLST )
      END IF

      CALL DTPOOL ( 'BODY399_ER', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_ER,', 0, GEOLST )
      END IF

      CALL DTPOOL ( 'BODY399_AE', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_AE,', 0, GEOLST )
      END IF

      IF ( GEOLST .NE. ' ' ) THEN

C
C        One of the geophysical constants was not found or wasn't of
C        the right type. Complain.
C
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN )
         CALL SETMSG ( 'The following geophysical constants were '//
     .                 'not provided to the program or their '    //
     .                 'values were not scalar DP numbers: #. '   //
     .                 'Check whether the name of a geophysical ' //
     .                 'constants PCK file was provided in the '  //
     .                 'setup file keyword ''#'', and if so, '    //
     .                 'whether the file contains appropriate '   //
     .                 'values for the keywords listed above. '   )
         CALL ERRCH  ( '#', GEOLST(:RTRIM(GEOLST)-1)              )
         CALL ERRCH  ( '#', PCKFIL                                )
         CALL SIGERR ( 'SPICE(MISSINGGEOCONSTS)'                  )

      END IF

C
C     Fetch the geophysical constants needed for the SGP4 propagation
C     model from the kernel pool.
C
      CALL BODVAR ( 399, 'J2', N, GEOPHS ( J2 ) )
      CALL BODVAR ( 399, 'J3', N, GEOPHS ( J3 ) )
      CALL BODVAR ( 399, 'J4', N, GEOPHS ( J4 ) )
      CALL BODVAR ( 399, 'KE', N, GEOPHS ( KE ) )
      CALL BODVAR ( 399, 'QO', N, GEOPHS ( QO ) )
      CALL BODVAR ( 399, 'SO', N, GEOPHS ( SO ) )
      CALL BODVAR ( 399, 'ER', N, GEOPHS ( ER ) )
      CALL BODVAR ( 399, 'AE', N, GEOPHS ( AE ) )


C
C     Read the next line of the file. It must the second line of 
C     the found TLE record.
C
      CALL RDTEXT (  INPFN, LINES(2), EOF )

      IF (  EOF ) THEN

C
C        Next line does not exist. Complain.
C
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN  )
         CALL SETMSG ( 'Second line of the first two-line element '  //
     .                 'set for object # does not exist'    )
         CALL ERRINT ( '#', OBIDVL(1)                       )
         CALL SIGERR ( 'SPICE(NOSECONDLINE)'                )

      END IF


C
C     Start the loop the read the TLE data. Track the number of TLE's
C     read, I. Set the flag indicating that no segment were written yet
C     (FMODEL) and the flag indicating that there is not need to
C     continue reading the input file because enough data covering the
C     specified time window was collected and written to the file
C     (DONE).
C
      I      = 0
      J      = 1 - NELEMS
      FMODEL = .TRUE.
      DONE   = .FALSE.

      DO WHILE ( .NOT. EOF .AND. .NOT. DONE )

C
C        Increment the record read index.
C
         I = I + 1
         J = J + NELEMS

C
C        Try to process this TLE record. If an error occurs during
C        processing, OK will have value .FALSE. and ERROR will contain
C        a description of the error.
C
         CALL ZZGETELM ( 1950, LINES, EPOCHS(I), ELEMS(J), OK, ERROR(2))

C
C        If we find an error, signal a standard SPICE error.
C
         IF ( .NOT. OK ) THEN

            CALL DAFCLS ( HANDLE )
            CALL DELFIL ( OUTFN )

            CALL SETMSG ( 'Error in TLE set #1. The error reads: #2' )
            CALL ERRINT ( '#1', I         )
            CALL ERRCH  ( '#2', ERROR(2)  )
            CALL SIGERR ( 'SPICE(BADTLE)' )

         END IF

C
C        Check if this is a duplicate record.
C
         IF ( ELEMD ( EPOCHS(I), DUPCHK ) ) THEN

C
C           The EPOCH value exists in the set. Ignore the record and
C           prevent its use for this loop iteration.
C
            I  = I - 1
            J  = J - NELEMS
            OK = .FALSE.

         END IF

C
C        If this record is not duplicate, buffer it or toss it based on
C        time constraints.
C
         IF ( OK ) THEN

            IF      ( BEGBOL .AND. ENDBOL ) THEN
C
C              We have both time constraints. Check this record's time
C              against start and stop times.
C
               IF ( EPOCHS(I) .GT. COVVAL(1) .AND.
     .              EPOCHS(I) .LT. COVVAL(2)       ) THEN

C
C                 This time is between start ands stop times. Leave
C                 this record in the buffer and add its time to the
C                 duplicate check set.
C
                  CALL INSRTD ( EPOCHS(I), DUPCHK )

               ELSE IF ( EPOCHS(I) .LE. COVVAL(1) ) THEN

C
C                 This time is less than or equal to the start time.
C                 For now, set the flag to ignore this record. We will
C                 reset it back if we will find that this is record is 
C                 the first record to be used as pad.
C
                  OK = .FALSE.                  

C
C                 Check if we need to save this record for padding.
C
                  IF ( ( COVVAL(1) - BSAVEP    ) .GT.
     .                 ( COVVAL(1) - EPOCHS(I) )      ) THEN
                     
C
C                    This record is closer to the start time than the
C                    currently saved record. Swap the currently saved
C                    time with this time in the duplicate check set and
C                    reset saved time and elements.
C
                     CALL REMOVD ( BSAVEP,    DUPCHK )
                     CALL INSRTD ( EPOCHS(I), DUPCHK )

                     IF ( .NOT. BSAVFL ) THEN
                        OK = .TRUE.
                     END IF

                     BSAVFL = .TRUE.
                     BSAVEP = EPOCHS(I)
                     CALL MOVED ( ELEMS(J), NELEMS, BSAVEL )

                  END IF

C
C                 Drop this record from the buffer because we only
C                 needed it for padding, which is saved separately.
C
                  I  = I - 1
                  J  = J - NELEMS

               ELSE IF ( EPOCHS(I) .GE. COVVAL(2) ) THEN

C
C                 This time is greater than or equal to the stop time.
C                 For now, set the flag to ignore this record. We will
C                 reset it back if we will find that this is record is 
C                 the first record to be used as pad.
C
                  OK = .FALSE.                  

C
C                 Check if we need to save this record for padding.
C
                  IF ( ( ESAVEP    - COVVAL(2) ) .GT.
     .                 ( EPOCHS(I) - COVVAL(2) )      ) THEN
                     
C
C                    This record is closer to the stop time than the
C                    currently saved record. Swap the currently saved
C                    time with this time in the duplicate check set and
C                    swap saved time and elements.
C
                     CALL REMOVD ( ESAVEP,    DUPCHK )
                     CALL INSRTD ( EPOCHS(I), DUPCHK )

                     IF ( .NOT. ESAVFL ) THEN
                        OK = .TRUE.
                     END IF

                     ESAVFL = .TRUE.
                     ESAVEP = EPOCHS(I)
                     CALL MOVED ( ELEMS(J), NELEMS, ESAVEL )

                  END IF

C
C                 Drop this record from the buffer because we only
C                 needed it for padding which is saved separately.
C
                  I  = I - 1
                  J  = J - NELEMS

               END IF


            ELSE IF ( BEGBOL ) THEN

C
C              We have only start time constraint. Check this record's
C              time against the start time.
C
               IF ( EPOCHS(I) .GT. COVVAL(1) ) THEN

C
C                 This time is greater than the start time. Leave this
C                 record in the buffer and add its time to the
C                 duplicate check set.
C
                  CALL INSRTD ( EPOCHS(I), DUPCHK )

               ELSE

C
C                 This time is less than or equal to the start time.
C                 For now, set the flag to ignore this record. We will
C                 reset it back if we will find that this is record is 
C                 the first record to be used as pad.
C
                  OK = .FALSE.                  

C
C                 Check if we need to save this record for padding.
C
                  IF ( ( COVVAL(1) - BSAVEP    ) .GT.
     .                 ( COVVAL(1) - EPOCHS(I) )      ) THEN
                     
C
C                    This record is closer to the start time than the
C                    currently saved record. Swap the currently saved
C                    time with this time in the duplicate check set and
C                    reset saved time and elements.
C
                     CALL REMOVD ( BSAVEP,    DUPCHK )
                     CALL INSRTD ( EPOCHS(I), DUPCHK )

                     IF ( .NOT. BSAVFL ) THEN
                        OK = .TRUE.
                     END IF

                     BSAVFL = .TRUE.
                     BSAVEP = EPOCHS(I)
                     CALL MOVED  ( ELEMS(J), NELEMS, BSAVEL )

                  END IF

C
C                 Drop this record from the buffer because we only
C                 needed it for padding which is saved separately.
C
                  I  = I - 1
                  J  = J - NELEMS
                  
               END IF

            ELSE IF ( ENDBOL ) THEN

C
C              We have only stop time constraint. Check this record's 
C              time against stop time.
C
               IF ( EPOCHS(I) .LT. COVVAL(2) ) THEN

C
C                 This time is less than the stop time. Leave this
C                 record in the buffer and add its time to the
C                 duplicate check set.
C
                  CALL INSRTD ( EPOCHS(I), DUPCHK )

               ELSE

C
C                 This time is greater than or equal to the stop time.
C                 For now, set the flag to ignore this record. We will
C                 reset it back if we will find that this is record is 
C                 the first record to be used as pad.
C
                  OK = .FALSE.                  

C
C                 Check if we need to save this record for padding.
C
                  IF ( ( ESAVEP    - COVVAL(2) ) .GT.
     .                 ( EPOCHS(I) - COVVAL(2) )      ) THEN
                     
C
C                    This record is closer to the stop time than the
C                    currently saved record. Swap the currently saved
C                    time with this time in the duplicate check set and
C                    reset saved time and elements.
C
                     CALL REMOVD ( ESAVEP,    DUPCHK )
                     CALL INSRTD ( EPOCHS(I), DUPCHK )

                     IF ( .NOT. ESAVFL ) THEN
                        OK = .TRUE.
                     END IF

                     ESAVFL = .TRUE.
                     ESAVEP = EPOCHS(I)
                     CALL MOVED  ( ELEMS(J), NELEMS, ESAVEL )

                  END IF

C
C                 Drop this record from the buffer because we only
C                 needed it for padding which is saved separately.
C
                  I  = I - 1
                  J  = J - NELEMS
                  
               END IF

            ELSE

C
C              We have no time constraints. Leave this record in the 
C              buffer and add its time to the duplicate check set.
C
               CALL INSRTD ( EPOCHS(I), DUPCHK )

            END IF

         END IF

C
C        Set total number of currently buffered records including saved
C        pad.
C
         TOTAL = I

         IF ( BSAVFL ) THEN
            TOTAL = TOTAL + 1 
         END IF

         IF ( ESAVFL ) THEN
            TOTAL = TOTAL + 1 
         END IF

C
C        If we filled up the EPOCHS buffer, then we need to complete
C        this segment. If this TLE set had duplicate time or was
C        outside of specified time boundaries, there is no reason to
C        evaluate this block. Wait for the next iteration of the loop.
C
         IF ( TOTAL .EQ. MAXEPC .AND. OK ) THEN

C
C           If we have padding records, add them to the end of the
C           buffer.
C
            IF ( BSAVFL ) THEN
               I         = I + 1
               J         = J + NELEMS
               EPOCHS(I) = BSAVEP
               CALL MOVED ( BSAVEL, NELEMS, ELEMS(J) )
            END IF

            IF ( ESAVFL ) THEN
               I         = I + 1
               J         = J + NELEMS
               EPOCHS(I) = ESAVEP
               CALL MOVED ( ESAVEL, NELEMS, ELEMS(J) )
            END IF

C
C           The epochs may are out of order. Get an order vector. Use
C           it to sort the EPOCHS vector to ascending order. Apply the
C           same ordering operation to the ELEMS vector.
C
            CALL ORDERD ( EPOCHS, MAXEPC,         IORDER )
            CALL REORDD ( IORDER, MAXEPC,         EPOCHS )
            CALL REORBD ( IORDER, MAXEPC, NELEMS, ELEMS  )

C
C           Sorting complete, check for S_i > S_i+1 issue.
C
            IF ( PRIORS ) THEN

               IF ( SVEPCH .GT. EPOCHS(1) ) THEN

C
C                 Signal an error then stop the application run.
C
                  CALL DAFCLS ( HANDLE )
                  CALL DELFIL ( OUTFN )

                  CALL SETMSG ( 'Current segment ordering indicates a '
     .                      //  'TLE ordering with the property the '
     .                      //  'final epoch of the previous '
     .                      //  'segment''s data represents a '
     .                      //  'date-time after the initial '
     .                      //  'date-time of the current segment''s '
     .                      //  'data. This may occur if the TLE data '
     .                      //  'set has an ordering in descending '
     .                      //  'order of epochs and the number of TLE '
     .                      //  'sets exceeds #.' )
                  CALL ERRINT ( '#', MAXEPC )
                  CALL SIGERR ( 'SPICE(INVALIDTLEORDER)' )

               END IF

            END IF

C
C           Set time coverage for this segment.
C
            IF ( BEGBOL ) THEN

C
C              We have start time. If it is the first segment, set
C              coverage start to that time and reset flag indicating
C              that this is the first segment. If not, set it to the
C              first TLE time.
C
               IF ( FMODEL ) THEN
                  BEGINT = COVVAL(1)
                  FMODEL = .FALSE.
               ELSE
                  BEGINT = EPOCHS(1)
               END IF

            ELSE

C
C              We have start pad. If it is the only segment, set
C              coverage start to the first TLE time minus pad. If not,
C              set it to the first TLE time.
C
               IF ( FMODEL ) THEN
                  BEGINT = EPOCHS(1) - BEGPAD
                  FMODEL = .FALSE.
               ELSE
                  BEGINT = EPOCHS(1)
               END IF

            END IF

C
C           If the time of the last buffered point is greater than or
C           equal to the specified stop time, we set the coverage end
C           time to the specified stop time and consider ourselves
C           done. Otherwise, we set coverage stop to the time of the
C           last point and assume that we will add more segments.
C
            IF ( ENDBOL .AND. EPOCHS(MAXEPC) .GE. COVVAL(2) ) THEN

               ENDINT = COVVAL(2)
               DONE   = .TRUE.
               
            ELSE

               ENDINT = EPOCHS(MAXEPC)
            
            END IF

C
C           Report that we write next (or final) SPK segment.
C
            CALL TOSTDO ( ' '                            )
            IF ( DONE ) THEN
               CALL TOSTDO ( 'Writing final SPK segment ...' )
            ELSE
               CALL TOSTDO ( 'Writing next SPK segment ...' )
            END IF

            CALL SPKW10 ( HANDLE,  OBIDVL(2),  CNIDVL, FRNMVL,
     .                    BEGINT,  ENDINT,     SGIDVL,
     .                    GEOPHS,  MAXEPC,     ELEMS,  EPOCHS )

C
C           We will continue processing of the input data and therefore
C           we need to achieve continuity of the data between segments.
C           To do that, we move one record from the end of the buffer
C           to the beginning of the buffer and reset all indexes
C           correspondingly.
C
            EPOCHS(1) = EPOCHS(MAXEPC)

            PRIORS = .TRUE.
            SVEPCH = EPOCHS(1)

            DO I = 1, NELEMS
               ELEMS(I) = ELEMS( (MAXEPC-1)*10 + I )
            END DO

C
C           Reset duplicate check set and pad records.
C            
            CALL SCARDD ( 0,         DUPCHK )
            CALL INSRTD ( EPOCHS(1), DUPCHK )

C
C           Reset padding flags and values.
C
            BSAVFL = .FALSE.
            BSAVEP = DPMIN()
            ESAVFL = .FALSE.
            ESAVEP = DPMAX()

C
C           Reset epoch and element buffer indexes.
C
            I = 1
            J = 1

         END IF

C
C        Unless we are done, keep reading lines from TLE data file
C        until we find next the TLE set for the requested ID or reach
C        the end of the file.
C
         IF ( .NOT. DONE ) THEN

            CALL RDTEXT ( INPFN, LINES(1), EOF )

            DO WHILE ( LINES(1)(1:7) .NE. CHOSE1(1:7) .AND. 
     .                 LINES(1)(1:7) .NE. CHOSE2(1:7) .AND. 
     .                 .NOT. EOF )

               CALL RDTEXT ( INPFN, LINES(1), EOF )

            END DO

C
C           If not the end of the file, read the second TLE line.
C
            IF ( .NOT. EOF ) THEN

               CALL RDTEXT ( INPFN, LINES(2), EOF )

               IF (  EOF ) THEN

C
C                 Next line does not exist. Complain.
C
                  CALL DAFCLS ( HANDLE )
                  CALL DELFIL ( OUTFN  )
                  CALL SETMSG ( 'Second line of the last two-line '  //
     .                          'element set for object # does not ' //
     .                          'exist'                               )
                  CALL ERRINT ( '#', OBIDVL(1)                        )
                  CALL SIGERR ( 'SPICE(NOSECONDLINE2)'                )

               END IF

            END IF

         END IF

      END DO

C
C     We can be done at this point if the previous segment had all the
C     data needed to cover through the specified stop time. If we are
C     not done, we will write one more segment.
C
      IF ( .NOT. DONE ) THEN

C
C        If we have padding records, add them to the buffer.
C
         IF ( BSAVFL ) THEN
            I         = I + 1
            J         = J + NELEMS
            EPOCHS(I) = BSAVEP
            CALL MOVED ( BSAVEL, NELEMS, ELEMS(J) )
         END IF

         IF ( ESAVFL ) THEN
            I         = I + 1
            J         = J + NELEMS
            EPOCHS(I) = ESAVEP
            CALL MOVED ( ESAVEL, NELEMS, ELEMS(J) )
         END IF

C
C        We either ran out of file, or we ran out of elements. In
C        either case if there are any remaining element sets to be
C        written, now is the time to do it. If we have more than one
C        point we need to take care of an out-of-order data.
C
         IF ( I .GT. 1 ) THEN

C
C           The epochs may are out of order. Get an order vector. Use
C           it to sort the EPOCHS vector to ascending order. Apply the
C           same ordering operation to the ELEMS vector.
C
            CALL ORDERD ( EPOCHS, I,         IORDER )
            CALL REORDD ( IORDER, I,         EPOCHS )
            CALL REORBD ( IORDER, I, NELEMS, ELEMS  )

C           
C           Sorting complete, check for  S_i > S_i+1 issue.
C
            IF ( PRIORS ) THEN

               IF ( SVEPCH .GT. EPOCHS(1) ) THEN

C
C                 Signal an error then stop the application run.
C
                  CALL DAFCLS ( HANDLE )
                  CALL DELFIL ( OUTFN )

                  CALL SETMSG ( 'Final segment ordering indicates a '
     .                      //  'TLE ordering with the property the '
     .                      //  'final epoch of the previous '
     .                      //  'segment''s data represents a '
     .                      //  'date-time after the '
     .                      //  'initial date-time of the final '
     .                      //  'segment''s data. This may occur if '
     .                      //  'the TLE data set has an ordering in '
     .                      //  'descending order of epochs and the '
     .                      //  'number of TLE sets exceeds #.' )
                  CALL ERRINT ( '#', MAXEPC )
                  CALL SIGERR ( 'SPICE(INVALIDTLEORDER)' )

               END IF
               
            END IF

         END IF

C
C        Set the SPK coverage time interval.
C
         IF ( BEGBOL ) THEN

C
C           We have start time. If it is the only segment, set coverage
C           start to that time. If not, set it to the first TLE time.
C
            IF ( FMODEL ) THEN
               BEGINT = COVVAL(1)
            ELSE
               BEGINT = EPOCHS(1)
            END IF

         ELSE

C
C           We have start pad. If it is the only segment, set coverage
C           start to the first TLE time minus pad. If not, set it to
C           the first TLE time.
C
            IF ( FMODEL ) THEN
               BEGINT = EPOCHS(1) - BEGPAD
            ELSE
               BEGINT = EPOCHS(1)
            END IF

         END IF


         IF ( ENDBOL ) THEN

C
C           We have stop time. Set coverage stop to that time.
C
            ENDINT   = COVVAL(2)

         ELSE

C
C           We have stop pad. Set coverage stop to the last TLE time
C           minus pad.
C
            ENDINT   = EPOCHS(I) + ENDPAD

         END IF

C
C        With coverage start and stop in hand we can check if they make
C        sense (i.e. start is earlier than stop). If they don't, we
C        will generate an error that will be more meaningful than the
C        error generated by a similar check in SPKW10.
C
         IF ( BEGINT .GE. ENDINT ) THEN

C
C           There is one case when this will not be an error. That is
C           when we already wrote at least one segment and ended up
C           here with a single point and zero pads in hand or a single
C           point exactly matching the stop time. In this case we don't
C           need to write another segment and can consider ourselves
C           done.
C
            IF ( .NOT. FMODEL ) THEN

               IF ( BEGINT .EQ. ENDINT ) THEN

                  DONE = .TRUE. 

               ELSE
                  CALL SETMSG ( 'There is a bug in the program. ' //
     .                          'Please contact NAIF.'            )
                  CALL SIGERR ( 'SPICE(MKSPKTLE2SPKBUG0)'         )
               END IF

            ELSE

C
C              Check what happened case by case based on what times
C              and pads were given in the setup file. No matter what
C              happened, there was an error so we need close and delete
C              the output SPK.
C
               CALL DAFCLS ( HANDLE )
               CALL DELFIL ( OUTFN )

               IF ( .NOT. BEGBOL .AND. .NOT. ENDBOL ) THEN

C                 
C                 No start or stop time was specified. In such cases
C                 the only real way to get here is if we had one input
C                 point and both pads were zero. Check that this is the
C                 case and, if so, report an error. Otherwise report a
C                 bug.
C
                  IF ( I      .EQ. 1    .AND. 
     .                 BEGPAD .EQ. 0.D0 .AND.  
     .                 ENDPAD .EQ. 0.D0       ) THEN
                     
                     CALL SETMSG ( 'Both TLE pads cannot be zero ' //
     .                             'when input TLE file contains ' //
     .                             'only one distinct record for ' //
     .                             'the specified object.'         )
                     CALL SIGERR ( 'SPICE(BADTLEPADS)'             )

                  ELSE

                     CALL SETMSG ( 'There is a bug in the program. ' //
     .                             'Please contact NAIF.'            )
                     CALL SIGERR ( 'SPICE(MKSPKTLE2SPKBUG1)'         )

                  END IF

               ELSE IF ( .NOT. BEGBOL ) THEN

C                 
C                 Only stop time was specified. In such cases the only
C                 real way to get here is if this stop time is less
C                 than or equal to start time determined from the first
C                 TLE record and default or user-specified pad. In such
C                 cases we must have buffered at most 2 records. Check
C                 that this is the case and, if so, report an error.
C                 Otherwise report a bug.
C
                  IF ( I .LE. 2 ) THEN
                     
                     CALL SETMSG ( 'Stop time, # (#) TDB, specified ' //
     .                             'in the setup file is earlier '    //
     .                             'than or equal to the '            //
     .                             'the start time, # (#) TDB, '      //
     .                             'computed from the time, # (#) '   //
     .                             'TDB, of the '                     //
     .                             'first applicable TLE record for ' //
     .                             'the specified object '            //
     .                             'and default or specified pad, # ' //
     .                             'seconds.'                         )
                     CALL ETCAL  ( ENDINT, HWORD                      )
                     CALL ERRDP  ( '#', ENDINT                        )
                     CALL ERRCH  ( '#', HWORD                         )
                     CALL ETCAL  ( BEGINT, HWORD                      )
                     CALL ERRDP  ( '#', BEGINT                        )
                     CALL ERRCH  ( '#', HWORD                         )
                     CALL ETCAL  ( EPOCHS(1), HWORD                   )
                     CALL ERRDP  ( '#', EPOCHS(1)                     )
                     CALL ERRCH  ( '#', HWORD                         )
                     CALL ERRDP  ( '#', BEGPAD                        )
                     CALL SIGERR ( 'SPICE(BADSTOPTIME)'               )

                  ELSE

                     CALL SETMSG ( 'There is a bug in the program. ' //
     .                             'Please contact NAIF.'             )
                     CALL SIGERR ( 'SPICE(MKSPKTLE2SPKBUG2)'          )

                  END IF

               ELSE IF ( .NOT. ENDBOL ) THEN

C                 
C                 Only start time was specified. In such cases the only
C                 real way to get here is if this start time is greater
C                 than or equal to stop time determined from the last
C                 TLE record and default or user-specified pad. In such
C                 cases we must have buffered at most 2 records. Check
C                 that this is the case and, if so, report an error.
C                 Otherwise report a bug.
C
                  IF ( I .LE. 2 ) THEN
                     
                     CALL SETMSG ( 'Start time, # (#) TDB, specified '//
     .                             'in the setup file is later '      //
     .                             'than or equal to the '            //
     .                             'the stop time, # (#) TDB, '       //
     .                             'computed from the time, # (#) '   //
     .                             'TDB, of the '                     //
     .                             'last applicable TLE record for '  //
     .                             'the specified object '            //
     .                             'and default or specified pad, # ' //
     .                             'seconds.'                         )
                     CALL ETCAL  ( BEGINT, HWORD                      )
                     CALL ERRDP  ( '#', BEGINT                        )
                     CALL ERRCH  ( '#', HWORD                         )
                     CALL ETCAL  ( ENDINT, HWORD                      )
                     CALL ERRDP  ( '#', ENDINT                        )
                     CALL ERRCH  ( '#', HWORD                         )
                     CALL ETCAL  ( EPOCHS(1), HWORD                   )
                     CALL ERRDP  ( '#', EPOCHS(1)                     )
                     CALL ERRCH  ( '#', HWORD                         )
                     CALL ERRDP  ( '#', ENDPAD                        )
                     CALL SIGERR ( 'SPICE(BADSTARTTIME)'              )

                  ELSE

                     CALL SETMSG ( 'There is a bug in the program. ' //
     .                             'Please contact NAIF.'             )
                     CALL SIGERR ( 'SPICE(MKSPKTLE2SPKBUG3)'          )

                  END IF

               ELSE

C
C                 We can get here only if both start and stop times
C                 were specified. But in this cases, they should have
C                 been checked them to be OK before calling this
C                 routine. So if we got here, it is a bug.
C
                  CALL SETMSG ( 'There is a bug in the program. ' //
     .                          'Please contact NAIF.'            )
                  CALL SIGERR ( 'SPICE(MKSPKTLE2SPKBUG4)'         )

               END IF

            END IF

         END IF

C
C        Write the last segment, but only if we need to.
C
         IF ( .NOT. DONE ) THEN

C
C           Write final SPK segment.
C
            CALL TOSTDO ( ' '                             )
            CALL TOSTDO ( 'Writing final SPK segment ...' )
            
            CALL SPKW10 ( HANDLE,  OBIDVL(2),  CNIDVL, FRNMVL,
     .                    BEGINT,  ENDINT,     SGIDVL,
     .                    GEOPHS,  I,          ELEMS,  EPOCHS )

         END IF

      END IF

C
C     Close input TLE file.
C
      CALL CLTEXT ( INPFN )

C
C     All done.
C
      CALL CHKOUT ( 'TLE2SPK' )

      END
