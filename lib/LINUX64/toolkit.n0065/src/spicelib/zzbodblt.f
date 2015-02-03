C$Procedure ZZBODBLT ( Private --- Retrieve Built-In Body-Code Maps )
 
      SUBROUTINE ZZBODBLT ( ROOM,  NAMES,  NORNAM, CODES,
     .                      NVALS, DEVICE, REQST          )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the umbrella routine that contains entry points to
C     access the built-in body name-code mappings.
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
C     BODY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzbodtrn.inc'
 
      INTEGER               ROOM
      CHARACTER*(*)         NAMES  ( * )
      CHARACTER*(*)         NORNAM ( * )
      INTEGER               CODES  ( * )
      INTEGER               NVALS
      CHARACTER*(*)         DEVICE
      CHARACTER*(*)         REQST
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ROOM       I   ZZBODGET
C     NAMES      O   ZZBODGET
C     NORNAM     O   ZZBODGET
C     CODES      O   ZZBODGET
C     NVALS      O   ZZBODGET
C     DEVICE     I   ZZBODLST
C     REQST      I   ZZBODLST
C
C$ Detailed_Input
C
C     See the entry points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the entry points for a discussion of their arguments.
C
C$ Parameters
C
C     See the include file 'zzbodtrn.inc' for the list of parameters
C     this routine utilizes.
C
C$ Exceptions
C
C     1) The error SPICE(BOGUSENTRY) is signaled if ZZBODBLT is
C        called directly.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODBLT should never be called directly, instead access
C     the entry points:
C
C        ZZBODGET      Fetch the built-in body name/code list.
C
C        ZZBODLST      Output the name-ID mapping list.
C
C$ Examples
C
C     See ZZBODTRN and its entry points for details.
C
C$ Restrictions
C
C     1) No duplicate entries should appear in the built-in
C        BLTNAM list.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.3.1, 27-FEB-2007 (EDW)
C
C        Completed the ZZBODLST decalrations section.
C
C-    SPICELIB Version 2.3.0, 17-MAR-2003 (EDW)
C
C        Added a call to ZZIDMAP to retrieve the default
C        mapping list. "zzbodtrn.inc" no longer
C        contains the default mapping list.
C
C-    SPICELIB Version 2.2.0  21-FEB-2003 (BVS)
C
C        Changed MER-A and MER-B to MER-1 and MER-2.
C
C-    SPICELIB Version 2.1.0  04-DEC-2002 (EDW)
C
C       Added new assignments to the default collection:
C
C       -226     ROSETTA
C        517     CALLIRRHOE
C        518     THEMISTO
C        519     MAGACLITE
C        520     TAYGETE
C        521     CHALDENE
C        522     HARPALYKE
C        523     KALYKE
C        524     IOCASTE
C        525     ERINOME
C        526     ISONOE
C        527     PRAXIDIKE
C
C-    SPICELIB Version 2.0.0, 23-AUG-2002 (FST)
C
C        Initial release.  This begins at Version 2.0.0 because
C        the entry point ZZBODLST was cut out of ZZBODTRN and
C        placed here at Version 1.0.0.
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 23-AUG-2002 (FST)
C
C        The entries following this one were copied from
C        the version section of ZZBODTRN.  SPICELIB has
C        been changed to ZZBODTRN for convenience in noting
C        version information relevant for that module.
C
C        This was done to carry the history of body name-code
C        additions with this new umbrella.
C
C        Added to the collection:
C        -236   MESSENGER
C
C-    ZZBODTRN Version 3.2.0, 14-AUG-2002 (EDW)
C
C        Added the ZZBODKIK entry point.
C
C        Moved the NAIF_BODY_NAME/CODE to subroutine
C        ZZBODKER. No change in logic.
C
C        Added logic to enforce the precedence masking;
C        logic removes duplicate assignments of ZZBODDEF.
C        Removed the NAMENOTUNIQUE error block.
C
C-    ZZBODTRN Version 3.1.5, 27-NOV-2001 (EDW)
C
C        Added to the collection:
C        -200   CONTOUR
C        -146   LUNAR-A
C        -135   DRTS-W
C
C        Added the subroutine ZZBODLST as an entry point.
C        The routine outputs the current name-ID mapping
C        list to some output device.
C
C-    ZZBODTRN Version 3.1.0, 17-OCT-2001 (EDW)
C
C        To improve clarity, the BEGXX block initialization now
C        exists in the include file zzbodtrn.inc.
C
C        Removed the comments concerning the 851, 852, ... temporary
C        codes.
C
C        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
C        as a DATA statement.
C
C        Edited headers to match information in naif_ids required
C        reading.
C
C        Edited headers, removed typos and bad grammar, clarified
C        descriptions.
C
C        Added to the collection
C        -41    MARS EXPRESS, MEX
C        -44    BEAGLE 2, BEAGLE2
C        -70    DEEP IMPACT IMPACTOR SPACECRAFT
C        -94    MO, MARS OBSERVER
C        -140   DEEP IMPACT FLYBY SPACECRAFT
C        -172   SLCOMB, STARLIGHT COMBINER
C        -205   SLCOLL, STARLIGHT COLLECTOR
C        -253   MER-A
C        -254   MER-B
C
C        Corrected typo, vehicle -188 should properly be MUSES-C,
C        previous versions listed the name as MUSES-B.
C
C        Removed from collection
C        -84    MARS SURVEYOR 01 LANDER
C        -154   EOS-PM1
C        -200   PLUTO EXPRESS 1, PEX1
C        -202   PLUTO EXPRESS 2, PEX2
C
C-    ZZBODTRN Version 3.0.0, 29-MAR-2000 (WLT)
C
C        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
C        ID coded for Pluto Express were removed.  The ID codes
C        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
C        and Contour were added.
C
C-    ZZBODTRN Version 2.0.0, 26-JAN-1998 (EDW)
C
C        The Galileo probe ID -228 replaces the incorrect ID -344.
C        DSS stations 5 through 65 added to the collection.
C
C        Added to the collection
C        -107   TROPICAL RAINFALL MEASURING MISSION, TRMM
C        -154,  EOS-PM1
C        -142   EOS-AM1
C        -151   AXAF
C        -1     GEOTAIL
C        -13    POLAR
C        -21    SOHO
C        -8     WIND
C        -25    LUNAR PROSPECTOR, LPM
C        -116   MARS POLAR LANDER, MPL
C        -127   MARS CLIMATE ORBITER, MCO
C        -188   MUSES-C
C        -97    TOPEX/POSEIDON
C        -6     PIONEER-6, P6
C        -7     PIONEER-7, P7
C        -20    PIONEER-8, P8
C        -23    PIONEER-10, P10
C        -24    PIONEER-11, P11
C        -178   NOZOMI, PLANET-B
C        -79    SPACE INFRARED TELESCOPE FACILITY, SIRTF
C        -29    STARDUST, SDU
C        -47    GENESIS
C        -48    HUBBLE SPACE TELESCOPE, HST
C        -200   PLUTO EXPRESS 1, PEX1
C        -202   PLUTO EXPRESS 2, PEX2
C        -164   YOHKOH, SOLAR-A
C        -165   MAP
C        -166   IMAGE
C        -53    MARS SURVEYOR 01 ORBITER
C         618   PAN
C         716   CALIBAN
C         717   SYCORAX
C        -30    DS-1 (low priority)
C        -58    HALCA
C        -150   HUYGEN PROBE, CASP
C        -55    ULS
C
C        Modified ZZBODC2N and ZZBODN2C so the user may load an
C        external IDs kernel to override or supplement the standard
C        collection.  The kernel must be loaded prior a call to
C        ZZBODC2N or ZZBODN2C.
C
C-    ZZBODTRN Version 1.1.0, 22-MAY-1996 (WLT)
C
C        Added the id-code for Comet Hyakutake, Comet Hale-Bopp,
C        Mars 96, Cassini Simulation, MGS Simulation.
C
C-    ZZBODTRN Version 1.0.0, 25-SEP-1995 (BVS)
C
C        Renamed umbrella subroutine and entry points to
C        correspond private routine convention (ZZ...). Added IDs for
C        tracking stations Goldstone (399001), Canberra (399002),
C        Madrid (399003), Usuda (399004).
C
C-    ZZBODTRN Version 2.2.0, 01-AUG-1995 (HAN)
C
C        Added the IDs for Near Earth Asteroid Rendezvous (-93),
C        Mars Pathfinder (-53), Ulysses (-55), VSOP (-58),
C        Radioastron (-59), Cassini spacecraft (-82), and Cassini
C        Huygens probe (-150).
C        Mars Observer (-94) was replaced with Mars Global
C        Surveyor (-94).
C
C-    ZZBODTRN Version 2.1.0, 15-MAR-1995 (KSZ) (HAN)
C
C        Two Shoemaker Levy 9 fragments were added, Q1 and P2
C        (IDs 50000022 and 50000023). Two asteroids were added,
C        Eros and Mathilde (IDs 2000433 and 2000253). The
C        Saturnian satellite Pan (ID 618) was added.
C
C-    ZZBODTRN Version 2.0.0, 03-FEB-1995 (NJB)
C
C        The Galileo probe (ID -344) has been added to the permanent
C        collection.
C
C-    ZZBODTRN Version 1.0.0, 29-APR-1994 (MJS)
C
C        SPICELIB symbol tables are no longer used. Instead, two order
C        vectors are used to index the NAMES and CODES arrays. Also,
C        this version does not support reading body name ID pairs from a
C        file.
C
C-    ZZBODTRN Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    ZZBODTRN Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C-    ZZBODTRN Version 1.0.0,  7-MAR-1991 (WLT)
C
C       Some items previously considered errors were removed
C       and some minor modifications were made to improve the
C       robustness of the routines.
C
C-    ZZBODTRN Version 1.0.0, 28-JUN-1990 (JEM)
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               LASTNB
 
      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local Parameters
C
 
C
C     Local Variables
C
      CHARACTER*(MAXL)      BLTNAM ( NPERM )
      CHARACTER*(MAXL)      BLTNOR ( NPERM )
      CHARACTER*(4)         ZZRQST
      CHARACTER*(MAXL)      ZZINT
      CHARACTER*(2*MAXL+3)  ZZLINE
 
      INTEGER               BLTCOD ( NPERM )
      INTEGER               I
      INTEGER               ZZOCOD ( NPERM )
      INTEGER               ZZONAM ( NPERM )
 
      LOGICAL               FIRST
 
C
C     Saved Variables
C
      SAVE                  BLTCOD
      SAVE                  BLTNAM
      SAVE                  BLTNOR
      SAVE                  FIRST
 
C
C     Data Statements
C
      DATA                  FIRST     / .TRUE. / 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZBODBLT'          )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'ZZBODBLT'          )
      END IF
 
      RETURN
 
 
 
C$Procedure ZZBODGET ( Private --- Body-Code Get Built-In List )
 
      ENTRY ZZBODGET ( ROOM, NAMES, NORNAM, CODES, NVALS )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Retrieve a copy of the built-in body name-code mapping lists.
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
C     BODY
C
C$ Declarations
C
C     INTEGER               ROOM
C     CHARACTER*(*)         NAMES  ( * )
C     CHARACTER*(*)         NORNAM ( * )
C     INTEGER               CODES  ( * )
C     INTEGER               NVALS
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ROOM       I   Space available in NAMES, NORNAM, and CODES.
C     NAMES      O   Array of built-in body names.
C     NORNAM     O   Array of normalized built-in body names.
C     CODES      O   Array of built-in ID codes for NAMES/NORNAM.
C     NVALS      O   Length of NAMES, NORNAM, CODES, and ORDNOM arrays.
C
C$ Detailed_Input
C
C     ROOM       is the maximum number of entries that NAMES, NORNAM,
C                and CODES may receive.
C
C$ Detailed_Output
C
C     NAMES      the array of built-in names.  This array is parallel
C                to NORNAM and CODES.
C
C     NORNAM     the array of normalized built-in body names.  This
C                array is computed from the NAMES array by compressing
C                groups of spaces into a single space, left-justifying
C                the name, and uppercasing the letters.
C
C     CODES      the array of built-in codes associated with NAMES
C                and NORNAM entries.
C
C     NVALS      the number of items returned in NAMES, NORNAM,
C                and CODES.
C
C$ Parameters
C
C     NPERM      the number of permanent, or built-in, body name-code
C                mappings.
C
C$ Exceptions
C
C     1) SPICE(BUG) is signaled if ROOM is less than NPERM, the
C        amount of space required to store the entire list of
C        body names and codes.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine simply copies it's local buffered version of the
C     built-in name-code mappings to the output arguments.
C
C$ Examples
C
C     See ZZBODTRN for sample usage.
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
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 17-MAR-2003 (EDW)
C
C        Added a call to ZZIDMAP to retrieve the default
C        mapping list. "zzbodtrn.inc" no longer
C        contains the default mapping list.
C
C-    SPICELIB Version 2.0.0, 23-AUG-2002 (FST)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODGET' )
      END IF


C
C     On the first invocation compute the normalized forms of BLTNAM
C     and store them in BLTNOR.
C
      IF ( FIRST ) THEN

C
C        Retrieve the default mapping list.
C
         CALL ZZIDMAP( BLTCOD, BLTNAM )

         DO I = 1, NPERM            
            CALL LJUST  (         BLTNAM(I), BLTNOR(I) )
            CALL UCASE  (         BLTNOR(I), BLTNOR(I) )
            CALL CMPRSS ( ' ', 1, BLTNOR(I), BLTNOR(I) )
         END DO
 
C
C        Do not do this again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Copy the contents of BLTNAM, BLTNOR, and BLTCOD to the output
C     arguments, but only if there is sufficient room.
C
      IF ( ROOM .LT. NPERM ) THEN
 
         CALL SETMSG ( 'Insufficient room to copy the stored '
     .   //            'body name-code mappings to the output '
     .   //            'arguments.  Space required is #, but '
     .   //            'the caller supplied #.'                 )
         CALL ERRINT ( '#', NPERM                               )
         CALL ERRINT ( '#', ROOM                                )
         CALL SIGERR ( 'SPICE(BUG)'                             )
         CALL CHKOUT ( 'ZZBODGET'                               )
         RETURN
 
      END IF
 
      CALL MOVEC ( BLTNAM, NPERM, NAMES  )
      CALL MOVEC ( BLTNOR, NPERM, NORNAM )
      CALL MOVEI ( BLTCOD, NPERM, CODES  )
 
      NVALS = NPERM
 
      CALL CHKOUT ( 'ZZBODGET' )
      RETURN
 
 
 
 
C$Procedure ZZBODLST ( Output permanent collection to some device. )
 
      ENTRY ZZBODLST ( DEVICE, REQST )
 
C$ Abstract
C
C     Output the complete list of built-in body/ID mappings to
C     some output devide. Thw routine generates 2 lists: one
C     sorted by ID number, one sorted by name.
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
C     NONE.
C
C$ Keywords
C
C     BODY
C
C$ Declarations
C
C      CHARACTER*(*)         DEVICE
C      CHARACTER*(*)         REQST
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     DEVICE     I   Device name to receive the output.
C     REQST      I   Data list name to output.
C
C$ Detailed_Input
C
C     DEVICE         identifies the device to receive the
C                    body/ID mapping list. WRLINE performs the
C                    output function and so DEVICE may have
C                    the values 'SCREEN' (to generate a screen dump),
C                    'NULL' (do nothing), or a device name (a
C                    file, or any other name valid in a FORTRAN OPEN
C                    statement).
C
C     REQST          A case insensitive string indicating the data
C                    set to output. REQST may have the value 'ID',
C                    'NAME', or 'BOTH'. 'ID' outputs the name/ID mapping
C                    ordered by ID number from least to highest value.
C                    'NAME' outputs the name/ID mapping ordered by ASCII
C                    sort on the name string. 'BOTH' outputs both
C                    ordered lists.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The entry point outputs ordered lists of the name/ID mappings
C     defined in ZZBODTRN.
C
C$ Examples
C
C     1. Write both sorted lists to screen.
C
C     PROGRAM X
C
C     CALL ZZBODLST( 'SCREEN', 'BOTH' )
C
C     END
C
C     2. Write an ID number sorted list to a file, "body.txt".
C
C     PROGRAM X
C
C     CALL ZZBODLST( 'body.txt', 'ID' )
C
C     END
C
C With SCREEN output of the form:
C
C   Total number of name/ID mappings: 414
C
C   ID to name mappings.
C   -550                                 | M96
C   -550                                 | MARS 96
C   -550                                 | MARS-96
C   -550                                 | MARS96
C   -254                                 | MER-2
C   -253                                 | MER-1
C
C     ..                                   ..
C
C   50000020                             | SHOEMAKER-LEVY 9-B
C   50000021                             | SHOEMAKER-LEVY 9-A
C   50000022                             | SHOEMAKER-LEVY 9-Q1
C   50000023                             | SHOEMAKER-LEVY 9-P2
C
C   Name to ID mappings.
C   1978P1                               | 901
C   1979J1                               | 515
C
C     ..                                   ..
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
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.1, 27-FEB-2007 (EDW)
C
C        Completed the ZZBODLST declarations section.
C
C-    SPICELIB Version 2.1.0, 17-MAR-2003 (EDW)
C
C        Added a call to ZZIDMAP to retrieve the default
C        mapping list. "zzbodtrn.inc" no longer
C        contains the default mapping list.
C
C-    SPICELIB Version 2.0.0, 23-AUG-2002 (FST)
C
C        This entry point was moved into ZZBODBLT and some
C        variable names were changed to refer to variables
C        in the umbrella.
C
C-    SPICELIB Version 1.0.0, 26-NOV-2001 (EDW)
C
C-&
 
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODLST' )
      END IF

C
C     Upper case the ZZRQST value.
C 
      CALL UCASE  ( REQST, ZZRQST )
      CALL INTSTR ( NPERM, ZZINT  )
 
      ZZLINE = 'Total number of name/ID mappings: '//ZZINT
 
      CALL WRLINE ( DEVICE, ZZLINE(1: LASTNB( ZZLINE ) ) )

C
C     Retrieve the current set of name/ID mappings
C
      CALL ZZIDMAP( BLTCOD, BLTNAM )
 
C
C      Branch as defined by the value of ZZRQST. 'ID' or 'BOTH'.
C
      IF ( EQSTR( ZZRQST , 'ID') .OR. EQSTR( ZZRQST , 'BOTH') ) THEN
 
         CALL ORDERI ( BLTCOD, NPERM, ZZOCOD )
 
         CALL WRLINE ( DEVICE, ' ' )
         CALL WRLINE ( DEVICE, 'ID to name mappings.' )
 
         DO I = 1, NPERM

            CALL INTSTR ( BLTCOD(ZZOCOD(I)) , ZZINT )
 
            ZZLINE = ZZINT // ' | ' // BLTNAM(ZZOCOD(I))
 
            CALL WRLINE ( DEVICE, ZZLINE(1: LASTNB( ZZLINE ) ) )
 
         END DO
 
      END IF
 
C
C     ... 'NAME' or 'BOTH'.
C
      IF ( EQSTR( ZZRQST, 'NAME') .OR. EQSTR( ZZRQST, 'BOTH') ) THEN
 
         CALL ORDERC ( BLTNAM, NPERM, ZZONAM )
 
         CALL WRLINE ( DEVICE, ' ' )
         CALL WRLINE ( DEVICE, 'Name to ID mappings.' )
 
         DO I = 1, NPERM
 
            CALL INTSTR ( BLTCOD(ZZONAM(I)) , ZZINT )
 
            ZZLINE = BLTNAM(ZZONAM(I)) // ' | ' // ZZINT
 
            CALL WRLINE ( DEVICE, ZZLINE(1:LASTNB( ZZLINE ) ) )
 
         END DO
 
      END IF
 
      CALL CHKOUT ( 'ZZBODLST' )
      RETURN
      
      END

