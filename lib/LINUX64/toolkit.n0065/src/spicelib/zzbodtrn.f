C$Procedure ZZBODTRN ( Private --- Body name and code translation )
 
      SUBROUTINE ZZBODTRN ( NAME, CODE, FOUND, USRCTR, UPDATE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the umbrella routine that contains entry points to
C     translate between body names and NAIF integer codes, and
C     for definition of new name/code pairs.
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
C     NAIF_IDS
C
C$ Keywords
C
C     BODY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE               'zzbodtrn.inc'
      INCLUDE               'zzctr.inc'
 
      CHARACTER*(*)         NAME
      INTEGER               CODE
      LOGICAL               FOUND
      INTEGER               USRCTR    ( CTRSIZ )
      LOGICAL               UPDATE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME      I/O  ZZBODN2C, ZZBODDEF, ZZBODC2N
C     CODE      I/O  ZZBODC2N, ZZBODDEF, ZZBODN2C
C     FOUND      O   ZZBODN2C and ZZBODC2N
C     USRCTR    I/O  ZZBCTRCK
C     UPDATE     O   ZZBCTRCK
C     MAXL       P   (All)
C     MAXP       P   ZZBODDEF
C     NPERM      P   (All)
C     MAXE       P   (All)
C     NROOM      P   (All)
C     CTRSIZ     P   (All)
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
C     These parameters are defined in zzbodtrn.inc:
C
C     MAXL        is the maximum length of a body name.
C
C     MAXP        is the maximum number of additional names that may
C                 be added via the ZZBODDEF interface.
C
C     NPERM       is the count of the mapping assignments built into
C                 SPICE.
C 
C     MAXE        is the size of the lists and hashes storing combined
C                 built-in and ZZBODDEF-defined name/ID mappings. To
C                 ensure efficient hashing this size is the set to the
C                 first prime number greater than ( MAXP + NPERM ).
C
C     NROOM       is the size of the lists and hashes storing the
C                 POOL-defined name/ID mappings. To ensure efficient
C                 hashing and to provide the ability to store nearly as
C                 many names as can fit in the POOL, this size is
C                 set to the first prime number less than MAXLIN
C                 defined in the POOL umbrella routine.
C
C     This parameter is defined in zzctr.inc:
C
C     CTRSIZ     is the dimension of the counter array used by various
C                SPICE subsystems to identify changes in their states.
C
C$ Exceptions
C
C     1) The error SPICE(BOGUSENTRY) is signaled if ZZBODTRN
C        is called directly.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODTRN should never be called, instead access the entry
C     points:
C
C        ZZBODN2C      Body name to code
C
C        ZZBODC2N      Body code to name
C
C        ZZBODDEF      Body name/code definition
C
C        ZZBODKIK      Force an examination of the kernel pool
C                      variables, subsequent processing and
C                      the generation of any error messages
C                      resultant from the processing.
C
C        ZZBODRST      Reset the mappings provided via the ZZBODDEF
C                      interface.
C
C        ZZBCTRCK      Check and, if needed, update the caller's copy
C                      of the ZZBODTRN state counter.
C
C     ZZBODN2C and ZZBODC2N perform translations between body names
C     and their corresponding integer codes used in SPK and PCK files
C     and associated routines.  A default set of name/code
C     pairs are automatically defined during the first call to
C     any of the entry points.  Additional name/code pairs may
C     be defined via ZZBODDEF for two purposes:
C
C        1) to associate another, perhaps more familiar or
C           abbreviated name with a previously defined body
C           integer code
C
C        2) to define a new body integer code and name
C
C     Each body name maps to a unique integer code, but more than
C     one name may map to a code.  Associating more than one
C     integer code with a particular name creates ambiguity.
C     Therefore the name-code mapping system establishes a
C     clearly defined precedence structure that assures at any
C     given instant only one code is assigned to a particular
C     name.
C
C     Entries provided via the kernel pool variables are examined
C     first to resolve name-code mappings.  The last listed entries
C     in the kernel pool arrays NAIF_BODY_CODE and NAIF_BODY_NAME
C     resolve any ambiguities that occur.  For example, consider
C     the following text kernel excerpt:
C
C        \begindata
C
C           NAIF_BODY_NAME += 'NAME'
C           NAIF_BODY_CODE += 1000
C
C           NAIF_BODY_NAME += 'NAME'
C           NAIF_BODY_CODE += 1001
C
C        \begintext
C
C     If, after loading this kernel, the following calls are made:
C
C        CALL ZZBODN2C ( 'NAME', CODE,  NAMFND )
C
C        CALL ZZBODC2N ( 1000,   NAME0, FND000 )
C        CALL ZZBODC2N ( 1001,   NAME1, FND001 )
C
C     The values of CODE, NAMFND, NAME0, FND000, NAME1, and FND001
C     will be:
C
C        NAMFND = .TRUE.,  CODE  = 1001
C        FND000 = .FALSE., NAME0 remains unchanged
C        FND001 = .TRUE.,  NAME1 = 'NAME'
C
C     FND000 is .FALSE., because this name-code mapping is masked
C     by the higher precedent 'NAME' <-> 1001 mapping.
C
C     If the name-code mapping is not resolved by the entries
C     provided in the kernel pool, the values assigned via the
C     ZZBODDEF interface are examined next.  As with the kernel
C     pool, the last assignment made via the ZZBODDEF interface
C     has the highest precedence.  Lastly, if the name-code
C     mapping is not resolved by the contents of ZZBODDEF, the
C     built-in mappings are examined.  In actuality, the built-in
C     mappings represent an initial state of the ZZBODDEF listings.
C     As changes are made to this listing, the original mappings
C     are discarded.
C
C     For the case in which multiple names map to a single code, a
C     ZZBODC2N call returns the name last assigned to that code - a
C     LIFO situation.
C
C$ Examples
C
C     1) The following code fragment shows SPKEZ compute the state
C        (position and velocity) of Jupiter as seen from the Galileo
C        Orbiter.  It requires the NAIF integer codes of the target
C        and observer, so we use ZZBODN2C to convert names to integer
C        codes for those bodies.
C
C           CALL ZZBODN2C ( 'JUPITER',         TARGET, FOUND )
C
C           CALL ZZBODN2C ( 'GALILEO ORBITER', OBSRVR, FOUND )
C
C           CALL SPKEZ    ( TARGET, EPOCH, FRAME, ABCORR,
C          .                OBSRVR, STATE, LT             )
C
C
C     2) This example assumes ZZBODDEF has not been called.
C        Thus, only the set of default name/code pairs has been
C        defined.
C
C        Given these names, ZZBODN2C returns the following codes:
C
C           Name                         Code    Found?
C           ------------------------   ------    ------
C           'EARTH'                       399    Yes
C           '  Earth '                    399    Yes
C           'EMB'                           3    Yes
C           'Solar System Barycenter'       0    Yes
C           'SolarSystemBarycenter'         -    No
C           'SSB'                           0    Yes
C           'Voyager 2'                   -32    Yes
C           'U.S.S. Enterprise'             -    No
C           ' '                             -    No
C           'Halley's Comet'                -    No
C
C        and, given these codes, ZZBODC2N returns the following
C        names:
C
C           Code        Name                        Found?
C           -------     -------------------         ------
C           399         'EARTH'                     Yes
C             0         'SOLAR SYSTEM BARYCENTER'   Yes
C             3         'EARTH BARYCENTER'          Yes
C           -77         'GALILEO ORBITER'           Yes
C            11          -                          No
C            -1         'GEOTAIL'                   Yes
C
C     3) This example shows the method to define a name/code pair.
C        You may associate a new name with a previously defined
C        code:
C
C           CALL ZZBODDEF ( 'JB', 5 )
C
C        You may also define the name and integer code for a new
C        body:
C
C           CALL ZZBODDEF ( 'Asteroid Frank', 20103456 )
C
C        After these calls to ZZBODDEF, ZZBODN2C would return
C        the following translations:
C
C           Name                         Code    Found?
C           ------------------------   ------    ------
C           'JB'                            5    Yes
C           'Jupiter Barycenter'            5    Yes
C           'ASTEROID FRANK'         20103456    Yes
C           'ASTEROIDFRANK'                 -    No
C           'Frank'                         -    No
C
C        and ZZBODC2N returns these translations:
C
C           Code        Name                     Found?
C           -------     -------------------      ------
C                  5    'JB'                     Yes
C           20103456    'Asteroid Frank'         Yes
C
C        ZZBODC2N exactly returns the string as used in the
C        body name/ID mapping definition.
C
C     4) To use an external IDs kernel, simply load via a FURNSH
C        call.
C
C           CALL FURNSH ( 'ids.ker' )
C
C        With ids.ker listing data such as:
C
C           \begintext
C
C           Define an additional set of body, ID code mappings.
C
C           \begindata
C
C           NAIF_BODY_CODE  = ( 22, 23, 24, 25 )
C
C           NAIF_BODY_NAME  = ( 'LARRY', 'MOE', 'CURLEY', 'SHEMP' )
C
C        Which maps the names defined in NAIF_BODY_NAME
C        to the corresponding index of NAIF_BODY_CODE, i.e.
C        LARRY -> 22, MOE -> 23, etc, and the IDs in NAIF_BODY_CODE
C        map to the corresponding index of NAIF_BODY_NAME.
C
C        NOTE:  When using an external NAME-ID kernel, all ID codes
C        MUST be listed in the kernel variable NAIF_BODY_CODE, and
C        all names MUST be listed in the kernel variable
C        NAIF_BODY_NAME.
C
C     5) Suppose you ran the utility program SPACIT to summarize
C        an SPK ephemeris file and the following data was output
C        to the terminal screen.
C
C           ----------------------------------------------------------
C           Segment identifier: JPL archive 21354
C           Body        : -77                         Center     : 399
C           From        : 1990 DEC 08 18:00:00.000
C           To          : 1990 DEC 10 21:10:00.000
C           Reference   : DE-200                      SPK Type    :1
C           ----------------------------------------------------------
C
C        You could write a program to translate the body codes
C        shown in the SPACIT output:
C
C           CALL ZZBODC2N ( -77, BODY,   FOUND )
C           CALL ZZBODC2N ( 399, CENTER, FOUND )
C
C           IF ( FOUND ) THEN
C
C              WRITE ( *,* ) 'BODY:    -77 = ', BODY
C              WRITE ( *,* ) 'CENTER:  399 = ', CENTER
C
C           END IF
C
C        You could also read the body and center codes directly from
C        the SPK files, using the appropriate DAF routines, and then
C        translate them, as above.
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
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     H.A. Neilan    (JPL)
C     B.V. Semenov   (JPL)
C     M.J. Spencer   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C     K.S. Zukor     (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
C
C        Changed to use name- and ID-based hashes instead of the order
C        arrays (see inline comments next to hash declarations for
C        descriptions of the hashes).
C
C        Changed to keep track of the POOL's and ZZBODTRN internal
C        states using state counters.
C
C        Changed to call ZZCVPOOL to bypass watcher look-ups if the
C        POOL counter did not change.
C
C        Added ZZBCTRCK routine to allow a caller to check the ZZBODTRN
C        state counter against the caller's saved counter.
C
C        Changed umbrella calling sequence to include USRCTR and UPDATE
C        arguments.
C
C-    SPICELIB Version 4.3.0, 05-MAR-2009 (NJB)
C
C        Bug fixes: the entry points ZZBODN2C, ZZBODC2N, and ZZBODKIK
C        now keep track of whether their kernel pool look-ups
C        succeeded. If not, a kernel pool lookup is attempted on the
C        next call to any entry point that calls ZZBODKER.
C
C-    SPICELIB Version 4.0.2, 19-SEP-2006 (EDW)
C
C        Added text to previously empty Declarations section.
C
C-    SPICELIB Version 4.0.1, 17-APR-2003 (EDW)
C
C        Corrected typo in header docs.
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Cleaned up  ZZBODTRN routine/entry point source code
C        and private subroutines used exclusively by ZZBODTRN
C        to process name-code mappings.
C
C        ZZBODLST has been removed from this umbrella and
C        added to the ZZBODBLT umbrella.
C
C        The built-in (permanent collection) of name-code
C        mappings has been moved from this umbrella into
C        the ZZBODBLT umbrella.  The collection is retrieved
C        from the entry point ZZBODGET in ZZBODBLT.
C
C        See the Revisions section below for details.
C
C-    SPICELIB Version 3.2.0, 14-AUG-2002 (EDW)
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
C-    SPICELIB Version 3.1.5, 27-NOV-2001 (EDW)
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
C-    SPICELIB Version 3.1.0, 17-OCT-2001 (EDW)
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
C-    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT)
C
C        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
C        ID coded for Pluto Express were removed.  The ID codes
C        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
C        and Contour were added.
C
C-    SPICELIB Version 2.0.0, 26-JAN-1998 (EDW)
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
C-    SPICELIB Version 1.1.0, 22-MAY-1996 (WLT)
C
C        Added the id-code for Comet Hyakutake, Comet Hale-Bopp,
C        Mars 96, Cassini Simulation, MGS Simulation.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
C
C        Renamed umbrella subroutine and entry points to
C        correspond private routine convention (ZZ...). Added IDs for
C        tracking stations Goldstone (399001), Canberra (399002),
C        Madrid (399003), Usuda (399004).
C
C-    Beta Version 2.2.0, 01-AUG-1995 (HAN)
C
C        Added the IDs for Near Earth Asteroid Rendezvous (-93),
C        Mars Pathfinder (-53), Ulysses (-55), VSOP (-58),
C        Radioastron (-59), Cassini spacecraft (-82), and Cassini
C        Huygens probe (-150).
C        Mars Observer (-94) was replaced with Mars Global
C        Surveyor (-94).
C
C-    Beta Version 2.1.0, 15-MAR-1995 (KSZ) (HAN)
C
C        Two Shoemaker Levy 9 fragments were added, Q1 and P2
C        (IDs 50000022 and 50000023). Two asteroids were added,
C        Eros and Mathilde (IDs 2000433 and 2000253). The
C        Saturnian satellite Pan (ID 618) was added.
C
C-    Beta Version 2.0.0, 03-FEB-1995 (NJB)
C
C        The Galileo probe (ID -344) has been added to the permanent
C        collection.
C
C-    Beta Version 1.0.0, 29-APR-1994 (MJS)
C
C        SPICELIB symbol tables are no longer used. Instead, two order
C        vectors are used to index the NAMES and CODES arrays. Also,
C        this version does not support reading body name ID pairs from a
C        file.
C
C-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
C
C       Some items previously considered errors were removed
C       and some minor modifications were made to improve the
C       robustness of the routines.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        For clarity, some variable names have changed.  The
C        mappings from the old names to the new are provided
C        below:
C
C           Old      New     Function
C           ---      ---     --------
C           NAMES    DEFNAM  Name definition as provided with ZZBODDEF
C           NORNAM   DEFNOR  Normalized name definitions
C           CODES    DEFCOD  Integer codes mapping to entries in DEFNAM
C           ORDCOD   DEFOCD  "Modified" order vector for DEFCOD
C           ORDNOM   DEFONR  Order vector for DEFNOR
C           NNAM     DEFSIZ  Size of DEFNAM, DEFNOR, DEFCOD, and DEFONR
C           NCOD     DEFOSZ  Size of DEFOCD
C
C           CVALS    KERNAM  Name definition as provided from pool
C           CVLNOM   KERNOM  Normalized name definitions
C           IVALS    KERCOD  Integer codes mapping to entries in KERNAM
C           XORDCD   KEROCD  "Modified" order vector for KERCOD
C           XORNOM   KERONR  Order vector for KERNOR
C           NUM(1)   DEFSIZ  Size of KERNAM, KERNOR, KERCOD, and KERONR
C           NUM(2)   DEFOSZ  Size of KEROCD
C
C        The reason for changing the names in this fashion,
C        is simply that these are two instances of variables
C        that have the same properties and utility.  The first
C        set implements the ZZBODDEF style mappings, and the
C        second implements the kernel pool style mappings.
C
C        ZZBODDEF now properly signals an error when a caller
C        attempts to use it to assign a blank string an ID code.
C        This should have never been allowed, but somehow
C        slipped by in previous versions.
C
C        The argument lists for ZZBODKER and ZZBODINI have
C        changed as of previous versions.  Some arguments
C        were removed, as they were no longer necessary.
C
C        ZZBODINI no longer normalizes the input name array;
C        rather it simply computes the order vector for the
C        normalized array input and the "modified" order
C        vector for the input code array.  This was done to
C        save from unnecessarily recomputing the normalization
C        array.
C
C        An additional umbrella has been added to the set of
C        modules of which ZZBODTRN makes use: ZZBODBLT.  This
C        umbrella houses the data statements that used to be
C        present in this module, which defines the "built-in"
C        name-code mappings.  These mappings, as of the changes
C        in N0053, store the mappings the define the initial
C        state of the DEF* arrays.  It contains two entry
C        points:
C
C           ZZBODGET    retrieve the initial values of DEFNAM,
C                       DEFNOR, DEFCOD, and DEFSIZ.
C
C           ZZBODLST    dump the "built-in" codes to a device.
C
C        ZZBODLST used to be present in this umbrella, but the
C        creation of ZZBODBLT made moving it there the logical
C        choice.
C
C        The entry point ZZBODRST has been added to the
C        ZZBODTRN umbrella.  This entry point resets the
C        state of the DEF* arrays to their initial values.
C        This effectively resets any changes made via the
C        ZZBODDEF interface.  It does not effect the kernel
C        pool mappings.
C
C        To support ZZBODRST, a logical BODCHG has been added
C        to the list of saved variables.  This variable
C        indicates when ZZBODDEF has been used to change the
C        built-in body list.
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Parameters
C

C
C     Length of watched POOL keywords.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
C
C     Lower bound of the built-in/BODDEF and kernel-defined hash
C     collision list arrays.
C
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )


C
C     Local variables.
C

C
C     DEFNAM, DEFNOR, and DEFCOD are the name, normalized name, and ID
C     code lists storing built-in/BODDEF name-code mappings. DEFSIZ is
C     the current size of the lists
C
      CHARACTER*(MAXL)      DEFNAM  ( MAXE )
      CHARACTER*(MAXL)      DEFNOR  ( MAXE )
      INTEGER               DEFCOD  ( MAXE )
      INTEGER               DEFSIZ

C
C     Name-based hash for built-in/BODDEF bodies. DNMLST, DNMPOL, and
C     DNMNMS provide the index in DNMIDX which stores the index of the
C     body name, normalized name, and ID in the arrays DEFNAM, DEFNOR,
C     DEFCOD.
C
      INTEGER               DNMLST  (          MAXE )
      INTEGER               DNMPOL  ( LBPOOL : MAXE )
      CHARACTER*(MAXL)      DNMNMS  (          MAXE )
      INTEGER               DNMIDX  (          MAXE )

C
C     ID-based hash for built-in/BODDEF bodies. DIDLST, DIDPOL, and
C     DIDIDS provide the index in DIDIDX which stores the index of the
C     body name, normalized name, and ID in the arrays DEFNAM, DEFNOR,
C     DEFCOD.
C
      INTEGER               DIDLST  (          MAXE )
      INTEGER               DIDPOL  ( LBPOOL : MAXE )
      INTEGER               DIDIDS  (          MAXE )
      INTEGER               DIDIDX  (          MAXE )

C
C     KERNAM, KERNOR, and KERCOD are the name, normalized name, and ID
C     code lists storing kernel POOL name-code mappings. KERSIZ is the
C     current size of the lists
C
      CHARACTER*(MAXL)      KERNAM  ( NROOM )
      CHARACTER*(MAXL)      KERNOR  ( NROOM )
      INTEGER               KERCOD  ( NROOM )
      INTEGER               KERSIZ

C
C     Name-based hash for kernel POOL bodies. KNMLST, KNMPOL, and
C     KNMNMS provide the index in KNMIDX which stores the index of the
C     body name, normalized name, and ID in the arrays KERNAM, KERNOR,
C     KERCOD.
C
      INTEGER               KNMLST  (          NROOM )
      INTEGER               KNMPOL  ( LBPOOL : NROOM )
      CHARACTER*(MAXL)      KNMNMS  (          NROOM )
      INTEGER               KNMIDX  (          NROOM )

C
C     ID-based hash for kernel POOL bodies. KIDLST, KIDPOL, and KIDIDS
C     provide the index in KIDIDX which stores the index of the body
C     name, normalized name, and ID in the arrays KERNAM, KERNOR,
C     KERCOD.
C
      INTEGER               KIDLST  (          NROOM )
      INTEGER               KIDPOL  ( LBPOOL : NROOM )
      INTEGER               KIDIDS  (          NROOM )
      INTEGER               KIDIDX  (          NROOM )

C
C     ZZBODTRN state counter and POOL state counter.
C
      INTEGER               SUBCTR ( CTRSIZ )
      INTEGER               PULCTR ( CTRSIZ )

C
C     Flag indicating whether the built-in list was altered by BODDEF
C     calls.
C
      LOGICAL               BODCHG

C
C     Flag indicating whether valid kernel POOL defined mappings
C     are present in the KERNAM, KERNOR, and KERCOD lists.
C
      LOGICAL               EXTKER

C
C     Flag indicating whether valid kernel POOL defined mappings
C     were successfully fetched from the POOL.
C
      LOGICAL               NODATA
  
C
C     Other variables.
C
      CHARACTER*(MAXL)      TMPNAM
      CHARACTER*(WDSIZE)    WNAMES  ( 2 )
 
      INTEGER               CODIDX
      INTEGER               I
      INTEGER               INDEX
      INTEGER               J
      INTEGER               NWATCH
 
      LOGICAL               FIRST
      LOGICAL               LUPDTE

C
C     Save all variables.
C
      SAVE
 
C
C     Data statements.
C
      DATA BODCHG            / .FALSE. /
      DATA FIRST             / .TRUE.  /
      DATA EXTKER            / .FALSE. /
      DATA NODATA            / .TRUE.  /
      DATA NWATCH            / 2 /
      DATA WNAMES            / 'NAIF_BODY_NAME', 'NAIF_BODY_CODE' /

C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZBODTRN'          )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'ZZBODTRN'          )
      END IF
 
      RETURN



C$Procedure ZZBODN2C ( Private --- Body name to code )
 
      ENTRY ZZBODN2C ( NAME, CODE, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Translate a body name to the corresponding SPICE integer code.
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
C     NAIF_IDS
C
C$ Keywords
C
C     BODY
C     CONVERSION
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               CODE
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Body name to be translated.
C     CODE       O   Integer code for that body.
C     FOUND      O   True if translated, otherwise false.
C     MAXL       P   Max name length.
C
C$ Detailed_Input
C
C     NAME       is an arbitrary name of a body which could be
C                a planet, satellite, barycenter, spacecraft,
C                asteroid, comet, or other ephemeris object.
C
C                Case and leading and trailing blanks in a name
C                are not significant.  However, when a name consists
C                of more than one word, they must be separated by
C                at least one blank, i.e., all of the following
C                strings are equivalent names:
C
C                   'JUPITER BARYCENTER'
C                   'Jupiter Barycenter'
C                   'JUPITER BARYCENTER   '
C                   'JUPITER    BARYCENTER'
C                   '   JUPITER BARYCENTER'
C
C                However, 'JUPITERBARYCENTER' is not equivalent to
C                the names above.
C
C                When ignoring trailing blanks, NAME must have fewer
C                than MAXL characters.
C
C$ Detailed_Output
C
C     CODE       is the NAIF or user defined integer code for the
C                named body.
C
C     FOUND      return as true if NAME has a translation.
C                Otherwise, FOUND returns as false.
C
C$ Parameters
C
C     MAXL       is the maximum length of a body name.  Defined in
C                the include file 'zzbodtrn.inc'.
C
C$ Exceptions
C
C     Errors may be signaled by routines in the call tree of this
C     routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODN2C is one of three related entry points,
C
C        ZZBODN2C      Body name to code
C
C        ZZBODC2N      Body code to name
C
C        ZZBODDEF      Body name/code definition
C
C     ZZBODN2C and ZZBODC2N perform translations between body names
C     and their corresponding integer codes used in SPK and PCK files
C     and associated routines.  A default set of name/code
C     pairs are automatically defined during the first call to
C     any of the entry points.  Additional name/code pairs may
C     be defined via ZZBODDEF.
C
C$ Examples
C
C     See the Examples section of the ZZBODTRN umbrella header.
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
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     B.V. Semenov   (JPL)
C     M.J. Spencer   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
C
C        Changed to use name-based hashes instead of the order arrays.
C
C        Changed to keep track of the POOL's and ZZBODTRN internal
C        states using state counters.
C
C        Changed to call ZZCVPOOL to bypass watcher look-ups if the
C        POOL counter did not change.
C
C-    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to any entry 
C        point that calls ZZBODKER.
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Cleaned up module header and source.  See the Revisions
C        section of ZZBODTRN for detailed update information.
C
C-    SPICELIB Version 3.1.0, 12-FEB-2001 (EDW)
C
C        Added logic to ensure the routine returns the NAME string
C        in the same format as when defined (case and space).
C        Added logic to handle error response in ZZBODINI.
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
C        Edited headers, removed typos and bad grammar, clarified
C        descriptions.
C
C-    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT)
C
C        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
C        ID coded for Pluto Express were removed.  The ID codes
C        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
C        and Contour were added.
C
C-    SPICELIB Version 2.0.0, 21-JAN-1999 (EDW)
C
C        Added code to use the external name/ID kernel.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
C
C        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
C
C        Renamed to ZZBODN2C (BVS)
C
C-    Beta Version 1.0.0, 29-APR-1994 (MJS)
C
C        SPICELIB symbol tables are no longer used. Instead, two order
C        vectors are used to index the NAMES and CODES arrays.
C
C-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
C
C       Items previously considered errors were downgraded
C       to simply be exceptions.  Any NAME is a legitimate input now.
C       If its not in the table, the FOUND flag is just set to .FALSE.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODN2C' )
      END IF
 
C
C     Assume we will not find the code we seek.
C
      FOUND = .FALSE.
 
C
C     On the first pass through this entry point, initialize the
C     built-in arrays, set the kernel pool watchers, and state
C     counters.
C
      IF ( FIRST ) THEN

C
C        Initialize counters. Set ZZBODTRN state counter, for
C        which this umbrella is the owner, to subsystem values. Set
C        POOL counter, for which this umbrella is the user, to user
C        values.
C 
         CALL ZZCTRSIN ( SUBCTR )
         CALL ZZCTRUIN ( PULCTR )
 
C
C        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
C        arrays from the built-in code-name list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODN2C' )
            RETURN
         END IF
 
C
C        Populate the initial built-in code-name hashes.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ, MAXE,
     .                   DNMLST, DNMPOL, DNMNMS, DNMIDX,
     .                   DIDLST, DIDPOL, DIDIDS, DIDIDX  )
          
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODN2C' )
            RETURN
         END IF
 
C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODN2C' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE. to not repeat initialization again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Check for updates to the kernel pool variables. Note: the first
C     call to ZZCVPOOL after initialization always returns .TRUE. for
C     LUPDTE.  This ensures that any initial assignments are properly
C     processed.
C
      CALL ZZCVPOOL ( 'ZZBODTRN', PULCTR, LUPDTE )

      IF ( LUPDTE .OR. NODATA ) THEN
 
C
C        Conservatively increment the ZZBODTRN state counter
C        in expectation of successful update.
C
         CALL ZZCTRINC ( SUBCTR ) 

C
C        Update kernel pool mapping lists and hashes.
C  
         CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                   KNMLST, KNMPOL, KNMNMS, KNMIDX,
     .                   KIDLST, KIDPOL, KIDIDS, KIDIDX  )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT ( 'ZZBODN2C' )
            RETURN

         END IF

         NODATA = .FALSE.
 
      END IF
 
C
C     Normalize the input argument NAME. We will look this normalized
C     name up in the built-in and kernel pool names hashes.
C
      CALL LJUCRS ( 1, NAME, TMPNAM )

C
C     If necessary, first examine the contents of the kernel pool
C     name-code mapping list.
C
      IF ( EXTKER ) THEN
 
C
C        Check if this name is in the kernel pool names hash.
C
         CALL ZZHSCCHK ( KNMLST, KNMPOL, KNMNMS, TMPNAM, I )
 
         IF ( I .NE. 0 ) THEN
            CODE  = KERCOD( KNMIDX( I ) )
            FOUND = .TRUE.
            CALL CHKOUT ( 'ZZBODN2C' )
            RETURN
         END IF
 
      END IF
 
C
C     If we reach here, we did not find this name in the kernel pool
C     names hash. Check the built-in names hash.
C
      CALL ZZHSCCHK ( DNMLST, DNMPOL, DNMNMS, TMPNAM, I )

      IF ( I .NE. 0 ) THEN

         CODE  = DEFCOD( DNMIDX( I ) )
         FOUND = .TRUE.

      END IF
 
      CALL CHKOUT ( 'ZZBODN2C' )
      RETURN



C$Procedure ZZBODC2N ( Private --- Body code to name )
 
      ENTRY ZZBODC2N ( CODE, NAME, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Translate the integer code of a body into a common name for
C     that body.
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
C     NAIF_IDS
C
C$ Keywords
C
C     BODY
C     CONVERSION
C
C$ Declarations
C
C     INTEGER               CODE
C     CHARACTER*(*)         NAME
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CODE       I   Integer code to be translated.
C     NAME       O   Common name for the body identified by CODE.
C     FOUND      O   True if translated, otherwise false.
C     MAXL       P   Max name length.
C
C$ Detailed_Input
C
C     CODE       is an integer code for a body ---
C                a planet, satellite, barycenter, spacecraft,
C                asteroid, comet, or other ephemeris object.
C
C$ Detailed_Output
C
C     NAME       is the common name of the body identified by CODE.
C                If CODE has more than one translation, then the
C                most recently defined NAME corresponding to CODE
C                is returned.  The routine returns NAME in the exact
C                format (case and blanks) as used when defining
C                the name/code pair.
C
C     FOUND      returns as true if NAME has a translation.
C                Otherwise, FOUND returns as false.
C
C$ Parameters
C
C     MAXL       is the maximum length of a body name.  Defined in
C                the include file 'zzbodtrn.inc'.
C$ Exceptions
C
C     Errors may be signaled by routines in the call tree of this
C     routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODC2N is one of three related entry points,
C
C        ZZBODN2C      Body name to code
C
C        ZZBODC2N      Body code to name
C
C        ZZBODDEF      Body name/code definition
C
C     ZZBODN2C and ZZBODC2N perform translations between body names
C     and their corresponding integer codes used in SPK and PCK files
C     and associated routines.  A default set of name/code
C     pairs are automatically defined during the first call to
C     any of the entry points.  Additional name/code pairs may
C     be defined via ZZBODDEF.
C
C     For the case in which multiple names map to a single code, a
C     ZZBODC2N call returns the name last assigned to that code - a
C     LIFO situation.
C
C$ Examples
C
C     See Examples section of ZZBODTRN umbrella header.
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
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     B.V. Semenov   (JPL)
C     M.J. Spencer   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
C
C        Changed to use name and ID-based hashes instead of the order
C        arrays.
C
C        Changed to keep track of the POOL's and ZZBODTRN internal
C        state using state counters.
C
C        Changed to call ZZCVPOOL to bypass watcher look-ups if the
C        POOL counter did not change.
C
C-    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to any entry 
C        point that calls ZZBODKER.
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Cleaned up module header and source code.  See the Revisions
C        section of ZZBODTRN for detailed update information.
C
C-    SPICELIB Version 3.2.0, 19-JUL-2002 (EDW)
C
C        Added logic to enforce the precedence masking.
C
C-    SPICELIB Version 3.1.0, 5-SEP-2001 (EDW)
C
C        Added logic to ensure the routine returns the NAME string
C        in the same format as when defined (case and space).
C        Added logic to handle error response in ZZBODINI.
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
C        Edited headers, removed typos and bad grammar, clarified
C        descriptions.
C
C-    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT)
C
C        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
C        ID coded for Pluto Express were removed.  The ID codes
C        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
C        and Contour were added.
C
C-    SPICELIB Version 2.0.0, 21-JAN-1999 (EDW)
C
C        Added code to use the external name/ID kernel.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
C
C        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
C
C        Renamed to ZZBODC2N (BVS)
C
C-    Beta Version 1.0.0, 29-APR-1994 (MJS)
C
C        SPICELIB symbol tables are no longer used. Instead, two order
C        vectors are used to index the NAMES and CODES arrays.
C
C-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
C
C       Checks to see the input integer code can be represented
C       as a character string were removed along with the exceptions
C       associated with these checks.  It is now the responsibility
C       of a maintenance programmer to make sure MAXL is large
C       enough to allow any integer to be converted to a string
C       representation.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODC2N' )
      END IF
 
C
C     Assume we will not find the name we seek.
C
      FOUND = .FALSE.
 
C
C     On the first pass through this entry point, initialize the
C     built-in arrays, set the kernel pool watchers, and state
C     counters.
C
      IF ( FIRST ) THEN
 
C
C        Initialize counters. Set ZZBODTRN state counter, for
C        which this umbrella is the owner, to subsystem values. Set
C        POOL counter, for which this umbrella is the user, to user
C        values.
C 
         CALL ZZCTRSIN ( SUBCTR )
         CALL ZZCTRUIN ( PULCTR )
 
C
C        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
C        arrays from the built-in code list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODC2N' )
            RETURN
         END IF
 
C
C        Populate the initial built-in code-name hashes.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ, MAXE,
     .                   DNMLST, DNMPOL, DNMNMS, DNMIDX,
     .                   DIDLST, DIDPOL, DIDIDS, DIDIDX  )
          
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODC2N' )
            RETURN
         END IF

C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODC2N' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE. to not repeat initialization again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Check for updates to the kernel pool variables. Note: the first
C     call to ZZCVPOOL after initialization always returns .TRUE. for
C     LUPDTE. This ensures that any initial assignments are properly
C     processed.
C
      CALL ZZCVPOOL ( 'ZZBODTRN', PULCTR, LUPDTE )
 
      IF ( LUPDTE .OR. NODATA ) THEN
 
C
C        Conservatively increment the ZZBODTRN state counter
C        in expectation of successful update.
C
         CALL ZZCTRINC ( SUBCTR ) 

C
C        Update kernel pool mapping lists and hashes.
C  
         CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                   KNMLST, KNMPOL, KNMNMS, KNMIDX,
     .                   KIDLST, KIDPOL, KIDIDS, KIDIDX  )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT ( 'ZZBODC2N' )
            RETURN

         END IF

         NODATA = .FALSE.
 
      END IF
 
C
C     If necessary, first examine the contents of the kernel pool
C     name-code mapping list.
C
      IF ( EXTKER ) THEN
 
C
C        Check if this code is in the kernel pool codes hash.
C
         CALL ZZHSICHK ( KIDLST, KIDPOL, KIDIDS, CODE, I )
 
         IF ( I .NE. 0 ) THEN
            NAME  = KERNAM(KIDIDX(I))
            FOUND = .TRUE.
            CALL CHKOUT ( 'ZZBODC2N' )
            RETURN
         END IF
 
      END IF
 
C
C     If we reach here, we did not find this code in the kernel pool
C     codes hash. Check the built-in codes hash.
C
      CALL ZZHSICHK ( DIDLST, DIDPOL, DIDIDS, CODE, I )
 
C
C     If we find a match, verify that it is not masked by a kernel pool
C     entry before returning.
C
      IF ( I .NE. 0 ) THEN
 
         IF ( EXTKER ) THEN
 
C
C           Only bother performing this check if there are actually
C           mappings present in the kernel pool lists.
C
            CALL ZZHSCCHK ( KNMLST, KNMPOL, KNMNMS, 
     .                                      DEFNOR(DIDIDX(I)), J )

            IF ( J .NE. 0 ) THEN
 
C
C              This name is defined in the kernel pool mappings. Set
C              FOUND to .FALSE., as the contents of the kernel pool
C              have higher precedence than any entries in the built-in
C              mapping list.
C
               FOUND = .FALSE.

            ELSE
 
C
C              No match for this name in the kernel pool mapping list.
C              Return the name.
C
               NAME  = DEFNAM(DIDIDX(I))
               FOUND = .TRUE.
 
            END IF
 
         ELSE
 
C
C           No kernel pool mappings were defined, simply return
C           return the name.
C
            NAME  = DEFNAM(DIDIDX(I))
            FOUND = .TRUE.
 
         END IF
 
      END IF
 
      CALL CHKOUT ( 'ZZBODC2N' )
      RETURN



C$Procedure ZZBODDEF ( Private --- Body name/code definition )
 
      ENTRY ZZBODDEF ( NAME, CODE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Define a body name/code pair for later translation by
C     ZZBODN2C or ZZBODC2N.
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
C     NAIF_IDS
C
C$ Keywords
C
C     BODY
C     CONVERSION
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               CODE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Common name of some body.
C     CODE       I   Integer code for that body.
C     MAXL       P   Max name length and max number of digits in code.
C     MAXP       P   Maximum number of name/code pair definitions.
C
C$ Detailed_Input
C
C     NAME       is an arbitrary name of a body which could be
C                a planet, satellite, barycenter, spacecraft,
C                asteroid, comet, or other ephemeris object.
C
C                The case and positions of blanks in a name
C                are significant. ZZBODC2N returns the exact
C                string (case and space) last mapped to a code.
C                When a name is made up of more than one word,
C                the words require separation by at least one blank,
C                i.e., all of the following strings belong to
C                the same equivalence class:
C
C                   'JUPITER BARYCENTER'
C                   'Jupiter Barycenter'
C                   'JUPITER BARYCENTER   '
C                   'JUPITER    BARYCENTER'
C                   '   JUPITER BARYCENTER'
C
C                However, 'JUPITERBARYCENTER' is not equivalent to
C                the names above.
C
C                When ignoring trailing blanks, NAME must have fewer
C                than MAXL characters.
C
C     CODE       is the integer code for the named body.
C
C                CODE may already have a name as defined by a
C                previous call to ZZBODDEF or as part of the set of
C                default definitions.  That previous definition
C                remains and a translation of that name still
C                returns the same CODE.  However, future translations
C                of CODE will give the new NAME instead of the
C                previous one.  This feature is useful for assigning
C                a more familiar or abbreviated name to a body.
C                For example, in addition to the default name for
C                body 5, 'JUPITER BARYCENTER', you could define the
C                abbreviation 'JB' to mean 5.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     The following relevant parameters are defined in zzbodtrn.inc:
C
C     MAXL        is the maximum length of a body name.
C
C     MAXP        is the maximum number of additional names that may
C                 be added via the ZZBODDEF interface.
C
C$ Exceptions
C
C     1) If the maximum number of definitions is exceeded, a the
C        error SPICE(TOOMANYPAIRS) is signaled.
C
C     2) If an attempt to assign a blank string an ID code is made,
C        the error SPICE(BLANKNAMEASSIGNED) is signaled.
C
C     3) Routines in the call tree of this routine may signal
C        errors.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODDEF is one of three related entry points,
C
C        ZZBODN2C      Body name to code
C
C        ZZBODC2N      Body code to name
C
C        ZZBODDEF      Body name/code definition
C
C     ZZBODN2C and ZZBODC2N perform translations between body names
C     and their corresponding integer codes used in SPK and PCK files
C     and associated routines.  A default set of name/code
C     pairs are automatically defined during the first call to
C     any of the entry points.  Additional name/code pairs may
C     be defined via ZZBODDEF for two purposes:
C
C        1.  to associate another, perhaps more familiar or
C            abbreviated name with a previously defined body integer
C            code or
C
C        2.  to define a new body integer code and name,
C
C     Each body has a unique integer code, but may have several
C     names.  Thus you may associate more than one name with
C     a particular integer code.  However, associating more
C     than one integer code with a particular name creates ambiguity.
C     Therefore, once a name has been defined, it may not be redefined
C     with a different integer code.
C
C     For example, Europa is the name of the second satellite of
C     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502)
C     is one of the default definitions.  Europa is also the name
C     of an asteroid.  Suppose you were able to associate the asteroid
C     integer code with the name EUROPA.  Then when you call ZZBODN2C to
C     translate the name EUROPA, which code should be returned?  That
C     of the asteroid or 502?
C
C     ZZBODDEF prevents this ambiguity by signaling an error
C     if the specified name has already been defined with a
C     different code.  In the case of EUROPA, you may want to use the
C     name ASTEROID EUROPA.  The set of default definitions are listed
C     in DATA statements in the umbrella routine ZZBODTRN for easy
C     reference.
C
C$ Examples
C
C     See the Examples section of the ZZBODTRN umbrella header.
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
C     J.E. McLean    (JPL)
C     B.V. Semenov   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
C
C        Changed to use name and ID-based hashes instead of the order
C        arrays.
C
C        Changed to keep track of the POOL's and ZZBODTRN internal
C        state using state counters.
C
C        Changed to call ZZCVPOOL to bypass watcher look-ups if the
C        POOL counter did not change.
C
C-    SPICELIB Version 4.0.1, 17-APR-2003 (EDW)
C
C        Correct typo in header docs.
C
C     SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Cleaned up module header and source code.  See the Revisions
C        section of ZZBODTRN for detailed update information.
C
C        Added the error SPICE(BLANKNAMEASSIGNED), when the caller
C        attempts to assign an ID code to a blank string.
C
C-    SPICELIB Version 1.3.0, 14-AUG-2002 (EDW)
C
C        Added logic to enforce the precedence masking;
C        logic removes duplicate assignments of ZZBODDEF.
C        Removed the NAMENOTUNIQUE error block.
C
C-    SPICELIB Version 1.2.0, 5-SEP-2001 (EDW)
C
C        Added logic to ensure the routine returns the NAME string
C        in the same format as when defined (case and space).
C        Added logic to handle error response from ZZBODINI.
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
C        Edited headers, removed typos and bad grammar, clarified
C        descriptions.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
C
C        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
C
C        Renamed to ZZBODDEF (BVS). More careful checking for overflow
C        of the recognized names is now performed.
C
C-    Beta Version 1.0.0, 29-APR-1994 (MJS)
C
C        SPICELIB symbol tables are no longer used. Instead, two order
C        vectors are used to index the NAMES and CODES arrays.
C
C-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
C
C       Checks to see an integer code can be represented
C       as a character string were removed along with the exceptions
C       associated with these checks.  It is now the responsibility
C       of a maintenance programmer to make sure MAXL is large
C       enough to allow any integer to be converted to a string
C       representation.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODDEF' )
      END IF

C
C     On the first pass through this entry point, initialize the
C     built-in arrays, set the kernel pool watchers, and state
C     counters.
C
      IF ( FIRST ) THEN
 
C
C        Initialize counters. Set ZZBODTRN state counter, for
C        which this umbrella is the owner, to subsystem values. Set
C        POOL counter, for which this umbrella is the user, to user
C        values.
C 
         CALL ZZCTRSIN ( SUBCTR )
         CALL ZZCTRUIN ( PULCTR )
 
C
C        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
C        arrays from the built-in code list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODDEF' )
            RETURN
         END IF
 
C
C        Populate the initial built-in code-name hashes.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ, MAXE,
     .                   DNMLST, DNMPOL, DNMNMS, DNMIDX,
     .                   DIDLST, DIDPOL, DIDIDS, DIDIDX  )
          
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODDEF' )
            RETURN
         END IF
 
C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODDEF' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE. to not repeat initialization again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Begin by verifying that the user is not attempting to assign a
C     blank string a code.
C
      IF ( NAME .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'An attempt to assign the code, #, to '
     .   //            'a blank string was made.  Check loaded '
     .   //            'text kernels for a blank string in '
     .   //            'the NAIF_BODY_NAME array.'               )
         CALL ERRINT ( '#', I                                    )
         CALL SIGERR ( 'SPICE(BLANKNAMEASSIGNED)'                )
         CALL CHKOUT ( 'ZZBODDEF'                                )
         RETURN
 
      END IF

C
C     Conservatively increment the ZZBODTRN state counter in
C     expectation of successful addition.
C
      CALL ZZCTRINC ( SUBCTR )  
 
C
C     Get normalized form of the input NAME.
C
      CALL LJUCRS ( 1, NAME, TMPNAM )
 
C
C     Determine if we are going to replace an entry currently present
C     in the DEF* lists.
C
      CALL ZZHSCCHK ( DNMLST, DNMPOL, DNMNMS, TMPNAM, I )

      IF ( I .NE. 0 ) THEN

         INDEX = DNMIDX( I )
 
C
C        We are going to replace an existing entry.  There are
C        two possible ways in which a replace operation can
C        happen:
C
C           1) The caller is attempting to replace the highest
C              precedent name-code mapping for a particular ID code.
C              When this happens, we need only change the entry in
C              DEFNAM at position INDEX. The user is simply changing
C              the name that maps to the same normalized name.
C
C           2) The caller is attempting to change the code
C              associated with a name, bump a lower precedence
C              name-code mapping to highest precedence, or some
C              combination of the two.
C
C        See if we should handle 1) first.
C
         CALL ZZHSICHK ( DIDLST, DIDPOL, DIDIDS, CODE, I )

         IF ( I .NE. 0 ) THEN
            CODIDX = DIDIDX( I )
         ELSE
            CODIDX = 0
         END IF

C
C        If CODIDX matches INDEX, then we simply have to replace the
C        entry in DEFNAM and return.
C
         IF ( CODIDX .EQ. INDEX ) THEN
 
C
C           We altered the built-in body list. Set BODCHG to .TRUE.
C
            BODCHG           = .TRUE.
 
            DEFNAM ( INDEX ) = NAME
 
            CALL CHKOUT ( 'ZZBODDEF' )
            RETURN
 
         END IF
 
C
C        At this point we have to replace all of the values for the
C        mapping defined at the INDEX position in DEFNAM, DEFNOR, and
C        DEFCOD. This will require recomputing the hashes. First
C        compress out the existing entry.
C
         DO I = INDEX+1, DEFSIZ
 
            DEFNAM( I-1 ) = DEFNAM( I )
            DEFNOR( I-1 ) = DEFNOR( I )
            DEFCOD( I-1 ) = DEFCOD( I )
 
         END DO
 
      ELSE
 
C
C        We need to add this entry to the list.  See if there
C        is room; signal an error and return if there is not.
C
         IF ( DEFSIZ .GE. MAXE ) THEN
 
            CALL SETMSG ( 'There is no room available for adding '
     .      //            '''#''  to the list of name/code pairs. '
     .      //            'The number of names that can be '
     .      //            'supported is #.  This number has been '
     .      //            'reached. '                               )
            CALL ERRCH  ( '#', NAME                                 )
            CALL ERRINT ( '#', DEFSIZ                               )
            CALL SIGERR ( 'SPICE(TOOMANYPAIRS)'                     )
            CALL CHKOUT ( 'ZZBODDEF'                                )
            RETURN
 
         END IF
 
C
C        If we reach here, then there is room in the list. Increase
C        it's size counter.
C
         DEFSIZ = DEFSIZ + 1
 
      END IF
 
C
C     We are changing the body list, inform ZZBODRST by setting BODCHG
C     to .TRUE.
C
      BODCHG = .TRUE.
 
C
C     Now, we need to add the new entry on to the end of the
C     DEFNAM, DEFNOR, and DEFCOD lists.
C
      DEFNAM ( DEFSIZ ) = NAME
      DEFNOR ( DEFSIZ ) = TMPNAM
      DEFCOD ( DEFSIZ ) = CODE
 
C
C     Reset the built-in/BODDEF hashes.
C
      CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ, MAXE,
     .                DNMLST, DNMPOL, DNMNMS, DNMIDX,
     .                DIDLST, DIDPOL, DIDIDS, DIDIDX  )
          
      CALL CHKOUT ( 'ZZBODDEF' )
      RETURN



C$Procedure ZZBODKIK ( Private --- Run the kernel read block )
 
      ENTRY ZZBODKIK ( )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine executes the kernel pool read instructions
C     if necessary.
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
C     BODY MAPPING
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     None.
C
C$ Detailed_Input
C
C     NONE.
C
C$ Detailed_Output
C
C     NONE.
C
C$ Parameters
C
C     NONE.
C
C$ Exceptions
C
C     NONE.
C
C$ Files
C
C     NONE.
C
C$ Particulars
C
C     This entry point provides a mechanism to allow a caller
C     to force the examination of the kernel pool variables that
C     define name-code mappings.  This is useful, if once a new
C     mapping is defined, diagnostics at the time of definition
C     are useful.  The way the system performs otherwise, the
C     diagnostics are not provided until a name-code conversion
C     is attempted.
C
C$ Examples
C
C     See ZZLDKER for sample usage.
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
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
C
C        Changed to use name and ID-based hashes instead of the order
C        arrays.
C
C        Changed to keep track of the POOL's and ZZBODTRN internal
C        state using state counters.
C
C        Changed to call ZZCVPOOL to bypass watcher look-ups if the
C        POOL counter did not change.
C
C-    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to any entry 
C        point that calls ZZBODKER.
C
C-    SPICELIB Version 4.0.2, 19-SEP-2006 (EDW)
C
C        Added text to previously empty Declarations section.
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Added checks to watchers and the initialization
C        block.
C
C-    SPICELIB Version 1.0.0, 16-JUN-2002 (EDW)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODKIK' )
      END IF

C
C     On the first pass through this entry point, initialize the
C     built-in arrays, set the kernel pool watchers, and state
C     counters.
C
      IF ( FIRST ) THEN
 
C
C        Initialize counters. Set ZZBODTRN state counter, for
C        which this umbrella is the owner, to subsystem values. Set
C        POOL counter, for which this umbrella is the user, to user
C        values.
C 
         CALL ZZCTRSIN ( SUBCTR )
         CALL ZZCTRUIN ( PULCTR ) 
 
C
C        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
C        arrays from the built-in code list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODKIK' )
            RETURN
         END IF
 
C
C        Populate the initial built-in/BODDEF hashes.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ, MAXE,
     .                   DNMLST, DNMPOL, DNMNMS, DNMIDX,
     .                   DIDLST, DIDPOL, DIDIDS, DIDIDX  )
          
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODKIK' )
            RETURN
         END IF
 
C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODKIK' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE. to not repeat initialization again.
C
         FIRST = .FALSE.
 
      END IF
 
 
C
C     Check for updates to the kernel pool variables. Note: the first
C     call to ZZCVPOOL after initialization always returns .TRUE. for
C     LUPDTE. This ensures that any initial assignments are properly
C     processed.
C
      CALL ZZCVPOOL ( 'ZZBODTRN', PULCTR, LUPDTE )
 
      IF ( LUPDTE .OR. NODATA ) THEN
 
C
C        Conservatively increment the ZZBODTRN state counter
C        in expectation of successful update.
C
         CALL ZZCTRINC ( SUBCTR ) 

C
C        Update kernel pool mapping lists and hashes.
C 
         CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                   KNMLST, KNMPOL, KNMNMS, KNMIDX,
     .                   KIDLST, KIDPOL, KIDIDS, KIDIDX  )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT ( 'ZZBODKIK' )
            RETURN

         END IF

         NODATA = .FALSE.
 
      END IF
 
      CALL CHKOUT ( 'ZZBODKIK' )
      RETURN



C$Procedure ZZBODRST ( Private --- Body List Reset )
 
      ENTRY ZZBODRST ( )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine resets the built-in body list, removing any
C     assignments or alterations made by the ZZBODDEF entry point.
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
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
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
C     1) Routines in the call tree of this routine may signal errors.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODRST resets the built-in body name-code mapping list.  This
C     list may only be modified by ZZBODDEF.  Further, any assignments
C     made through the kernel pool mechanism remain unaltered as a
C     result of invoking this routine.
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
C     B.V. Semenov    (JPL)
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
C
C        Changed to use name and ID-based hashes instead of the order
C        arrays.
C
C        Changed to keep track of the POOL's and ZZBODTRN internal
C        state using state counters.
C
C-    SPICELIB Version 4.0.0, 26-AUG-2002 (FST)
C
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODRST' )
      END IF

C
C     On the first pass through this entry point, initialize the
C     built-in arrays, set the kernel pool watchers, and state
C     counters.
C
      IF ( FIRST ) THEN
 
C
C        Initialize counters. Set ZZBODTRN state counter, for
C        which this umbrella is the owner, to subsystem values. Set
C        POOL counter, for which this umbrella is the user, to user
C        values.
C 
         CALL ZZCTRSIN ( SUBCTR )
         CALL ZZCTRUIN ( PULCTR )
 
C
C        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
C        arrays from the built-in code list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODRST' )
            RETURN
         END IF
 
C
C        Populate the initial built-in code-name hashes.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ, MAXE,
     .                   DNMLST, DNMPOL, DNMNMS, DNMIDX,
     .                   DIDLST, DIDPOL, DIDIDS, DIDIDX  )
          
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODRST' )
            RETURN
         END IF
 
C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODRST' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE. to not repeat initialization again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     See if the body list needs to be reset.
C
      IF ( BODCHG ) THEN
 
         BODCHG = .FALSE.
 
C
C        Conservatively increment the ZZBODTRN state counter
C        in expectation of successful update.
C
         CALL ZZCTRINC ( SUBCTR ) 
 
C
C        Fetch the initial body name-code mapping list. Note: we need
C        not check FAILED() here, because if an error had occurred due
C        to the improper specification of MAXE it would have been
C        signaled already to the user.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
C
C        Reset the built-in/BODDEF hashes.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ, MAXE,
     .                   DNMLST, DNMPOL, DNMNMS, DNMIDX,
     .                   DIDLST, DIDPOL, DIDIDS, DIDIDX  )
          
      END IF
 
      CALL CHKOUT ( 'ZZBODRST' )
      RETURN



C$Procedure ZZBCTRCK ( Private -- check/update user's ZZBODTRN counter )
 
      ENTRY ZZBCTRCK ( USRCTR, UPDATE )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Check and update the ZZBODTRN state counter tracked by a caller
C     (user) routine.
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
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               USRCTR    ( CTRSIZ )
C     LOGICAL               UPDATE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     USRCTR    I/O  ZZBODTRN state counter tracked by the caller
C     UPDATE     O   Flag indicating if input counter was updated
C
C     CTRSIZ     P   Dimension of the counter array
C
C$ Detailed_Input
C
C     USRCTR      is the value of the ZZBODTRN state counter tracked by
C                 (saved in) the caller (user) routine.
C
C$ Detailed_Output
C
C     USRCTR      is the current ZZBODTRN state counter. 
C
C     UPDATE      is the logical flag indicating whether the input
C                 ZZBODTRN state counter was different from the current
C                 ZZBODTRN state counter and, therefore, had to be
C                 updated (UPDATE = .TRUE.) or if it was the same
C                 (UPDATE = .FALSE.).
C
C$ Parameters
C
C     CTRSIZ      is the dimension of the counter array used by
C                 various SPICE subsystems to uniquely identify
C                 changes in their states. This parameter is 
C                 defined in the private include file 'zzctr.inc'.
C
C$ Exceptions
C
C     1) Routines in the call tree of this routine may signal errors.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is not part of the SPICELIB API. This routine may be
C     removed in a later version of the SPICE Toolkit, or its interface
C     may change.
C
C     SPICE-based application code should not call this routine.
C
C     This routine allows other routines to be aware of ZZBODTRN state
C     change due to addition or deletion body name-code mappings via
C     ZZBODTRN calls and/or kernel POOL NAIF_BODY_NAME/NAIF_BODY_CODE
C     variables. Such awareness is needed to be able to locally save
C     some ZZBODTRN-based data (e.g. a particular body name/ID mapping)
C     and only update these locally saved values if the ZZBODTRN state
C     has changed.
C
C     To make use of the ZZBODTRN state counter to achieve this goal the
C     caller routines save the ZZBODTRN state counter returned by the
C     first call to this routine and then check that saved value
C     against the current ZZBODTRN state counter and update it by
C     subsequent calls this routine.
C
C     This routine checks if the watched POOL name-ID mapping keywords
C     changed and if so updates the internally buffered POOL-based
C     name-ID mappings.
C
C$ Examples
C
C     The routines that need to be aware of and act on the ZZBODTRN
C     state change initialize a local ZZBODTRN counter array using
C     ZZCTRUIN, save it, and check it against the current ZZBODTRN
C     state counter and update it, if needed, using this entry point,
C     as follows:
C
C        C
C        C     Include zzctr.inc to access CTRSIZ.
C        C
C              INCLUDE              'zzctr.inc'
C              ...
C
C        C
C        C     In local variable declarations declare and save
C        C     the local ZZBODTRN state counter. Also declare the 
C        C     update flag.
C        C
C              INTEGER               USRCTR ( CTRSIZ )
C              LOGICAL               UPDATE
C              ...      
C              SAVE                  USRCTR
C              ...
C
C        C
C        C     In all places where initialization is done  
C        C     initialize the local ZZBODTRN state counter using  
C        C     ZZCTRUIN to ensure an update on the first check. 
C        C
C              IF ( FIRST ) THEN
C                 ...
C                 CALL ZZCTRUIN( USRCTR )
C                 FIRST = .FALSE.
C              END IF
C              ...
C
C        C
C        C     In all places where there is a need to check for 
C        C     the ZZBODTRN state change call this entry to 
C        C     check and update the local POOL state counter.
C        C
C              CALL ZZBCTRCK ( USRCTR, UPDATE )
C
C              IF ( UPDATE ) THEN
C
C        C
C        C        It the ZZBODTRN state changed, do what needs to 
C        C        be done to deal with saved values based 
C        C        on ZZBODTRN data.
C        C
C                 ...
C
C              END IF
C
C$ Restrictions
C
C     1) This is a private routine. See $Particulars above.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

C
C     Check for updates to the kernel pool variables.
C
      CALL ZZCVPOOL ( 'ZZBODTRN', PULCTR, LUPDTE )
 
      IF ( LUPDTE .OR. NODATA ) THEN

C
C        Check in because ZZBODKER can fail.
C
         CALL CHKIN ( 'ZZBCTRCK' )
         
C
C        Conservatively increment the ZZBODTRN state counter in
C        expectation of successful update.
C
         CALL ZZCTRINC ( SUBCTR ) 

C
C        Update kernel pool mapping lists and hashes.
C  
         CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                   KNMLST, KNMPOL, KNMNMS, KNMIDX,
     .                   KIDLST, KIDPOL, KIDIDS, KIDIDX  )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT ( 'ZZBCTRCK' )
            RETURN

         END IF

         NODATA = .FALSE.

         CALL CHKOUT ( 'ZZBCTRCK' )

      END IF

C
C     Check the input counter against the ZZBODTRN counter.
C
      CALL ZZCTRCHK ( SUBCTR, USRCTR, UPDATE )
      
      RETURN

      END

