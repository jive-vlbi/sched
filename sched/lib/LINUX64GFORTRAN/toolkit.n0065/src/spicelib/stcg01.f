C$Procedure   STCG01 ( STAR catalog type 1, get star data )
 
      SUBROUTINE STCG01 ( INDEX,  RA,     DEC,    RASIG,
     .                    DECSIG, CATNUM, SPTYPE, VMAG )
 
C$ Abstract
C
C     Get data for a single star from a SPICE type 1 star catalog.
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
C     EK
C
C$ Keywords
C
C     None.
C
C$ Declarations
 
      INTEGER               INDEX
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      RASIG
      DOUBLE PRECISION      DECSIG
      INTEGER               CATNUM
      CHARACTER*(*)         SPTYPE
      DOUBLE PRECISION      VMAG
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INDEX       I   Star index.
C     RA          O   Right ascension in radians.
C     DEC         O   Declination in radians.
C     RAS         O   Right ascension uncertainty in radians.
C     DECS        O   Declination uncertainty in radians.
C     CATNUM      O   Catalog number.
C     SPTYPE      O   Spectral type.
C     VMAG        O   Visual magnitude.
C
C$ Detailed_Input
C
C     INDEX       is the index of the star in the list of stars
C                 that satisfy the selection criteria specified in
C                 the last call to STCF01.
C
C$ Detailed_Output
C
C     RA          is right ascension of the star at the catalog epoch
C                 in radians relative to the J2000 inertial frame.
C
C     DEC         is declination of the star at the catalog epoch in
C                 radians relative to the J2000 inertial frame.
C
C     RASIG       is the uncertainty in right ascension of the star at
C                 the catalog epoch in radians.
C
C     DECSIG      is the uncertainty in declination of the star at
C                 the catalog epoch in radians.
C
C     CATNUM      is the star number in the catalog.
C
C     SPTYPE      is the star's spectral type. See catalog description
C                 for more information regarding encoding of spectral
C                 type values.
C
C     VMAG        is the visual magnitude of the star.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If fetching of any of output values fails, then
C        the error 'SPICE(BADSTARINDEX)' is signalled.
C
C     2) If no star catalog has been loaded, the error is dianosed
C        by a routine called by this one.
C
C     3) If STCF01 was not called first, the EK query
C        error 'SPICE(INVALIDINDEX)' is signalled.
C
C$ Files
C
C     This routine reads the data from SPICE type 1 star catalog file
C     loaded into the program by a call to STCL01.
C
C     SPICE type 1 star catalog files MUST contain a single data table.
C     It can occupy a single segment or it can spread across multiple
C     segments. This table MUST include the following columns:
C
C        column name                data type          units
C     -------------------------------------------------------
C        RA                   DOUBLE PRECISION        DEGREES
C        DEC                  DOUBLE PRECISION        DEGREES
C        RA_SIGMA             DOUBLE PRECISION        DEGREES
C        DEC_SIGMA            DOUBLE PRECISION        DEGREES
C        CATALOG_NUMBER       INTEGER
C        SPECTRAL_TYPE        CHARACTER*(4)
C        VISUAL_MAGNITUDE     DOUBLE PRECISION
C
C     Nulls are not allowed in any of the columns.
C     Other columns can also be present in the table but their data
C     will NOT be accessible through STCF01 and STCG01 --
C     the interface used to access data in the catalog. Note
C     that the names and attributes of these additional columns
C     must be identical for all segments containing this table.
C
C$ Particulars
C
C     This routine is intended to be a part of the user interface to
C     the SPICE type 1 star catalog. It allows the caller to retrieve
C     data for a single star found by STCF01 using the star's
C     index within the search result array. This subroutine MUST
C     NOT be called before a search by STCF01 was done.
C
C     Other routines in the SPICE type 1 star catalog access
C     family are:
C
C        STCL01  load the catalog file and make its data
C                available for search and retrieval.
C
C        STCF01  search through the catalog for all stars within
C                a specified RA-DEC rectangle.
C
C$ Examples
C
C     In the following code fragment, STCG01 is used to retrieve
C     position and characteristics for every star within an RA - DEC
C     rectangle from a particular SPICE type 1 star catalog.
C
C     C
C     C     Load catalog file.
C     C
C           CALL STCL01 ( CATFN, TABNAM, HANDLE )
C     C
C     C     Search through the loaded catalog.
C     C
C           CALL STCF01 ( TABNAM, RAMIN,  RAMAX,
C          .              DECMIN, DECMAX, NSTARS )
C     C
C     C     Retrieve data for every star found.
C     C
C           DO I = 1, NSTARS
C
C              CALL STCG01 ( I, RA, DEC, RASIG, DECSIG,
C          .                 CATNUM, SPTYPE, VMAG )
C
C           END DO
C
C$ Restrictions
C
C     1) The catalog file STCG01 reads data from MUST be loaded
C        by STCL01 and a search through the catalog MUST be done by
C        STCF01 before STCG01 is called.
C
C     2) No other EK queries can be made between the call to STCF01
C        and the call to STCG01.
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
C-    SPICELIB Version 1.0.0, 15-MAY-1996 (BVS)
C
C-&
 
C$ Index_Entries
C
C     get data for single star from a type 1 star catalog
C
C-&
 
C$ Revisions
C
C-&
 
C
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      RPD
 
C
C     Local variables.
C
      LOGICAL               FOUND
      LOGICAL               NULL
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STCG01' )
      END IF
 
C
C     Fetch data from the catalog in the following order
C     as defined QUERY string template in STCF01 routine
C
C           RA, DEC, RASIG, DECSIG, CATNUM, SPTYPE, VMAG
C
C     Check FOUNDs and report error if any of the parameters
C     is not found.
C
C     Since NULLs are not allowed in any of the star catalog
C     columns, no check for NULLs is performed.
C
      CALL EKGD (  1, INDEX, 1, RA, NULL, FOUND )
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'RA value for star # not found. '    )
         CALL ERRINT ( '#', INDEX                           )
         CALL SIGERR ( 'SPICE(BADSTARINDEX)'                )
         CALL CHKOUT ( 'STCG01' )
         RETURN
      END IF
 
      CALL EKGD (  2, INDEX, 1, DEC, NULL, FOUND )
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'DEC value for star # not found. '   )
         CALL ERRINT ( '#', INDEX                           )
         CALL SIGERR ( 'SPICE(BADSTARINDEX)'                )
         CALL CHKOUT ( 'STCG01' )
         RETURN
      END IF
 
      CALL EKGD (  3, INDEX, 1, RASIG, NULL, FOUND )
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'RASIG value for star # not found. ' )
         CALL ERRINT ( '#', INDEX                           )
         CALL SIGERR ( 'SPICE(BADSTARINDEX)'                )
         CALL CHKOUT ( 'STCG01' )
         RETURN
      END IF
 
      CALL EKGD (  4, INDEX, 1, DECSIG, NULL, FOUND )
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'DECSIG value for star # not found.' )
         CALL ERRINT ( '#', INDEX                           )
         CALL SIGERR ( 'SPICE(BADSTARINDEX)'                )
         CALL CHKOUT ( 'STCG01' )
         RETURN
      END IF
 
      CALL EKGI (  5, INDEX, 1, CATNUM, NULL, FOUND )
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'CATNUM value for star # not found.' )
         CALL ERRINT ( '#', INDEX                           )
         CALL SIGERR ( 'SPICE(BADSTARINDEX)'                )
         CALL CHKOUT ( 'STCG01' )
         RETURN
      END IF
 
      CALL EKGC (  6, INDEX, 1, SPTYPE, NULL, FOUND )
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'SPTYPE value for star # not found.' )
         CALL ERRINT ( '#', INDEX                           )
         CALL SIGERR ( 'SPICE(BADSTARINDEX)'                )
         CALL CHKOUT ( 'STCG01' )
         RETURN
      END IF
 
      CALL EKGD (  7, INDEX, 1, VMAG, NULL, FOUND )
      IF ( .NOT. FOUND ) THEN
         CALL SETMSG ( 'VMAG value for star # not found. '  )
         CALL ERRINT ( '#', INDEX                           )
         CALL SIGERR ( 'SPICE(BADSTARINDEX)'                )
         CALL CHKOUT ( 'STCG01' )
         RETURN
      END IF
 
C
C     Convert angles to radians before return.
C
      RA     = RA     * RPD()
      DEC    = DEC    * RPD()
      RASIG  = RASIG  * RPD()
      DECSIG = DECSIG * RPD()
 
      CALL CHKOUT ( 'STCG01' )
      RETURN
      END
