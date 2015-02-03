C$Procedure   STCF01 (STAR catalog type 1, find stars in RA-DEC box)
 
      SUBROUTINE STCF01 ( CATNAM, WESTRA, EASTRA, STHDEC, NTHDEC,
     .                    NSTARS)
 
C$ Abstract
C
C     Search through a type 1 star catalog and return the number of
C     stars within a specified RA - DEC rectangle.
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
 
      CHARACTER*(*)         CATNAM
      DOUBLE PRECISION      WESTRA
      DOUBLE PRECISION      EASTRA
      DOUBLE PRECISION      STHDEC
      DOUBLE PRECISION      NTHDEC
      INTEGER               NSTARS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CATNAM      I   Catalog table name.
C     WESTRA      I   Western most right ascension in radians.
C     EASTRA      I   Eastern most right ascension in radians.
C     STHDEC      I   Southern most declination in radians.
C     NTHDEC      I   Northern most declination in radians.
C     NSTARS      O   Number of stars found.
C
C$ Detailed_Input
C
C     CATNAM      is name of the catalog data table. This name is
C                 returned by the catalog loader routine STCL01.
C
C     WESTRA      are right ascension and declination constraints
C     EASTRA      giving the western, eastern, southern and northern
C     STHDEC      boundaries of a search rectangle as follows:
C     NTHDEC
C                       RA  BETWEEN WESTRA  AND EASTRA  and
C                       DEC BETWEEN STHDEC AND NTHDEC
C
C                 where RA and DEC are the right ascension and
C                 declination of a star. WESTRA always represents
C                 "west" side of this rectangle and EASTRA -- the
C                 "east" side.  STHDEC represents the "south" side
C                 of the rectangle, NTHDEC represents the "north"
C                 side of the rectangle.
C
C                 For an observer standing on the surface
C                 of the earth at the equator, the west side of the
C                 rectangle ( the side associated with WESTRA) rises
C                 first. The east side (the side associated with
C                 EASTRA) rises last.  All meridians that rise between
C                 the rising of the west and east edges of the
C                 rectangle  cross through the RA-DEC rectangle.
C
C                 To specify the 6 degrees wide RA-DEC
C                 square centered on the celestical equator that
C                 has western most right ascension of 357 degrees,
C                 use the following values for WESTRA, EASTRA, STHDEC,
C                 and NTHDEC (we multiply the angles by the SPICELIB
C                 function RPD to convert degrees to radians).
C
C                      WESTRA  = 357.0D0 * RPD()
C                      EASTRA  =   3.0D0 * RPD()
C                      STHDEC  =  -3.0D0 * RPD()
C                      DEXMAX  =   3.0D0 * RPD()
C
C                 To specify a 5 degree wide RA-DEC square that has
C                 western most right ascension 10 degrees and
C                 eastern most right ascension 15 degrees and southern
C                 most declination of 45 degrees, assign the following
C                 values to WESTRA, EASTRA, STHDEC and NTHDEC.
C
C                      WESTRA  =  10.0D0 * RPD()
C                      EASTRA  =  15.0D0 * RPD()
C                      STHDEC  =  45.0D0 * RPD()
C                      DEXMAX  =  50.0D0 * RPD()
C
C                 All RA and DECS should be in radians and relative
C                 to the J2000 inertial frame.  
C
C                 All Right Ascension values should be in the
C                 interval [0, 2*pi ).  This routine does
C                 not "fold" Right Ascension values into the this
C                 interval.  For example if you request stars in
C                 whose right ascensions lie between 3*pi and 4*pi
C                 no stars will be found.
C
C                 All Declination values should be in the interval
C                 [-pi,pi].
C
C$ Detailed_Output
C
C     NSTARS      is number of catalog stars found within the
C                 specified RA - DEC rectangle.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If no star catalog has been loaded, an error will be
C        signalled by a routine in the call tree of this routine.
C
C     2) If the catalog query fails for any reason then
C        the error 'SPICE(QUERYFAILURE)'is signalled.
C
C$ Files
C
C     This routine searches for stars within SPICE type 1 star catalog
C     files that have been loaded by calls to the STCL01 routine and
C     that contain that catalog data table named CATNAM.
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
C     the SPICE type 1 star catalog. It allows the caller to find all
C     stars within a specified RA - DEC rectangle in the SPICE
C     EK type 1 star catalog files loaded by STCL01. This
C     subroutine MUST NOT be called before a catalog file has
C     been loaded.
C
C     Other routines in the SPICE type 1 star catalog access
C     family are:
C
C        STCL01  load the catalog file and make its data
C                available for search and retrieval.
C
C        STCG01  retrieve position and characteristics for
C                a specified star in the set found by this
C                routine.
C
C$ Examples
C
C     In the following code fragment, STCF01 is used to find
C     all stars within a specified RA - DEC rectangle in a SPICE
C     EK type 1 star catalog.
C
C     C
C     C     Load catalog file.
C     C
C           CALL STCL01 ( CATFN, TABNAM, HANDLE )
C     C
C     C     Search through the loaded catalog.
C     C
C           CALL STCF01 ( TABNAM, WESTRA,  EASTRA,
C          .              STHDEC, NTHDEC, NSTARS )
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
C     1) The catalog file STCF01 searches through MUST be loaded
C        by STCL01 before STCF01 is called.
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
C      find stars in RA-DEC rectangle in type 1 star catalog
C
C-&
 
C
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      DPR
 
C
C     Local parameters.
C
      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 512 )
 
      INTEGER               QRYLEN
      PARAMETER           ( QRYLEN = 512 )
 
C
C     Local variables
C
      CHARACTER*(ERRLEN)    ERRMSG
      CHARACTER*(QRYLEN)    QUERY
      CHARACTER*(QRYLEN)    QRYTM1
      CHARACTER*(QRYLEN)    QRYTM2
 
      DOUBLE PRECISION      RAMIN
      DOUBLE PRECISION      RAMAX
      DOUBLE PRECISION      DECMIN
      DOUBLE PRECISION      DECMAX
 
      LOGICAL               ERROR
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STCF01' )
      END IF
 
C
C     Query templates.
C
      QRYTM1 =
     . 'SELECT '                                            //
     . 'RA, DEC, RA_SIGMA, DEC_SIGMA,'                      //
     . 'CATALOG_NUMBER, SPECTRAL_TYPE, VISUAL_MAGNITUDE '   //
     . 'FROM # '                                            //
     . 'WHERE '                                             //
     . '( RA  BETWEEN # AND # ) AND '                       //
     . '( DEC BETWEEN # AND # ) '
 
      QRYTM2 =
     . 'SELECT '                                            //
     . 'RA, DEC, RA_SIGMA, DEC_SIGMA,'                      //
     . 'CATALOG_NUMBER, SPECTRAL_TYPE, VISUAL_MAGNITUDE '   //
     . 'FROM # '                                            //
     . 'WHERE '                                             //
     . '( ( RA BETWEEN # AND 360 ) OR '                     //
     . '  ( RA BETWEEN 0 AND #   )      ) AND '             //
     . '  ( DEC BETWEEN # AND # ) '
 
C
C     Choose query template to be used.
C
      IF ( WESTRA .LE. EASTRA ) THEN
         QUERY = QRYTM1
      ELSE
         QUERY = QRYTM2
      END IF
 
C
C     Convert angles in radians to angles in degrees.
C
      RAMIN   = WESTRA * DPR()
      RAMAX   = EASTRA * DPR()
      DECMIN  = STHDEC * DPR()
      DECMAX  = NTHDEC * DPR()
 
C
C     Construct query using inputs and chosen template.
C
      CALL REPMC( QUERY, '#', CATNAM,     QUERY )
      CALL REPMD( QUERY, '#', RAMIN,  15, QUERY )
      CALL REPMD( QUERY, '#', RAMAX,  15, QUERY )
      CALL REPMD( QUERY, '#', DECMIN, 15, QUERY )
      CALL REPMD( QUERY, '#', DECMAX, 15, QUERY )
 
C
C     Submit query and get number of stars. Check for
C     errors in QUERY.
C
      CALL EKFIND( QUERY, NSTARS, ERROR, ERRMSG )
 
      IF ( ERROR ) THEN
         CALL SETMSG ( 'Error querying type 1 star catalog. '//
     .                 'Error message: # '                  )
         CALL ERRCH  ( '#', ERRMSG                          )
         CALL SIGERR ( 'SPICE(QUERYFAILURE)'                )
         CALL CHKOUT ( 'STCF01' )
         RETURN
      END IF
 
      CALL CHKOUT ( 'STCF01' )
      RETURN
      END
