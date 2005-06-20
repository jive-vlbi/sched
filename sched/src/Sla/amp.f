      SUBROUTINE sla_AMP (RA, DA, DATE, EQ, RM, DM)
*+
*     - - - -
*      A M P
*     - - - -
*
*  Convert star RA,Dec from geocentric apparent to mean place
*
*  The mean coordinate system is the post IAU 1976 system,
*  loosely called FK5.
*
*  Given:
*     RA       d      apparent RA (radians)
*     DA       d      apparent Dec (radians)
*     DATE     d      TDB for apparent place (JD-2400000.5)
*     EQ       d      equinox:  Julian epoch of mean place
*
*  Returned:
*     RM       d      mean RA (radians)
*     DM       d      mean Dec (radians)
*
*  References:
*     1984 Astronomical Almanac, pp B39-B41.
*     (also Lederle & Schwan, Astron. Astrophys. 134,
*      1-6, 1984)
*
*  Notes:
*
*  1)  The distinction between the required TDB and TT is
*      always negligible.  Moreover, for all but the most
*      critical applications UTC is adequate.
*
*  2)  The accuracy is limited by the routine sla_EVP, called
*      by sla_MAPPA, which computes the Earth position and
*      velocity using the methods of Stumpff.  The maximum
*      error is about 0.3 milliarcsecond.
*
*  3)  Iterative techniques are used for the aberration and
*      light deflection corrections so that the routines
*      sla_AMP (or sla_AMPQK) and sla_MAP (or sla_MAPQK) are
*      accurate inverses;  even at the edge of the Sun's disc
*      the discrepancy is only about 1 nanoarcsecond.
*
*  4)  Where multiple apparent places are to be converted to
*      mean places, for a fixed date and equinox, it is more
*      efficient to use the sla_MAPPA routine to compute the
*      required parameters once, followed by one call to
*      sla_AMPQK per star.
*
*  Called:  sla_MAPPA, sla_AMPQK
*
*  P.T.Wallace   Starlink   19 January 1993
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION RA,DA,DATE,EQ,RM,DM

      DOUBLE PRECISION AMPRMS(21)



      CALL sla_MAPPA(EQ,DATE,AMPRMS)
      CALL sla_AMPQK(RA,DA,AMPRMS,RM,DM)

      END
