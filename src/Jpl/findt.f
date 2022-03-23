      DOUBLE PRECISION FUNCTION FINDT (YEAR)
c
c finds deltaT (TDT - UT1), given the year.  interpolates into 
c values on page K-9 of the Astronomical Almanac, and predictions 
c from the NEOS up to 2008.  currently only does a linear 
c interpolation, which is probably OK for epochs near 2000.0.  
c strictly, should probably use 2nd order interpolation, since the 
c variations seem to go as time squared at some points.
c
c NOTE: as new accurate values for deltaT become available, they
c       should be used to replace the predictions from 1997-2008
c

      DOUBLE PRECISION YEAR, TAB_DT(1820:2008), sla_DT
      INTEGER IYEAR

      DATA TAB_DT /
     * 12.00,11.70,11.40,11.10,10.60,10.20, 9.60, 9.10, 8.60, 8.00,
c 1830
     *  7.50, 7.00, 6.60, 6.30, 6.00, 5.80, 5.70, 5.60, 5.60, 5.60,
c 1840
     *  5.70, 5.80, 5.90, 6.10, 6.20, 6.30, 6.50, 6.60, 6.80, 6.90,
c 1850
     *  7.10, 7.20, 7.30, 7.40, 7.50, 7.60, 7.70, 7.70, 7.80, 7.80,
c 1860
     *  7.88, 7.82, 7.54, 6.97, 6.40, 6.02, 5.41, 4.10, 2.92, 1.82,
c 1870
     *  1.61, 0.10,-1.02,-1.28,-2.69,-3.24,-3.64,-4.54,-4.71,-5.11,
c 1880
     * -5.40,-5.42,-5.20,-5.46,-5.46,-5.79,-5.63,-5.64,-5.80,-5.66,
c 1890
     * -5.87,-6.01,-6.19,-6.64,-6.44,-6.47,-6.09,-5.76,-4.66,-3.74, 
c 1900
     * -2.72,-1.54,-0.02, 1.24, 2.64, 3.86, 5.37, 6.14, 7.75, 9.13,
c 1910
     * 10.46,11.53,13.36,14.65,16.01,17.20,18.24,19.06,20.25,20.95,
c 1920
     * 21.16,22.25,22.41,23.03,23.49,23.62,23.86,24.49,24.34,24.08,
c 1930
     * 24.02,24.00,23.87,23.95,23.86,23.93,23.73,23.92,23.96,24.02,
c 1940
     * 24.33,24.83,25.30,25.70,26.24,26.77,27.28,27.78,28.25,28.71,
c 1950
     * 29.15,29.57,29.97,30.36,30.72,31.07,31.35,31.68,32.18,32.68,
c 1960
     * 33.15,33.59,34.00,34.47,35.03,35.73,36.54,37.43,38.29,39.20, 
c 1970
     * 40.18,41.17,42.23,43.37,44.49,45.48,46.46,47.52,48.53,49.59,
c 1980
     * 50.54,51.38,52.17,52.96,53.79,54.34,54.87,55.32,55.82,56.30,
c 1990
     * 56.86,57.57,58.31,59.12,59.98,60.79,61.99,62.60,63.60,64.60,
c 2000
     * 65.82,66.77,67.72,68.67,69.62,70.58,71.55,72.54,73.54 /

        IF (YEAR .GT. 1820.0 .AND. YEAR .LE. 2008) THEN
           IYEAR = INT (YEAR)
           FINDT = TAB_DT(IYEAR) +
     *             (YEAR - IYEAR) * (TAB_DT(IYEAR+1) - TAB_DT(IYEAR))
        ELSE
           FINDT = sla_DT (YEAR)
        END IF
        RETURN
        END
