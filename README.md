# vegetatsion
vegetation analysis

This series of scripts make several vegetation analysis based on a sampling vegetation database and can obtain results for different strata (trees, shrubs and low). Currently all the names and descriptions are in Spanish. Was one of my first projects so it needs several adjustments and functions might not be the fastest :). Vegetation database need to have determined names so the scripts can work correctly.

The scripts are chained so the proper order to use them should be:
1. veg_an
2. Dend
3. SP
4. descripcion
5. sitios

veg_an
Does an analysis for each site, each species, and each species and site (combined). Obtains attributes as density, basal area, coverage and relative importance value (RIV).

Dend
Obtains a dendrogram to associate the sampling plots according to the their species composition and associated RIV.
Needs a .csv with the names of each group to inser it in the plot of the dendrogram

SP 
Does species accumulation curve

Descripcion
Species and Families count

Sitios
Makes a short description of the characteristics of the sampling sites, according to their altitude, orientation, slope.
