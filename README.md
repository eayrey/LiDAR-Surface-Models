# LiDAR-Surface-Models
Some simple tools for creating digital surface, elevation, and height models from LiDAR in R.

-Simple_binning_DSM provides tools for creating a height model using rLiDAR, so that you don't need to import a CHM developed by another program. The tools use simple binning and thin plate spline interpolation. The result are rasters which can be exported or analized to create tree lists.

-Tiner_DSM creates surface models using a triangulated irregular network. This can be very slow with large datasets, but can produce better results than simple binning. 
