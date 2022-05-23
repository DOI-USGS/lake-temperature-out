# Use this script to set the environment variable. Needed if you are running without a container.
libpath <- "/caldera/projects/usgs/water/iidd/datasci/lake-temp/lake-temperature-out/Rlib_3_6"
Sys.setenv("R_LIBS_USER" = libpath)
.libPaths(libpath)

