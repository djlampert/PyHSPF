PyHSPF, Version 0.1.6
last updated: 08/02/2014
Developed by David J. Lampert and May M. Wu, Argonne National Laboratory

Summary: PyHSPF contains a library of subroutines to run the Hydrological 
Simulation Program in Fortran (HSPF), Version 12.2, Python extensions to 
the HSPF library, and classes for for building the required input files, 
performing simulations, and postprocessing simulation results.  

HSPF requires flowline and catchment data for a stream network, land use 
data for the stream reach subbasins, time series data of climate 
parameters, and hydrology parameters for each land use category/subbasin.  
These data sources can be supplied externally as needed (e.g., using 
Python extensions for geographic information systems (GIS) software). 
Alternatively, a series of preprocessing classes and routines were 
developed based on flowline and catchment data from the National 
Hydrography Dataset Plus Version 2 (NHDPlus), climate data from the 
National Climate Data Center (NCDC), and landuse data from the National 
Agricultural Statistics Service (NASS) Cropland Data Layer (CDL). 
The preprocessing and calibration routines require GDAL, PyShp, and 
Pillow in addition to NumPy, SciPy, and Matplotlib, and require the user
to download the source data files from NASS and NHDPlus. For more info 
contact me.

PyHSPF can be used to assimilate data into an HSPF model, build the 
HSPF input files, simulate the model over a period of time, and then 
provide statistics and plots of the simulation output. The "core" module 
requires NumPy, SciPy, and Matplotlib and can be used for generating input 
files. A series of examples is provided to illustrate PyHSPF core module 
usage.

The preprocessing modules contain classes to automate extraction of 
data from various databases including the National Solar Radiation 
Database (NSRDB), Global Historical Climate Network Daily (GHCND), Global
Summary of the Day (GSOD), the NCDC Hourly Precipitation Database 
(DSI-3240), the National Water Information System (NWIS), and the National
Hydrography Dataset Plus Version 2 (NHDPlus). The data extractors and 
calculators can be used independently of the HSPF classes.

Core Dependencies: 
Python Programming Language Version 3
Numeric Python (NumPy)
Scientific Python (SciPy)
Matplotlib

Preprocessing and Calibration Dependencies:
Geospatial Data Abstract Library (GDAL)
Python Shapefile Library (PyShp)
Python Imaging Library (Pillow)
7-zip

Installation: Extract the source zip files, then open a command prompt,
navigate to the PyHSPF directory, and run "python setup.py install." 
I am providing a few pre-built distributions in addition, although I
cannot guarantee that they will port to different machines. The binaries 
can be rebuilt using the open source GNU compiler collection (other 
compilers may work but are untested). The HSPF13 folder contains the 
source code including some minor modifications needed to simplify the 
compilation. Assuming gfortran and gcc are available as environment 
variables, the HSPF library can be rebuilt by navigating as before, 
running "python setup.py build," and then (as adminstrator) 
"python setup.py install." Contact me if you have trouble with the 
installation.

Testing: Open the Python interpreter (from a command prompt simply type 
"python"). Then try "import pyhspf"; the base Fortran subroutines can be
accessed from pyhspf.hspf. If the import succeeds, then try to run 
pyhspf.hspf.sydatepy(), which should print the date as a tuple. There 
are numerous examples scripts provided. The examples are simple and designed
to illustrate the basic concepts used to build HSPF models. Each example 
contains a detailed explanation of about the particular HSPF model used
and how to build the model with PyHSPF.

Acknowledgements: Developed with funding from the United States Department of 
Energy, Energy Efficiency & Renewable Energy, Bioenergy Technologies Office
(BETO). The sponsor in no way endorses this program.

Other Related Pages:
WATER (water analysis tool for energy resources) http://water.es.anl.gov/
Bioenergy KDF https://bioenergykdf.net/
