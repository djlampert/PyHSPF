**PyHSPF**
 		 
PyHSPF contains Python extensions to the Hydrological Simulation Program in Fortran (HSPF), including classes for gathering input data, building input files, performing simulations, postprocessing results, calibrating hydrology process parameters, and forecasting climate and land use change effects on water resources. Flowline and catchment data for a stream network, land use data for the stream reach subbasins, time series data of climate parameters, and hydrology parameters for each land use category/subbasin are needed to use HSPF. The data can be supplied externally as needed (e.g., using Python extensions for various classes of data) or using PyHSPF's preprocessing classes. Tools have developed to interact with data from the following databases:

- National Hydrography Dataset Plus Version 2 (NHDPlus)
- National Water Information System (NWIS)
- National Inventory of Dams (NID)
- Cropland Data Layer (CDL)
- National Solar Radiation Database (NSRDB)
- Global Historical Climate Network Daily (GHCND)
- Global Summary of the Day (GSOD)
- Hourly Precipitation Database (DSI-3240)

PyHSPF can be used to assimilate data into an HSPF model, build the HSPF input files, simulate the model over a period of time, and then provide statistics and plots of the simulation output. The "core" module requires NumPy, SciPy, and Matplotlib and can be used for generating input files. The "preprocessing" modules contain classes to automate extraction of data from the various databases. The data extractors and calculators can be used independently of the HSPF classes.
		
**Core Dependencies:**

- Python Programming Language Version 3
- Numeric Python (NumPy)
- Scientific Python (SciPy)
- Matplotlib

**Preprocessing Dependencies:**

- Geospatial Data Abstract Library (GDAL)
- Python Shapefile Library (PyShp)
- Python Imaging Library (Pillow)
- USGS Data Retrieval Python Package (dataretrieval)
- 7-zip

**Installation:** Pre-built distributions are provided for Windows at the Python Package Index (PyPI), although these cannot be guaranteed to port to different machines. The HSPF library and associated PyHSPF code can be rebuilt using the open source GNU compiler collection (other compilers may work but are untested). The misc directory contains the HSPF source code including some minor modifications needed to simplify the compilation. Assuming gfortran and gcc are available as environment variables, the HSPF library can be built by navigating to the PyHSPF src folder, running "python setup.py config --compiler=mingw32 build --compiler=mingw32," and then (as adminstrator) "python setup.py install." Please report any issues encountered with the compilation, installation or running the example files.

**Testing:** Open the Python interpreter (from a command prompt simply type "python"). Then try "import pyhspf"; the base Fortran subroutines can be accessed from pyhspf.hspf. If the import succeeds, then try to run pyhspf.hspf.sydatepy(), which should print the date as a tuple. There are numerous examples scripts provided. The examples are designed to illustrate the basic concepts used to build HSPF models and extract relevant hydrological data.

**Acknowledgements:** Developed in part with funding from the United States Department of Energy, Energy Efficiency & Renewable Energy, Bioenergy Technologies Office. 
