The examples provided illustrate how to use PyHSPF and how to think in HSPF. Here is a summary of the examples:

1.  Building an HSPF model (hypothetical example with no external data sources)
2.  Building the Hunting Creek HSPF model (the example from HSPExp) with a daily time step
3.  Building the Hunting Creek HSPF model with an hourly time step
4.  Utilizing the PyHSPF Postprocessor class to analyze the Hunting Creek example
5.  Performing a calibration of the Hunting Creek watershed with Python and PyHSPF
6.  Repeat of example 1 but illustrating the use of Special Actions
7.  Utilizing PyHSPF's built-in extractor for Global Historical Climate Network Daily data
8.  Utilizing PyHSPF's built-in extractor for all HSPF climate data needs
9.  Utilizing PyHSPF's built-in extractor for NHDPlus data
10. Utilizing PyHSPF's built-in extractor for NWIS data
11. Utilizing PyHSPF's built-in delineator for the watershed of a gage within a HUC8

I have also put together some simulations using the test simulations distributed with HSPF. 
These files will use the data in the "data/tests" directory, and will make output files there. The files are as follows:

test01.py -- shows how to create the "test.wdm" file, runs "test01.uci" and plots the data
test02.py -- adds datasets to "test.wdm" and runs "test02.uci"
test03.py -- adds datasets to "test.wdm" and runs "test03.uci"
tests04_15.py -- runs test04.uci through test15.uci simulations (no WDM file manipulation)
tests04_15_parallel.py -- shows how to run HSPF simulations in parallel

PyHSPF has more capabilities but they need to be developed. Also note that these examples may not be compatible with older versions of PyHSPF.
