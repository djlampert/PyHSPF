The examples provided illustrate how to use PyHSPF and how to think in HSPF. Here is a summary of the examples:

1. Building an HSPF model (hypothetical example with no external data sources)
2. Building and running the Hunting Creek HSPF model with a daily time step consistent with HSPExp
3. Building the Hunting Creek HSPF model with an hourly time step consistent with HSPExp
4. Utilizing the PyHSPF Postprocessor class to analyze the Hunting Creek example
5. Performing a calibration of the Hunting Creek watershed with Python and PyHSPF
6. Repeat of example 1 but illustrating the use of Special Actions
7. Utilizing PyHSPF's built-in extractor for Global Historical Climate Network Daily data
8. Utilizing PyHSPF's built-in extractor for all HSPF climate data needs

PyHSPF can do way more than this though--I can develop more examples as needed. Just contact me.

I have also put together some simulations using the test simulations distributed with HSPF. 
These files will use the data in the "data/tests" directory, and will make output files there.
The files are as follows:

test01.py -- creates the "test.wdm" file using the WDMUtil class, runs "TEST01.UCI" and plots the data
test02.py -- runs "TEST02.UCI"
test03.py -- runs "TEST03.UCI"
tests04_15.py -- runs all the other simulations (these are simple because there is no WDM file manipulation)
tests04_15_parllel.py -- runs the other simulations and shows how to run simulations in parallel