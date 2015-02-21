These scripts were developed to run the test simulations distributed with the HSPF source code. They use data in the files located in the "data" directory, and make output files there. These scripts may be of interest to people who are already familiar with HSPF or want a deeper understanding of how PyHSPF communicates with HSPF. Information from the real world must be translated into the User Control Input (UCI) and Watershed Data Management (WDM) files to perform HSPF simulations. Existing UCI files could be adapted and run in Python using the approaches outlines in these examples. The scripts do the following:

test01.py -- shows how to create the "test.wdm" file, runs "test01.uci" and plots the data
test02.py -- adds datasets to "test.wdm" and runs "test02.uci"
test03.py -- adds datasets to "test.wdm" and runs "test03.uci"
tests04_15.py -- runs test04.uci through test15.uci simulations (no WDM file manipulation)
tests04_15_parallel.py -- shows how to run HSPF simulations in parallel

