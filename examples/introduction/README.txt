These scripts are designed to illustrate how HSPF organizes information about the world into a computer program and how PyHSPF can be used to interact with HSPF. I would suggest going through these first to understand how HSPF works and how to use Python to supply the necessary information to HSPF. Open up and look at the comments in the scripts to understand how to apply the software for your own purposes. Here is a summary of what the scripts do:

intro01.py   -- Builds an HSPF model (hypothetical example with no external data sources)
intro02.py   -- Uses python scripting to re-create the Hunting Creek Daily HSPF model 
intro03.py   -- Builds the Hunting Creek HSPF model with an hourly time step
intro04.py   -- Utilizes the PyHSPF Postprocessor class to analyze the Hunting Creek example
intro05.py   -- Performs a calibration of the Hunting Creek watershed with Python and PyHSPF
hbnreader.py -- Reads the data in the "base.hbn" file
