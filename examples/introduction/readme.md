These scripts illustrate how HSPF organizes information about the world into a computer program and how PyHSPF can be used to interact with the HSPF Library. They are designed to show how to use Python to supply the necessary information to HSPF and collect results from HSPF simulations. The scripts contain extensive comments that describe the software logic that may be helpful to undersatnd how to apply the software for alternative purposes.

- **intro01.py:** Builds an HSPF model (hypothetical example with no external data sources)
- **intro02.py:** Uses python scripting to re-create the Hunting Creek Daily HSPF model 
- **intro03.py:** Builds the Hunting Creek HSPF model with an hourly time step
- **intro04.py:** Utilizes the PyHSPF Postprocessor class to analyze the Hunting Creek example
- **intro05.py:** Illustrates how to modify parameters for calibration
- **intro06.py:** Illustrates how to search for a maximum value of an objective function
- **hbnreader.py:** Reads the data in the "base.hbn" file
