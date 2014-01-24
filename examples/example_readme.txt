My perspective on HSPF is very different than that presented in other HSPF interfaces. I have attempted to create a more transparent approach to understand the calculations and conceptions of the Stanford Watershed Model (the basis for HSPF) rather than hiding everything behind a GUI. I realize this may not be what many people want but I would have prefered it if something like this existed a few years ago when I started learning HSPF.  I hate black boxes.  So, a brief description of the examples: 

example1.py -- illustrates the basic concepts of running HSPF, the watershed data management (WDM) file for timeseries and the user control input (UCI) file for commands. this example uses no external data files.

example2.py -- illustrates how to run a daily time step model using the data from the HSPF Expert (HSPExp) example, Hunting Creek watershed in the Patuxent River Basin, Maryland

example3.py -- illustrates how to build a basic hourly time step model.  In the end the base hydrology model will need to be changed, so the idea is to store all the info in one object (an instance of the HSPFModel class).  In a "real" model, the idea is to build the base model and save it, then run it later.

example4.py -- illustrates how to use the Postprocessor class to gather information from the simulation.

example5.py -- illustrates one way to calibrate HSPF.

lots more examples are needed.  PyHSPF can do many, many more things.  one potentially useful application (with nothing to do with HSPF) is the Penman Equation calculator.  I don't believe anything like this exists elsewhere (it was a lot of work).  but I don't have to illustrate it all.  contact me with any questions.

Dave
