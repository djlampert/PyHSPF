The potential evapotranspiration (PET) or atmospheric water demand of a land segment is one of the critical external time series required to use HSPF. Historically, PET was assumed to be some fraction (approximately 70%) of the observed evaporation from Class A pan data. There has been a great deal of work done since the development of the Stanford Watershed Model on evapotranspiration, and one of the current best models for PET is the Food and Agricultural Organization (FAO) method. The method uses the Penman-Monteith Equation to estimate reference evapotranspiration (RET), which is the PET for a reference crop (typically well-watered grass), and then extends the reference values to different land use categories using an empirical crop coefficient. The Penman-Monteith Equation is derived from an energy balance with the atmosphere. PyHSPF has an ETCalculator class that can estimate daily or sub-daily PET using climate data and landuse-specific crop coefficients. The class usage is illustrated in the following example scripts.

etcalculator01.py -- daily RET calculation
etcalculator02.py -- hourly RET calculation over short time
etcalculator03.py -- hourly RET calculation over long time
etcalculator04.py -- daily landuse-specific PET calculation
etcalculator05.py -- hourly landuse-specific PET calculation
etcalculator06.py -- hourly PET for several common categories
