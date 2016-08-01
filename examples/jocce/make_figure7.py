# make_figure7.py
#
# David Lampert
#
# Reads the statistics from the two-year calibrations for HUC 07080106.
# the script is memory-intensive since the WDM files with hourly runoff data
# must be loaded into memory, so parallel computing didn't decrease run
# time on my PC. The routine is customized using WDMUtil rather than the
# Postprocessor to save a bit of run time.
#

import os, gc, pickle, csv, datetime, time, numpy

from pyhspf import Postprocessor, WDMUtil

# output file

csvfile = 'two_year_calibrations.csv'

# paths to the data files

directory = 'C:/HSPF_data/07080106/two_year'
NWISgage  = '05472500'
start     = datetime.datetime(1981, 1, 1)
end       = datetime.datetime(2011, 1, 1)

# run the script if the results are not available

if not os.path.isfile(csvfile):

    # create a data structure to store the results

    calibrations = []

    # iterate through each year and calculate the summary data. 

    years = [y for y in range(1981, 2010, 2)]
    
    for y in years:

        begin = time.time()

        print('reading year', y)

        p = '{}/{}_{}/{}'.format(directory, y, y + 2, NWISgage)
    
        with open(p, 'rb') as f: hspfmodel = pickle.load(f)

        # calculate the runoff components in each land segment and store
        # the results in a structure as [subbasin][landuse][runoff/area]

        results = {}

        # use WDMUtil to read the data

        output = hspfmodel.filename + '_out.wdm'
        wdmutil = WDMUtil()
        wdmutil.open(output, 'r')

        # read the metadata for each timeseries in the WDM file

        dsns = wdmutil.get_datasets(output)    
        idconss = [wdmutil.get_attribute(output, n, 'IDCONS') for n in dsns]
        descrps = [wdmutil.get_attribute(output, n, 'DESCRP') for n in dsns]
        staids  = [wdmutil.get_attribute(output, n, 'STAID ') for n in dsns]

        # go through the impervious land segments to get the surface runoff

        for o in hspfmodel.implnds:

            c = o.subbasin
        
            # make a data dictionary for each subbasin
        
            results[c] = {}

            # make a data dictionary for the land use and add the area
        
            results[c][o.landtype] = {'area': o.area}

            # find the time series number for the variables in the WDM file

            for v in ('SURO', 'IMPEV'):

                iterable = zip(dsns, idconss, descrps, staids)
                for n, idcons, descrp, staid in iterable:

                    # first pass: subbasin

                    if staid == o.subbasin:

                        # second: land use category

                        if descrp == o.landtype:

                            # third: constituent id

                            if idcons == v:

                                # get the time series data
                        
                                data = wdmutil.get_data(output, n,
                                                        start=start, end=end)

                                # add the total to the database
                            
                                results[c][o.landtype][v] = data.sum()

        # go through the pervious land segments and get the area and runoff

        for o in hspfmodel.perlnds:

            c = o.subbasin
        
            # add the area
        
            results[c][o.landtype] = {'area': o.area}
           
            # add the runoff components

            for v in ('SURO', 'IFWO', 'AGWO', 'TAET'):
            
                # find the time series number for the variables in the WDM file

                iterable = zip(dsns, idconss, descrps, staids)
                for n, idcons, descrp, staid in iterable:

                    # first pass: subbasin

                    if staid == c:

                        # second: land use category
                    
                        if descrp == o.landtype:

                            # third: constituent id
                
                            if idcons == v:

                                # get the time series data

                                data = wdmutil.get_data(output, n,
                                                        start=start, end=end)

                                # add the total to the database

                                results[c][o.landtype][v] = data.sum()

        # close up the WDM file

        wdmutil.close(output)
        wdmutil = None

        # free up memory
        
        gc.collect()
    
        # find the part of the watershed contributing to the gage
    
        d = {v:k for k, v in hspfmodel.subbasin_timeseries['flowgage'].items()}
        comid = d[NWISgage]

        # find the comids of the all the subbasins upstream of the gage

        comids = [comid]
        n = 0
        while len(comids) != n:
            n = len(comids)
            for c in hspfmodel.updown:
                if hspfmodel.updown[c] in comids and c not in comids:
                    comids.append(c)

        # aggregate the results to get the overall averages

        suro = 0
        ifwo = 0
        agwo = 0
        evap = 0
        area = 0
        for c in results:
            if c in comids:
                for l in results[c]:
                    suro += results[c][l]['SURO']  * results[c][l]['area']
                    area += results[c][l]['area']

                    if l == 'Impervious':
                        evap += results[c][l]['IMPEV'] * results[c][l]['area']
                    else:
                        ifwo += results[c][l]['IFWO'] * results[c][l]['area']
                        agwo += results[c][l]['AGWO'] * results[c][l]['area']
                        evap += results[c][l]['TAET'] * results[c][l]['area']
        
        runoff             = suro / area / (end - start).days * 365.25
        interflow          = ifwo / area / (end - start).days * 365.25
        baseflow           = agwo / area / (end - start).days * 365.25
        evapotranspiration = evap / area / (end - start).days * 365.25

        # use the postprocessor to get the simulation stats

        dates = start,end
        postprocessor = Postprocessor(hspfmodel, dates, comid = comid)

        # get the NSE during the calibration period

        d = datetime.datetime(y, 1, 1), datetime.datetime(y + 2 , 1, 1)
        
        stimes, sflow = postprocessor.get_sim_flow(comid, tstep = 'daily',
                                                   dates = d)
        otimes, oflow = postprocessor.get_obs_flow(tstep = 'daily',
                                                   dates = d)

        NSEcalibration = (1 - sum((numpy.array(sflow)-numpy.array(oflow))**2) /
                          sum((numpy.array(oflow) - numpy.mean(oflow))**2))

        # get the total precipitation during the calibration period

        ptimes, precip = postprocessor.get_precipitation(comid, dates = d)

        precipitation = sum(precip) / (d[1] - d[0]).days * 365.25

        # get the validation NSE

        stimes, sflow = postprocessor.get_sim_flow(comid, tstep = 'daily')
        otimes, oflow = postprocessor.get_obs_flow(tstep = 'daily')
    
        NSEvalidation = (1 - sum((numpy.array(sflow) - numpy.array(oflow))**2) /
                         sum((numpy.array(oflow) - numpy.mean(oflow))**2))
    
        # percent error in runoff
    
        error = (sum(sflow) - sum(oflow)) / sum(oflow)

        # close up the files

        postprocessor.close()

        data = {}
        data['NSE validation']     = NSEvalidation
        data['NSE calibration']    = NSEcalibration
        data['baseflow']           = baseflow
        data['interflow']          = interflow
        data['surface runoff']     = runoff
        data['evapotranspiration'] = evapotranspiration
        data['precipitation']      = precipitation
        data['percent error']      = error

        calibrations.append(data)
    
        print('finished', y, 'in',
              '{:.1f}'.format(time.time() - begin), 'seconds')

    rows = []
    categories = ['NSE Calibration',
                  'NSE Validation',
                  'Percent Error in Total Runoff',
                  'Precipitation During Calibration Period (mm/yr)',
                  'Evapotranspiration (mm/yr)',
                  'Baseflow (mm/yr)',
                  'Interflow (mm/yr)',
                  'Surface Runoff (mm/yr)',
                  ]
                      
    rows.append(['Year'] + categories)
    
    for year, data in zip(years, calibrations):

        row = [year] + [data['NSE calibration'],
                        data['NSE validation'],
                        data['percent error'],
                        data['precipitation'],
                        data['evapotranspiration'],
                        data['baseflow'],
                        data['interflow'],
                        data['surface runoff'],
                        ]
        rows.append(row)

    with open(csvfile, 'w', newline = '') as f:
        writer = csv.writer(f)
        for row in rows: writer.writerow(row)

else:

    print('exists!')

print('done!')
