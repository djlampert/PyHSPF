# calibratormodel.py
#
# David J. Lampert (djlampert@gmail.com)
#
# contains the CalibratorModel class, a child of the HSPFModel class to use
# for running simulations during a hydrology calibration that minimizes 
# memory requirements

from pyhspf.core import HSPFModel, WDMUtil

class CalibratorModel(HSPFModel):
    """
    Child of the HSPFModel class that strips down the external targets block.
    """

    def __init__(self,
                 units = 'Metric',
                 ):

        HSPFModel.__init__(self, units = units)

    def build_uci(self,
                  reach,
                  start, 
                  end, 
                  states = None, 
                  atemp = False, 
                  snow = False, 
                  hydrology = False, 
                  verbose = False,
                  ):
        """
        Builds the User Control Input (UCI) file for an HSPF Simulation for
        a hydrology calibration.
        """

        if verbose: print('generating the UCI file from the watershed data\n')


        # file types and name; see HSPF documentation for more explanation  
        # ftypes are the different file classes for HSPF I/O (echo, WDM, etc)
        # funits are the Fortran numbers assigned to the files (10-98)
        # fnames are the names or paths to the files

        self.ucifile    = '{}.uci'.format(self.filename)
        self.wdmoutfile = '{}_out.wdm'.format(self.filename)

        # echo file for input file processing name (assumed same as uci)

        self.echofile = self.ucifile[:-4] + '.ech'
        
        # external target names

        self.ftypes = ['MESSU',               'WDM1',          'WDM2']
        self.fnames = [self.echofile, self.wdminfile, self.wdmoutfile]
        self.funits = [10,                        11,              12]

        # set the initial states if provided (in dictionary form from the
        # Postprocessor class)

        if states is not None: self.set_states(states)

        # "RUN" marks the start of the execution

        lines = ['RUN', '']

        # add the GLOBAL block

        lines = lines + self.global_block(1, start, end)

        # add the FILES block

        lines = lines + self.files_block()

        # add the OPN SEQUENCE block

        lines = lines + self.opn_sequence_block()

        # add SPEC-ACTIONS block

        lines = lines + self.spec_actions_block()

        # add the PERLND block

        lines += self.perlnd_block(hydrology = hydrology, 
                                   atemp = atemp, snow = snow) 

        # add the IMPLND block

        lines += self.implnd_block(hydrology = hydrology, atemp = atemp,
                                   snow = snow)

        # add the RCHRES block if needed

        if hydrology: lines += self.rchres_block(hydrology = hydrology) 

        # add the EXT SOURCES block

        lines = lines + self.ext_sources_block()

        # add the SCHEMATIC and MASS-LINK blocks if needed

        if hydrology:

            lines = lines + self.schematic_block()

            lines = lines + self.masslink_block(hydrology = hydrology)

        # add the EXT TARGETS block

        lines += self.ext_targets_block(reach, start.year, verbose = verbose)

        # add the FTABLES block for the RCHRESES if needed

        if hydrology: lines += self.ftables_block()

        # "close" the simulation info

        lines.append('END RUN')

        # write the lines to the uci file

        with open(self.ucifile, 'w') as f:
        
            for line in lines: f.write(line + '\n')

    def ext_targets_block(self, 
                          comid, 
                          year,
                          tcode = 4,
                          tsstep = 1,
                          verbose = False,
                          ):
        """
        Adds the EXT TARGETS block to a UCI file and creates the output WDM 
        file.
        
        tcode is the time code: 2 = minutes, 3 = hours, 4 = days
        tsstep is the time step in tcode units
        
        e.g., tcode = 3, tsstep = 4 is a 4-hour time step

        this method enables a single external target with aggregation that
        isn't possible using the HSPFModel in the core.
        """
        
        lines = ['EXT TARGETS',
                 '<-Volume-> <-Grp> <-Member-><--Mult-->Tran <-Volume->' +
                 ' <Member> Tsys Aggr Amd ***',
                 '<Name>   x        <Name> x x<-factor->strg <Name>' +
                 '   x <Name>qf  tem strg strg***']
    
        wdm = WDMUtil(verbose = verbose, messagepath = self.messagepath)
        wdm.open(self.wdmoutfile, 'w')

        # dataset numbers are assigned by reach in order (subject to revision)
        # keep track of dsns in a dictionary

        n = 1

        # since this class is just for calibration of a single gage, only need
        # to keep up with reach outflow volume

        otype  = 'RCHRES'
        group  = 'HYDR'
        var    = 'ROVOL'
        tstype = 'VOL'
        tsform = 1
        idcons = 'ROVOL'
        func   = 'SUM '

        # this overwrites all the rchreses with just the comid for the gage

        reaches = [r for r in self.rchreses if r.subbasin == comid]

        new = self.add_ext_targets(reaches, wdm, year, n, otype, 
                                   group, var, tsform, tstype, idcons, func,
                                   tcode, tsstep)
        lines = lines + new
        n    += len(new)        

        # close the wdmeditor

        wdm.close(self.wdmoutfile)
        wdm.close_message()
    
        # finish up 
    
        lines = lines + ['END EXT TARGETS', '']
    
        return lines

    def build_submodel(self,
                       hspfmodel,
                       comid,
                       upcomids = [],
                       name = None,
                       verbose = True,
                       ):
        """
        Builds a submodel from an existing HSPFModel "model" by removing
        any subbasins downstream from "comid" and (optionally) any subbasins
        upstream from "upcomids." Removing upstream subbasins necessitates
        external time series representing mass inflows.
        """

        if name is None: name = comid

        self.build_from_existing(hspfmodel, name)

        # turn on the modules

        #if self.atemp:     model.add_atemp()
        #if self.snow:      model.add_snow()
        #if self.hydrology: model.add_hydrology()

        # add the time series

        #for f in self.hspfmodel.flowgages:
        #    start, tstep, data = self.hspfmodel.flowgages[f]
        #    model.add_timeseries('flowgage', f, start, data, tstep = tstep)

        #for p in self.hspfmodel.precipitations: 
        #    start, tstep, data = self.hspfmodel.precipitations[p]
        #    model.add_timeseries('precipitation', p, start, data, tstep = tstep)

        #for e in self.hspfmodel.evaporations: 
        #    start, tstep, data = self.hspfmodel.evaporations[e]
        #    model.add_timeseries('evaporation', e, start, data, tstep = tstep)

        #for t in self.hspfmodel.temperatures:
        #    start, tstep, data = self.hspfmodel.temperatures[t]
        #    model.add_timeseries('temperature', t, start, data, tstep = tstep)

        #for t in self.hspfmodel.dewpoints:
        #    start, tstep, data = self.hspfmodel.dewpoints[t]
        #    model.add_timeseries('dewpoint', t, start, data, tstep = tstep)

        #for t in self.hspfmodel.windspeeds:
        #    start, tstep, data = self.hspfmodel.windspeeds[t]
        #    model.add_timeseries('wind', t, start, data, tstep = tstep)

        #for t in self.hspfmodel.solars:
        #    start, tstep, data = self.hspfmodel.solars[t]
        #    model.add_timeseries('solar', t, start, data, tstep = tstep)

        #for t in self.hspfmodel.snowfalls:
        #    start, tstep, data = self.hspfmodel.snowfalls[t]
        #    model.add_timeseries('snowfall', t, start, data, tstep = tstep)

        #for t in self.hspfmodel.snowdepths:
        #    start, tstep, data = self.hspfmodel.snowdepths[t]
        #    model.add_timeseries('snowdepth', t, start, data, tstep = tstep)

        #for tstype, identifier in self.hspfmodel.watershed_timeseries.items():

        #    model.assign_watershed_timeseries(tstype, identifier)

        #for tstype, d in self.hspfmodel.subbasin_timeseries.items():

        #    for subbasin, identifier in d.items():
                
        #        if subbasin in model.subbasins:

        #            model.assign_subbasin_timeseries(tstype, subbasin, 
        #                                             identifier)

        #for tstype, d in self.hspfmodel.landuse_timeseries.items():

        #    for luc, identifier in d.items():

        #        model.assign_landuse_timeseries(tstype, luc, identifier)

        #return model


        # find the subbasins between the outlet and the upstream comids and
        # store in an updown dictionary

        updown = {up:down 
                  for up, down in hspfmodel.updown.items() 
                  if down == comid}

        current = 0
        while current != len(updown):

            # see if the current length changes to check if done

            current = len(updown)

            # iterate throught the subbasins and see if any need to be added

            for up, down in hspfmodel.updown.items():

                if (up not in updown   and   # not already there
                    up not in upcomids and   # between the boundaries
                    down in updown):         # downstream is there
                    
                    updown[up] = down
        
        # overwrite the old updown dictionary

        self.updown = updown

        # overwrite the inlets and outlets

        self.inlets  = [hspfmodel.updown[c] for c in upcomids]
        self.outlets = [comid]

        # overwrite the old subbasin dictionary

        self.subbasins = {c: subbasin 
                              for c, subbasin in self.subbasins.items()
                              if c in updown or c == comid}

        # overwrite the perlnd, implnd, rchres lists

        self.perlnds  = [p for p in self.perlnds 
                             if p.subbasin in self.subbasins]
        self.implnds  = [i for i in self.implnds
                             if i.subbasin in self.subbasins]
        self.rchreses = [r for r in self.rchreses 
                             if r.subbasin in self.subbasins]
        
        # build with the updated model subbasin info

        #submodel.build()

        # add in the modules

        #if self.temp: submodel.add_temp()

        #if self.snow: 
        #    
        #    densities = [o.RDENPF 
        #                 for o in hspfmodel.perlnds + hspfmodel.implnds]
        #    depths    = [o.packsnow / o.RDENPF 
        #                 for o in hspfmodel.perlnds + hspfmodel.implnds]
        #
        #    depth   = sum(depths) / len(depths)
        #    density = sum(densities) / len(densities)
        #
        #    submodel.add_snow(depth = depth, density = density)            
        #
        #if self.hydrology: submodel.add_hydrology()
        
        # add the flowgage data to the model

        for identifier in hspfmodel.flowgages:
            if identifier == comid:
                start_date, tstep, data = hspfmodel.flowgages[identifier]
                self.add_timeseries('flowgage', identifier, start_date, 
                                        data, tstep = tstep)

        # add the watershed time series dictionaries for the model

        timeseries = {'inflow':        hspfmodel.inflows,
                      'temperature':   hspfmodel.temperatures,
                      'dewpoint':      hspfmodel.dewpoints,
                      'wind':          hspfmodel.windspeeds,
                      'solar':         hspfmodel.solars,
                      'snowfall':      hspfmodel.snowfalls,
                      'snowdepth':     hspfmodel.snowdepths,
                      'precipitation': hspfmodel.precipitations,
                      'evaporation':   hspfmodel.evaporations,
                      'flowgage':      hspfmodel.flowgages,
                      }

        #for tstype, d in timeseries.items():
        #    for identifier in d: 
        #        start_date, tstep, data = d[identifier]
        #        self.add_timeseries(tstype, identifier, start_date, data, 
        #                                tstep = tstep)

        # add and assign all the watershed timeseries

        for tstype, identifier in hspfmodel.watershed_timeseries.items():
            ts = timeseries[tstype]
            s, t, data = ts[identifier]
            self.add_timeseries(tstype, identifier, s, data, tstep = t)
            self.assign_watershed_timeseries(tstype, identifier)

        # add and assign all the land use time series

        for tstype, d in hspfmodel.landuse_timeseries.items():
            ts = timeseries[tstype]
            for landuse, identifier in d.items():
                s, t, data = ts[identifier]
                self.add_timeseries(tstype, identifier, s, data, tstep = t)
                self.assign_landuse_timeseries(tstype, landuse, identifier)

        # add and assign subbasin land use time series inside the submodel

        for tstype, d in hspfmodel.subbasin_timeseries.items():
            ts = timeseries[tstype]
            for c, identifier in d.items():
                if c in updown or c == comid:
                    s, t, l = ts[identifier]
                    self.add_timeseries(tstype, identifier, s, l, tstep = t)
                    self.assign_subbasin_timeseries(tstype, c, identifier)

        # add and assign operation time series inside the submodel

        for tstype, d1 in hspfmodel.operation_timeseries.items():
            ts = timeseries[tstype]
            for subbasin, d2 in d1.items():
                for otype, id in d2.items():
                    if subbasin in self.subbasins:
                        s, t, l = ts[id]
                        self.add_timeseries(tstype, id, s, l, tstep = t)
                        self.assign_operation_timeseries(tstype, subbasin,
                                                         otype, id)

        # add the influent flows as needed
             
        for upcomid in upcomids:

            print('warning: input flow time series for subbasin ' +
                  '{} must be specified'.format(upcomid))



        # add the subbasin timeseries as needed

        #for identifier in hspfmodel.subbasin_timeseries:
        #    if identifier in submodel.subbasins.keys():
        #        start_date, tstep, data = hspfmodel.precipitations[identifier]
        #        submodel.add_timeseries('precipitation', identifier, start_date,
        #                                data, tstep = tstep)

        # add the landuse timeseries as needed

        #landuse_keys = {'Corn':          'cereals',
        #                'Soybeans':      'legumes',
        #                'Pasture/grass': 'pasture',
        #                'Other grain':   'cereals',
        #                'Hay/alfalfa':   'alfalfa',
        #                'Water/wetland': 'wetlands',
        #                'Fallow land':   'fallow',
        #                'Forest':        'others',
        #                'Developed':     'others',
        #                'Impervious':    'others',
        #                'Other':         'others',
        #                }

        #ltypes = [landuse_keys[i] for i in hspfmodel.landuse]

        #for identifier in hspfmodel.evaporations:
        #    if identifier in ltypes:
        #        start_date, tstep, data = hspfmodel.evaporations[identifier]
        #        submodel.add_timeseries('evaporation', identifier, start_date,
        #                                data, tstep = tstep)




        # add the influent flows as needed
             
        #for upcomid in upcomids:

            # find the upstream gage number

        
        #    upgage  = [v for k, v in 
        #               hspfmodel.subbasin_timeseries['flowgage'].items() 
        #               if k == upcomid][0]
        #    incomid = hspfmodel.updown[upcomid]

            # find the outlet flows from the previous upstream calibration

        #    t = (self.directory, self.HUC8, upgage)
        #    flowfile = '{}/{}/calibrations/{}/outletflows'.format(*t)
            
            # get the time series and add it to the model

        #    if not os.path.isfile(flowfile): 
        #        raise RuntimeError('warning: upstream calibration of gage ' +
        #                           '{} does not exist\n'.format(upgage))
        #    with open(flowfile, 'rb') as f: times, data = pickle.load(f)

        #    tstep = math.ceil((times[1] - times[0]).total_seconds() / 60)

        #    submodel.add_timeseries('inflow', '{}'.format(incomid), times[0], 
        #                            data, tstep = tstep)

            # assign the inflows from upstream to any subbasins

        #    otype = 'Reach'

        #    submodel.assign_operation_timeseries('inflow', incomid, 'Reach', 
        #                                         '{}'.format(incomid))
            
        # assign as needed

        #for tstype, identifier in hspfmodel.watershed_timeseries.items():
        #    
        #    submodel.assign_watershed_timeseries(tstype, identifier)

        #for tstype, d in hspfmodel.subbasin_timeseries.items():

        #    for subbasin, identifier in d.items():
            
        #        if subbasin in submodel.subbasins:

        #            submodel.assign_subbasin_timeseries(tstype, subbasin,
        #                                                identifier)

        #for tstype, d in hspfmodel.landuse_timeseries.items():

        #    for landtype, identifier in d.items():
            
        #        if landtype in submodel.landuse:

        #            submodel.assign_landuse_timeseries(tstype, landtype,
        #                                               identifier)

        #for tstype, d1 in hspfmodel.operation_timeseries.items():

        #    for subbasin, d2 in d1.items():

        #        for otype, identifier in d2.items():

        #            if subbasin in submodel.subbasins:

        #                submodel.assign_operation_timeseries(tstype, subbasin,
        #                                                     otype, identifier)

        #with open(picklefile, 'wb') as f: pickle.dump(submodel, f)
