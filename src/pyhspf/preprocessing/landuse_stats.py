# Land use statistics calculator
#
# David J. Lampert, PhD, PE
#
# last updated: 12/07/2012
#
# Calculates land use summary statistics for a shapefile representing a 
# watershed.  Assumes directory structure that contains sub directories to 
# other csv files with the raw data and a file to group data by category.

import os, csv, pickle

from shapefile import Reader

from pyhspf.preprocessing.gisplots import get_aggregate_map
from pyhspf.preprocessing.gisplots import landuse_plots

def is_number(s):
    try: float(s) 
    except ValueError: return False
    return True

def landuse_stats(directory, HUC8, aggregatefile, subbasinfile, csvoutput, 
                  picklefile = None, overwrite = False, units = 'sqkm',
                  plots = False, res = 2000, verbose = True, vverbose = False):
    """Aggregates all the statistics together for an 8-digit watershed."""

    if not overwrite and os.path.isfile(csvoutput):
        if vverbose: print('file %s exists, returning' % csvoutput)

    # conversion factor for km2 to acres

    acres_per_sqkm = 247.105

    # open up the aggregate info file and setup a dictionary to map values

    m, landtypes, groups = get_aggregate_map(aggregatefile)

    # store the subbasin data in a list

    subbasin_data = []

    # get the comids of the subbasin

    sf = Reader(subbasinfile, shapeType = 5)

    comid_index = sf.fields.index(['ComID',    'N',  9, 0]) - 1
    area_index  = sf.fields.index(['AreaSqKm', 'N', 10, 2]) - 1

    subbasins      = ['{}'.format(r[comid_index]) for r in sf.records()]
    subbasin_areas = [float(r[area_index]) for r in sf.records()]

    # go through each subbasin and read the land use data file
    # first need to find all land types in the whole watershed

    for subbasin in subbasins:

        if vverbose: print('reading land use data for {}'.format(subbasin))

        landfile = '{}/{}/{}/landuse.csv'.format(directory, HUC8, subbasin)

        with open(landfile, 'r') as f:
            reader = csv.reader(f)
            rows = [row for row in reader][2:2 + len(m)]

        # iterate through the land types and aggregate the areas for each type

        years = [int(year) for year in rows[0][2:]]
        areas = {group:[0. for year in years] for group in groups}
        for row in rows[1:]:
            code       = int(row[0])
            group      = m[code]
            groupareas = areas[group]
            
            for i, value in zip(range(len(groupareas)), row[2:]):
                if is_number(value): groupareas[i] += float(value)

        # add the data to the cumulative area list

        subbasin_data.append([areas[group] for group in groups])

    watershed_areas = [[0. for year in years] for group in groups]

    # find the cumulative areas for the groups

    for data in subbasin_data:
        for areas, group_areas in zip(data, watershed_areas):
            for i, area in zip(range(len(group_areas)), areas):
                group_areas[i] += area

    # get the units

    if units == 'sqkm':
 
        font = 'Areas, km\u00B2'

        for data in subbasin_data:
            for row in data:
                for i in range(len(row)):
                    if type(row[i]) is float: row[i] = round(row[i], 4)
        for areas in watershed_areas:
            for i in range(len(areas)):
                areas[i] = round(areas[i], 4)

    elif units == 'acres':   

        font = 'Acreage'

        for data in subbasin_data:
            for row in data:
                for i in range(len(row)):
                    if type(row[i]) is float: 
                        row[i] = round(row[i] * acres_per_sqkm, 2)
        for areas in watershed_areas:
            for i in range(len(areas)):
                areas[i] = round(areas[i] * acres_per_sqkm, 2)

    elif units == 'percent': 

        font = 'Land Use Percentages'

        subbasin_area = subbasin_areas[0]
        for subbasin_area, data in zip(subbasin_areas, subbasin_data):
            for row in data:
                for i in range(len(row)):
                    if type(row[i]) is float:
                        row[i] = round(row[i] / subbasin_area * 100, 1)

        tot_area = sum([a[0] for a in watershed_areas])
        for areas in watershed_areas:
            for i in range(len(areas)):
                areas[i] = round(areas[i] / tot_area * 100, 1)

    # pickle the landuse data to a file for easy access later

    if picklefile is not None and not os.path.isfile(picklefile) or overwrite:
        obj = (years, 
               {comid: data for comid, data in zip(subbasins, subbasin_data)})
        with open(picklefile, 'wb') as f: pickle.dump(obj, f)

    # write the results to the new csv file

    if not os.path.isfile(csvoutput) or overwrite:

        with open(csvoutput, 'w', newline = '') as f:

            writer = csv.writer(f)

            # add the title

            writer.writerow(['Watershed %s Annual Land Use %s' % (HUC8, font)])
            writer.writerow([''])

            # add the cumulative areas

            writer.writerow(['Cumulative Annual ' + font])
            writer.writerow(['Code', 'Group'] + years)
            for group, group_areas in zip(groups, watershed_areas):
                writer.writerow([group, landtypes[group]] + group_areas)

            # iterate through the subbasins and write the yearly landuse data

            for subbasin, data in zip(subbasins, subbasin_data):
 
                writer.writerow([''])
                writer.writerow(['Subbasin {} Annual {}'.format(subbasin,font)])
                writer.writerow(['Code', 'Group'] + years)
            
                for group, group_areas in zip(groups, data):
                    writer.writerow([group, landtypes[group]] + group_areas)

    if plots: 
        landuse_plots(directory, HUC8, years, aggregatefile, pixels = res,
                      verbose = vverbose)
