import csv, pandas

from shapefile import Reader

sf = 'C:/HSPF_data/07080106/hydrography/subbasin_catchments'

# read the areas from the shapefile into a lookup dictionary

r = Reader(sf)

comid_index = [f[0] for f in r.fields].index('ComID') - 1
area_index = [f[0] for f in r.fields].index('AreaSqKm') - 1

areas = {row[comid_index]: row[area_index] for row in r.records()}

# directory to the land use data

p = 'C:/HSPF_new/07080106/landuse'

# store the results in a data structure

rows = [['Year']]

for y in range(2000,2011):

    # expand the structure for the next file
    
    rows.append([y])
    
    # land use csv file for the year (contains the fractions for each comid)
    
    f = '{}/{}landuse.csv'.format(p, y)

    # read the data into a dataframe (first two rows are headers)
    
    df = pandas.read_csv(f, skiprows = 2)

    # add the headers the first time through

    if y == 2000: rows[0] += [col for col in df if col != 'ComID']

    # add the areas to the dataframe
    
    df['Area'] = [areas[comid] for comid in df['ComID']]

    # iterate through the columns and add the result to the output groups

    for col in df:

        # ignore the area and comid columns
        
        if col != 'ComID' and col != 'Area':

            # take the dot product of the area and fraction columns
            
            rows[-1].append((df[col] * df['Area']).sum())

# make a new summary csv file of the accumulated results

output = '07080106landuse.csv'

with open(output, 'w', newline = '') as f:

    writer = csv.writer(f)
    for row in rows: writer.writerow(row)

