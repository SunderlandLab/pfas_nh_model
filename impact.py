### Determine the potential impact of industries emitting PFAS on nearby wells
### in New Hampshire
### Bridger Ruyle 11/15/2020

##load packages
import numpy as np
import pandas as pd
import geopandas as gpd
from geopandas.tools import sjoin
import fiona
import geopy.distance
import requests
import urllib

##USGS Elevation Point Query Service
url = r'https://nationalmap.gov/epqs/pqs.php?'

##Load sources and sinks
industry=gpd.read_file('shapefiles/nh_industry.shp') #epsg=32618
well=gpd.read_file('shapefiles/unique_wells.shp') #epsg=32618

##Reproject sources and sinks in NAD83 to correspond with USGS data
industry=industry.to_crs('EPSG:4326')
well=well.to_crs('EPSG:4326')
wellx=well['geometry'].x
welly=well['geometry'].y
industryx=industry['geometry'].x
industryy=industry['geometry'].y

##Find elevation of industries by searching USGS National Map (3DEP)
##This takes a long time and only need to be done once
##output saved to elevations folder
# source_elevation=[]
# for lat,lon in zip(industryy,industryx):
#     source_params={'output':'json','x':lon,'y':lat,'units':'Meters'}
#     source_result=requests.get((url + urllib.parse.urlencode(source_params)))
#     source_elevation.append(source_result.json()['USGS_Elevation_Point_Query_Service']['Elevation_Query']['Elevation'])
# np.savetxt('elevation/industry_elevations.txt',source_elevation)
source_elevation=np.loadtxt('elevation/industry_elevations.txt')

##Find elevation of wells by searching USGS National Map (3DEP)
##This takes a long time and only need to be done once
##output saved to elevations folder
# sink_elevation=[]
# for lat,lon in zip(welly,wellx):
#     sink_params={'output':'json','x':lon,'y':lat,'units':'Meters'}
#     sink_result=requests.get((url + urllib.parse.urlencode(sink_params)))
#     sink_elevation.append(sink_result.json()['USGS_Elevation_Point_Query_Service']['Elevation_Query']['Elevation'])
# np.savetxt('elevation/well_elevations.txt',sink_elevation)
sink_elevation=np.loadtxt('elevation/well_elevations.txt')

##Load HUC12 shapefile
huc12_file='wbdhu12_a_us_september2020.gdb'
WBDHU12=fiona.listlayers(huc12_file)[0]
huc12=gpd.read_file(huc12_file,layer=WBDHU12) #epsg=4326
NH_str=[i for i in set(huc12['states']) if type(i)==np.str and 'NH' in i]
NH_watersheds=huc12['states'].isin(NH_str)

##Match well and industry locations to watershed
well=sjoin(well,huc12[NH_watersheds],how='left')
industry=sjoin(industry,huc12[NH_watersheds],how='left')

##Establish impact matrix
impact=pd.DataFrame(np.zeros((len(well),len(set(industry['indstr_'])))),columns=set(industry['indstr_']))

##Determine impact of industry types on each well
for sink in well.index:
    #Elevation of well
    sie=sink_elevation[sink]

    #HUC code of well
    watershed=well['huc12'][sink]
    #All industries within the same watershed
    local=industry['huc12']==watershed

    if sum(local>0):
        for source in industry[local].index:
            #Elevation of industry
            soe=source_elevation[source]

            #Industries are assumed to be able to impact the well if they are higher in elevation
            if soe>sie:
                #Calculate Haversine distance between industry and well
                radius=geopy.distance.geodesic((wellx[sink],welly[sink]),
                            (industryx[source],industryy[source]))
                #Potential impact factor = 1/e^r, modified from Zhang et al. 2016
                impact[industry[local]['indstr_'][source]][sink]+=np.exp(radius.km)**-1 #radius in km

##Save results
impact.index=well['StationID']
impact.to_csv('potential_impact.csv')
