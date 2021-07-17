#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Access ECMWF CAMS-reanalysis NO2 dataset
@author: liutzuli
web-based query: https://ads.atmosphere.copernicus.eu/cdsapp#!/dataset/cams-global-reanalysis-eac4?tab=form
reference: https://ads.atmosphere.copernicus.eu/api-how-to
"""
# %pwd   # check working directory
import os
os.getcwd()
os.chdir("/Masterarbeit/analysis/1_data/raw/ECMWF-CAMS-NO2")
# ==================================
# Save your API key credentials into a file
# ==================================
# the credentials (API key) saved as "/Users/liutzuli/.cdsapirc"
# can be displayed in Finder by Command + Shift + .

# ==================================
# Run download request
# selection can be made here: 
# https://ads.atmosphere.copernicus.eu/cdsapp#!/dataset/cams-global-reanalysis-eac4?tab=form
# ==================================
import cdsapi

c = cdsapi.Client()

c.retrieve(
    'cams-global-reanalysis-eac4',
    {
        'date': '2019-01-01/2019-12-31',
        'time': [
            '00:00', '03:00', '06:00',
            '09:00', '12:00', '15:00',
            '18:00', '21:00',
        ],
        'area': [
            48.44, 3.5, 44.52,
            11.11,
        ],
        'format': 'netcdf',
        'variable': [
            'total_column_nitrogen_dioxide', 'total_column_nitrogen_monoxide',
        ],
    },
    'CAMS-NO2.nc')

# the .grib files are corrupted
# c.retrieve(
#     'cams-global-reanalysis-eac4',
#     {
#         'date': '2019-01-01/2019-12-31',
#         'time': [
#             '00:00', '03:00', '06:00',
#             '09:00', '12:00', '15:00',
#             '18:00', '21:00',
#         ],
#         'area': [
#             48.44, 3.5, 44.52,
#             11.11,
#         ],
#         'format': 'grib',
#         'variable': [
#             'total_column_nitrogen_dioxide', 'total_column_nitrogen_monoxide',
#         ],
#     },
#     'CAMS-NO2.grib')

