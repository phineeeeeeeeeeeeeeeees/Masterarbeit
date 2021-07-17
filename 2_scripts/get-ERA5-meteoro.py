#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Access ECMWF ERA-5 meteorological dataset
Created on Mon Apr 12 20:20:51 2021
@author: liutzuli
reference: https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5 
"""
import os
os.getcwd()
os.chdir("/Masterarbeit/analysis/1_data/raw/ECMWF-ERA5")
# ==================================
# Install the CDS API key into a file
# ==================================
# 最簡單的方法：用RStudio 到/Users/liutzuli 
# 建立一個純問字檔，由 https://cds.climate.copernicus.eu/api-how-to 取得API ID和key
# 存到/Users/liutzuli  檔名為.cdsapirc

# 在Finder中顯示隱藏檔案：Command + Shift + .


# ==================================
# Run download request
# selection can be made here: 
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form
# ==================================
# pip install cdsapi
import cdsapi

c = cdsapi.Client()

c.retrieve(
    'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'variable': [
            '10m_u_component_of_wind', '10m_v_component_of_wind', '2m_temperature',
            'boundary_layer_height', 'surface_pressure', 'total_cloud_cover',
            'total_precipitation',
        ],
        'year': '2019',
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
        ],
        'day': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
            '13', '14', '15',
            '16', '17', '18',
            '19', '20', '21',
            '22', '23', '24',
            '25', '26', '27',
            '28', '29', '30',
            '31',
        ],
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
    },
    'ERA5-meteorology.nc')


