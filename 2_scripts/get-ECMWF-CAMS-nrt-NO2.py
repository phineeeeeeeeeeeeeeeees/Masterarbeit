#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Access ECMWF CAMS near-real-time NO2 dataset
@author: liutzuli
web-based query: https://apps.ecmwf.int/datasets/data/cams-nrealtime
reference: https://confluence.ecmwf.int/display/WEBAPI/Access+ECMWF+Public+Datasets
syntax: https://confluence.ecmwf.int/display/WEBAPI/Brief+request+syntax
"""
# %pwd   # check working directory
import os
os.getcwd()
os.chdir("/Masterarbeit/analysis/1_data/raw/ECMWF-CAMS-NO2")
# ==================================
# Save your API key credentials into a file
# ==================================
# https://api.ecmwf.int/v1/key/ 
# the credentials (API key) saved as "/Users/liutzuli/.ecmwfapirc"
# can be displayed in Finder by Command + Shift + .

# ==================================
# Run download request
# selection can be made here: 
# https://apps.ecmwf.int/datasets/data/cams-nrealtime
# ==================================
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()

server.retrieve({
    "class": "mc",
    "dataset": "cams_nrealtime",
    "date": "2019-01-01/to/2019-12-31",
    "expver": "0001",
    "levtype": "sfc",
    "param": "27.218/125.210",
    "step": "0",
    "stream": "oper", 
    "time": "00:00:00/12:00:00",
    "type": "an",
    "format": "netcdf",             # https://confluence.ecmwf.int/display/UDOC/How+to+retrieve+data+in+netCDF+format+-+Web+API+FAQ
    "target": "CAMS-near-real-time-NO2_an.nc",
    "grid": "0.125/0.125",
    "area": "48.44/3.5/44.52/11.11" # https://confluence.ecmwf.int/display/UDOC/How+to+request+a+limited+area+region+-+Web+API+FAQ
})

server.retrieve({
    "class": "mc",
    "dataset": "cams_nrealtime",
    "date": "2019-01-01/to/2019-12-31",
    "expver": "0001",
    "levtype": "sfc",
    "param": "27.218/125.210",
    "step": "0/3/6/9",
    "stream": "oper",
    "time": "00:00:00/12:00:00",
    "type": "fc",
    "format": "netcdf",
    "target": "CAMS-near-real-time-NO2_fc.nc",
    "grid": "0.125/0.125",
    "area": "48.44/3.5/44.52/11.11" 
})

