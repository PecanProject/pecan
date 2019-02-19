def extract_modis_data(product, band, lat, lon, start_date, end_date, size, band_qc, band_sd):
    ### 1) Set-up some pre-run parameter fixes
    # size (aka kmAboveBelow and kmLeftRight) will default to size 0 (i.e. 1 pixel) until PEcAn is ready to start handeling raster data.
    size = 0
    
    
    ### 2) Import required modules and set request URL as headers
    import requests
    import json
    import re
    import ast
    import pandas
    import sys

    url = "https://modis.ornl.gov/rst/api/v1/"
    header = {"Accept": "application/json"}

    ### 3) Sanity check to make sure user inputs exist

    # check that product requested is available from API
    response = requests.get('https://modis.ornl.gov/rst/api/v1/products', headers = header)
    products = [str(i['product']) for i in (json.loads(response.text)['products'])]
    if product not in products:
        description = [str(i['description']) for i in (json.loads(response.text)['products'])]
        for x in range(0, len(products)):
            print([products[x], description[x]])
        sys.exit("Product provided by user not available for MODIS API. Please choose from list above.")
    else:
        print("Check #1: Product exists!")

    # Check that requested dates are within the range of available dates for data product.
    response = requests.get('https://modis.ornl.gov/rst/api/v1/'+product+'/dates?latitude='+str(lat)+'&longitude='+str(lon), headers=header)
    dates = json.loads(response.text)['dates']

     # report dates in modis form. Remove from dictionary into an array.
    modis_dates = [i['modis_date'] for i in dates]

    # convert modis dates out of unicode and into format for json request.
    modis_dates_string = [str(item) for item in modis_dates]
    modis_dates_int = [int(item[1:]) for item in modis_dates_string]

    if int(start_date) <=  modis_dates_int[0] or int(end_date) >= modis_dates_int[len(modis_dates_int)-1]:
        print("Available date range: "+str(modis_dates_int[0])+" - "+str(modis_dates_int[len(modis_dates_int)-1]))
        sys.exit("Start and/or end dates provide not in range of requested dataset. Please choose date between range listed above.")
    else:
        print("Check #2: Dates are available!")

    # Check that data band is correct for the product dataset requested.
    response = requests.get('https://modis.ornl.gov/rst/api/v1/'+product+'/bands', headers=header)
    bands = [str(i['band']) for i in (json.loads(response.text)['bands'])]
    if band not in bands:
        for x in range(0, len(bands)):
            print(bands[x])
        sys.exit("Band requested not available for product dataset. Please choose from listed bands above.")
    else:
        print("Check #3: Band exists!")
        
      
    ### 4) 10 tile limit per request to ensure reponsivity of the service and to avoid time outs. The dates function returns a list of available dates for the spcified coordinates and MODIS products.
    print("Extracting data")

    # dates requrested in the call_MODIS function may not match exactly with the dates of availabel data. This function finds the closest dates to the requested start_date and end_date.
    #def nearest_date(items, pivot): return min(items, key = lambda x: abs(pivot-x))

    greater_than_start_date = [modis_dates_int[index] for index, item in enumerate(modis_dates_int) if item >= int(start_date)]
    less_than_end_date = [modis_dates_int[index] for index, item in enumerate(modis_dates_int) if item <= int(end_date)]

    sd =greater_than_start_date[0]
    ed = less_than_end_date[len(less_than_end_date)-1]
   
    # use start and end date to subset the list of dates/data user wants to download.
    m_dates = modis_dates[modis_dates.index('A'+str(sd)):modis_dates.index('A'+str(ed))+1]
    m_dates = [str(item) for item in m_dates]

    ### 4) grab the LAI data using the user defined parameters
    output = []

    for dt in m_dates:
        # grab basic data for data available
        lstresponse_lai = requests.get(''.join([url, product, '/subset?', 'latitude=', str(lat), '&longitude=', str(lon), '&band=', band, '&startDate=', dt, '&endDate=', dt, '&kmAboveBelow=', str(size), '&kmLeftRight=', str(size)]), headers = header)
        info = ast.literal_eval(lstresponse_lai.text)
        # scale is the scale factor to apply to the extracted lai data, or stdev data if availabl
        scale = float(info.get("scale"))
        latitude = str(info.get("latitude"))
        longitude = str(info.get("longitude"))
        
        # if QC information is available or provided as a parameter by user, grab that data
        if band_qc is "":
            qc = float("nan")
        else:
            lstresponse_qc = requests.get("".join([url, product, "/subset?", "latitude=", str(lat), "&longitude=", str(lon), "&band=", band_qc, "&startDate=", dt, "&endDate=", dt, "&kmAboveBelow=", str(size), "&kmLeftRight=", str(size) ]), headers=header)
            qc = str(int(json.loads(lstresponse_qc.text)['subset'][0]['data'][0]))

        if band_sd is "":
            stdev = float("nan")
        else:
            lstresponse_sd = requests.get("".join([url, product, "/subset?", "latitude=", str(lat), "&longitude=", str(lon), "&band=", band_sd, "&startDate=", dt, "&endDate=", dt, "&kmAboveBelow=", str(size), "&kmLeftRight=", str(size) ]), headers=header)
            stdev = str(json.loads(lstresponse_sd.text)['subset'][0]['data'][0]*scale)
        
        # Append all data to output array
        output.append([dt, json.loads(lstresponse_lai.text)['subset'][0]['calendar_date'].encode("ascii","ignore"),json.loads(lstresponse_lai.text)['subset'][0]['band'].encode("ascii","ignore"),json.loads(lstresponse_lai.text)['subset'][0]['tile'].encode("ascii","ignore"),latitude,longitude,str(len(json.loads(lstresponse_lai.text)['subset'][0]['data'])),str(float(json.loads(lstresponse_lai.text)['subset'][0]['data'][0])*scale), qc, stdev])



    ### 5) transform the output array into a dataframe so it is more readable in R.
    data = pandas.DataFrame(output, columns = ["modis_date", "calendar_date", "band", "tile", "lat", "lon", "pixels", "data", "qc", "sd"])
    return(data)
        

