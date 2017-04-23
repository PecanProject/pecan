import requests
import json

"""
host_url - clowder url (https://terreref.ncsa.illinois.edu/clowder)
site_name - name of location whose datapoints should be downloaded
start_time, end_time - format "YYYY-mm-DDTHH:MM::SSZ"
Example - So, if you want data from January 1st 2016 to January 6th 2016, 
start_time = "2016-01-01T06:45:00Z" , end_time = "2016-01-06T06:45:00Z".
This will get data from 2016-01-01 midnight to 2016-01-06 midnight.
"06:45:00" is needed to account for time offset of 7 hours.
"""




def download_geostreams(host_url, site_name, start_time, end_time, outfile):
    # https://terraref.ncsa.illinois.edu/clowder
    url = "%s/api/geostreams/sensors?sensor_name=%s" % (host_url, site_name)
    sensor_info = requests.get(url)
    sensor_id = sensor_info.json()[0]['id']
    payload = {"sensor_id": sensor_id, "since": start_time, "until": end_time}
    result = requests.get("%s/api/geostreams/datapoints" %(host_url), payload)

    with open(outfile, 'w') as out:
        json.dump(result.json(), out)


if __name__ == "__main__":
    host_url = "http://localhost:9000"
    site_name = "Energy Farm Met Station CEN"
    start_time = "2016-01-01T08:45:00Z"
    end_time = "2016-01-01T09:45:00Z"

    outfile = "results.dat"
    download_geostreams(host_url, site_name, start_time, end_time, outfile)
