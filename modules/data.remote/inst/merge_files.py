import xarray
import os
import time
import pandas as pd

def nc_merge(old, new, outdir):


    head, tail = os.path.split(new)

    
    orig_nameof_newfile = new
    timestamp = time.strftime("%y%m%d%H%M%S")
    changed_new = os.path.join(outdir, tail + 'temp' + timestamp + '.nc')

    os.rename(orig_nameof_newfile, changed_new)
    

    ds = xarray.open_mfdataset([old, changed_new], combine='by_coords')

    

    ds.to_netcdf(os.path.join(outdir, tail))

    return os.path.abspath(os.path.join(outdir, tail))


def csv_merge(old, new, outdir):
    head, tail = os.path.split(new)

    orig_nameof_newfile = new
    timestamp = time.strftime("%y%m%d%H%M%S")
    changed_new = os.path.join(outdir, tail + 'temp' + timestamp + '.csv')

    os.rename(orig_nameof_newfile, changed_new)

    df_old = pd.read_csv(old)
    df_changed_new = pd.read_csv(changed_new)

    merged_df = pd.concat([df_old, df_changed_new])

    merged_df = merged_df.sort_values(by='Date')

    merged_df.to_csv(os.path.join(outdir, tail), index=False)

    return os.path.abspath(os.path.join(outdir, tail))












