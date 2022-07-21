# !pip install ecmwf-opendata

def ecmwflatest(time, step, stream, type, params):
  from ecmwf.opendata import Client
  
  client = Client("ecmwf", beta=True)
  
  if stream == "mmsf":
    latest= client.latest(
      time = int(time),
      step= step,
      stream= stream,
      type= type,
      param= params,
      )
  
  elif stream == "enfo":
    latest= client.latest(
      time = int(time),
      step= int(step),
      stream= stream,
      type= type,
      param= params,
      )
  
  return latest


def ecmwfdownload(date, time, step, stream, type, params, filename):
  from ecmwf.opendata import Client
  
  client = Client("ecmwf", beta=True)
  
  file = client.retrieve(
    date= int(date),
    time= int(time),
    step= int(step),
    stream= stream,
    type= type,
    param= params,
    target= filename
  )
  
  return file
