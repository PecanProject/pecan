#!/usr/bin/env python
"""
Get MODIS data using the ORNL DAAC MODIS web service.
http://daac.ornl.gov/MODIS/MODIS-menu/modis_webservice.html
"""
__author__ = "Tristan Quaife"
__version__ = "0.3 (29.07.2010)"
__email__ = "tquaife@gmail.com"

import sys, os
import numpy as np
import optparse
import pickle 
import tempfile
tempfile.tempdir="/tmp/"
from copy import copy
from suds.client import *
import netCDF4

DEBUG_PRINTING=True

defaultURL='http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.wsdl'

class modisData( object ):

	def __init__( self ):
	
		self.server=None
		self.product=None
		self.latitude=None
		self.longitude=None

		self.band=None          
		self.nrows=None         
		self.ncols=None         
		self.cellsize=None      
		self.scale=None         
		self.units=None         
		self.yllcorner=None
		self.xllcorner=None  
		
		self.kmAboveBelow=0
		self.kmLeftRight=0

		self.dateStr=[]
		self.dateInt=[]
		self.data=[]
		self.QA=[]

		#self.header=None        
		#self.subset=None        

		self.isScaled=False

	
	def getFilename( self ):
	
		d='.'
	
		fn=self.product
		fn=fn+d+self.band
		fn=fn+d+'LAT__'+str(self.latitude)
		fn=fn+d+'LON__'+str(self.longitude)
		fn=fn+d+self.dateStr[0]
		fn=fn+d+self.dateStr[-1]
		fn=fn+d+str(int(self.nrows))
		fn=fn+d+str(int(self.ncols))
		
		
		return fn


	def pickle( self ):
	
		fn=self.getFilename()+'.'+'pkl'
			
		f=open( fn, 'w' )
		pickle.dump( self, f )
		f.close( )


	def applyScale( self ):
		
		if self.isScaled==False:
			self.data=self.data*self.scale
			self.isScaled=True


	def filterQA( self, QAOK, fill=np.nan ):

		if np.size( self.data ) != np.size( self.QA ):
			#should do this using an exception
			print >> sys.stderr, 'data and QA are different sizes'
			sys.exit()
		
		r=np.shape( self.data )[0]
		c=np.shape( self.data )[1]
	
		for i in xrange( c ):
			for j in xrange( r ):
				if np.sum( QAOK == self.QA[j][i] ) == 0:
					self.data[j][i] = fill



def __getDummyDateList( ):
	"""
	Generate a dummy date list for testing without 
	hitting the server
	"""

	D=[]
	for y in xrange( 2001,2010 ):
		for d in xrange( 1,365,1 ):
			D.append( 'A%04d%03d'%(y,d) )
		
	return D



def __error( msg ):
	raise Exception, msg
		
def latLonErr( ):
	__error( 'Latitude and longitude must both be specified' )
	
def serverDataErr( ):	
	__error( 'Server not returning data (possibly busy)' )
	

def mkIntDate( s ):
	"""
	Convert the webserver formatted dates
	to an integer format by stripping the
	leading char and casting
	"""
	n=s.__len__( )
	d=int( s[-(n-1):n] )

	return d


def setClient( wsdlurl=defaultURL ):
		
	return Client(wsdlurl)
	

def printList( l ):

	for i in xrange( l.__len__() ):
		print l[ i ]
		
		
def printModisData( m ):

		
	print 'server:', m.server
	print 'product:', m.product
	print 'latitude:', m.latitude
	print 'longitude:', m.longitude

	print 'band:',m.band          
	print 'nrows:',m.nrows        
	print 'ncols:',m.ncols         
	print 'cellsize:',m.cellsize      
	print 'scale:',m.scale         
	print 'units:',m.units         
	print 'xllcorner:',m.yllcorner
	print 'yllcorner:',m.xllcorner  

	print 'kmAboveBelow:', m.kmAboveBelow
	print 'kmLeftRight:', m.kmLeftRight

	print 'dates:', m.dateStr

	print 'QA:', m.QA
	print m.data


def __debugPrint( o ):

	if DEBUG_PRINTING:
		print >> sys.stderr,'DB> ',o
		sys.stderr.flush


def modisGetQA( m, QAname, client=None, chunkSize=8 ):

	startDate=m.dateInt[0]
	endDate=m.dateInt[-1]

	q = modisClient( client, product=m.product, band=QAname, lat=m.latitude, lon=m.longitude, 
			startDate=startDate, endDate=endDate, chunkSize=chunkSize, kmAboveBelow=m.kmAboveBelow, kmLeftRight=m.kmLeftRight )
	
	m.QA = copy( q.data )
	
			


def modisClient( client=None, product=None, band=None, lat=None, lon=None, startDate=None, endDate=None, chunkSize=8, kmAboveBelow=0, kmLeftRight=0 ):
	"""
	modisClient: function for building a modisData object
	"""

	m=modisData()

	m.kmABoveBelow=kmAboveBelow
	m.kmLeftRight=kmLeftRight
	
	if client==None:
		client=setClient( )
		
	m.server=client.wsdl.url	
		
	if product==None:
		prodList=client.service.getproducts(  )
		return prodList

	m.product=product

	if band==None:
		bandList=client.service.getbands( product )
		return bandList
	
	m.band=band
	
	if lat==None or lon==None:
		latLonErr( )
	
	m.latitude=lat
	m.longitude=lon
	
	# get the date list regardless so we can
	# process it into appropriately sized chunks
	 
	dateList=client.service.getdates( lat, lon, product )
		
	if startDate==None or endDate==None:
		return dateList



	#count up the total number of dates
	i=-1
	nDates=0	
	while i < dateList.__len__( )-1:	
		i=i+1
	
		#__debugPrint( 'i=%d'%i )
		
		thisDate=mkIntDate( dateList[i] )
		
		if thisDate < startDate:
			continue
		if thisDate > endDate:
			break	

		nDates=nDates+1

		m.dateInt.append( thisDate )
		m.dateStr.append( dateList[i] )

	__debugPrint( m.dateStr )
	
	n=0	
	i=-1	
	while i < dateList.__len__( )-1:	
		i=i+1
	
		thisDate=mkIntDate( dateList[i] )
		
		if thisDate < startDate:
			continue
		if thisDate > endDate:
			break	
		
		requestStart=dateList[i]
		
		j=min( chunkSize, dateList.__len__( )-i )
		
		__debugPrint( 'i=%d, j=%d, dateList__len__()=%d'%(i,j,dateList.__len__( ))  )
		while mkIntDate( dateList[i+j-1] ) > endDate:
			j=j-1
		
		
		
		requestEnd=dateList[i+j-1]
		i=i+j-1
		
		#print >> sys.stderr, requestStart, requestEnd
		
		data = client.service.getsubset( lat, lon, product, band, requestStart, requestEnd, kmAboveBelow, kmLeftRight )
		
		
		# now fill up the data structure with the returned data...

		if n == 0:
		
			m.nrows=data.nrows         
			m.ncols=data.ncols         
			m.cellsize=data.cellsize      
			m.scale=data.scale         
			m.units=data.units         
			m.yllcorner=data.yllcorner
			m.xllcorner=data.xllcorner  	

			m.data=np.zeros( (nDates,m.nrows*m.ncols) )

		for j in xrange( data.subset.__len__( ) ):
			kn=0
			__debugPrint( data.subset	)		
			for k in data.subset[j].split(",")[5:]:
				__debugPrint( k )
				try:
					m.data[ n*chunkSize+j,kn] = int( k )
				except ValueError:
					serverDataErr( )
				
				kn=kn+1



		n=n+1



	return( m )



#def m_data_to_netCDF(filename, varname, data):
#	rootgrp = netCDF4.Dataset(filename, 'w', format='NETCDF3_64BIT')
#	rootgrp.createDimension('ncol', data.shape[1])
#	rootgrp.createDimension('nrow', data.shape[0])
#	m_data = rootgrp.createVariable(varname, 'f8', ('nrow', 'ncol'))
#	m_data[:] = data
#	rootgrp.close()


def m_data_to_netCDF(filename, m, k):
	rootgrp = netCDF4.Dataset(filename, 'w', format='NETCDF3_64BIT')
	rootgrp.createDimension('ncol', m.data.shape[1])
	rootgrp.createDimension('nrow', m.data.shape[0])
	rootgrp.createDimension('dates', len(m.dateInt))
	m_data = rootgrp.createVariable('LAI', 'f8', ('nrow', 'ncol'))
	m_std = rootgrp.createVariable('LAIStd', 'f8', ('nrow', 'ncol'))
	m_date = rootgrp.createVariable('Dates', 'i7', ('dates'))
	m_data[:] = m.data
	m_std[:] = 0.1*k.data
	m_date[:] = m.dateInt
	rootgrp.close()

#def m_date_to_netCDF(filename, varname, data):
#	rootgrp = netCDF4.Dataset(filename, 'w', format='NETCDF3_64BIT')
#	rootgrp.createDimension('ncol', data.shape[1])
#	rootgrp.createDimension('nrow', data.shape[0])
#	rootgrp.createDimension('ncol', len(data))
#	m_data = rootgrp.createVariable(varname, 'S1', ('ncol'))
#	m_data[:] = data
#	rootgrp.close()


def run_main(start_date=2004001, end_date=2004017, la=45.92, lo=-90.45, kmAB=0, kmLR=0, fname='m_data.nc',product='MOD15A2',band='Lai_1km',qcband='FparLai_QC',sdband='LaiStdDev_1km'):

	client=setClient( )

	prodList = modisClient( client )
#	printList( prodList )

	bandList = modisClient( client, product=product )
#	printList( bandList )
	
	dateList = modisClient( client, product=product, band=band, lat=la, lon=lo )
#	printList( dateList )
	
	m = modisClient( client, product=product, band=band, lat=la, lon=lo, startDate=start_date, endDate=end_date, kmAboveBelow=kmAB, kmLeftRight=kmLR)
	if len(m.dateInt) == 0:
		print "No data available for these dates"
		return np.array([[]]), np.array([[]])

#  print(m.dateStr)
	k = modisClient( client, product=product, band=sdband, lat=la, lon=lo, startDate=start_date, endDate=end_date, kmAboveBelow=kmAB, kmLeftRight=kmLR)
	date = m.dateInt
#	data[:] = m.data


#	print(m.dateStr)

	modisGetQA(m, qcband, client=client )
	modisGetQA(k, qcband, client=client )
		
	m.applyScale()
	m.filterQA( range(0,2**16,2), fill=-1 )

	m_data_to_netCDF(fname, m, k)	

#	print(len(m.data))
#	print(len(k.data))

	return m, k, date



def main():
	m, k, date = run_main()

#	m_data_to_netCDF('m_data.nc', 'm_data', m.data)

#	for i in range(m.data.shape[0]):
#		for j in range(m.data.shape[1]):
#			print "%12i%12i%19.14F"%(i+1,j+1,m.data[i,j])

#	printModisData( m )


#def get_m():
if __name__ == "__main__":
	main()

#def load_m():
#	f = open('MODIS_LAI.dat','rb')
#	
#	m = pickle.load(f)
#	return m

 
#if __name__ == "__main__":
#	get_m_data()
#	get_m()
#	m = load_m()
#	m_data_to_netCDF('m_data.nc', m.data)
#	for i in range(m.data.shape[0]):
#		for j in range(m.data.shape[1]):
#			print "%12i%12i%19.14F"%(i+1,j+1,m.data[i,j])
#
