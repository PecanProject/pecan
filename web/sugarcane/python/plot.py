#!/usr/bin/python
import os,time
import re,sys,tempfile
dir=os.path.dirname(sys.argv[0]) 
os.environ['MPLCONFIGDIR'] = tempfile.mkdtemp()
import matplotlib
matplotlib.use('agg')
import matplotlib.pyplot as plt

f=open(dir+"/../plot_data/output","r")
rows=filter(None, f.readlines())
#keys=[rows[i] for i in range(len(rows)) if i%2==0]
#values=[filter(None, rows[i].split()) for i in range(len(rows)) if i%2==1]
numbers=[]
for index,row in enumerate(rows):
	if index==0:
		variables=re.findall(r'[^ \t\r\n]+', row)
	else:
		numbers.append(re.findall(r'[0-9\-\.]+', row))

if (len(sys.argv) > 2):
	data=[]
	for index,x in enumerate(sys.argv):
		if index>0: # first argument is the file name/path. ignore it
			i=variables.index(x)+1 # it is necessary to add 1 because the first row always has one less item because of the row numbers
			column=[]
			for number_row in numbers:
				column.append(float(number_row[i])) # converting rows to columns
			data.append(column)
	#print data

fig = plt.figure(figsize=(5, 4), dpi=80)
ax1 = fig.add_subplot(1,1,1)
ax1.set_xlabel(sys.argv[1]);
ax1.set_ylabel(sys.argv[2]);
#ax1.plot(range(len(data[0])),data[0],range(len(data[1])),data[1])
plt.plot(data[0],data[1])

#ax2 = fig.add_subplot(2,1,2)
#ax2.plot(range(len(data[1])),data[1])

path=dir+'/png/'+os.urandom(6).encode('hex')+'.png'

fig.savefig(path)
print path
