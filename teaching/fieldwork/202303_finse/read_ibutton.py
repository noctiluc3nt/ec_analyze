#script for reading ibutton measurements of temperature
#reformating of date and time
#produces a new file (txt) in same folder in an easier to read format

import os as os             
import matplotlib.pyplot as plt
import numpy as np
import pickle
import calendar


#adapt:
dataFileLocation = "./"; #storage directory
dataFileNum = int(4); #number of files (in the python way of counting)

#files
os.chdir(dataFileLocation);
dirAll = os.listdir('.')
dirAll.sort();

dirContents = [];

for tempName in dirAll:
    if tempName.endswith('csv'):
        dirContents.append(tempName);
    

i = 0;
while i < len(dirContents):
    print(str(i) + '\t' + dirContents[i]);
    i = i + 1;
    

dataFile = open(dirContents[dataFileNum],'r');
data = dataFile.readlines();
dataFile.close();

#read, skip first 15 lines
data = data[15:len(data)];

#allocate
unit = [];
T = [];
trash=[];

#temporary
time_tmp = [];
year_tmp = [];
month_tmp = [];
day_tmp = []; 
hour_tmp = []; 
minute_tmp = []; 
second_tmp = []; 

year = [];
month = [];
day = [];
hour = [];
minute = [];
second = [];
td = [];


## use this for converting hours minutes seconds to days https://matplotlib.org/api/dates_api.html

#loop over all lines
i = 0;
while i < len(data):
	date, time_tmp, unitTemp, T_Temp   = data[i].split(',');
	trash, time_tmp, AMPM_tmp = time_tmp.split(' ');
	month_tmp, day_tmp, year_tmp = date.split('/');
	hour_tmp,minute_tmp,second_tmp = time_tmp.split(':');
	if int(month_tmp) < 10:
		month_tmp = str('0' + month_tmp);
	if int(day_tmp) < 10:
		day_tmp = str('0' + day_tmp);
	if (int(hour_tmp) < 10) & (int(hour_tmp) != 0):
		hour_tmp = str('0' + hour_tmp);
	# convert the clock to 24hr
	if AMPM_tmp == 'PM':
		if int(hour_tmp) == 12:
			hour_tmp = '12';
		else:
			hour_tmp = str(int(hour_tmp)+12);
	elif int(hour_tmp) == 12:      ## when it's in the mo'nin' and clock should read 00:00
		hour_tmp = '00';
	#dateTime.append(dateTimeTemp);
	year.append(int('20'+year_tmp));
	month.append(month_tmp);
	day.append(day_tmp);
	hour.append(hour_tmp);
	minute.append(minute_tmp);
	second.append(second_tmp);
	## time difference from the beginning of the year.
	td_tmp = np.datetime64('20' + year_tmp + '-' + month_tmp + '-' + day_tmp + 'T' + hour_tmp + ':' + minute_tmp + ':' + second_tmp)- np.datetime64('20' + year_tmp + '-01-01T00:00:00');
	td_tmp = td_tmp / np.timedelta64(1,'s'); #converts this into a decimal seconds value
	# converts this into a decimal year value.
	if calendar.isleap(year[i]):
		td_tmp = td_tmp/86400/366;
	else:
		td_tmp = td_tmp/86400/365;
	td.append(td_tmp);
	unit.append(unitTemp);
	T.append(float(T_Temp));
	i = i + 1;
	
 
#write in new file
dataFile = open(dirContents[dataFileNum][:-3]+'txt','w'); #file name 
dataFile.write('date, time, time_decimal, temp, unit\n'); #header

i = 0;
while i<len(td):
	dataFile.write(str(year[i]) + '-' + str(month[i]) + '-' + str(day[i]) + ',' + str(hour[i]) + ':' + str(minute[i]) + ':' + str(second[i]) + ',' + str(td[i]) + ',' + str(T[i]) + ',' + unit[i] + '\n');
	i = i + 1;

dataFile.close();



    

