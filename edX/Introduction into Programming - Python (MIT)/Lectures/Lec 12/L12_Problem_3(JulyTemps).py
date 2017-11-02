import pylab

inFile = open('julyTemps.txt')

highTemp = []
lowTemp = []



# looking through all lines and adding temps to the needed lists:
for line in inFile:
    fields = line.split(' ')
    if len(fields) != 3 or 'Boston' == fields[0] or 'Day' == fields[0]:
        filt = None
    else:
            #print fields
        high = fields[1]
        low = fields[2][:2]
        highTemp.append(high)
        lowTemp.append(low)

highAndLow = (highTemp, lowTemp)


#def producePlot(highTemp, lowTemp):
diffTemps = []
for i in range(31):
    diffTemps.append(int(highTemp[i]) - int(lowTemp[i]))
pylab.figure(1)
pylab.plot(range(1,32), diffTemps)
pylab.title('Day by Day Ranges in Temperature in Boston in July 2012')
pylab.xlabel('Days')
pylab.ylabel('Temperature Ranges')
#pylab.savefig('L12_P5_graph')
pylab.show()

