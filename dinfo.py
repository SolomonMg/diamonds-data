

import urllib
from bs4 import BeautifulSoup
import csv
import os
import time

filedir = "/Users/directory-to-write-output-here"

shape = 'Round'
minCarat = '0.1'
maxCarat = '10'
minColor = '1'
maxColor = '9'
minPrice = '1'
maxPrice = '100000'
minCut = '5'
maxCut = '1'
minClarity = '1'
maxClarity = '10'
minDepth = '0'
maxDepth = '90'
minWidth = '0'
maxWidth = '90'
gia = '1'
ags = '1'
egl = '1'
oth = '1'
currency = 'USD'
rowStart = '0'
sortCol = 'price'
sortDir = 'ASC'
thisrow = 0

# about 26k pages
# about 598k diamonds
# chunk by about 2.5k diamonds
# 598000/2500 = 239

thischunk = 0
thisrow =  thischunk * 2500
rowStart = str(thisrow)
thiscount = 0

while (thischunk < 239):
    try:
        f = csv.writer(open(filedir + str(thischunk) + ".csv", "wb"), dialect='excel')
        # Write column headers as the first line
        f.writerow(["carat", "cut", "color", "clarity", "table", "depth", "cert", "measurements", "price"])
        while (thiscount < 2500):
            uri = "http://www.diamondse.info/webService.php?shape="+shape+"&minCarat="+minCarat+"&maxCarat="+maxCarat+"&minColor="+minColor+"&maxColor="+maxColor+"&minPrice="+minPrice+"&maxPrice="+maxPrice+"&minCut="+minCut+"&maxCut="+maxCut+"&minClarity="+minClarity+"&maxClarity="+maxClarity+"&minDepth="+minDepth+"&maxDepth="+maxDepth+"&minWidth="+minWidth+"&maxWidth="+maxWidth+"&gia="+gia+"&ags="+ags+"&egl="+egl+"&oth="+oth+"&currency="+currency+"&rowStart="+rowStart+"&sortCol="+sortCol+"&sortDir="+sortDir
            urllines = urllib.urlopen(uri)
            pagedat = urllines.read()
            urllines.close()
            soup = BeautifulSoup(pagedat)
            for row in soup.find_all("tr"):
                tds = row.find_all("td")
                try:
                    carat = str(tds[2].get_text())
                    cut = str(tds[3].get_text())
                    color = str(tds[4].get_text())
                    clarity = str(tds[5].get_text())
                    table = str(tds[6].get_text())
                    depth = str(tds[7].get_text())
                    cert = str(tds[8].get_text())
                    measurements = str(tds[9].get_text())
                    price = str(tds[10].get_text())
                except:
                    print "bad string"
                    time.sleep(20)
                    continue
                #print [carat, cut, color, clarity, table, depth, cert, measurements, price]
                f.writerow([carat, cut, color, clarity, table, depth, cert, measurements, price])
            time.sleep(0.1)
            thiscount = thiscount + 20
            thisrow = thisrow + 20
            rowStart = str(thisrow)
            print 'this row = ' + rowStart
        thischunk = thischunk + 1
        thiscount = 0
    except:
        print "Possible connectivity issues"
        time.sleep(10)
        thisrow =  thischunk * 2500
        rowStart = str(thisrow)
        continue

