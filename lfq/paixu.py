import sys,csv,operator
import os
base="pick"
outpath = "paixu"
if os.path.exists(outpath) != True:
    os.makedirs (outpath)
for i in range(100):
    filename = os.path.join(base,'dump_' + str(i) + '.csv')
    data = csv.reader(open(filename),delimiter=',')
    sortedlist = sorted(data,key = lambda x:x[1],reverse=False)
    print(sortedlist)
    outfilename = os.path.join(outpath,str(i) + ".csv")
    with open(outfilename,"w",newline='') as f:
        fileWritwr = csv.writer(f,delimiter=',')
        for row in sortedlist:
            print(row)
            fileWritwr.writerow(row)
    f.close()
