import pandas as pd
import os
import glob 


csv_list = glob.glob('paixu/*csv')  #查看同一文件夹下的csv文件数
print('共发现%s个csv文件'% len(csv_list))
paixupath = "paixu"
print (csv_list)
# print('正在处理')
for csv in csv_list: #循环读取同一文件夹下的csv文
   # filename = os.path.join(paixupath,csv)           
    fr = open(csv,'rb').read()
    with open('all.csv','ab') as f:
        f.write(fr)
        #print('写入成功')
print('合并完毕')
