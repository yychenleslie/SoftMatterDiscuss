# 导入一个需要的函数库叫做numpy简称为np
import numpy as np 

# 如何产生一个[0，1]之间的随机数?
num = np.random.rand()
print('num: {}'.format(num))

# 如何产生两个[0，1]之间的随机数?
x, y = np.random.rand(2)
# {} 是占位的意思, 值由后面format函数中的值给出。 \n代表换行
print('x:{} \ny:{}'.format(x, y)) 

# 内切圆的圆心(xc,yc)是多少？
xc, yc = 0.5, 0.5
print('xc:{} \nyc:{}'.format(xc, yc))

# 如何计算点(x,y)到圆心(xc,yc)的距离？
distance = np.sqrt((x-xc)**2 + (y-yc)**2)
print('Distance between (x,y) to (xc, yc) is:\n{}'.format(distance))

# 如何判断点(x,y)是否在内切圆内？
R = 0.5
inCircle = distance < R 
print('inCircle: {}'.format(inCircle))


# 向正方形里面随机扔点
total_number = 10000
inNumber = 0 
for i in range(total_number):
    x, y = np.random.rand(2)
    distance = np.sqrt((x-xc)**2 + (y-yc)**2)
    inCircle = distance < R 
    if inCircle:
        inNumber = inNumber + 1
    
print('inNumber:{}'.format(inNumber))
pi = 4*inNumber/total_number

print('4*inNumber/total_number:{}'.format(pi))

# Ref: https://www.cnblogs.com/juking/p/9498884.html
