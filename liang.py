import numpy as np

array = np.random.randint(0,10,size=[3,3,3])
print(array)

def distance(x0,x1,3)
    delta = np.abs(x0 - x1)
    delta = np.where(delta > 0.5*3,delta - 3,delta)
    return np.sqrt((delta ** 2).sum(axis=-1))

    print(distance)

