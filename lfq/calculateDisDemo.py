'''
Show how to use function of outer file.
'''

import numpy as np 
from tools.util import distance

if __name__ == "__main__":
    point1 = np.array([1,1,1]) 
    point2 = np.array([2,2,2]) 
    box = np.array([5, 5, 5])
    # dis = distance(point1, point2, box)
    dis = distance(p1=point1, p2=point2, box=box)
    # Todo by FQ
    print('This distance is {}'.format(dis))