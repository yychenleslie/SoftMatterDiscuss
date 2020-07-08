'''
This progrogram shows how to calculate RDF (Radial Distribution Function)
'''

import numpy as np 
import itertools
from tools.util import distance

def gen3DCoordinates(numPoint, box):
    '''
    Function to generate XYZ coordinates of in a box.
    Input: numPoint: number of points in box range. 
    box: box size of this systems. eg. np.array([5,5,5])
    '''
    # https://stackoverflow.com/questions/15305719/pick-combinations-from-multiple-lists
    x = np.linspace(0, box[0], numPoint)
    y = np.linspace(0, box[1], numPoint)
    z = np.linspace(0, box[2], numPoint)
    comb = itertools.product(x, y, z)
    XYZ = np.array(list(comb))
    return XYZ


class frame():
    def __init__(self, XYZ):
        self.XYZ = XYZ
    
    def exclude(self, points):
        # 
        leftXYZ = None
        return leftXYZ
    
    # Functions in class call method.
    def calRDF(self, center, dr, maxTime):
        # 1. Find all XYZs excluding center
        # 2. Calculate distances 
        # 3. Count
        pass

    def calRDFMean(self):
        # 1. Use calRDF many times 
        pass


if __name__ == "__main__":
    # 1. Genreate a simple system has many particals
    num = 6
    box = np.array([5,5,5])
    XYZ =  gen3DCoordinates(num, box)
    print('XYZs of all particales: \n{}'.format(XYZ))

    # 2. 
