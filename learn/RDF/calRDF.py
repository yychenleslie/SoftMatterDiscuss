'''
This progrogram shows how to calculate RDF (Radial Distribution Function)
'''

import numpy as np 
import itertools
from tools.util import distance
import matplotlib.pyplot as plt 
from tqdm import tqdm

def gen3DCoordinates(numPoint, box):
    '''
    Function to generate XYZ coordinates of in a box.
    Input: numPoint: number of points in box range. 
    box: box size of this systems. eg. np.array([5,5,5])
    '''
    # https://stackoverflow.com/questions/15305719/pick-combinations-from-multiple-lists
    x = np.linspace(0, box[0], numPoint, endpoint=False)
    y = np.linspace(0, box[1], numPoint, endpoint=False)
    z = np.linspace(0, box[2], numPoint, endpoint=False)
    comb = itertools.product(x, y, z)
    XYZ = np.array(list(comb))
    return XYZ


class frame():
    def __init__(self, XYZ, box):
        self.XYZ = XYZ
        self.box = box 
        self.numParticles = self.XYZ.shape[0]
        self.globalDensity = self.numParticles/np.prod(self.box)
    
    def exclude(self, points):
        '''
        Method to return self.XYZ with points excluded.
        '''
        mask = np.logical_not(np.all(self.XYZ==points, axis=1))
        leftXYZ = self.XYZ[mask, :]
        # Identical to the folllowing line of code
        # leftXYZ = self.XYZ[mask]
        return leftXYZ
    
    # Functions in class call method.
    def calRDF(self, center, dr, maxTime, show=False):
        # 1. Find all XYZs excluding center
        leftXYZ = self.exclude(center)
        # print('leftXYZ \n {}'.format(leftXYZ))
        # 2. Calculate distances 
        distances = distance(center, leftXYZ, self.box)
        # print('Distances are \n {}'.format(distances))
        # print(len(distances))
        # 3. Count
        bins = [i*dr for i in range(maxTime)]
        # 0, dr, 2dr, 3dr
        if show:
            plt.grid()
            plt.xlabel('r')
            plt.ylabel('Number of particles')
            plt.hist(distances, bins=bins)
            plt.show()
        hist, bins = np.histogram(distances, bins)
        # print('Len hist and bins {} {}'.format(len(hist), len(bins)))
        # print('Hist \n{} \nBins \n{}'.format(hist, bins))

        volOfShells = 4 * np.pi * (bins[1:]**2) * dr
        RDF = hist/volOfShells/self.globalDensity
        # print('hist shape\n{} \n {}'.format(len(hist), hist))
        # print('volOfShells shape\n{} \n {}'.format(len(volOfShells), volOfShells))
        # print('Global density:{}'.format(self.globalDensity))
        # print('RDF\n {}'.format(RDF))
        if show:
            plt.grid()
            plt.xlabel('r')
            plt.ylabel('g(r)')
            plt.plot(bins[1:], RDF)
            plt.show()
        return RDF, bins



    def calRDFPointsMean(self, dr, maxTime):
        # 1. Use calRDF many times 
        RDFList = []
        for i in tqdm(range(self.numParticles)):
            temp, bins = self.calRDF(center=self.XYZ[i], dr=dr, maxTime=maxTime)
            RDFList.append(temp)
        RDFArray = np.mean(np.array(RDFList), axis=0)
        return RDFArray, bins


if __name__ == "__main__":
    # 1. Genreate a simple system has many particals
    num = 7
    box = np.array([5,5,5])
    XYZ =  gen3DCoordinates(num, box)
    # print('XYZs of all particales: \n{}'.format(XYZ))
    # print('Shape of XYZ {}'.format(XYZ.shape))

    # 2. 
    point = np.array([0,0,0])
    f = frame(XYZ, box)
    # left = f.exclude(point)
    # print('Left XYZs \n {}'.format(left))
    # f.calRDF(point, 0.1, 50, show=True)

    RDFArray, bins = f.calRDFPointsMean(0.01, 500)
    # print(RDFArray.shape)
    # print(RDFArray)
    plt.grid()
    plt.xlabel('r')
    plt.ylabel('g(r)')
    # plt.hist(RDFArray, bins=bins)
    plt.plot(bins[1:], RDFArray)
    plt.show()





