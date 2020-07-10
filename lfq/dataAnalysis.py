import numpy as np 
from tools.util import distance
import matplotlib.pyplot as plt
from tqdm import tqdm

class frame():
    def __init__(self, GXYZ, box):
        self.GXYZ = GXYZ
        self.XYZ = self.GXYZ[:,1:]
        self.box = box 
        self.numParticles = self.XYZ.shape[0]
        self.globalDensity = self.numParticles/np.prod(self.box)
    
    def exclude(self, groupID):
        '''
        Method to return self.XYZ with group points excluded.
        '''
        mask = np.logical_not(self.GXYZ[:,0] == groupID)
        leftXYZ = self.XYZ[mask, :]
        # print('leftXYZ \n{}'.format(leftXYZ))
        return leftXYZ
    
    # Functions in class call method.
    def calRDF(self, gxyz, dr, maxTime, show=False):
        '''
        gxyz [GID, X, Y, Z]
        '''
        # 1. Find all XYZs excluding center group
        leftXYZ = self.exclude(gxyz[0])
        # print('leftXYZ \n {}'.format(leftXYZ))
        # 2. Calculate distances 
        xyz = np.array(gxyz[1:])
        # print('xyz shape {} \n {}'.format(xyz.shape, xyz))
        # print('left XYZ shape {} \n {}'.format(leftXYZ.shape, leftXYZ))
        distances = distance(xyz, leftXYZ, self.box)
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
            temp, bins = self.calRDF(gxyz=self.GXYZ[i], dr=dr, maxTime=maxTime)
            RDFList.append(temp)
        RDFArray = np.mean(np.array(RDFList), axis=0)
        return RDFArray, bins

if __name__ == "__main__":
    # Load all.csv
    data = np.loadtxt('all.csv', delimiter = ',')

    # Get one frame
    # Leave the last 10000 rows
    # Remove the first 3 columns
    XYZ = data[-2000:, 3:]


    numRows = XYZ.shape[0]
    numAtomsPerGroup = 5
    boxSize = 30
    box = np.array([boxSize, boxSize, boxSize])
    # print('Number of rows: {}'.format(numRows))

    # Grouping
    # Group Num: |0, 0, 0, 0, 0,| 1, 1, 1, 1, 1,|  ..., |399, 399, 399, 399, 399|.
    groupNumList = []
    positionNumList = []
    for i in range(numRows):
        groupNum = int(i/numAtomsPerGroup)
        groupNumList.append(groupNum)

    # Convert list to np array
    groupNumArray = np.array(groupNumList)
    # print(groupNumArray, type(groupNumArray))

    # Change shape from a row to a colum
    groupNumArray = np.reshape(groupNumArray, (2000, 1))

    # print(groupNumArray)
    # Concatenate groupNumArray and data.
    GXYZ = np.concatenate((groupNumArray, XYZ), axis=1)
    # print('GXYZ shape \n{}'.format(GXYZ.shape))
    # print('GXYZ \n{}'.format(GXYZ))
    f = frame(GXYZ, box)
    # # print('f.XYZ \n{}'.format(f.XYZ))
    # gxyz = GXYZ[0]
    # # print('gxyz[0] {}'.format(gxyz[0]))
    # leftXYZ = f.exclude(gxyz[0])
    # print('leftXZY \n{}'.format(leftXYZ))

    RDFArray, bins = f.calRDFPointsMean(0.01, 2000)
    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1)
    # Major ticks every 20, minor ticks every 5
    major_ticks = np.arange(0, 40, 5)
    minor_ticks = np.arange(0, 40, 1)
    ax.set_yticks(major_ticks)
    ax.set_yticks(minor_ticks, minor=True)
    ax.grid(which='both')
    # print(RDFArray.shape)
    # print(RDFArray)
    plt.xlabel('r')
    plt.ylabel('g(r)')
    # plt.hist(RDFArray, bins=bins)
    plt.plot(bins[1:], RDFArray)
    plt.show()

    