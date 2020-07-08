import numpy as np

def distance(p1, p2, box):
    '''
    calculate the distance between point p1 and p2.
    box: the size of poriodic box. eg. np.array([5, 5, 5])
    '''
    delta = np.abs(p1 - p2)
    delta = np.where(delta > 0.5*box, delta - box , delta)
    dis =  np.sqrt((delta ** 2).sum(axis=-1))
    return dis


if __name__ == "__main__":
    box = np.array([5,5,5])
    point1 = np.array([1,1,1]) 
    point2 = np.array([2,2,2]) 

    d = distance(p1=point1,p2=point2,box=box)
    print(d)

    box = np.array([5,5,5])
    point1 = np.array([2,2,2]) 
    point2 = np.array([6,6,6]) 

    e = distance(p1=point1,p2=point2,box=box)
    print(e)

