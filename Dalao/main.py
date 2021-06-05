from dump_reader import *
import numpy as np 

# file_path: replace with your own
file_path = 'dump.fus1.lammpstrj'

# The line number before the first xyz coordinates,
# which equals to the line number  counting to the first line of 
# 'ITEM: ATOMS id type x y z ' in my case.
extra_line_num = 9

# Get all the dump list  
dump_list= read_dumps(file_path=file_path, extra_line_num=9)

# Dump number
print('Dump number: {}'.format(len(dump_list)))

# The first dump 
print(dump_list[0].time_step)
print(dump_list[0].xyz_df)

for dump in dump_list:
    # Select by type: for example type 3
    XYZ = dump.get_xyz_type(3)
    XYZ = XYZ.loc[:, 2:].to_numpy()
    print(XYZ, type(XYZ))
    X = XYZ[:,0]
    Y = XYZ[:,1]
    Z = XYZ[:,2]
    print(np.mean(X))
    # Calculate distribution
    # https://numpy.org/doc/stable/reference/generated/numpy.histogram.html
    hist, bins = np.histogram(X, bins=10)
    print(hist, bins)
    # Calculate center of mass
    x_c = np.mean(X)
    y_c = np.mean(Y)
    z_c = np.mean(Z)
    print('Center of mass:{} {} {}'.format(x_c, y_c, z_c))






