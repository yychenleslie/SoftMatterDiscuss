from dump_reader import *
import os

# file_path: replace with your own
file_path = './dump.np.lammpstrj'

# The line number before the first xyz coordinates,
# which equals to the line number  counting to the first line of 
# 'ITEM: ATOMS id type x y z ' in my case.
extra_line_num = 9

# Get all the dump list  
dump_list= read_dumps(file_path=file_path, extra_line_num=9)

# Show the first dump   
print(dump_list[0].time_step)
    
print(dump_list[1].xyz_df)

# Select all the atoms of type 2

print(dump_list[2].get_xyz_type(2))

# Get specific part of dataframe 
# Example: The xyz coordinates of all the atoms of type 2 at the last dump


xyz = dump_list[0].get_xyz_type(2)
xyz = xyz.loc[:,2:]
print(xyz)

xyz = dump_list[1].get_xyz_type(2)
xyz = xyz.loc[:,2:]
print(xyz)

xyz = dump_list[2].get_xyz_type(2)
xyz = xyz.loc[:,2:]
print(xyz)
# print(len(dump_list))
folderName = 'pick'
if os.path.exists(folderName) == False:
    os.mkdir(folderName)

for i in range(len(dump_list)):
    xyz = dump_list[i].get_xyz_type(2)
    xyz = xyz.loc[:,0:]

    outputName = os.path.join(folderName,'dump_' + str(i) + ".csv")
    xyz.to_csv(outputName,header=None)


