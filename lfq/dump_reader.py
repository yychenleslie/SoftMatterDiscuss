import pandas as pd 

class dump:
    def __init__(self, time_step, xyz_df):
        self.time_step = time_step
        self.len = len(xyz_df)
        self.xyz_df = xyz_df

    def get_xyz_type(self, type_num):
        df_selected = self.xyz_df[self.xyz_df[1] == type_num]
        return df_selected

def read_dumps(file_path, extra_line_num):
    line_pointer = 0
    num_of_atom_df = pd.read_csv(file_path, skiprows=line_pointer + 3, \
                            nrows=1, header=None)
    num_of_atom = int(num_of_atom_df[0])
    dump_list = []
    while True:
        try:
            time_step_df = pd.read_csv(file_path, skiprows=line_pointer + 1, \
                                    nrows=1, header=None)
        except:
            break
        time_step = int(time_step_df[0])

        xyz_df = pd.read_csv(file_path, skiprows=line_pointer + 9, \
                                   nrows=num_of_atom, sep=' ', header=None)
        xyz_df = xyz_df.iloc[:,:-1]
        line_pointer += (extra_line_num + num_of_atom)

        print('Time Step: ', time_step, ' completed.')
        dump_list.append(dump(time_step, xyz_df))
    return dump_list 
