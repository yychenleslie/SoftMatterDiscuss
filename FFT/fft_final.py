import numpy as np
data = np.loadtxt('dfpde_total.plt', skiprows=2)

NX, NY, NZ = 32, 32, 32

# x, y, z, phi_A, phi_B 
XYZ = data[:, [0,1,2]]
phi_A = data[:, 3]
phi_B = data[:, 4]

# reshape to 3d array
phi_A = np.reshape(phi_A, (NX, NY, NZ))
phi_B = np.reshape(phi_B, (NX, NY, NZ))

# FFT
phi_A_fft = np.fft.fftn(phi_A)
phi_B_fft = np.fft.fftn(phi_B)

# reshape back to 1 colum
phi_A_fft_one_col = np.ndarray.flatten(phi_A_fft).reshape((-1, 1))
phi_B_fft_one_col = np.ndarray.flatten(phi_B_fft).reshape((-1, 1))

# Get the modulus of complex number
phi_A_fft_one_col_abs = np.absolute(phi_A_fft_one_col)
phi_B_fft_one_col_abs = np.absolute(phi_B_fft_one_col)



# Get index of hkl space
HKL =  np.empty((32**3, 3))
counter = 0
for h in range(NX):
    for k in range(NY):
        for l in range(NZ):
            hkl = np.array([h, k, l])
            HKL[counter] = hkl
            counter += 1 
print(HKL)

# Complex results of FFT
data_save = np.concatenate((phi_A_fft_one_col, phi_B_fft_one_col), axis=-1)
np.savetxt('0_result.txt', data_save)

# q_h, q_k, q_l, |phi_A_q|^2, |phi_A_q|
np.savetxt('1_result.txt', np.concatenate((HKL, phi_A_fft_one_col_abs**2, phi_A_fft_one_col_abs), axis=-1))

# q_h, q_k, q_l, |phi_B_q|^2, |phi_B_q|
np.savetxt('2_result.txt', np.concatenate((HKL, phi_B_fft_one_col_abs**2, phi_B_fft_one_col_abs), axis=-1))

# q_h, q_k, q_l, q_h^2 + q_k^2 + q_l^2, |phi_A_q|^2
np.savetxt('3_result.txt', np.concatenate((HKL, np.sum(HKL**2, axis=-1).reshape((-1,1)), phi_A_fft_one_col_abs**2), axis=-1))

# q_h, q_k, q_l, q_h^2 + q_k^2 + q_l^2, |phi_B_q|^2
np.savetxt('4_result.txt', np.concatenate((HKL, np.sum(HKL**2, axis=-1).reshape((-1,1)), phi_B_fft_one_col_abs**2), axis=-1))




