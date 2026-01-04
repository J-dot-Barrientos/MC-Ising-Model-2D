#!/usr/bin/env python
# coding: utf-8

# In[5]:


import numpy as np
import matplotlib.pyplot as plt

def block_average(data, block_size):
    n = len(data)
    n_blocks = n // block_size
    if n_blocks < 2:
        raise ValueError("Muy pocos bloques")
    data = data[:block_size * n_blocks]
    blocks = data.reshape((n_blocks, block_size))
    block_means = np.mean(blocks, axis=1)
    mean = np.mean(block_means)
    error = np.std(block_means, ddof=1) / np.sqrt(n_blocks)
    return mean, error


# ==========================
# Cargar datos
# ==========================
fname = "Energy.dat"
data = np.loadtxt(fname)
N = len(data)

# ==========================
# Barrido en número de bins
# ==========================
max_bins = 100

n_bins_list = []
block_sizes = []
errors = []
means = []

for n_bins in range(2, max_bins + 1):
    block_size = N // n_bins
    if block_size < 1:
        break

    mean, err = block_average(data, block_size)

    n_bins_list.append(n_bins)
    block_sizes.append(block_size)
    means.append(mean)
    errors.append(err)

    #print(
     #   f"n_bins = {n_bins:3d} | "
      #  f"block_size = {block_size:6d} | "
       # f"mean = {mean:.6f} | "
        #f"error = {err:.6f}"
    #)

n_bins_list = np.array(n_bins_list)
errors = np.array(errors)

# ==========================
# Gráfica error vs número de bins
# ==========================
plt.figure(figsize=(7,5))
plt.plot(n_bins_list, errors, marker='o')
plt.xlabel("Number of bins")
plt.ylabel("Statistical error")
plt.title("Binning analysis: error vs number of bins")
plt.grid(True, ls="--", alpha=0.6)
plt.tight_layout()
plt.show()

