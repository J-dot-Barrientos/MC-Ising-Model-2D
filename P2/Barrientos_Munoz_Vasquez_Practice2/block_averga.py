def block_average(data, block_size=16):
    n = len(data)
    n_blocks = n // block_size
    data = data[:block_size * n_blocks]
    blocks = data.reshape((n_blocks, block_size))
    block_means = np.mean(blocks, axis=1)
    mean = np.mean(block_means)
    error = np.std(block_means, ddof=1) / np.sqrt(n_blocks)
    return mean, error

fname = "test1.dat"

data = np.loadtxt(fname)
spin = data[:]
spin_mean, spin_error = block_average(spin)
print(f"Spin: {spin_mean} ± {spin_error}")
