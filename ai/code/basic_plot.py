#!/usr/bin/env python3
"""
Basic plotting example
"""
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# https://chrisalbon.com/python/pandas_dataframe_importing_csv.html
my_data_frame = pd.read_csv('../example-data/billionaires.csv')
print(type(my_data_frame))

import pdb
pdb.set_trace()

basic_range = np.arange(0.0, 2.0, 0.01) # create range: start, stop, step
sin_func = 1 + np.sin(2*np.pi*basic_range)

import code; code.interact(local=dict(globals(), **locals()))
plt.plot(basic_range, sin_func)

plt.xlabel('time (s)')
plt.ylabel('voltage (mV)')
plt.title('About as simple as it gets, folks')
plt.grid(True)
plt.savefig("test.png") # write it out to file
# plt.show()
