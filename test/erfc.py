# -*- coding: utf-8 -*-
"""
Created on Fri Aug  7 10:52:57 2020

@author: tatarchuck1900
"""

import math
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
snr=np.linspace(5,14,10)

r=890/10240


# pe=math.erfc(math.sqrt(snr*(1-r)))
p=[]

for i in range(5,15):
    print(i)
    plt.yticks(np.arange(0, 10e9,10e3 ))
    
    p.append(math.erfc(math.sqrt(i*(1-r))))
    
    
    

plt.plot(snr,p)