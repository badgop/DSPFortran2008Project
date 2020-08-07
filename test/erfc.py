# -*- coding: utf-8 -*-
"""
Created on Fri Aug  7 10:52:57 2020

@author: tatarchuck1900
"""

import math

snr=8
r=890/10240


pe=math.erfc(math.sqrt(snr*(1-r)))

print(pe)