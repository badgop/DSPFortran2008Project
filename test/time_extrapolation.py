# -*- coding: utf-8 -*-
"""
Created on Wed Aug  5 14:12:28 2020

@author: tatarchuck1900
"""

time = 20.0

iteration=5000*2

secund=1
minute=60
hour=minute*60
day= hour*24

time_in_hours=time*iteration/hour
time_in_days=time*iteration/day


print('время в часах ',time_in_hours)
print('время  в днях',time_in_days)