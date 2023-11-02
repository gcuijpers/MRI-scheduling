# -*- coding: utf-8 -*-
"""
Created on Thu Nov  2 14:33:52 2023
"""

import numpy as np
from queue import PriorityQueue

lam_1 = 15
scan_mean_1 = 0.6
scan_sd_1 = 0.2

lam_2 = 9
scan_mean_2 = 0.5
scan_sd_2 = 0.1

n_slot_1 = 12
n_slot_2 = 10

def gen_one(lam, mean, sd):
    day = 0 
    time = 8
    patients = []
    
    while day < 2:
        arr_time = time + 9*np.random.exponential(1/lam)
        if arr_time > 17:
            day += 1 
            arr_time -= 9    
        scan_dur = np.random.normal(mean,sd)
        time = arr_time
        patients.append([day,time,scan_dur,1])
    patients = np.array(patients)
    patients = patients[:-1]
    return patients   

def gen_two(lam, mean, sd):
    day = 0 
    time = 8
    patients = []
    
    while day < 2:
        arr_time = time + 9*np.random.exponential(1/lam)
        if arr_time > 17:
            day += 1 
            arr_time -= 9    
        scan_dur = np.random.normal(mean,sd)
        time = arr_time
        patients.append([day,time,scan_dur,2])
    patients = np.array(patients)
    patients = patients[:-1]
    return patients   

def merge(pat_1, pat_2):
    patients = []
    n_two = pat_2.shape[0]
    j = 0
    for i in range(pat_1.shape[0]):
        while pat_2[j][0] <= pat_1[i][0] and pat_2[j][1] <= pat_1[i][1] and j < n_two-1:
            patients.append(pat_2[j])
            j += 1
        patients.append(pat_1[i])
    while j < n_two:
        patients.append(pat_2[j])
        j += 1
    patients = np.array(patients)
    return patients
'''
def merge(pat_1, pat_2):
    patients = []
    n_one = pat_1.shape[0]
    n_two = pat_2.shape[0]
    i = 0
    j = 0
    for k in range(n_one + n_two):
        if pat_1[i][0] <= pat_2[j][0] and pat_1[i][1] <= pat_2[j][1]:
            patients.append(pat_1[i])
            i += 1
            if i == n_one:
                while j < n_two:
                    patients.append(pat_2[j])
                    j += 1
                break
        else:
            patients.append(pat_2[j])
            j += 1
            if j == n_two:
                while i < n_one:
                    patients.append(pat_1[i])
                    i += 1
                break
    patients = np.array(patients)
    return patients
'''
pat_1 = gen_one(lam_1, scan_mean_1, scan_sd_1)
pat_2 = gen_two(lam_2, scan_mean_2, scan_sd_2)
patients = merge(pat_1, pat_2)
print(patients)










