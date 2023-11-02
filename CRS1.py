# -*- coding: utf-8 -*-
"""
Created on Wed Nov  1 15:30:36 2023
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
    
    while day < 5:
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
    
    while day < 5:
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

def schedule_one(patients, n_slot):
    schedule = []
    sch_day = 1
    slot = 0
    for pat in patients:
        if pat[0] == sch_day or slot == n_slot:
            sch_day += 1 
            slot = 0
        patient_slot = [sch_day,8+slot*(9/n_slot),1,pat[2],pat[0],pat[1],pat[3]]
        slot += 1 
        schedule.append(patient_slot)
    schedule = np.array(schedule)
    return schedule


pat_1 = gen_one(lam_1, scan_mean_1, scan_sd_1)
sch_1 = schedule_one(pat_1, n_slot_1)
print(sch_1)

















