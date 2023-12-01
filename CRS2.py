# -*- coding: utf-8 -*-
"""
Created on Thu Nov  2 14:33:52 2023
"""

import numpy as np

lam_1 = 16.47826
scan_mean_1 = 0.432
scan_sd_1 = 0.0974

lam_2 = 10.3913
scan_shape = 12.583
scan_scale = 1/18.800

n_slot_1 = 18
n_slot_2 = 12

days = 1000

def gen_one(lam, mean, sd):
    day = 0 
    time = 8
    patients = []
    
    while day < days:
        arr_time = time + 9*np.random.exponential(1/lam)
        if arr_time > 17:
            day += 1 
            arr_time -= 9    
        scan_dur = np.random.normal(mean,sd)
        time = arr_time
        patients.append([day,time,scan_dur])
    patients = np.array(patients)
    patients = patients[:-1]
    return patients   

def gen_two(lam, mean, sd):
    day = 0 
    time = 8
    patients = []
    
    while day < days:
        arr_time = time + 9*np.random.exponential(1/lam)
        if arr_time > 17:
            day += 1 
            arr_time -= 9    
        scan_dur = np.random.gamma(mean,sd)
        time = arr_time
        patients.append([day,time,scan_dur])
    patients = np.array(patients)
    patients = patients[:-1]
    return patients   

def schedule(patients, n_slot):
    schedule = []
    sch_day = 1
    slot = 0
    for pat in patients:
        if pat[0] == sch_day or slot == n_slot:
            sch_day += 1 
            slot = 0
        patient_slot = [sch_day,8+slot*(9/n_slot)]
        slot += 1 
        schedule.append(patient_slot)
    schedule = np.array(schedule)
    return schedule

def perf_eval(schedule, patients):
    lateness = []
    overtime = []
    idletime = []
    scan_sum = 0
    time = 8

    for i in range(len(schedule)):
        lateness.append(max(0, time - schedule[i][1]))
        scan_sum += patients[i][2]
        time = max(time, schedule[i][1]) + patients[i][2]
        
        if i == len(schedule) -1:
            break
        if(schedule[i+1][1] == 8): # new day
            overtime.append(max(0, time - 17))
            idletime.append(max(0, 9 - scan_sum))
            scan_sum = 0
            time = 8
        
    return lateness, overtime, idletime


pat_1 = gen_one(lam_1, scan_mean_1, scan_sd_1)
pat_2 = gen_two(lam_2, scan_shape, scan_scale)

sch_1 = schedule(pat_1, n_slot_1)
sch_2 = schedule(pat_2, n_slot_2)

days_wait_type1 = [int(sch_day - call_day) for sch_day, call_day in zip(sch_1[:,0], pat_1[:,0])] 
days_wait_type2 = [int(sch_day - call_day) for sch_day, call_day in zip(sch_2[:,0], pat_2[:,0])] 

lateness_t1, overtime_t1, idletime_t1 = perf_eval(sch_1, pat_1)
lateness_t2, overtime_t2, idletime_t2 = perf_eval(sch_2, pat_2)



print("TYPE 1")
print("Average lateness:", round(np.average(lateness_t1)*60,2), "minutes")
print("lateness 25th quantile:", round(np.quantile(lateness_t1, 0.25)*60,2), "minutes")
print("lateness 50th quantile:", round(np.quantile(lateness_t1, 0.5)*60,2), "minutes")
print("lateness 75th quantile:", round(np.quantile(lateness_t1, 0.75)*60,2), "minutes")
print("Maximum lateness:", round(np.max(lateness_t1)*60,2), "minutes")
print("Average days wait:", round(np.average(days_wait_type1),2), "days")
print("Maximum days wait:", round(np.max(days_wait_type1),2), "days")
print("Average overtime:", round(np.average(overtime_t1)*60,2), "minutes")
print("Average idle time:", round(np.average(idletime_t1),2), "h/day \n") # except last day


print("TYPE 2")
print("Average lateness:", round(np.average(lateness_t2)*60,2), "minutes")
print("lateness 25th quantile:", round(np.quantile(lateness_t2, 0.25)*60,2), "minutes")
print("lateness 50th quantile:", round(np.quantile(lateness_t2, 0.5)*60,2), "minutes")
print("lateness 75th quantile:", round(np.quantile(lateness_t2, 0.75)*60,2), "minutes")
print("Maximum lateness:", round(np.max(lateness_t2)*60,2), "minutes")
print("Average days wait:", round(np.average(days_wait_type2),2), "days")
print("Maximum days wait:", round(np.max(days_wait_type2),2), "days")
print("Average overtime:", round(np.average(overtime_t2)*60,2), "minutes")
print("Average idle time:", round(np.average(idletime_t2),2), "h/day") # except last day









