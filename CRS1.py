# -*- coding: utf-8 -*-
"""
Created on Wed Nov  1 15:30:36 2023
"""

import numpy as np

#type 1
lam_1 = 16.47826 # arrivals per day ~Poisson (inter-arrival ~Exp(9/lam))
scan_mean_1 = 0.432 # scan duration ~N
scan_sd_1 = 0.0974

#type 2
ia_mean = 0.8666387 # interarrival times ~N
ia_sd = 0.31
scan_shape = 12.583 # scan duration ~Gamma
scan_scale = 1/18.800

n_slot_1 = 18
n_slot_2 = 12

n_days = 10000

#generates list of type 1 patients and assigns randomly drawn scan duration as attribute of patient
def gen_one(lam, mean, sd): 
    day = 0 
    time = 8
    patients = []
    
    while day < n_days:
        arr_time = time + 9*np.random.exponential(1/lam)
        if arr_time > 17:
            day += 1 
            arr_time -= 9    
        scan_dur = np.random.normal(mean,sd)
        time = arr_time
        patients.append([day,time,scan_dur]) # save day + time of call. scan_dur as attribute
    patients = np.array(patients)
    patients = patients[:-1] # last patients calls at day = n_days + 1
    return patients   

#generates list of type 2 patients and assigns randomly drawn scan duration as attribute of patient
def gen_two(ia_mean, ia_sd, scan_mean, scan_sd):
    day = 0 
    time = 8
    patients = []
    
    while day < n_days:
        arr_time = time + np.random.normal(ia_mean, ia_sd)
        if arr_time > 17:
            day += 1 
            arr_time -= 9    
        scan_dur = np.random.gamma(scan_mean,scan_sd)
        time = arr_time
        patients.append([day,time,scan_dur,2])
    patients = np.array(patients)
    patients = patients[:-1]
    return patients   

#schedule: returns list of day, time, patient index
def schedule(patients, n_slot):
    schedule = []
    sch_day = 1
    slot = 0
    for pat in patients:
        if pat[0] == sch_day or slot == n_slot: # go to next day if no more available slots
            sch_day += 1 
            slot = 0
        patient_slot = [sch_day,8+slot*(9/n_slot)] # save day, time of start of slot
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
        lateness.append(max(0, time - schedule[i][1])) # if early, lateness = 0
        scan_sum += patients[i][2]
        time = max(time, schedule[i][1]) + patients[i][2]
        
        if i == len(schedule) -1:
            break
        if(schedule[i+1][1] == 8): # new day
            overtime.append(max(0, time - 17)) # overtime if time after last finish > 17 
            idletime.append(max(0, 9 - scan_sum + overtime[-1])) # idle time on day = length of day (incl. overtime) - sum of scan durations
            scan_sum = 0
            time = 8
        
    return lateness, overtime, idletime


pat_1 = gen_one(lam_1, scan_mean_1, scan_sd_1)
pat_2 = gen_two(ia_mean, ia_sd, scan_shape, scan_scale)

sch_1 = schedule(pat_1, n_slot_1)
sch_2 = schedule(pat_2, n_slot_2)

# day of scheduling - day of calling
days_wait_type1 = [int(sch_day - call_day) for sch_day, call_day in zip(sch_1[:,0], pat_1[:,0])] 
days_wait_type2 = [int(sch_day - call_day) for sch_day, call_day in zip(sch_2[:,0], pat_2[:,0])] 

lateness_t1, overtime_t1, idletime_t1 = perf_eval(sch_1, pat_1)
lateness_t2, overtime_t2, idletime_t2 = perf_eval(sch_2, pat_2)

print("TYPE 1")
print("Average lateness:", round(np.average(lateness_t1)*60,2), "minutes")
print("lateness 25th quantile:", round(np.quantile(lateness_t1, 0.25)*60,2), "minutes")
print("lateness 50th quantile:", round(np.quantile(lateness_t1, 0.5)*60,2), "minutes")
print("lateness 75th quantile:", round(np.quantile(lateness_t1, 0.75)*60,2), "minutes")
print("lateness 90th quantile:", round(np.quantile(lateness_t1, 0.9)*60,2), "minutes")
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
print("lateness 90th quantile:", round(np.quantile(lateness_t2, 0.9)*60,2), "minutes")
print("Maximum lateness:", round(np.max(lateness_t2)*60,2), "minutes")
print("Average days wait:", round(np.average(days_wait_type2),2), "days")
print("Maximum days wait:", round(np.max(days_wait_type2),2), "days")
print("Average overtime:", round(np.average(overtime_t2)*60,2), "minutes")
print("Average idle time:", round(np.average(idletime_t2),2), "h/day") # except last day

















