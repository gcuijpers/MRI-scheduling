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

slotlength_1 = 9/17
slotlength_2 = 9/11

n_days = 1000

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
        patients.append([day,time,scan_dur,1])
    patients = np.array(patients)
    patients = patients[:-1]
    return patients   

def gen_two(lam, mean, sd):
    day = 0 
    time = 8
    patients = []
    
    while day < n_days:
        arr_time = time + 9*np.random.exponential(1/lam)
        if arr_time > 17:
            day += 1 
            arr_time -= 9    
        scan_dur = np.random.gamma(mean,sd)
        time = arr_time
        patients.append([day,time,scan_dur,2])
    patients = np.array(patients)
    patients = patients[:-1]
    return patients   

def merge(pat_1, pat_2): #merge and sort by incoming call time
    stacked_patients = np.vstack((pat_1, pat_2))
    sorted_indices = np.lexsort((stacked_patients[:, 1], stacked_patients[:, 0]))
    return stacked_patients[sorted_indices]
    
def schedule(patients, slotlengths):
    schedule_1 = []
    schedule_2 = []
    sch_days = [1,1]
    schedule_1.append([1,8,0])
    schedule_2.append([1,8,1])
    sch_time_1 = 8 + slotlengths[int(patients[0][3]-1)]
    sch_time_2 = 8 + slotlengths[int(patients[1][3]-1)]
    

    for i in range(2,len(patients)): #day, time, patient's index
    
        #only shift if neither type can be scheduled anymore or patients call on same day
        if sch_time_1 + min(slotlengths) > 17 or patients[i][0] == sch_days[0]:
            sch_days[0] += 1
            sch_time_1 = 8
        if sch_time_2 + min(slotlengths) > 17 or patients[i][0] == sch_days[1]:
            sch_days[1] += 1
            sch_time_2 = 8
            
        if sch_time_1 + slotlengths[int(patients[i][3] -1)] > 17 and sch_time_2 + slotlengths[int(patients[i][3] -1)] > 17:
            sch_days[1] += 1 
            sch_time_2 = 8
    
        if (schedule_1[-1][0], schedule_1[-1][1]) <= (schedule_2[-1][0], schedule_2[-1][1]) and sch_time_1 + slotlengths[int(patients[i][3] -1)] <= 17:
            schedule_1.append([sch_days[0], round(sch_time_1,2), i])
            sch_time_1 += slotlengths[int(patients[i][3] -1)]
        else:
            schedule_2.append([sch_days[1], round(sch_time_2,2), i])
            sch_time_2 += slotlengths[int(patients[i][3] -1)]
            
    return schedule_1, schedule_2

def performance_eval(schedule, patients):
    lateness = [] # hours scan starts later than scheduled
    days_wait = [] # n. days in future patients are scheduled upon calling
    overtime = [] # hours of overtime for each day (after 17:00)
    idle_time = [] # machine idle time between 8-17 per day (except last day)
    idle_time_curr = 0
    
    curr_time = 8
    
    for slot in schedule:
        if slot[1] == 8 and slot[0] > 1: # first patient always scheduled at 8
            overtime.append(max(0, curr_time - 17))
            idle_time_curr += max(0, 17 - curr_time)
            idle_time.append(idle_time_curr)
            idle_time_curr = 0
            
            curr_time = 8
            
        pat = patients[slot[2]]
        lateness.append(max(0, curr_time - slot[1]))
        idle_time_curr += max(0, slot[1] - curr_time) #starting time - prev. finish
        days_wait.append(int(slot[0] - pat[0]))
        
        curr_time = max(curr_time, slot[1]) + pat[2]
        
    return lateness, days_wait, overtime, idle_time

pat_1 = gen_one(lam_1, scan_mean_1, scan_sd_1)
pat_2 = gen_two(lam_2, scan_shape, scan_scale)
patients = merge(pat_1, pat_2)
schedules = schedule(patients, [slotlength_1, slotlength_2])

lateness_m1, days_wait_m1, overtime_m1, idle_time_m1 = performance_eval(schedules[0], patients)
lateness_m2, days_wait_m2, overtime_m2, idle_time_m2 = performance_eval(schedules[1], patients)

lateness = lateness_m1 + lateness_m2
days_wait = days_wait_m1 + days_wait_m2
overtime = [max(m1, m2) for m1, m2 in zip(overtime_m1, overtime_m2)] # entire team stays if overtime on 1 machine?
idle_time = [m1 + m2 for m1, m2 in zip(idle_time_m1, idle_time_m2)]

#print(lateness,"\n")
print("Average lateness:", round(np.average(lateness)*60,2), "minutes")
print("lateness 25th quantile:", round(np.quantile(lateness, 0.25)*60,2), "minutes")
print("lateness 50th quantile:", round(np.quantile(lateness, 0.5)*60,2), "minutes")
print("lateness 75th quantile:", round(np.quantile(lateness, 0.75)*60,2), "minutes")
print("Maximum lateness:", round(np.max(lateness)*60,2), "minutes")
print("Average days wait:", round(np.average(days_wait),2), "days")
print("Maximum days wait:", round(np.max(days_wait),2), "days")
print("Average overtime:", round(np.average(overtime)*60,2), "minutes")
print("Average (combined) idle time:", round(np.average(idle_time),2), "h/day \n") # except last day









