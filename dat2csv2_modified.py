#!/usr/bin/env python
# coding: utf-8


# %load dat2csv2.py
import csv
import os

directory = '/Users/whitneyhung/Documents/panc-local/thesis/whitneydata'
# directory = '/Users/mbpr/Downloads/tmp'
list_of_caffe = ['02','03','05','07','10','12','17','19','20','21'] #experiment group who took caffeine
list_of_control = ['04','06','08','09','11','13','14','16','18','22']
# 寫出成一個檔案, 偵測沒有header的話(第一次寫入檔案時)，write header
headers = ['num', 'trialid', 'lum', 'trigger1', 'trigger2', 'eccent', 'delay', 
           'type', 'probech', 'crit_pos', 'crit_fix', 'next_pos', 'next_fix', 'fixdir', 
           'resprt', 'respcorr', 'freq', 'prev_pos', 'prev_fix', 'id', 'exp', 'prepost']

csv_file = open('data2.csv', 'a+')
csv_writer = csv.writer(csv_file) #build writer for csv
if len(csv_file.read())==0: csv_writer.writerow(headers)

for filename in os.listdir(directory):
    # filename = "T102.dat"
    # 這邊只是為了增加欄位設定的內容
    id_ = filename[:-4]
    idnum = id_[2:]
    print(idnum)
    dig3 = id_[1:2] # pre or post test

    if filename.endswith(".dat"):
        with open(filename) as dat_file:
            dat_ls = dat_file.readlines()[1:] # read dat into a variable (type: list) and OMIT the header
            for line in dat_ls:
                row = [field.strip() for field in line.split('\t')]
                row.append(id_) # 這邊只是想要增加欄位
                if idnum in list_of_caffe: #exp(1) or control(0) grp
                    row.append('1')
                else:
                    row.append('0')
                if dig3 == '0': # pre test = 0, post test = 1
                    row.append('0')
                else:
                    row.append('1')
                print(row)
                csv_writer.writerow(row)

