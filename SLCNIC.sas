/***************SLCO NIC Residents***************/

/*Creation Date - 2022-07-07*/
/*Created By - Jake Ortega*/

/* This code uses the most recent export from eHARS as well as a export from Epi-Trax to determine Not in Care number*/

/*This code identifies people with a current address in SLCO who are Not in Care.  Not in Care is defined as an individual who
has not had a CD4, Viral Load, or Genotype test result within the last 18 months*/

%let start_dt='01JAN2021'd;
%let LHD = 'SALT LAKE CO.';
libname person 'T:\HIV Prevention\01_HIV_Surveillance\Data\eHARS Exports\QA Exports\person_sas7bdat';
libname library 'C:\AIDS\SAS\Formats';
options fmtsearch=(library.Eharsfmt) nofmterr yearcutoff=1920;

/*Import Epi-Trax export to get CMR info - file pathway will change*/

proc import datafile='T:\HIV Prevention\01_HIV_Surveillance\Data\UT-NEDSS Exports\CMR.csv' DBMS=csv out=CMR; guessingrows=max;
run;

proc copy in=Person out=work;
run;

/*keeping variables of interest as well as reorder and cleaning*/

data CMR;
set CMR (rename=(other_data_2=stateno));
run;

data Person_SLC;
	retain vital_status hiv_categ hiv_dx_dt stateno dob first_name middle_name last_name cur_county_name race cd4_recent_cnt_dt cd4_recent_cnt_pct_dt dna_recent_dt vl_recent_dt test_recent_dt test_recent_type arv_hiv_tx_last_use_dt UT_FOLLOWUP_STATUS;
	set person (keep=vital_status hiv_categ hiv_dx_dt stateno dob first_name middle_name last_name cur_county_name race cd4_recent_cnt_dt cd4_recent_cnt_pct_dt dna_recent_dt vl_recent_dt test_recent_dt test_recent_type arv_hiv_tx_last_use_dt UT_FOLLOWUP_STATUS);
	run;

/* merge dataset CMR with dataset Person_SLC*/

	proc sort data=CMR;
by stateno;
run;

proc sort data=Person_SLC;
by stateno;
run;

data SLC_NIC;
	merge CMR (in=A) person_SLC(in=B);
	by stateno;
	if A=1;
run;

/*keep only alive positive SL CO. residents*/

data SLC_NIC1;
	set SLC_NIC;
	where cur_county_name = &LHD and vital_status ne '2' and hiv_categ = '1';
	run;
/*data check*/
	proc freq data=SLC_NIC1;
	table vital_status hiv_categ cur_county_name;
	run;

/* Delete everyone with a positive lab within the last 18 months and reformatting test variables*/
data DNA;
set SLC_NIC1;

format dna_recent mmddyy10.;
dna_yr = substr(dna_recent_dt, 1, 4); 
dna_mo = substr(dna_recent_dt, 5, 2);
dna_dy = substr(dna_recent_dt, 7, 2);

if dna_yr = '' or dna_yr = '..' then dna_yr = '9999'; /*adding arbritry dates to blanks*/
if dna_mo = '' or dna_mo = '..' then dna_mo = '01';
if dna_dy = '' or dna_dy = '..' then dna_dy = '01';


dna_recent = mdy(dna_mo, dna_dy, dna_yr);
if dna_recent > &start_dt then delete; /*Deletes all events with labs dates within the last 18 months*/
run;

data cd4cnt;
	set SLC_NIC1;
format cd4cnt_recent mmddyy10.;
cd4cnt_yr = substr(cd4_recent_cnt_dt, 1, 4); 
cd4cnt_mo = substr(cd4_recent_cnt_dt, 5, 2);
cd4cnt_dy = substr(cd4_recent_cnt_dt, 7, 2);

if cd4cnt_yr = '' or cd4cnt_yr = '..' then cd4cnt_yr = '9999'; /*adding arbritry dates to blanks*/
if cd4cnt_mo = '' or cd4cnt_mo = '..' then cd4cnt_mo = '01';
if cd4cnt_dy = '' or cd4cnt_dy = '..' then cd4cnt_dy = '01';


cd4cnt_recent = mdy(cd4cnt_mo, cd4cnt_dy, cd4cnt_yr);
if cd4cnt_recent > &start_dt then delete; /*Deletes all events with labs dates within the last 18 months*/
run;

data cd4pct;
	set SLC_NIC1;
format cd4pct_recent mmddyy10.;
cd4pct_yr = substr(cd4_recent_cnt_pct_dt, 1, 4); 
cd4pct_mo = substr(cd4_recent_cnt_pct_dt, 5, 2);
cd4pct_dy = substr(cd4_recent_cnt_pct_dt, 7, 2);

if cd4pct_yr = '' or cd4pct_yr = '..' then cd4pct_yr = '9999'; /*adding arbritry dates to blanks*/
if cd4pct_mo = '' or cd4pct_mo = '..' then cd4pct_mo = '01';
if cd4pct_dy = '' or cd4pct_dy = '..' then cd4pct_dy = '01';


cd4pct_recent = mdy(cd4pct_mo, cd4pct_dy, cd4pct_yr);
if cd4pct_recent > &start_dt then delete; /*Deletes all events with labs dates within the last 18 months*/
run;

data VL;
	set SLC_NIC1;
format vl_recent mmddyy10.;
vl_yr = substr(vl_recent_dt, 1, 4); 
vl_mo = substr(vl_recent_dt, 5, 2);
vl_dy = substr(vl_recent_dt, 7, 2);

if vl_yr = '' or vl_yr = '..' then vl_yr = '9999'; /*adding arbritry dates to blanks*/
if vl_mo = '' or vl_mo = '..' then vl_mo = '01';
if vl_dy = '' or vl_dy = '..' then vl_dy = '01';


vl_recent = mdy(vl_mo, vl_dy, vl_yr);
if vl_recent > &start_dt then delete; /*Deletes all events with labs dates within the last 18 months*/

run;

/*Combine all the datasets with the newly formatted test dates and deduplicate any CMR's*/

data SLC_NIC_Final;
	set Cd4cnt Cd4pct DNA VL;
	run;
proc sort data = SLC_NIC_Final nodupkey;
		by patient_record_number;
		run;
	
proc print data=SLC_NIC_Final;
var hiv_categ patient_record_number stateno first_name middle_name last_name; 
title 'SLCO Residents Not In Care Past 18 Months';
run;

/*checking to make sure no test dates are within the 18 month mark.  Also checking for duplicate CMR's*/

proc freq data=SLC_NIC_Final;
tables cd4cnt_recent cd4pct_recent dna_recent vl_recent;
run;

proc freq data=SLC_NIC_Final;
	tables patient_record_number;
	run;
