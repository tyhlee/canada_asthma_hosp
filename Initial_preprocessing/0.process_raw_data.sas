

%include "C:\Users\CIHI_USER\Documents\H.CIHI\Codes\_HMacros_V05.sas";

libname   D01_94  "C:\CIHI\DSS_DataRequestBPM6218_Data_94_00";
libname   D02_03  "C:\CIHI\DSS_DataRequestBPM6218_Data_icd9_01_03";
libname   D03_17  "C:\CIHI\DSS_DataRequestBPM6218_Data_icd10_01_17";


libname   Q01_00  "C:\CIHI\DSS_DataRequestBPM6859_Quebec_1994_2000_no PW";
libname   Q02_17  "C:\CIHI\DSS_DataRequestBPM6859_Quebec_2001_2017_no PW";
libname   h01  "C:\CIHI\DSS_DataRequestBPM6218_Data_hmdb_01_03";

libname ab "C:\Users\CIHI_USER\Documents\H.CIHI\data";



/*part 1*/
%h_contents(D01_94.FINAL_HMDB_DAD_1994,_1994);
%h_contents(D01_94.FINAL_HMDB_DAD_1996,_1996);
%h_contents(D01_94.FINAL_HMDB_DAD_2000,_2000);
data ab.D01_2000;
set 
D01_94.FINAL_HMDB_DAD_1994
D01_94.FINAL_HMDB_DAD_1995
D01_94.FINAL_HMDB_DAD_1996
D01_94.FINAL_HMDB_DAD_1997
D01_94.FINAL_HMDB_DAD_1998
D01_94.FINAL_HMDB_DAD_1999
D01_94.FINAL_HMDB_DAD_2000;
run;



/*part 2*/
%h_contents(h01.final_hmdb_2001,_h2001);
%h_contents(h01.final_hmdb_2002,_h2002);
%h_contents(h01.final_hmdb_2003,_h2003);
data ab.hmd01_2003;
set 
h01.final_hmdb_2001
h01.final_hmdb_2002
h01.final_hmdb_2003;
run;



/*part 3*/
%h_contents(D02_03.final_icd9_2001,_D2001);
%h_contents(D02_03.final_icd9_2002,_D2002);
%h_contents(D02_03.final_icd9_2003,_D2003);
data ab.D02_2003;
set 
D02_03.final_icd9_2001
D02_03.final_icd9_2002
D02_03.final_icd9_2003;
run;

/*part 4*/

%h_contents(D03_17.final_icd10_2001,_D2001);
%h_contents(D03_17.final_icd10_2009,_D2009);
%h_contents(D03_17.final_icd10_2017,_D2017);

data ab.D03_2017;
set 
D03_17.final_icd10_2001
D03_17.final_icd10_2002
D03_17.final_icd10_2003
D03_17.final_icd10_2004
D03_17.final_icd10_2005
D03_17.final_icd10_2006
D03_17.final_icd10_2007
D03_17.final_icd10_2008
D03_17.final_icd10_2009
D03_17.final_icd10_2010
D03_17.final_icd10_2011
D03_17.final_icd10_2012
D03_17.final_icd10_2013
D03_17.final_icd10_2014
D03_17.final_icd10_2015
D03_17.final_icd10_2016
D03_17.final_icd10_2017
;
run;



/*part 5 Quebec*/

%h_contents(Q01_00.final_qc_1994,_Q1994);
%h_contents(Q01_00.final_qc_1998,_Q1998);
%h_contents(Q01_00.final_qc_2000,_Q2000);

data ab.Q01_2000;
set 
Q01_00.final_qc_1994
Q01_00.final_qc_1995
Q01_00.final_qc_1996
Q01_00.final_qc_1997
Q01_00.final_qc_1998
Q01_00.final_qc_1999
Q01_00.final_qc_2000
;
run;


/*part 6 Quebec 2001-2017*/

%h_contents(Q02_17.final_qc_2001,_Q2001);
%h_contents(Q02_17.final_qc_2009,_Q2009);
%h_contents(Q02_17.final_qc_2017,_Q2017);

data ab.Q02_2005;
set 
Q02_17.final_qc_2001
Q02_17.final_qc_2002
Q02_17.final_qc_2003
Q02_17.final_qc_2004
Q02_17.final_qc_2005
 
;
run;
 
data ab.Q02_2017;
set 

Q02_17.final_qc_2006
Q02_17.final_qc_2007
Q02_17.final_qc_2008
Q02_17.final_qc_2009
Q02_17.final_qc_2010
Q02_17.final_qc_2011
Q02_17.final_qc_2012
Q02_17.final_qc_2013
Q02_17.final_qc_2014
Q02_17.final_qc_2015
Q02_17.final_qc_2016
Q02_17.final_qc_2017
;
run;												


data D01_2000;
length case_id MBUN 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE   LOS_NUM 8. total_los_days total_scu_los_hours 8. Sex $1. Age 8. AGE_CD $1. ;
 
set ab.D01_2000;
rename MBUN=studyid;
rename Diag_Cd_1-Diag_Cd_16=diag1-diag16;
rename Diag_Type_Cd_1- Diag_Type_Cd_16=Type_ICD9_1-Type_ICD9_16;
ADDATE=MDY(input(substr(ADMISSION_DT,5,2),8.),input(substr(ADMISSION_DT,7,2),8.),input(substr(ADMISSION_DT,1,4),8.));
SEPDATE=MDY(input(substr(Separation_Dt,5,2),8.),input(substr(Separation_Dt,7,2),8.),input(substr(Separation_Dt,1,4),8.));

if AGE_CD='Y' then BirthYear=year(ADDATE)-AGE_NUM;
else BirthYear=year(ADDATE-AGE_NUM*30);

if AGE_CD='Y' then Age=round(AGE_NUM,1);
else Age=round(AGE_NUM/12,1);

if SEX_CD=1 then Sex='M';
else if SEX_CD=2 then Sex='F';
else Sex='U';

If PROVINCE_CD ="10" Then Province="NF";
Else If PROVINCE_CD ="11" Then Province="PE";
Else If PROVINCE_CD ="12" Then Province="NS";
Else If PROVINCE_CD ="13" Then Province="NB";
Else If PROVINCE_CD ="24" Then Province="QC";
Else If PROVINCE_CD ="35" Then Province="ON";
Else If PROVINCE_CD ="46" Then Province="MA";
Else If PROVINCE_CD ="47" Then Province="Sk";
Else If PROVINCE_CD ="48" Then Province="AB";
Else If PROVINCE_CD ="59" Then Province="BC";
Else If PROVINCE_CD ="A " Then Province="TR";



Format ADDATE SEPDATE yymmdd10.;
run;


%h_count(D01_2000)

* DAD 2003 ;



data D02_2003_0;
length case_id MBUN 8.  Province $2. 	fiscal_year 8. BirthYear      total_los_days  total_scu_los_hours 8.   gender_code $1. Age 8.   ;
 
set ab.D02_2003;
rename MBUN=studyid;
rename diag_code_1-diag_code_25=diag1-diag25;
rename diag_type_1- diag_type_25=Type_ICD9_1-Type_ICD9_25;
  
rename gender_code=Sex;
rename admission_date =ADDATE;
rename discharge_date=SEPDATE;
rename age_code=AGE_CD;
rename age_units=AGE_NUM;
rename submitting_prov_code=PROVINCE_CD;


if age_code='Y' then BirthYear=year(admission_date)-age_units;
else BirthYear=year(admission_date-age_units*30);

if age_code='Y' then Age=round(age_units,1);
else Age=round(age_units/12,1);

If submitting_prov_code ="10" Then Province="NF";
Else If submitting_prov_code ="1" Then Province="PE";
Else If submitting_prov_code ="2" Then Province="NS";
Else If submitting_prov_code ="3" Then Province="NB";
Else If submitting_prov_code ="4" Then Province="QC";
Else If submitting_prov_code ="5" Then Province="ON";
Else If submitting_prov_code ="6" Then Province="MA";
Else If submitting_prov_code ="7" Then Province="Sk";
Else If submitting_prov_code ="8" Then Province="AB";
Else If submitting_prov_code ="9" Then Province="BC";
Else If submitting_prov_code ="A " Then Province="TR";

 
Format admission_date discharge_date yymmdd10.;
run;

data D02_2003;
length case_id studyid 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE   LOS_NUM 8. LOS_NUM total_los_days total_scu_los_hours 8.  Sex $1. Age 8. AGE_CD $1. ;
set  D02_2003_0;
run;



* DAD D03_2017 ;

data D03_2017_0;
length case_id MBUN 8.  Province $2. 	fiscal_year 8. BirthYear      total_los_days  total_scu_los_hours 8.   gender_code $1. Age 8.   ;
 
set ab.D03_2017;
rename MBUN=studyid;
rename diag_code_1-diag_code_25=diagx1-diagx25;
rename diag_type_1- diag_type_25=Type_ICD10_1-Type_ICD10_25;
  
rename gender_code=Sex;
rename admission_date =ADDATE;
rename discharge_date=SEPDATE;
rename age_code=AGE_CD;
rename age_units=AGE_NUM;
rename submitting_prov_code=PROVINCE_CD;


if age_code='Y' then BirthYear=year(admission_date)-age_units;
else BirthYear=year(admission_date-age_units*30);

if age_code='Y' then Age=round(age_units,1);
else Age=round(age_units/12,1);

If submitting_prov_code ="0" Then Province="NF";
Else If submitting_prov_code ="1" Then Province="PE";
Else If submitting_prov_code ="2" Then Province="NS";
Else If submitting_prov_code ="3" Then Province="NB";
Else If submitting_prov_code ="4" Then Province="QC";
Else If submitting_prov_code ="5" Then Province="ON";
Else If submitting_prov_code ="6" Then Province="MA";
Else If submitting_prov_code ="7" Then Province="Sk";
Else If submitting_prov_code ="8" Then Province="AB";
Else If submitting_prov_code ="9" Then Province="BC";
Else If submitting_prov_code ="A" Then Province="TR";

 
Format admission_date discharge_date yymmdd10.;
run;

data D03_2017;
length case_id studyid 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE   LOS_NUM 8. LOS_NUM total_los_days total_scu_los_hours 8.  Sex $1. Age 8. AGE_CD $1. ;
set  D03_2017_0;
run;


/*Q01_2000 */



data Q01_2000;
length case_id MBUN 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE   LOS_NUM 8. total_los_days total_scu_los_hours 8. Sex $1. Age 8. AGE_CD $1. ;
 
set ab.Q01_2000;
rename MBUN=studyid;
rename Diag_Cd_1-Diag_Cd_16=diag1-diag16;
rename Diag_Type_Cd_1- Diag_Type_Cd_16=Type_ICD9_1-Type_ICD9_16;
ADDATE=MDY(input(substr(ADMISSION_DT,5,2),8.),input(substr(ADMISSION_DT,7,2),8.),input(substr(ADMISSION_DT,1,4),8.));
SEPDATE=MDY(input(substr(Separation_Dt,5,2),8.),input(substr(Separation_Dt,7,2),8.),input(substr(Separation_Dt,1,4),8.));

if AGE_CD='Y' then BirthYear=year(ADDATE)-AGE_NUM;
else BirthYear=year(ADDATE-AGE_NUM*30);

if AGE_CD='Y' then Age=round(AGE_NUM,1);
else Age=round(AGE_NUM/12,1);

if SEX_CD=1 then Sex='M';
else if SEX_CD=2 then Sex='F';
else Sex='U';

If PROVINCE_CD ="10" Then Province="NF";
Else If PROVINCE_CD ="11" Then Province="PE";
Else If PROVINCE_CD ="12" Then Province="NS";
Else If PROVINCE_CD ="13" Then Province="NB";
Else If PROVINCE_CD ="24" Then Province="QC";
Else If PROVINCE_CD ="35" Then Province="ON";
Else If PROVINCE_CD ="46" Then Province="MA";
Else If PROVINCE_CD ="47" Then Province="Sk";
Else If PROVINCE_CD ="48" Then Province="AB";
Else If PROVINCE_CD ="59" Then Province="BC";
Else If PROVINCE_CD ="A " Then Province="TR";



Format ADDATE SEPDATE yymmdd10.;
run;


* Q02_2005 ;

data Q02_2005_0;
length case_id MBUN 8.  Province $2. 	fiscal_year 8. BirthYear      total_los_days  total_scu_los_hours 8.   gender_code $1. Age 8.   ;
 
set ab.Q02_2005;
rename MBUN=studyid;
rename diag_code_1-diag_code_25=diag1-diag25;
rename diag_type_1-diag_type_25=Type_ICD9_1-Type_ICD9_25;

rename gender_code=Sex;
rename admission_date =ADDATE;
rename discharge_date=SEPDATE;
rename age_code=AGE_CD;
rename age_units=AGE_NUM;
rename submitting_prov_code=PROVINCE_CD;


if age_code='Y' then BirthYear=year(admission_date)-age_units;
else BirthYear=year(admission_date-age_units*30);

if age_code='Y' then Age=round(age_units,1);
else Age=round(age_units/12,1);

If submitting_prov_code ="0" Then Province="NF";
Else If submitting_prov_code ="1" Then Province="PE";
Else If submitting_prov_code ="2" Then Province="NS";
Else If submitting_prov_code ="3" Then Province="NB";
Else If submitting_prov_code ="4" Then Province="QC";
Else If submitting_prov_code ="5" Then Province="ON";
Else If submitting_prov_code ="6" Then Province="MA";
Else If submitting_prov_code ="7" Then Province="Sk";
Else If submitting_prov_code ="8" Then Province="AB";
Else If submitting_prov_code ="9" Then Province="BC";
Else If submitting_prov_code ="A" Then Province="TR";

 
Format admission_date discharge_date yymmdd10.;
run;

data Q02_2005;
length case_id studyid 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE   LOS_NUM 8. LOS_NUM total_los_days total_scu_los_hours 8.  Sex $1. Age 8. AGE_CD $1. ;
set  Q02_2005_0;
run;


 


* Q02_2017  ;

data Q02_2017_0;
length case_id MBUN 8.  Province $2. 	fiscal_year 8. BirthYear      total_los_days  total_scu_los_hours 8.   gender_code $1. Age 8.   ;
 
set ab.Q02_2017;
rename MBUN=studyid;
rename diag_code_1-diag_code_25=diagx1-diagx25;
rename diag_type_1-diag_type_25=Type_ICD10_1-Type_ICD10_25;
rename diag_code_26-diag_code_35=diagx26-diagx35;
rename diag_type_26-diag_type_35=Type_ICD10_26-Type_ICD10_35; 
rename gender_code=Sex;
rename admission_date =ADDATE;
rename discharge_date=SEPDATE;
rename age_code=AGE_CD;
rename age_units=AGE_NUM;
rename submitting_prov_code=PROVINCE_CD;


if age_code='Y' then BirthYear=year(admission_date)-age_units;
else BirthYear=year(admission_date-age_units*30);

if age_code='Y' then Age=round(age_units,1);
else Age=round(age_units/12,1);

If submitting_prov_code ="0" Then Province="NF";
Else If submitting_prov_code ="1" Then Province="PE";
Else If submitting_prov_code ="2" Then Province="NS";
Else If submitting_prov_code ="3" Then Province="NB";
Else If submitting_prov_code ="4" Then Province="QC";
Else If submitting_prov_code ="5" Then Province="ON";
Else If submitting_prov_code ="6" Then Province="MA";
Else If submitting_prov_code ="7" Then Province="Sk";
Else If submitting_prov_code ="8" Then Province="AB";
Else If submitting_prov_code ="9" Then Province="BC";
Else If submitting_prov_code ="A" Then Province="TR";

 
Format admission_date discharge_date yymmdd10.;
run;

data Q02_2017;
length case_id studyid 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE   LOS_NUM 8. LOS_NUM total_los_days total_scu_los_hours 8.  Sex $1. Age 8. AGE_CD $1. ;
set  Q02_2017_0;
run;



* HMDB ;



data HMD01_2003_0;
length case_id MBUN 8.  Province $2. 	fiscal_year 8. BirthYear      total_los_days  total_scu_los_hours 8.   gender_code $1. Age 8.   ;
 
set ab.HMD01_2003;
rename MBUN=studyid;
rename diag_code_1-diag_code_25=diag1-diag25;
rename diag_type_1- diag_type_25=Type_ICD9_1-Type_ICD9_25;
  
rename gender_code=Sex;
rename admission_date =ADDATE;
rename discharge_date=SEPDATE;
rename age_code=AGE_CD;
rename age_units=AGE_NUM;
rename submitting_prov_code=PROVINCE_CD;


if age_code='Y' then BirthYear=year(admission_date)-age_units;
else BirthYear=year(admission_date-age_units*30);

if age_code='Y' then Age=round(age_units,1);
else Age=round(age_units/12,1);

If submitting_prov_code ="10" Then Province="NF";
Else If submitting_prov_code ="1" Then Province="PE";
Else If submitting_prov_code ="2" Then Province="NS";
Else If submitting_prov_code ="3" Then Province="NB";
Else If submitting_prov_code ="4" Then Province="QC";
Else If submitting_prov_code ="5" Then Province="ON";
Else If submitting_prov_code ="6" Then Province="MA";
Else If submitting_prov_code ="7" Then Province="Sk";
Else If submitting_prov_code ="8" Then Province="AB";
Else If submitting_prov_code ="9" Then Province="BC";
Else If submitting_prov_code ="A " Then Province="TR";

 
Format admission_date discharge_date yymmdd10.;
run;

data HMD01_2003;
length case_id studyid 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE   LOS_NUM 8. LOS_NUM total_los_days total_scu_los_hours 8.  Sex $1. Age 8. AGE_CD $1. ;
set  HMD01_2003_0;
run;






Data CIHI;
set 
Q02_2017 ( in=a)
Q02_2005 ( in=b)
Q01_2000 ( in=c)
D03_2017 ( in=d)
D02_2003 ( in=e)
D01_2000 ( in=f)
HMD01_2003 ( in=g);
if a or b or c then Database='Quebec';
else if  d or e or f then Database='DAD   ';
else if  g then Database='HMDB  ';
run;


data ab.CIHI ( compress=yes);

length case_id 8. Pstudyid $12. studyid 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE   LOS_NUM 8. LOS_NUM total_los_days total_scu_los_hours 8. 
Sex $1. Age 8. AGE_CD $1.  AGE_NUM 8. 
diag1	diag2	diag3	diag4	diag5	diag6	diag7	diag8	diag9	diag10	diag11	diag12	diag13	diag14	diag15	diag16	diag17	diag18	diag19	diag20	diag21	diag22	diag23	diag24	diag25
 $7. 
diagx1	diagx2	diagx3	diagx4	diagx5	diagx6	diagx7	diagx8	diagx9	diagx10	diagx11	diagx12	diagx13	diagx14	diagx15	diagx16	diagx17	diagx18	diagx19	diagx20	diagx21	diagx22	diagx23	diagx24	diagx25
 $7.  
diagx26	diagx27	diagx28	diagx29	diagx30	diagx31	diagx32	diagx33	diagx34	diagx35
 $7. 
Type_ICD9_1	Type_ICD9_2	Type_ICD9_3	Type_ICD9_4	Type_ICD9_5	Type_ICD9_6	Type_ICD9_7	Type_ICD9_8	Type_ICD9_9	Type_ICD9_10	Type_ICD9_11	Type_ICD9_12	Type_ICD9_13	Type_ICD9_14	Type_ICD9_15	Type_ICD9_16	Type_ICD9_17	Type_ICD9_18	Type_ICD9_19	Type_ICD9_20	Type_ICD9_21	Type_ICD9_22	Type_ICD9_23	Type_ICD9_24	Type_ICD9_25
 $1. 
Type_ICD10_1	Type_ICD10_2	Type_ICD10_3	Type_ICD10_4	Type_ICD10_5	Type_ICD10_6	Type_ICD10_7	Type_ICD10_8	Type_ICD10_9	Type_ICD10_10	Type_ICD10_11	Type_ICD10_12	Type_ICD10_13	Type_ICD10_14	Type_ICD10_15	Type_ICD10_16	Type_ICD10_17	Type_ICD10_18	Type_ICD10_19	Type_ICD10_20	Type_ICD10_21	Type_ICD10_22	Type_ICD10_23	Type_ICD10_24	Type_ICD10_25
 $1. 
Type_ICD10_26	Type_ICD10_27	Type_ICD10_28	Type_ICD10_29	Type_ICD10_30	Type_ICD10_31	Type_ICD10_32	Type_ICD10_33	Type_ICD10_34	Type_ICD10_35
 $1.;
  
set  CIHI ;
Pstudyid=Province ||"_" || left(put(studyid, 8.));
run;
proc sort data=ab.CIHI;
by Pstudyid  ADDATE ;
run;

data ab.CIHI;
length case_id 8. Pstudyid $12. studyid 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE ADDYEAR  LOS_NUM 8. LOS_NUM total_los_days total_scu_los_hours 8. ;
set ab.CIHI;
ADDYEAR=year(ADDATE);
run;

data _missing ;
set ab.CIHI;
where studyid=.;
run;
%let lib=Q01_00;
%let data=FINAL_QC_;
%let i=1994;
%put &lib..&data.&i;
%macro L2017(lib,data);
%do i =2001 %to 2017;
Title &data.&i;
	proc sql;
	 
		select sum( case when MBUN=. then 1 else 0 end ) as missing, count(*) as count ,(calculated missing)/count(*) as Percentage
		from &lib..&data.&i;
		 
		run;
%end;
%mend;%L2017(Q02_17,FINAL_QC_);

data _missing ;
set ab.HMD01_2003;
where mbun=.;
run;


%macro L2017(lib,data);
%do i =2003 %to 2003;
Title &data.&i;
	proc sql;
	 
		select sum( case when MBUN=. then 1 else 0 end ) as missing, count(*) as count ,(calculated missing)/count(*) as Percentage
		from &lib..&data.&i;
		 
		run;
%end;
%mend;%L2017(ab,HMD01_);


proc sql;
Create table _Duplicated1 as 
select studyid, Province , count(*) as count from
ab.CIHI 
where studyid~=. /*and Province~='QC'*/
group by 
studyid, Province 
having count(*)>1
order by
studyid, province;

proc sql;
Create table _Duplicated2 as 
select studyid , count(*) as count from
_Duplicated1 
 group by 
studyid  
having count(*)>1
order by
studyid ;

proc sql;
Create table _Duplicated3 as 
select studyid, Province , count(*) as count from
_Duplicated1 
where studyid in ( select studyid from _Duplicated2);

data _count9;
set ab.CIHI;
array icd9 icd9_1-icd9_25;
array diag diag1-diag25;
do i=1 to 25;
if diag{i}~='' then icd9{i}=1;else icd9{i}=0;
end;



run;
proc freq data=_count9;
table icd9_1--icd9_25;
run;

data _count10;
set ab.CIHI;
array icd10 icd10_1-icd10_35;
array diagx diagx1-diagx35;
do i=1 to 35;
if diagx{i}~='' then icd10{i}=1;else icd10{i}=0;
end;
run;
 

proc freq data=_count10;
table icd10_1--icd10_35;
run;
 

/* General descriptive analysis */

%h_count(ab.CIHI, studyid=case_id);
%h_count(ab.CIHI, studyid=Pstudyid);
proc freq data=ab.CIHI;
table ADDYEAR Database sex Province;
run;
%h_univariate(ab.CIHI,age)
%h_univariate(ab.CIHI,LOS_NUM)


data ab.CIHI_Diagnosis;
length case_id 8. Pstudyid $12. studyid 8. PROVINCE_CD Province $2. 	fiscal_year 8. BirthYear ADDATE SEPDATE   Asthma COPD 8.
LOS_NUM 8. LOS_NUM total_los_days total_scu_los_hours 8. ;
set ab.CIHI;

if (substr(diag1,1,3)='493' and substr(diag1,1,4)~='4932' )  or upcase(substr(diagx1,1,3)) in ('J46','J45') then Asthma=1;
else Asthma=0;

if  substr(diag1,1,3) in ('491','492','496') or upcase(substr(diagx1,1,3)) in ('J41','J42', 'J43','J44')  or substr(diag1,1,4) ='4932' then COPD=1;
else COPD=0;
Run;

proc freq data=ab.CIHI_Diagnosis;
table Asthma COPD;
run;


/*Asthma Analysis*/
data _asthma;
set ab.CIHI_Diagnosis;
where asthma=1;
run;



%h_count(_asthma, studyid=case_id);
%h_count(_asthma, studyid=Pstudyid);
proc freq data=_asthma;
table ADDYEAR Database sex Province;
run;
%h_univariate(_asthma,age)
%h_univariate(_asthma,LOS_NUM)


/*COPD Analysis*/
data _COPD;
set ab.CIHI_Diagnosis;
where COPD=1;
run;



%h_count(_COPD, studyid=case_id);
%h_count(_COPD, studyid=Pstudyid);
proc freq data=_COPD;
table  Database sex Province ADDYEAR ;
 run;
%h_univariate(_COPD,age)
%h_univariate(_COPD,LOS_NUM)
 
