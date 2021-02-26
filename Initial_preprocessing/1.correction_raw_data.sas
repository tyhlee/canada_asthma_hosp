libname tyl "C:\Users\CIHI_USER\Documents\TYL_CIHI\data";

data tmp;
set tyl.cihi_diagnosis;
run;

proc contents data=tmp;
run;

data tyl.cihi_diagnosis_simplified;
set tmp;
row_num = _n_;
if missing(diag1) then ICD_type =10; 
else if missing(diagx1) then ICD_type=9;
else ICD_type=' '; 
run;

data tyl.cihi_diagnosis_no_diag;
set tyl.cihi_diagnosis_simplified(drop=diag:);
run;

data tyl.cihi_diagnosis_no_diag;
set tyl.cihi_diagnosis_no_diag(drop=TYPE:);
run;

proc export data = tyl.cihi_diagnosis_no_diag
outfile = "C:\Users\CIHI_USER\Documents\TYL_CIHI\data\cihi_no_diag.csv"
dbms=csv
replace;
run;

data tyl.cihi_diagnosis_diag_code_type;
set tyl.cihi_diagnosis_simplified(keep=row_num studyid ICD_type asthma copd TYPE: diag:);
run;

data tyl.cihi_diagnosis_code_condensed;
	set tyl.cihi_diagnosis_diag_code_type;
	if ICD_type=9 then 
		do; 
			primary_diag = diag1;
			secondary_diag = catx(';',of diag2-diag25);
			primary_diag_type = Type_ICD9_1;
			secondary_diag_type = catx(';',of Type_ICD9_2--Type_ICD9_25);
		end;
	else 
		do;
			primary_diag = diagx1;
			secondary_diag = catx(';',of diagx2--diagx35);
			primary_diag_type = Type_ICD10_1;
			secondary_diag_type = catx(';',of Type_ICD10_2--Type_ICD10_35);
		end;
	keep row_num studyid ICD_type asthma COPD primary_diag secondary_diag primary_diag_type secondary_diag_type;
run;

proc export data = tyl.cihi_diagnosis_code_condensed
outfile = "C:\Users\CIHI_USER\Documents\TYL_CIHI\data\cihi_diag_code_condensed.csv"
dbms=csv
replace;
run;

libname tyl "C:\Users\CIHI_USER\Documents\TYL_CIHI\data";

/* read the data */
data tmp;
set tyl.cihi_diagnosis_no_diag;
run;

/* make a safe copy of the current version */
proc export data = tmp
outfile = "C:\Users\CIHI_USER\Documents\TYL_CIHI\data\cihi_no_diag_oct.csv"
dbms=csv
replace;
run;


/* fix age */
/* all age in years*/
data tmp_age;
	set tmp;
	if (AGE_CD = 'Y' | AGE_CD = 'E') then
		do; /* years (E= estimated) */
			age = AGE_NUM;
		end;
	else if AGE_CD ='M' then
		do; /* months */
			age = AGE_NUM/12;
		end;
	else if AGE_CD ='D' then
		do; /* days */
			age = AGE_NUM/365.25;
		end;
	else if AGE_CD ='U' then
		do; /* unknown */
			age=.;
		end;
	else /* still briths */
		do;
			age = 0;
		end;
run;

/* export the data */
proc export data = tmp_age
outfile = "C:\Users\CIHI_USER\Documents\TYL_CIHI\data\cihi_no_diag_nov.csv"
dbms=csv
replace;
run;
