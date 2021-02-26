options mprint mlogic   ;


%Macro H_Count(db /*Insert database name with full library name */
				,Studyid=Studyid /*Default value is STUDYID,but  you can add any value*/)  ;  

%Global H_Count;
	Proc sql;
		Select Count(*) as Count into:H_Count  From ( Select Distinct &Studyid From &db);
	
	Run;
%Put &H_Count;
%Mend ;
*%H_Count(pac.A01_REG);



%Macro H_Univariate(db /*Insert database name with full library name */
				,Var /*Add numerci variable to get the univariate analysis and histogram */) ; 
	Proc univariate data=&db;
	var  &Var;
	histogram;
	Run;
%mend;

%Macro H_CountStudyID( data,studyid=studyid);
%global VarCount;
Title "Dataset : &data ";
	proc sql;
	select count(*) into:VarCount  from ( select distinct &studyid from &data);
	run;

	%Put &VarCount;
%mend;

%Macro H_Missing_To_Zero( data );
Data &data;
Set &data;
array field _numeric_;
Do over field;
if field=. then field=0;
end;
run;
%Mend;


%Macro H_Missingvar_To_Zero( data, vars );
%put %sysfunc(count(&vars,' '));
%put %sysfunc(count(&vars,' '));
Data &data;
Set &data;
array field &vars;
Do over field;
if field=. then field=0;
end;
run;
%Mend;
%Macro H_Show_MacroVariables   ;
Proc sql ;
Create table ___H_Show_MacroVariables as 
Select name  from dictionary.macros
	where scope="GLOBAL"
	and name not like 'GRAPH%'
	and name not Like '%SYS'
	and name not Like '%SAS'
	and name not Like '%SQL'
	and name not Like '%SYS_'
	and name ne 'SASWORKLOCATION'
	and Upcase(substr(name,1,7)) ne "SYS_SQL"
	and name not like '/_%' escape '/';
Run;
%Put _User;
%Mend;


%Macro H_Show_TablesName(Libname= /*Name of library you would like to extract tablenames , the results strors into work.___H_Show_TablesName*/)   ;
proc sql;
Create table ___H_Show_TablesName as 
Select memname as TableName  from sashelp.vstabvw 
where upcase(libname)= upcase("&Libname")  ;
run;
%Mend;
*%H_Show_TablesName(Libname=work);

%Macro H_Sort(db /*Insert database name with full library name */
				,vars /*Add the list of variable with space seperated */, out=&db)  ; 

Proc sort data=&db out=&out;
by &vars;
run;
%Mend;
*%H_Sort(t,STUDYID,out=t2);
%Macro H_First(data,out,PartitionBy=) ;
data &out;
set &data;
proc sort;
by &PartitionBy;
run;
%put %sysfunc(count(&PartitionBy,' '));
%Let Firstpart=%sysfunc(scan(&PartitionBy,1,' '));
%put &Firstpart;

data &out;
set  &out;

by &PartitionBy  ;
if first.&Firstpart ;
run;
%Mend;
*%H_Data_First(_A00_PHARM ,_t,PartitionBy=  quantity  );

*%H_Data_Last(pac.B301_MOCK_PHARM,studyid,__tmp3);

%Macro H_ODS_Freqs(data=, Vars=, out=);
proc freq data=&data;
table 
&Vars/missing;
ods output OneWayFreqs=_Freq;
run;

proc contents data=_Freq out=_tmp;
run;
proc sql; select NAME into :Fieldnames SEPARATED by "," From _tmp where substr(NAME ,1 ,2)="F_";QUIT;

%Put &Fieldnames;
DATA &out(keep=Variable _level_ Frequency CumFrequency Percent);
set _Freq;
Variable=left(Trim(Tranwrd(Table,"Table ","")));
_level_=left(cat(&Fieldnames));
Run;
proc delete data=_tmp;run;
proc delete data=_Freq;run;
%mend;
*%H_ODS_Freqs(data=_tmp01_Char_AsthmaRemission,vars=&VariableToFreqs,out=pac.TbR021_ch1_Remission);



%Macro DeleteTable(tabels=) ;

%Put %sysfunc(countw(&tabels," " ));
%DO d = 1 %to %sysfunc(countw(&tabels," " ));
%Put %sysfunc(scan(&tabels, &d));
proc delete data=%sysfunc(scan(&tabels, &d));
RUN;
%END;
%Mend;



%**************                    macro to calculate means of variables             ***********************;


%macro TransMeansResult(data=,vars=,out=);

proc means data=&data noprint;
var
&vars;

output	out=vars_n 			(drop=_FREQ_	_TYPE_)	n=;
output	out=vars_mean 		(drop=_FREQ_	_TYPE_)	mean=;
output	out=vars_std 		(drop=_FREQ_	_TYPE_)	std=;
output	out=vars_min 		(drop=_FREQ_	_TYPE_)	min=;
output	out=vars_p5 		(drop=_FREQ_	_TYPE_)	p5=;
output	out=vars_q1 		(drop=_FREQ_	_TYPE_)	q1=;
output	out=vars_median 	(drop=_FREQ_	_TYPE_)	median=;
output	out=vars_q3 		(drop=_FREQ_	_TYPE_)	q3=;
output	out=vars_p95 		(drop=_FREQ_	_TYPE_)	p95=;
output	out=vars_max 		(drop=_FREQ_	_TYPE_)	max=;
output	out=vars_Sum 		(drop=_FREQ_	_TYPE_)	Sum=;
run;

%H_Show_TablesName(Libname=work);

data _null_ ;
set ___H_Show_TablesName end=eof;
do i=1 to _n_;
Stt=scan(TableName,2,"_");
call symputx('Tbname'|| left(_n_),TableName,'G');
call symputx('Stt'|| left(_n_),Stt,'G');
if eof then call symputx('no' ,_n_,'G');
end;
where substr(TableName,1,5) ="VARS_";
run;
%put &Tbname1 &Stt1;

proc sql;
select cat('t',TableName) into: allTables separated by " "  from ___H_Show_TablesName 
where substr(TableName,1,5) ="VARS_";
run;
%Do i =1 %to &no;

proc transpose data=&&Tbname&i. out=t&&Tbname&i.;
var &vars;
run;

data t&&Tbname&i.;
set t&&Tbname&i.;
rename COL1=&&Stt&i.;
proc sort ;
by _NAME_;
run;

%Put &&Tbname&i.;
Proc delete data=work.&&Tbname&i.;
run;
%end;

data &out;
merge   &allTables;
by _NAME_;
run; 
%DeleteTable(tabels=&allTables);
quit;
%Mend;


*%TransMeansResult(data=_A20_COHORT,vars= Log_PY AgeOfABATACEPT MSP_count_inYear,out=_testMean);





************************   PACI Macros  ***************************************;

%macro H_age_integer (agevar=,dob=,eventdate=)  ;
&agevar.=floor((intck('month',&dob.,&eventdate.)-(day(&eventdate.)<day(&dob.)))/12);
%mend H_age_integer;

%macro H_age_continuous (agevar=,dob=,eventdate=)  ;
	m_age_int = floor((intck('month',&dob.,&eventdate.)-(day(&eventdate.)<day(&dob.)))/12);
	m_prior_bday_correction = (month(&dob.)eq 2)*(day(&dob.)eq 29)*
	(put(MDY(12,31,(year(&dob.)+m_age_int)),JULDAY.)eq "365");
	m_prior_bday = MDY(month(&dob.+m_prior_bday_correction),
	day (&dob.+m_prior_bday_correction),
	year (&dob.)+m_age_int);
	m_next_bday_correction = (month(&dob.)eq 2)*(day(&dob.)eq 29)*
	(put(MDY(12,31,(year(&dob.)+m_age_int+1)),JULDAY.)eq "365");
	m_next_bday = MDY(month(&dob.+m_next_bday_correction),
	day (&dob.+m_next_bday_correction),
	year (&dob.)+m_age_int+1);
	&agevar. = m_age_int+((&eventdate.-m_prior_bday)/(m_next_bday-m_prior_bday));
	drop m_age_int m_prior_bday_correction m_prior_bday m_next_bday_correction m_next_bday;
%mend H_age_continuous;



proc format;
value Agegroup 	0="Under 15"
				1= "15-19"
				2= "20-25"
				3= "26-35"
				4= "36-45"
				5= "46-55"
				6= "56-65"
				7= "More than 65";

value YesNo    1="YES"
			   0="No";
run;



%Macro H_Contents(data, out);
Proc contents data=&data out=_Out_master noprint;
run;
proc  sort ;
by varnum;
run;

data &out(keep=NAME varnum FORMAT);
set _Out_master;
run;
%mend;

%Macro RandomeDate(min, max, RandomVar=RandDate);
range=max-min+1;
&RandomVar=mindate+int(ranuni(1234)*range);
format &RandomVar yymmdd10.;
%Mend;

%macro RandBetween(min, max);
(&min+Floor((1+&max-&min)*rand("uniform")));
%Mend;
* 2017-08-22 Mean function adjusted to accept the "WHERE" statemant ;


%macro H_Mean(data=,vars=,out=, Where=NOTHING);

proc means data=&data noprint;
var
&vars;
output	out=vars_n 			(drop=_FREQ_	_TYPE_)	n=;
output	out=vars_mean 		(drop=_FREQ_	_TYPE_)	mean=;
output	out=vars_std 		(drop=_FREQ_	_TYPE_)	std=;
output	out=vars_min 		(drop=_FREQ_	_TYPE_)	min=;
output	out=vars_p5 		(drop=_FREQ_	_TYPE_)	p5=;
output	out=vars_q1 		(drop=_FREQ_	_TYPE_)	q1=;
output	out=vars_median 	(drop=_FREQ_	_TYPE_)	median=;
output	out=vars_q3 		(drop=_FREQ_	_TYPE_)	q3=;
output	out=vars_p95 		(drop=_FREQ_	_TYPE_)	p95=;
output	out=vars_max 		(drop=_FREQ_	_TYPE_)	max=;
output	out=vars_Sum 		(drop=_FREQ_	_TYPE_)	Sum=;
%If &Where=NOTHING %then %do;
%Put NOTHING;
%end;
%else %do;
%Put &where;
Where &where;
%end;
run;




%H_Show_TablesName(Libname=work);

data _null_ ;
set ___H_Show_TablesName end=eof;
do i=1 to _n_;
Stt=scan(TableName,2,"_");
call symputx('Tbname'|| left(_n_),TableName,'G');
call symputx('Stt'|| left(_n_),Stt,'G');
if eof then call symputx('no' ,_n_,'G');
end;
where substr(TableName,1,5) ="VARS_";
run;
%put &Tbname1 &Stt1;

proc sql;
select cat('t',TableName) into: allTables separated by " "  from ___H_Show_TablesName 
where substr(TableName,1,5) ="VARS_";
run;
%Do i =1 %to &no;

proc transpose data=&&Tbname&i. out=t&&Tbname&i.;
var &vars;
run;

data t&&Tbname&i.;
set t&&Tbname&i.;
rename COL1=&&Stt&i.;
proc sort ;
by _NAME_;
run;

%Put &&Tbname&i.;
Proc delete data=work.&&Tbname&i.;
run;
%end;

data &out;
merge   &allTables;
by _NAME_;
run; 
%DeleteTable(tabels=&allTables);
quit;
%Mend;


%Macro H_Under5_finder(data, Ncolumns);
Data &data ;
set &data;
targetcolumn=&Ncolumns;
run;

Data &data;
set &data;
array field _numeric_;
Do over field;
if targetcolumn<=5 then field=999999999;
end;
drop targetcolumn;
run;
%Mend;
%put "HMacros ran successfully"