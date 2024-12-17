libname aa '/vol/userdata9/sta_room241';


%let main_sick1 = (substr(main_sick, 1,3) in ('F00','F01','F02','F03','F05','G30') or substr(main_sick,1,4) in ('G114','I278','I420','I731','M351','M353','F051','G311') or substr(main_sick,1,5) in ('G3181','G3188'));

%let main_sick2 = (substr(main_sick, 1,3) in ('F00','G30'));

%let main_sick3 = (substr(main_sick, 1,3) in ('F00','F01','F02','F03','G30') or substr(main_sick,1,4) in ('F051','G311'));


/*******************************************************************************/
/*** outcome code 받은 자들 중에 first_outcome_date가 2009,2010인 사람 제외***/
/*******************************************************************************/

%macro cohort(type, days, main_sick);
proc sql;
create table only_outcome as
select distinct jid, min(recu_fr_dd) as first_outcome_date
from aa.washout2
where &main_sick
group by jid;
quit; *;

/* only_outcome에서 2009, 2010인 사람 뽑 */
proc sql;
create table washout_year as
select distinct jid
from only_outcome
where (substr(first_outcome_date, 1, 4) in ('2009','2010')); quit; *;

/* 앞선 aa.washout2에서 only outcome이 2009,2010인 사람 제외 */
proc sql;
create table washout3_1 as
select *
from aa.washout2
where jid not in (select jid from washout_year); quit;


/* n수 체크 */
proc sort data=washout3_1 nodupkey out=washout3_1_id; by jid; run;  *;



/*** 앞서 두번의 washout 거친 진단 db에 약물 db 붙이기 ***/
proc sql;
create table diag_flu as
select a.*, b.div_cd, b.drug_date, b.drug_age
from washout3_1_id as a
left join aa.t300_t530_flu as b on a.jid=b.jid; quit;

/* 진단+약물 db에서 fludarabine(1601, 4778)에서 min_date 뽑기 */
proc sql;
create table only_flu as
select distinct jid, min(drug_date) as first_flu_date, min(drug_age) as first_flu_age
from diag_flu
where ( substr(div_cd,1,4) in ('1601','4778') )
group by jid; run;

data only_flu; set only_flu;
format first_flu_date yymmdd10.; run;

/* only_flu에서 first_flu_date가 2009, 2010인 사람 뽑 */
proc sql;
create table washout_year as
select distinct jid
from only_flu
where year(first_flu_date) in (2009,2010); quit; *;

/* 진단+약물 db에서 fisrt_flu_date가 2009,2010인 사람 제외 */
proc sql;
create table washout as
select *
from diag_flu
where jid not in (select jid from washout_year);
quit;


/* 진단+약물 db에 flu wash out 거친 것에 first_flu_date 붙이기 */
proc sql;
create table cohort_flu_1 as
select a.*, b.first_flu_date, b.first_flu_age
from washout as a
left join only_flu as b on a.jid=b.jid; quit;

/* n수 세기 */
proc sort data=cohort_flu_1 nodupkey out=cohort_flu_1_id; by jid; run; *;


/* exposure period 2yr 적용 */
proc sql;
create table exposure_2yr as
select distinct jid
from cohort_flu_1
where abs(first_c_date - first_flu_date)<=365.25; *365.25;
quit;

proc sql;
create table cohort_flu_2 as
select *
from cohort_flu_1
where jid in (select jid from exposure_2yr); quit;

/*n수 세기*/
proc sort data=cohort_flu_2 nodupkey out=cohort_flu_2_id; by jid; quit; *;


/* 약물 db에서 복용군 정의하기 */
proc sql;
create table drug_group as
select distinct jid,
case when (first_flu_date+&days. <= drug_date) then 1 end as drug_group
from cohort_flu_2
where ( substr(div_cd,1,4) in ('1601','4778') )
group by jid; quit;

proc sql;
create table cohort_flu2_1 as
select a.*, b.drug_group
from cohort_flu_2 as a
left join drug_group as b on a.jid=b.jid; quit;

proc sort data=cohort_flu2_1; by jid descending drug_group; run;
proc sort data=cohort_flu2_1 nodupkey out=cohort_flu2_1_id; by jid; run; *;

proc freq data= cohort_flu2_1_id; table drug_group; run; *drug_group=1  n=;


/* 진단 코호트에 가공한 약물 db, 사망 변수 붙이기 */
proc sql;
create table diag_med1 as
select a.jid, a.sex_tp_cd, a.first_c_date, a.first_c_age,  a.first_flu_date, a.first_flu_age, a.drug_group, 
b.last_dig_date, 
c.dgrslt_tp_cd_2
from cohort_flu2_1_id as a 
left join aa.last_dig_date as b on a.jid=b.jid
left join aa.dgrslt_tp_cd_2_id as c on a.jid=c.jid; quit;


data cohort_surv1; set diag_med1;
if last_dig_date < mdy(12,31,2021) or dgrslt_tp_cd_2 = 1 then death_yn=1; run;

data cohort_surv1; set cohort_surv1;
if death_yn=1 then death_date = last_dig_date;
format death_date yymmdd8.; run;




/* cci, outcome, etc 정의를 위해 t20 raw db에서 최종 screening db에 해당하는 jid의 all data 뽑 */
proc sql;
create table all_info_1 as
select jid, main_sick, sub_sick, recu_fr_dd, pat_age
from aa.t200_2024q3_02
where jid in (select jid from cohort_surv1); quit;

data all_info_1; set all_info_1;
dig_date = mdy(substr(recu_fr_dd,5,2), substr(recu_fr_dd,7,2), substr(recu_fr_dd,1,4)); format dig_date yymmdd10.; run;

/* aa.all_info_1에 first_flu_date, first_c_date, drug_group 붙이기 */
proc sql;
create table all_info_11 as
select a.*, b.first_flu_date, b.first_c_date, b.drug_group
from all_info_1 as a
left join cohort_surv1 as b on a.jid=b.jid; quit;


/* group에서 first_c_date 이전에 outcome code 있는 사람 뽑기 */
proc sql;
create table before_c_index1 as
select jid
from all_info_11
where (dig_date <= first_c_date) and &main_sick; quit;

/* n수 체크 */
proc sort data=before_c_index1 nodupkey out=before_c_index1_id; by jid; run;  *;

proc sql;
create table all_info_111 as
select *
from all_info_11
where jid not in (select distinct jid from before_c_index1); quit; *;

proc sql;
create table cohort_surv11 as
select *
from cohort_surv1
where jid not in (select distinct jid from before_c_index1); quit; *;

proc freq data= cohort_surv11; table drug_group; run; *drug_group=1  n=;


/****************** outcome과 cci 정의 *********************/

/******************/
/* drug_group일때 */
/******************/
proc sql;
create table drug_outcome as
select jid,
max(case when &main_sick  then 1 else . end) as dem_outcome
from all_info_111
where (dig_date >= first_c_date)
group by jid; quit;

/* first_dm_date 정의 */
proc sql;
create table drug_first as
select jid, min(dig_date) as first_dm_date, min(pat_age) as first_dm_age
from all_info_111
where (dig_date >= first_c_date) and &main_sick
group by jid; quit;

data drug_first; set drug_first;
format first_dm_date yymmdd10.; run;


proc sql;
create table drug_outcome_yes_1 as
select a.*, b.first_dm_date, b.first_dm_age
from drug_outcome as a 
left join drug_first as b on a.jid=b.jid; quit;


proc sql;
create table drug_cci as
select jid,

/*C8*/
max(case when ( substr(main_sick,1,3) in ('C81','C82','C83','C84','C85','C88') ) then 1 else 0 end) as c8_yn1,
sum(case when ( substr(sub_sick,1,3) in ('C81','C82','C83','C84','C85','C88') ) then 1 else 0 end) as c8_yn0,

/*C9*/
max(case when ( substr(main_sick,1,3) in ('C90','C91','C92','C93','C94','C95') ) then 1 else 0 end) as c9_yn1,
sum(case when ( substr(sub_sick,1,3) in ('C90','C91','C92','C93','C94','C95') ) then 1 else 0 end) as c9_yn0,

/*MI*/
max(case when (substr(main_sick,1,3) in ('I21','I22') or  substr(main_sick,1,4)='I252') then 1 else 0 end) as mi_yn1,
sum(case when (substr(sub_sick,1,3) in ('I21','I22') or  substr(sub_sick,1,4)='I252') then 1 else 0 end) as mi_yn0, 
/*CHF*/
max(case when (substr(main_sick,1,3) in ('I43','I50') or substr(main_sick,1,4) in ('I099','I110','I130','I132','I255','I420','I425','I426','I427','I428','I429','P290')) then 1 else 0 end) as chf_yn1,
sum(case when (substr(sub_sick,1,3) in ('I43','I50') or substr(sub_sick,1,4) in ('I099','I110','I130','I132','I255','I420','I425','I426','I427','I428','I429','P290')) then 1 else 0 end) as chf_yn0,
/*PVD*/
max(case when (substr(main_sick,1,3) in ('I70','I71') or  substr(main_sick,1,4) in ('I731','I738','I739','I771','I790','I792','K551','K558','K559','Z958','Z959')) then 1 else 0 end) as pvd_yn1,
sum(case when (substr(sub_sick,1,3) in ('I70','I71') or  substr(sub_sick,1,4) in ('I731','I738','I739','I771','I790','I792','K551','K558','K559','Z958','Z959')) then 1 else 0 end) as pvd_yn0,
/*CVD*/
max(case when (substr(main_sick,1,3) in ('G45','G46','I60','I61','I62','I63','I64','I65','I66','I67','I68','I69') or  substr(main_sick,1,4) ='H340') then 1 else 0 end) as cvd_yn1,
sum(case when (substr(sub_sick,1,3) in ('G45','G46','I60','I61','I62','I63','I64','I65','I66','I67','I68','I69') or  substr(sub_sick,1,4) ='H340') then 1 else 0 end) as cvd_yn0,
/*CPD*/
max(case when (substr(main_sick,1,3) in ('J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62','J63','J64','J65','J66','J67') or  substr(main_sick,1,4) in ('I278','I279','J684','J701','J703')) then 1 else 0 end) as cpd_yn1,
sum(case when (substr(sub_sick,1,3) in ('J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62','J63','J64','J65','J66','J67') or  substr(sub_sick,1,4) in ('I278','I279','J684','J701','J703')) then 1 else 0 end) as cpd_yn0,
/*Rheumatic disease*/
max(case when (substr(main_sick,1,3) in ('M05','M06','M32','M33','M34') or  substr(main_sick,1,4) in ('M315','M351','M353','M360')) then 1 else 0 end) as rhe_yn1,
sum(case when (substr(sub_sick,1,3) in ('M05','M06','M32','M33','M34') or  substr(sub_sick,1,4) in ('M315','M351','M353','M360')) then 1 else 0 end) as rhe_yn0,
/*PUD*/
max(case when (substr(main_sick,1,3) in ('K25','K26','K27','K28')) then 1 else 0 end) as pud_yn1,
sum(case when (substr(sub_sick,1,3) in ('K25','K26','K27','K28')) then 1 else 0 end) as pud_yn0,
/*MLD*/
max(case when (substr(main_sick,1,3) in ('B18','K73','K74') or  substr(main_sick,1,4) in ('K700','K701','K702','K703','K709','K713','K714','K715','K717','K760','K762','K763','K764','K768','K769','Z944')) then 1 else 0 end) as mld_yn1,
sum(case when (substr(sub_sick,1,3) in ('B18','K73','K74') or  substr(sub_sick,1,4) in ('K700','K701','K702','K703','K709','K713','K714','K715','K717','K760','K762','K763','K764','K768','K769','Z944')) then 1 else 0 end) as mld_yn0,
/*DWOC*/
max(case when (substr(main_sick,1,4) in ('E100','E101','E106','E108','E109','E110','E111','E116','E118','E119','E120','E121','E126','E128','E129','E130','E131','E136','E138','E139','E140','E141','E146','E148','E149')) then 1 else 0 end) as dwoc_yn1,
sum(case when (substr(sub_sick,1,4) in ('E100','E101','E106','E108','E109','E110','E111','E116','E118','E119','E120','E121','E126','E128','E129','E130','E131','E136','E138','E139','E140','E141','E146','E148','E149')) then 1 else 0 end) as dwoc_yn0,
/*DWCC*/
max(case when (substr(main_sick,1,4) in ('E102','E103','E104','E105','E107','E112','E113','E114','E115','E117','E122','E123','E124','E125','E127','E132','E133','E134','E135','E137','E142','E143','E144','E145','E147')) then 1 else 0 end) as dwcc_yn1,
sum(case when (substr(sub_sick,1,4) in ('E102','E103','E104','E105','E107','E112','E113','E114','E115','E117','E122','E123','E124','E125','E127','E132','E133','E134','E135','E137','E142','E143','E144','E145','E147')) then 1 else 0 end) as dwcc_yn0,
/*Hemiplegia or paraplegia*/
max(case when (substr(main_sick,1,3) in ('G81','G82') or  substr(main_sick,1,4) in ('G041','G114','G801','G802','G830','G831','G832','G833','G834','G839')) then 1 else 0 end) as hp_yn1,
sum(case when (substr(sub_sick,1,3) in ('G81','G82') or  substr(sub_sick,1,4) in ('G041','G114','G801','G802','G830','G831','G832','G833','G834','G839')) then 1 else 0 end) as hp_yn0,
/*Renal diseases*/
max(case when (substr(main_sick,1,3) in ('N18','N19') or  substr(main_sick,1,4) in ('I120','I131','N032','N033','N034','N035','N036','N037','N052','N053','N054','N055','N056','N057','N250','Z490','Z491','Z492','Z940','Z992')) then 1 else 0 end) as rd_yn1,
sum(case when (substr(sub_sick,1,3) in ('N18','N19') or  substr(sub_sick,1,4) in ('I120','I131','N032','N033','N034','N035','N036','N037','N052','N053','N054','N055','N056','N057','N250','Z490','Z491','Z492','Z940','Z992')) then 1 else 0 end) as rd_yn0,
/*Moderate severe liver disease*/
max(case when (substr(main_sick,1,4) in ('I850','I859','I864','I982','K704','K711','K721','K729','K765','K766','K767')) then 1 else 0 end) as sld_yn1,
sum(case when (substr(sub_sick,1,4) in ('I850','I859','I864','I982','K704','K711','K721','K729','K765','K766','K767')) then 1 else 0 end) as sld_yn0,
/*AIDS/HIV*/
max(case when (substr(main_sick,1,3) in ('B20','B21','B22','B24')) then 1 else 0 end) as aids_yn1,
sum(case when (substr(sub_sick,1,3) in ('B20','B21','B22','B24')) then 1 else 0 end) as aids_yn0

from all_info_111
where (first_c_date >= dig_date)
group by jid; quit;

data drug_cci_yes; set drug_cci;
if c8_yn1 =1 or c8_yn0 >=2 then c8_yes=1; else c8_yes=0;
if c9_yn1 =1 or c9_yn0 >=2 then c9_yes=1; else c9_yes=0;

if mi_yn1 =1 or mi_yn0 >=2 then mi_yes=1; else mi_yes=0;
if chf_yn1 =1 or chf_yn0 >=2 then chf_yes=1; else chf_yes=0;
if pvd_yn1 =1 or pvd_yn0 >=2 then pvd_yes=1; else pvd_yes=0;
if cvd_yn1 =1 or cvd_yn0 >=2 then cvd_yes=1; else cvd_yes=0;
if cpd_yn1 =1 or cpd_yn0 >=2 then cpd_yes=1; else cpd_yes=0;
if rhe_yn1 =1 or rhe_yn0 >=2 then rhe_yes=1; else rhe_yes=0;
if pud_yn1 =1 or pud_yn0 >=2 then pud_yes=1; else pud_yes=0;
if mld_yn1 =1 or mld_yn0 >=2 then mld_yes=1; else mld_yes=0;
if dwoc_yn1 =1 or dwoc_yn0 >=2 then dwoc_yes=1; else dwoc_yes=0;
if dwcc_yn1 =1 or dwcc_yn0 >=2 then dwcc_yes=1; else dwcc_yes=0;
if hp_yn1 =1 or hp_yn0 >=2 then hp_yes=1; else hp_yes=0;
if rd_yn1 =1 or rd_yn0 >=2 then rd_yes=1; else rd_yes=0;
if sld_yn1 =1 or sld_yn0 >=2 then sld_yes=1; else sld_yes=0;
if aids_yn1 =1 or aids_yn0 >=2 then aids_yes=1; else aids_yes=0;
keep jid c8_yes c9_yes mi_yes chf_yes pvd_yes cvd_yes cpd_yes rhe_yes pud_yes mld_yes dwoc_yes dwcc_yes hp_yes rd_yes sld_yes aids_yes; run;

data drug_cci_yes; set drug_cci_yes;
if mld_yes=1 or sld_yes=1 then ld_yes=1;
else ld_yes=0;
if dwoc_yes=1 or dwcc_yes=1 then diab_yes=1;
else diab_yes=0;
drop mld_yes sld_yes dwoc_yes dwcc_yes; run;


proc sql;
create table drug_diag as
select a.*, b.*, c.dem_outcome, c.first_dm_date, c.first_dm_age
from cohort_surv11 as a 
left join drug_cci_yes as b on a.jid=b.jid
left join drug_outcome_yes_1 as c on a.jid=c.jid; quit;



data aa.all_diag_&type._&days.; set drug_diag;
if death_yn=. then death_day = mdy(12,31,2022) - first_c_date;
else if death_yn=1 then death_day = death_date - first_c_date;
death_year = death_day/365.25;
run;



/*** landmark ***/
/* 1년 이내 죽은 사람 뽑 */
proc sql;
create table landmark_1yr as
select *
from aa.all_diag_&type._&days.
where (death_year <=1 and death_yn=1); quit;

/* 원래 db에서 1년 이내 죽은 사람 제외 */
proc sql;
create table subset as
select * 
from aa.all_diag_&type._&days. 
where jid not in (select jid from landmark_1yr); quit;

/* x축 당기기 위해 death_year에서 -1년 */
data subset; set subset;
death_year = death_year -1; run;

/* death_year에서 양수인 것만 select */
data aa.all_diag_&type._&days._land; set subset;
if death_year >=0; run;
%mend;


%cohort(type=1, days=15, main_sick=&main_sick1);
%cohort(type=1, days=30, main_sick=&main_sick1);
%cohort(type=1, days=60, main_sick=&main_sick1);

%cohort(type=2, days=15, main_sick=&main_sick2);
%cohort(type=2, days=30, main_sick=&main_sick2);
%cohort(type=2, days=60, main_sick=&main_sick2);

%cohort(type=3, days=15, main_sick=&main_sick3);
%cohort(type=3, days=30, main_sick=&main_sick3);
%cohort(type=3, days=60, main_sick=&main_sick3);


/*** landmark n year macro***/
%macro landmark(type, days, year);
/* n년 이내 죽은 사람 뽑 */
proc sql;
create table landmark_yr as
select *
from aa.all_diag_&type._&days.
where (death_year <=&year. and death_yn=1); quit;

/* 원래 db에서 n년 이내 죽은 사람 제외 */
proc sql;
create table subset as
select * 
from aa.all_diag_&type._&days. 
where jid not in (select jid from landmark_yr); quit;

/* x축 당기기 위해 death_year에서 -n년 */
data subset; set subset;
death_year = death_year - &year.; run;

/* death_year에서 양수인 것만 select */
data aa.all_diag_&type._&days._land&year.year; set subset;
if death_year >=0; run;
%mend;

%landmark(type=1, days=15, year=1);
%landmark(type=1, days=30, year=1);
%landmark(type=1, days=60, year=1);

%landmark(type=2, days=15, year=1);
%landmark(type=2, days=30, year=1);
%landmark(type=2, days=60, year=1);

%landmark(type=3, days=15, year=1);
%landmark(type=3, days=30, year=1);
%landmark(type=3, days=60, year=1);


/************************************************************************************************/
/* 아래는 검토를 위한 check */
/*2. drug_group=1일때 outcome_code있을때 각 jid의 min_date 보기*/
/*main part check*/
proc sql;
create table chk as
select distinct jid, min(dig_date) as date_check
from aa.cci2_1_60
where (substr(main_sick, 1,3) in ('C81','C82','C83','C84','C85','C88','C90','C91','C92','C93','C94','C95')) or
(substr(sub_sick, 1,3) in ('C81','C82','C83','C84','C85','C88','C90','C91','C92','C93','C94','C95'))
group by jid; run;
data chk; set chk;
format date_check yymmdd10.; run;
proc sql;
create table chk2 as
select jid, date_check
from chk
where year(date_check) in (2009,2010); quit;


