libname aa '/vol/userdata9/sta_room241';

/* outcome code type1 */

proc sort data= aa.t200_2024q3_02 nodupkey out=jid_num; by jid; run; *228,264;


/* ������ ������ ���� */
proc sql;
create table aa.last_dig_date as
select jid, max(recu_to_dd) as last_dig_date
from aa.t200_2024q3_02
group by jid; quit;
data aa.last_dig_date; set aa.last_dig_date;
last_dig_date_tmp = input(last_dig_date, yymmdd10.);
format last_dig_date_tmp yymmdd10.;
drop last_dig_date;
rename last_dig_date_tmp = last_dig_date; run;

/* ��� ���� */
/* ��� ���� ������ �̵� �ڿ��� ���ϰ��� */
data aa.dgrslt_tp_cd_2; set aa.t200_2024q3_02;
if dgrslt_tp_cd='4' then dgrslt_tp_cd_2=1;
keep jid dgrslt_tp_cd dgrslt_tp_cd_2; run;

proc sort data=aa.dgrslt_tp_cd_2; by jid descending dgrslt_tp_cd_2; run;
proc sort data=aa.dgrslt_tp_cd_2 nodupkey out=aa.dgrslt_tp_cd_2_id; by jid; run;  




/************************************************************/
/**************** Part I. cohort ����**************************/
/************************************************************/
/* �ֻ����� */
/* 1. C�ڵ� */
proc sql;
create table aa.only_c8c9 as
select distinct jid, min(recu_fr_dd) as first_c_date, min(pat_age) as first_c_age
from aa.t200_2024q3_02
where (substr(main_sick, 1,3) in ('C81','C82','C83','C84','C85','C88','C90','C91','C92','C93','C94','C95'))
group by jid;
quit; *201,591;

/* t20 raw db���� only_c8c9�� ���ϴ� �ֵ鸸 �� */
proc sql;
create table aa.main_sick as
select *
from aa.t200_2024q3_02
where jid in (select jid from aa.only_c8c9); quit; *;

/* n�� ���� */
proc sort data=aa.main_sick nodupkey out=aa.main_sick_id; by jid; quit; *201,591;

/* �ռ� �Ѵܰ� screening�� db�� first_date ���̱� */
proc sql;
create table aa.main_sick2 as
select a.*, b.first_c_date, b.first_c_age
from aa.main_sick as a
left join aa.only_c8c9 as b on a.jid=b.jid; quit;

/* 1. washout period */
/* first_c_date�� 2009,2010�� n�� ��� */
proc sql;
create table aa.washout_year as
select distinct jid
from aa.main_sick2
where substr(first_c_date,1,4) in ('2009', '2010'); quit; *;

/* washout period ���� */
proc sql;
create table aa.washout as
select *
from aa.main_sick2
where jid not in (select jid from aa.washout_year); quit;

data aa.washout2; set aa.washout;
tmp = input(first_c_date, yymmdd10.);
format tmp yymmdd10.;
drop first_c_date;
rename tmp = first_c_date; run;

/* n�� üũ */
proc sort data=aa.washout2 nodupkey out=aa.washout_id; by jid; run;  *158,001;



/************************************************************/
/*************************** �๰ �̱� *********************/
/************************************************************/

/* ����ó�� + ����ó�� */
/*data t300_flu; set aa.t300_2024q3_02;
where substr(div_cd,1,4) in ('1601','4778');
keep mid jid div_cd; run;

proc sql; *�� �����ɸ�;
create table aa.t300_t200_flu as
select a.*, b.recu_fr_dd, b.pat_age as drug_age
from t300_flu as a left join aa.t200_2024q3_02 as b on a.mid=b.mid; quit;

data aa.t300_t200_flu; set aa.t300_t200_flu;
drug_date = mdy(substr(recu_fr_dd,5,2), substr(recu_fr_dd,7,2), substr(recu_fr_dd,1,4)); format drug_date yymmdd8.;
run;


data t530_flu; set aa.t530_2024q3_02;
where substr(div_cd,1,4) in ('1601','4778');
keep mid jid div_cd; run;

proc sql;
create table aa.t530_t200_flu as
select a.*, b.recu_fr_dd, b.pat_age as drug_age
from t530_flu as a left join aa.t200_2024q3_02 as b on a.mid=b.mid; quit;

data aa.t530_t200_flu; set aa.t530_t200_flu;
drug_date = mdy(substr(recu_fr_dd,5,2), substr(recu_fr_dd,7,2), substr(recu_fr_dd,1,4)); format drug_date yymmdd8.;
run;


/* t30 + 53 */
/*proc sql;
create table aa.t300_t530_flu as
select *
	from aa.t300_t200_flu
			union all
select *
	from aa.t530_t200_flu;
quit;


proc sort data=aa.t300_t530_flu; by jid drug_date; run;
/******************************************************************************/