
SELECT * FROM `mdv-t2d.mdv_jp_2021.DiseaseData`
WHERE icd10code LIKE "E11%" OR
icd10code LIKE "E12%" OR
icd10code LIKE "E13%" OR 
icd10code LIKE "E14%";

SELECT * FROM `mdv-t2d.mdv_jp_2021.DiseaseData`
WHERE icd10code LIKE "E66%" OR
icd10code LIKE "R632" OR
icd10code LIKE "R635" OR 
icd10code LIKE "R638";


SELECT * FROM `mdv-t2d.mdv_jp_2021.FF1Data`
WHERE icd10code1 LIKE "E11%" OR
icd10code1 LIKE "E12%" OR
icd10code1 LIKE "E13%" OR 
icd10code1 LIKE "E14%" OR
icd10code2 LIKE "E11%" OR
icd10code2 LIKE "E12%" OR
icd10code2 LIKE "E13%" OR 
icd10code2 LIKE "E14%" OR
icd10code3 LIKE "E11%" OR
icd10code3 LIKE "E12%" OR
icd10code3 LIKE "E13%" OR 
icd10code3 LIKE "E14%";


SELECT * FROM `mdv-t2d.mdv_jp_2021.FF1Data`
WHERE icd10code1 LIKE "E66%" OR
icd10code1 LIKE "R632" OR
icd10code1 LIKE "R635" OR 
icd10code1 LIKE "R638" OR
icd10code2 LIKE "E66%" OR
icd10code2 LIKE "R632" OR
icd10code2 LIKE "R635" OR 
icd10code2 LIKE "R638" OR
icd10code3 LIKE "E66%" OR
icd10code3 LIKE "R632" OR
icd10code3 LIKE "R635" OR 
icd10code3 LIKE "R638";


SELECT * FROM `mdv-t2d.mdv_jp_2021.FF1Data`
WHERE icd10code1 LIKE "E11%" OR
icd10code1 LIKE "E12%" OR
icd10code1 LIKE "E13%" OR 
icd10code1 LIKE "E14%" OR
icd10code2 LIKE "E11%" OR
icd10code2 LIKE "E12%" OR
icd10code2 LIKE "E13%" OR 
icd10code2 LIKE "E14%" OR
icd10code3 LIKE "E11%" OR
icd10code3 LIKE "E12%" OR
icd10code3 LIKE "E13%" OR 
icd10code3 LIKE "E14%" OR
icd10code1_2003 LIKE "E11%" OR
icd10code1_2003 LIKE "E12%" OR
icd10code1_2003 LIKE "E13%" OR 
icd10code1_2003 LIKE "E14%" OR
icd10code2_2003 LIKE "E11%" OR
icd10code2_2003 LIKE "E12%" OR
icd10code2_2003 LIKE "E13%" OR 
icd10code2_2003 LIKE "E14%" OR
icd10code3_2003 LIKE "E11%" OR
icd10code3_2003 LIKE "E12%" OR
icd10code3_2003 LIKE "E13%" OR 
icd10code3_2003 LIKE "E14%";

SELECT * FROM `mdv-t2d.mdv_jp_2021.FF1Data`
WHERE icd10code1 LIKE "E66%" OR
icd10code1 LIKE "R632" OR
icd10code1 LIKE "R635" OR 
icd10code1 LIKE "R638" OR
icd10code2 LIKE "E66%" OR
icd10code2 LIKE "R632" OR
icd10code2 LIKE "R635" OR 
icd10code2 LIKE "R638" OR
icd10code3 LIKE "E66%" OR
icd10code3 LIKE "R632" OR
icd10code3 LIKE "R635" OR 
icd10code3 LIKE "R638" OR
icd10code1_2003 LIKE "E66%" OR
icd10code1_2003 LIKE "R632" OR
icd10code1_2003 LIKE "R635" OR 
icd10code1_2003 LIKE "R638" OR
icd10code2_2003 LIKE "E66%" OR
icd10code2_2003 LIKE "R632" OR
icd10code2_2003 LIKE "R635" OR 
icd10code2_2003 LIKE "R638" OR
icd10code3_2003 LIKE "E66%" OR
icd10code3_2003 LIKE "R632" OR
icd10code3_2003 LIKE "R635" OR 
icd10code3_2003 LIKE "R638";



SELECT a.patientid, a.admittingdate, a.dischargingdate, a.weight, a.height, 
a.icd10code1, a.icd10code1_2003, a.icd10code2, a.icd10code2_2003, a.icd10code3, 
a.icd10code3_2003, a.kacodeuni  
FROM `mdv-t2d.mdv_jp_2021.T2D_sub_FF1Data_NEW` AS a;


SELECT a.patientid, a.admittingdate, a.dischargingdate, a.weight, a.height, 
a.icd10code1, a.icd10code1_2003, a.icd10code2, a.icd10code2_2003, a.icd10code3, 
a.icd10code3_2003, a.kacodeuni  
FROM `mdv-t2d.mdv_jp_2021.Obesity_sub_FF1Data_NEW` AS a;



SELECT a.patientid, a.datamonth, a.fromdate, a.nyugaikbn, a.dpcdiseasesegment, 
a.icd10code, a.diseasecode, a.diseasename_eng   
FROM `mdv-t2d.mdv_jp_2021.Obesity_sub_DiseaseData` AS a;


SELECT a.patientid, a.datamonth, a.fromdate, a.nyugaikbn, a.icd10code, 
a.diseasecode, a.diseasename_eng   
FROM `mdv-t2d.mdv_jp_2021.Obesity_sub_DiseaseData` AS a;

SELECT a.patientid, a.datamonth, a.dpcdiseasesegment, a.fromdate, 
a.nyugaikbn, a.icd10code, a.diseasecode, a.diseasename_eng   
FROM `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData` AS a;

SELECT a.patientid, a.datamonth, a.fromdate, a.nyugaikbn,
 a.icd10code, a.diseasecode, a.diseasename_eng  
 FROM `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData` AS a;






SELECT * FROM  `mdv-t2d.mdv_jp_2021.MDrug`AS a
WHERE a.atccode_ephmra LIKE "A10%";

SELECT * FROM  `mdv-t2d.mdv_jp_2021.MDrug`AS a
WHERE a.atccode_ephmra LIKE "A08%";

SELECT m.atccode_ephmra, m.drugusagecode, m.genericcode, m.receiptcode, m.druggeneralname_eng 
FROM  `mdv-t2d.mdv_jp_2021.MDrug_A10s_only` AS m;

SELECT m.atccode_ephmra, m.drugusagecode, m.genericcode, m.receiptcode, m.druggeneralname_eng 
FROM  `mdv-t2d.mdv_jp_2021.MDrug_A08_only` AS m;

SELECT * FROM `mdv-t2d.mdv_jp_2021.ActData_FILTERED`
RIGHT OUTER JOIN `mdv-t2d.mdv_jp_2021.MDrug_A10s_only_SHORT` USING (receiptcode);

SELECT * FROM `mdv-t2d.mdv_jp_2021.ActData_FILTERED`
RIGHT OUTER JOIN `mdv-t2d.mdv_jp_2021.MDrug_A08_only_SHORT` USING (receiptcode);



SELECT a.patientid, a.sex, a.age, a.datamonth
FROM `mdv-t2d.mdv_jp_2021.Obesity_sub_DiseaseData` AS a;

SELECT a.patientid, a.receiptcode,a.datamonth
FROM `mdv-t2d.mdv_jp_2021.ActData` AS a;

SELECT a.patientid, a.receiptcode
FROM `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth` AS a;

SELECT *
FROM `mdv-t2d.mdv_jp_2021.Obesity_sub_DiseaseData_FILTERED` a
LEFT JOIN `mdv-t2d.mdv_jp_2021.ActData_ID_Receiptcode` b USING (patientid);


SELECT DISTINCT patientid, datamonth, receiptcode
FROM    `mdv-t2d.mdv_jp_2021.Obesity_sub_DiseaseData_FILTERED_with_ACT_DATA`;


SELECT DISTINCT patientid,
FROM    `mdv-t2d.mdv_jp_2021.Obesity_Disease_Data_Unique_Act_Data`;


SELECT *
  FROM `mdv-t2d.mdv_jp_2021.Obesity_Disease_Data_Unique_Act_Data`
  ORDER BY datamonth DESC ;


SELECT a.patientid, a.datamonth, a.datakbn
FROM `mdv-t2d.mdv_jp_2021.ActData` a
WHERE a.receiptcode = 611190062;


Select count(distinct patientid) from `mdv-t2d.mdv_jp_2021.Obesity_sub_DiseaseData_FILTERED`;




select count(distinct patientid) from 
(select * from 
    (Select distinct t1.patientid, t2.receiptcode, t2.datamonth
    from `mdv-t2d.mdv_jp_2021.Obesity_sub_DiseaseData_FILTERED` t1
    left join `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth` t2 ON t1.patientid = t2.patientid
    order by datamonth, patientid, receiptcode desc)
where extract (year from datamonth) > 2020);



select * from 
    (Select distinct(t1.patientid), t2.receiptcode, t2.datamonth
    from `mdv-t2d.mdv_jp_2021.Obesity_sub_DiseaseData_FILTERED` t1
    left join `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth` t2 ON t1.patientid = t2.patientid)
where extract (year from datamonth) >= 2020 and  extract (month from datamonth) > 09;




select * from 
    (Select distinct(t1.patientid), t2.receiptcode, t2.datamonth
    from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData_FILTERED` t1
    left join `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth` t2 ON t1.patientid = t2.patientid)
where extract (year from datamonth) >= 2020 and  extract (month from datamonth) > 09
order by patientid;




SELECT t2disease.icd10code
FROM `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData_FILTERED` as t2disease;


SELECT t2disease.nyugaikbn
FROM `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData_FILTERED` as t2disease;


SELECT t2disease.datamonth
FROM `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData_FILTERED` as t2disease;

SELECT t2disease.datamonth, t2disease.patientid
FROM `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData_FILTERED` as t2disease;

select count(distinct t.patientid)
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData` as t;

select distinct t.age
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData` as t;

select count(distinct t.age)
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData` as t;

select count(t.age)
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData` as t;


select  age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`;

select  age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by age, datamonth;

select  age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by age, datamonth
order by age;

select  age, datamonth, count(*)
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by age, datamonth
order by age;

select age, datamonth, count(*)
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by age, datamonth order by age;

select  patientid, age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`;


select  distinct(patientid), age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`;

select  distinct(patientid), age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by patientid;

select  distinct(patientid), age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by patientid, age, datamonth;

select  distinct(patientid), age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by patientid, age, datamonth
order by patientid;

select  distinct(patientid), age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by patientid, age, datamonth
order by patientid, age, datamonth;

select  distinct(patientid), age, datamonth, MAX(datamonth)
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by patientid, age, datamonth
order by patientid, age, datamonth;


with t1 as (select patientid, age, datamonth, MAX(datamonth) as maxdata
                from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
                group by patientid, age, datamonth)
select t1.patientid, t1.age
from t1
where datamonth = maxdata;

select patientid, age, datamonth, MAX(datamonth) as maxdata
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by patientid, age, datamonth;

select patientid, age, datamonth, MAX(datamonth) as maxdata
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
group by patientid, age, datamonth;

select patientid, age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData` 
group by patientid, age, datamonth
order by patientid, age, datamonth;

with t1 as (select patientid, age, datamonth, MAX(datamonth) as maxdata
                from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
                group by patientid, age, datamonth)
select t1.patientid, t1.age
from t1
where datamonth = maxdata;

with t1 as (select patientid, age, datamonth, MAX(datamonth) as maxdata
                from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData`
                group by patientid, age, datamonth)
select t1.patientid, t1.age, t1.datamonth
from t1
where datamonth = maxdata;


select patientid, age, datamonth
from `mdv-t2d.mdv_jp_2021.T2D_sub_DiseaseData` 
group by patientid, age, datamonth
order by patientid, age, datamonth;


select t1.*, t2.age
from `mdv-t2d.mdv_jp_2021.T2D_id_date_maxmonth` as t1
inner join `mdv-t2d.mdv_jp_2021.T2D_id_age_datamonth` as t2
ON  t1.f0_ = t2.datamonth and t1.patientid = t2.patientid;

select t1.*, t2.age
from `mdv-t2d.mdv_jp_2021.T2D_id_date_maxmonth` as t1
inner join `mdv-t2d.mdv_jp_2021.T2D_id_age_datamonth` as t2
ON  t1.f0_ = t2.datamonth and t1.patientid = t2.patientid;


select patientid, age
from `mdv-t2d.mdv_jp_2021.T2DM_if_maxmonth_age`;


select patientid, datamonth, receiptcode
from `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth`
where datamonth > '2020-10-01' and 
(receiptcode = 610444147 OR
receiptcode = 610463145 OR
receiptcode = 613960032 OR
receiptcode = 620002859 OR
receiptcode = 620004480 OR
receiptcode = 620005570 OR
receiptcode = 620009133 OR
receiptcode = 621676001 OR
receiptcode = 621974701 OR
receiptcode = 622070801 OR
receiptcode = 622242501 OR
receiptcode = 622412701 OR
receiptcode = 622417101 OR
receiptcode = 622417201 OR
receiptcode = 622421101 OR
receiptcode = 622421201 OR
receiptcode = 622421901 OR
receiptcode = 622422001 OR
receiptcode = 622424401 OR
receiptcode = 622424501 OR
receiptcode = 622427201 OR
receiptcode = 622427301 OR
receiptcode = 622432601 OR
receiptcode = 622432701 OR
receiptcode = 622436301 OR
receiptcode = 622438401 OR
receiptcode = 622438501 OR
receiptcode = 622448601 OR
receiptcode = 622466601 OR
receiptcode = 622784601 OR
receiptcode = 622784701 OR
receiptcode = 622822401 OR
receiptcode = 622822501 OR
receiptcode = 621986301 OR
receiptcode = 621986401 OR
receiptcode = 622517101 OR
receiptcode = 622654401 OR
receiptcode = 622654501 OR
receiptcode = 622450301 OR
receiptcode = 622450401);






select patientid, datamonth, receiptcode
from `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth`
where datamonth > '2020-10-01' and 
(receiptcode = 621950901 OR receiptcode = 621951001 OR receiptcode = 621951101 OR receiptcode = 621970601 OR receiptcode = 621970701
OR receiptcode = 621970801 OR receiptcode =622277501 OR receiptcode = 622288401 OR receiptcode = 622625702);


select patientid, datamonth, receiptcode
from `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth`
where datamonth > '2020-10-01' and 
(receiptcode = 610432040 OR
receiptcode =  610432041 OR
receiptcode = 621990901 OR
receiptcode = 621991001 OR
receiptcode = 622041202 OR
receiptcode = 622041302 OR
receiptcode = 622041402 OR
receiptcode = 622041502 OR
receiptcode = 622042901 OR
receiptcode = 622043001 OR
receiptcode = 622045201 OR
receiptcode = 622045301 OR
receiptcode = 622045401 OR
receiptcode = 622045501 OR
receiptcode = 622046801 OR
receiptcode = 622046901 OR
receiptcode = 622047701 OR
receiptcode = 622047801 OR
receiptcode = 622049901 OR
receiptcode = 622050001 OR
receiptcode = 622053101 OR
receiptcode = 622053201 OR
receiptcode = 622053801 OR
receiptcode = 622055801 OR
receiptcode = 622055901 OR
receiptcode = 622056001 OR
receiptcode = 622056101 OR
receiptcode = 622059201 OR
receiptcode = 622059301 OR
receiptcode = 622061001 OR
receiptcode = 622061401 OR
receiptcode = 622061501 OR
receiptcode = 622061601 OR
receiptcode = 622061701 OR
receiptcode = 622062301 OR
receiptcode = 622062302 OR
receiptcode = 622062401 OR
receiptcode = 622062402 OR
receiptcode = 622063001 OR
receiptcode = 622063101 OR
receiptcode = 622063201 OR
receiptcode = 622063301 OR
receiptcode = 622065101 OR
receiptcode = 622065201 OR
receiptcode = 622065301 OR
receiptcode = 622065401 OR
receiptcode = 622066201 OR
receiptcode = 622066301 OR
receiptcode = 622071701 OR
receiptcode = 622071801 OR
receiptcode = 622071901 OR
receiptcode = 622072001 OR
receiptcode = 622078301 OR
receiptcode = 622078401 OR
receiptcode = 622079101 OR
receiptcode = 622079201 OR
receiptcode = 622081801 OR
receiptcode = 622081901 OR
receiptcode = 622144601 OR
receiptcode = 622144701 OR
receiptcode = 622147301 OR
receiptcode = 622147401 OR
receiptcode = 622147501 OR
receiptcode = 622147601 OR
receiptcode = 622155701 OR
receiptcode = 622155801 OR
receiptcode = 622155901 OR
receiptcode = 622156001 OR
receiptcode = 622156901 OR
receiptcode = 622157001 OR
receiptcode = 622159401 OR
receiptcode = 622159501 OR
receiptcode = 622163301 OR
receiptcode = 622163401 OR
receiptcode = 622164301 OR
receiptcode = 622164401 OR
receiptcode = 622166801 OR
receiptcode = 622166901 OR
receiptcode = 622167201 OR
receiptcode = 622167301 OR
receiptcode = 622172101 OR
receiptcode = 622172201 OR
receiptcode = 622175401 OR
receiptcode = 622175401 OR
receiptcode = 622175501 OR
receiptcode = 622175601 OR
receiptcode = 622175701 OR
receiptcode = 622178601 OR 
receiptcode = 622178701 OR
receiptcode = 622182401 OR
receiptcode = 622182501 OR
receiptcode = 622320800 OR
receiptcode = 622320900 OR
receiptcode = 622691700 OR
receiptcode = 622741400 OR
receiptcode = 622795901 OR
receiptcode = 622796001 OR
receiptcode = 622048401 OR
receiptcode = 622048501 OR
receiptcode = 621986301 OR
receiptcode = 621986401 OR
receiptcode = 622086001 OR
receiptcode = 622086101);



select patientid, datamonth, receiptcode
from `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth`
where datamonth > '2020-10-01' and 
(receiptcode = 610443002 OR
receiptcode =610443003 OR
receiptcode =621982701 OR
receiptcode =621997001 OR
receiptcode =621997101 OR
receiptcode =621998701 OR
receiptcode =621998801 OR
receiptcode =621998901 OR
receiptcode =621999001 OR
receiptcode =621999301 OR
receiptcode =621999401 OR
receiptcode =621999701 OR
receiptcode =621999801 OR
receiptcode =622000601 OR
receiptcode =622000701 OR
receiptcode =622001701 OR
receiptcode =622001801 OR
receiptcode =622004701 OR
receiptcode =622004702 OR
receiptcode =622004801 OR
receiptcode =622004802 OR
receiptcode =622005501 OR
receiptcode =622005601 OR
receiptcode =622008701 OR
receiptcode =622008801 OR
receiptcode =622009901 OR
receiptcode =622010001 OR
receiptcode =622011401 OR
receiptcode =622011501 OR
receiptcode =622011601 OR
receiptcode =622011701 OR
receiptcode =622013501 OR
receiptcode =622013601 OR
receiptcode =622016001 OR
receiptcode =622016101 OR
receiptcode =622017301 OR
receiptcode =622017401 OR
receiptcode =622017501 OR
receiptcode =622017901 OR
receiptcode =622018001 OR
receiptcode =622020901 OR
receiptcode =622020903 OR
receiptcode =622021001 OR
receiptcode =622021003 OR
receiptcode =622021801 OR
receiptcode =622021901 OR
receiptcode =622022001 OR
receiptcode =622022101 OR
receiptcode =622023501 OR
receiptcode =622023601 OR
receiptcode =622025201 OR
receiptcode =622025301 OR
receiptcode =622025801 OR
receiptcode =622025901 OR
receiptcode =622026501 OR
receiptcode =622026601 OR
receiptcode =622029901 OR
receiptcode =622030001 OR
receiptcode =622031401 OR
receiptcode =622031501 OR
receiptcode =622033001 OR
receiptcode =622033101 OR
receiptcode =622033201 OR
receiptcode =622033701 OR
receiptcode =622033801 OR
receiptcode =622035701 OR
receiptcode =622035801 OR
receiptcode =622037901 OR
receiptcode =622038001 OR
receiptcode =622038801 OR
receiptcode =622038901 OR
receiptcode =622058801 OR
receiptcode =622058901 OR
receiptcode =622059001 OR
receiptcode =622059002 OR
receiptcode =622059101 OR
receiptcode =622059102 OR
receiptcode =622088301 OR
receiptcode =622088401 OR
receiptcode =622114701 OR
receiptcode =622114801 OR
receiptcode =622118501 OR
receiptcode =622122201 OR
receiptcode =622122301 OR
receiptcode =622127301 OR
receiptcode =622127401 OR
receiptcode =622127501 OR
receiptcode =622128101 OR
receiptcode =622137701 OR
receiptcode =622141101 OR
receiptcode =622144001 OR
receiptcode =622159301 OR
receiptcode =622169301 OR
receiptcode =622171301 OR
receiptcode =622176301 OR
receiptcode =622177501 OR
receiptcode =622186201 OR
receiptcode =622187301 OR
receiptcode =622190001 OR
receiptcode =622190002 OR
receiptcode =622190801 OR
receiptcode =622193301 OR
receiptcode =622194901 OR
receiptcode =622198001 OR
receiptcode =622202201 OR
receiptcode =622202801 OR
receiptcode =622205101 OR
receiptcode =622205501 OR
receiptcode =622208901 OR
receiptcode =622211501 OR
receiptcode =622217701 OR
receiptcode =622219701 OR
receiptcode =622221001 OR
receiptcode =622222001 OR
receiptcode =622242001 OR
receiptcode =622246801 OR
receiptcode =622252501 OR
receiptcode =622254701 OR
receiptcode =622271101 OR
receiptcode =622271201 OR
receiptcode =622271301 OR
receiptcode =622313300 OR
receiptcode =622338501 OR
receiptcode =622338601 OR
receiptcode =622338701 OR
receiptcode =622631401 OR
receiptcode =622636501 OR
receiptcode =622636601 OR
receiptcode =622847100 OR
receiptcode =622048401 OR
receiptcode =622048501);


select patientid, datamonth, receiptcode
from `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth`
where datamonth > '2020-10-01' and 
(receiptcode = 622306601 OR 
receiptcode = 622306701 OR 
receiptcode = 622625702);


select patientid, datamonth, receiptcode
from `mdv-t2d.mdv_jp_2021.ActData_ID_Receipt_Datamonth`
where datamonth > '2020-10-01' and 
(receiptcode = 622442201 );
