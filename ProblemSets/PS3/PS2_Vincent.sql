.mode csv 
.import FL_insurance_sample.csv insurance_data
SELECT * FROM insurance_data LIMIT 10;
SELECT DISTINCT county FROM insurance_data;
SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation FROM insurance_data;
SELECT construction, COUNT(*) * 1.0 / (SELECT COUNT(*) FROM insurance_data) AS fraction
FROM insurance_data
GROUP BY construction;
