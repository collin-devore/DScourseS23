-- Question 5
-- Part A (importing FL_insurance_sample.csv)
CREATE TABLE PS3_DeVore(
"policyID" TEXT,
"statecode" TEXT,
"county" TEXT,
"eq_site_limit" REAL,
"hu_site_limit" REAL,
"fl_site_limit" REAL,
"fr_site_limit" REAL,
"tiv_2011" REAL,
"tiv_2012" REAL,
"eq_site_deductible" REAL,
"hu_site_deductible" REAL,
"fl_site_deductible" REAL,
"fr_site_deductible" REAL,
"point_latitude" REAL,
"point_longitude" REAL,
"line" TEXT,
"construction" TEXT,
"point_granularity" REAL
);
.mode csv
.import FL_insurance_sample.csv PS3_DeVore
-- Part B (Printing the first ten observations)
SELECT * FROM PS3_DeVore LIMIT 10;
-- Part C (Listing the Counties)
SELECT DISTINCT county FROM PS3_DeVore;
-- Part D (Mean of tiv_2012 - tiv_2011)
SELECT AVG(tiv_2012-tiv_2011) FROM PS3_DEVORE;
-- Part E (Frequency Table of construction)
SELECT construction, COUNT(*) FROM PS3_DeVore GROUP BY construction;
