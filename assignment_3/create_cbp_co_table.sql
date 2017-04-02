BEGIN;

DROP TABLE IF EXISTS data.cbp_msa07;

CREATE TABLE data.cbp_msa07 (
"msa" text,
"naics" text,
"empflag" text,
"emp_nf" text,
"emp" text,
"qp1_nf" text,
"qp1" text,
"ap_nf" text,
"ap" text,
"est" integer,
"n1_4" integer,
"n5_9" integer,
"n07_19" integer,
"n20_49" integer,
"n50_99" integer,
"n070_249" integer,
"n250_499" integer,
"n500_999" integer,
"n0700" integer,
"n0700_1" integer,
"n0700_2" integer,
"n0700_3" integer,
"n0700_4" integer
);

\copy data.cbp_msa07 from cbp07msa.txt delimiter ',' header csv;
ALTER TABLE data.cbp_msa07 ADD COLUMN year text default '2007';

END;
