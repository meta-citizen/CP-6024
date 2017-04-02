BEGIN;

CREATE TABLE data.cbp_atl_msa AS

SELECT
	*
FROM
(
SELECT
	*
FROM
	data.cbp_msa14
WHERE
	msa = '12060'
) as a
UNION
(
SELECT
	*
FROM
	data.cbp_msa13
WHERE
	msa = '12060'
)
UNION
( SELECT * FROM data.cbp_msa12 WHERE msa = '12060')
UNION
( SELECT * FROM data.cbp_msa11 WHERE msa = '12060')
UNION
( SELECT * FROM data.cbp_msa10 WHERE msa = '12060')
UNION
( SELECT * FROM data.cbp_msa09 WHERE msa = '12060')
UNION
( SELECT * FROM data.cbp_msa08 WHERE msa = '12060')
UNION
( SELECT * FROM data.cbp_msa07 WHERE msa = '12060')
;

END;
