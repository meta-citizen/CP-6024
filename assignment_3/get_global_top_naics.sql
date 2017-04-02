BEGIN;

CREATE VIEW data.usa_total AS 

SELECT
	year,
	naics_fix as naics,
	sum as usa_employment
FROM
	data.cbp_msa_agg
WHERE
	naics_fix ~ '[0-9]{2}.(000)'
ORDER BY
	year,
	naics
;

END;
