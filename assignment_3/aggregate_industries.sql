BEGIN;
	CREATE TABLE data.cbp_msa_agg AS

	SELECT
		year,
		naics,
		sum(empflag_decode)
	FROM
		data.cbp_msa
	GROUP BY
		year,
		naics
	;
END;
