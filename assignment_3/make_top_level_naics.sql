BEGIN;
	DROP TABLE IF EXISTS data.msa_base;

	CREATE TABLE data.msa_base AS
	
	SELECT
		msa,
		year,
		naics_fix as naics,
		empflag,
		emp
	FROM
		data.cbp_msa
	WHERE
		naics_fix ~ '[0-9][0-9].(000)'
	ORDER BY
		msa,
		year,
		naics
	;

END;
