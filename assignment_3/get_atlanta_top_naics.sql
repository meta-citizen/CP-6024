BEGIN;

	CREATE VIEW data.atl_total AS

	SELECT
		year,
		naics,
		employment
	FROM
		data.msa_base
	WHERE
		msa = '12060'
		AND naics ~ '[0-9]{2}.(000)'
	ORDER BY
		year,
		naics
	;
END;
