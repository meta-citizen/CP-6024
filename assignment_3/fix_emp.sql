BEGIN;
	ALTER TABLE data.cbp_msa_agg ADD COLUMN employment INTEGER;

	
	UPDATE data.cbp_msa_agg as m

	SET
		employment = n.employment
	FROM
	(
	SELECT
		mb.id,
		msa,
		year,
		naics,
		empflag,
		CASE
			WHEN mb.empflag IS NOT NULL THEN ced.median
			ELSE mb.emp::integer
		END as employment
	FROM
		data.cbp_msa_agg mb
		LEFT JOIN data.cbp_emp_decode ced
			ON mb.empflag = ced.value
	) as n

	WHERE
		m.id = n.id
	;
END;
