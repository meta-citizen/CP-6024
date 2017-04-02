BEGIN;
	ALTER TABLE data.cbp_msa ADD COLUMN empflag_decode integer;

	UPDATE data.cbp_msa AS m

	SET
		empflag_decode = n.decode
	FROM
	(
		SELECT
			cam.id,
			cam.naics,
			cam.year,
			cam.empflag,
			ced.value,
			CASE
				WHEN cam.empflag IS NULL THEN emp::integer
				ELSE ced.median
			END AS decode
		FROM
			data.cbp_msa cam
			LEFT JOIN data.cbp_emp_decode ced
				ON cam.empflag = ced.value
	) as n

	WHERE
		m.id = n.id
	;
END;
