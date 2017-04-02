BEGIN;
	CREATE TABLE data.cbp_report AS
	SELECT
		cam.year,
		cam.naics,
		cma.sum as msa_total,
		cam.empflag_decode as atl_total,
		cam.empflag_decode::numeric/cma.sum as l_q
	FROM
		data.cbp_atl_msa cam
		INNER JOIN data.cbp_msa_agg cma
			ON cam.year = cma.year
			AND COALESCE(cam.naics,'') = COALESCE(cma.naics,'')
	;
END;
