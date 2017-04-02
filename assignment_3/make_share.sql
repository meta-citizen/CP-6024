BEGIN;

CREATE OR REPLACE VIEW data.econ_analysis AS

SELECT
	a.id,
	a.year,
	a.naics,
	a.msa_total,
	round(a.msa_share,3) as msa_share,
	a.atl_total,
	round(a.atl_share,3) as atl_share,
	round(a.atl_l_q,3) as atl_location_quotient,
	a.basic_emp,
	a.n_basic_emp,
	CASE WHEN
		a.is_basic IS TRUE THEN ROUND(((1-(1/atl_l_q))*100),3)
		ELSE NULL
	END as emp_mul
FROM
(
	SELECT
		m.*,
		CASE
			WHEN is_basic IS TRUE
				THEN round(m.atl_total-(m.atl_total*(m.msa_total::numeric/m.msa_top)),0)
			ELSE 0
		END as basic_emp,
		CASE
			WHEN is_basic IS TRUE
				THEN round(m.atl_total - (m.atl_total-(m.atl_total*(m.msa_total::numeric/m.msa_top))),0)
			ELSE
				m.atl_total
		END as n_basic_emp
	FROM
	(
		SELECT
			n.*,
			n.atl_share/msa_share as atl_l_q,
			CASE
				WHEN n.atl_share/msa_share >= 1 THEN TRUE
				ELSE FALSE
			END as is_basic	
		FROM
		(
			SELECT
				cr.*,
				cr.msa_total::numeric/ctl.msa_total as msa_share,
				cr.atl_total::numeric/ctl.atl_total as atl_share,
				ctl.msa_total as msa_top,
				ctl.atl_total as atl_top
			FROM
				data.cbp_report cr
				INNER JOIN data.cbp_top_level ctl
					ON cr.year = ctl.year
		) as n
	) as m
) as a;

END;
