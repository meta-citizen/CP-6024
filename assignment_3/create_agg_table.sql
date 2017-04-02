BEGIN;
	CREATE TABLE data.cbp_msa AS

	SELECT
		a.*
	FROM
	( SELECT * FROM data.cbp_msa14 ) as a
	UNION
	( SELECT * FROM data.cbp_msa13 )
	UNION
	( SELECT * FROM data.cbp_msa12 )
	UNION
	( SELECT * FROM data.cbp_msa11 )
	UNION
	( SELECT * FROM data.cbp_msa10 )
	UNION
	( SELECT * FROM data.cbp_msa09 )
	UNION
	( SELECT * FROM data.cbp_msa08 )
	UNION
	( SELECT * FROM data.cbp_msa07 )
	;

	ALTER TABLE data.cbp_msa ADD COLUMN id serial primary key;
END;
