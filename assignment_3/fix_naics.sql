BEGIN;
	ALTER TABLE data.cbp_msa_agg ADD COLUMN naics_fix text;

	UPDATE data.cbp_msa_agg SET naics_fix = regexp_replace(naics,'(/|-)','0','g');
END;
