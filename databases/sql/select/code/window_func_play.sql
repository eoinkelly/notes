
create database window_play;

CREATE TABLE instruments (
  id integer not null,
  instrument character varying,
  manufacture_date date,
  created_at timestamp without time zone,
  updated_at timestamp without time zone
)


-- I had to add "created_at DESC" to the ordering because some rows have the same manufacturing date so the sort was unstable
--
SELECT * FROM instruments WHERE id IN (
		SELECT DISTINCT first_value(id) OVER (PARTITION BY instrument ORDER BY manufacture_date DESC, created_at DESC) FROM instruments
	);
-- The nested SELECT returns an array of id values which match the rows we are interested in
-- Results: 50, 49, 48, 47

-- SELECT * FROM instruments;
-- SELECT * FROM instruments WHERE instrument = 'tuba' ORDER BY manufacture_date DESC, created_at DESC;
-- SELECT * FROM instruments WHERE instrument = 'violin' ORDER BY manufacture_date DESC, created_at DESC;
-- SELECT * FROM instruments WHERE instrument = 'french horn' ORDER BY manufacture_date DESC, created_at DESC;
-- SELECT * FROM instruments WHERE instrument = 'cello' ORDER BY manufacture_date DESC, created_at DESC;