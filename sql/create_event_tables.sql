DROP TABLE IF EXISTS events;
CREATE TABLE events (
       url text NOT NULL,
       location text NOT NULL,
       start_date integer NOT NULL
);