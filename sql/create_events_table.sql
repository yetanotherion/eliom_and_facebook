DROP TABLE IF EXISTS events;
CREATE TABLE events (
       url text NOT NULL,
       location text NOT NULL,
       start_date integer NOT NULL,
       owner text NOT NULL,
       name text NOT NULL,
       nb_attending integer NOT NULL,
       nb_declined integer NOT NULL,
       nb_invited integer NOT NULL
);
