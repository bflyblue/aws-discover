CREATE TABLE nodes (
    id serial NOT NULL PRIMARY KEY,
    labels text[],
    properties jsonb
);

CREATE TABLE edges (
    id serial NOT NULL PRIMARY KEY,
    labels text[],
    properties jsonb,
    a int REFERENCES nodes(id) ON DELETE CASCADE ON UPDATE CASCADE,
    b int REFERENCES nodes(id) ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE INDEX node_labels ON nodes USING gin (labels);
CREATE INDEX node_props ON nodes USING gin (properties);

CREATE INDEX edge_labels ON edges USING gin (labels);
CREATE INDEX edge_props ON edges USING gin (properties);
CREATE INDEX edge_a ON edges (a);
CREATE INDEX edge_b ON edges (b);