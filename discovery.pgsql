CREATE TYPE tag AS (
	key text,
	value text
);

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

CREATE VIEW tags AS
 SELECT a.id,
    (a.tag).key AS key,
    (a.tag).value AS value
   FROM ( SELECT nodes.id,
            jsonb_populate_recordset(NULL::tag, nodes.properties -> 'tags') AS tag
           FROM nodes
          WHERE (jsonb_typeof((nodes.properties -> 'tags')) = 'array')) a;

CREATE VIEW ec2_instances AS
 SELECT nodes.id,
    (nodes.properties ->> 'resourceARN') AS "arn",
    (nodes.properties ->> 'instanceId') AS "instanceId",
    (nodes.properties ->> 'ownerId') AS "ownerId",
    (nodes.properties ->> 'region') AS "region",
    ( SELECT tags.value
           FROM tags
          WHERE ((tags.id = nodes.id) AND (tags.key = 'Name'))) AS name,
    (nodes.properties ->> 'instanceType') AS "instanceType",
    ((nodes.properties -> 'state') ->> 'name'::text) AS state,
    (nodes.properties ->> 'vpcId') AS "vpcId",
    (nodes.properties ->> 'privateIpAddress') AS "privateIpAddress",
    (nodes.properties ->> 'privateDnsName') AS "privateDnsName",
    (nodes.properties ->> 'publicIpAddress') AS "publicIpAddress",
    (nodes.properties ->> 'publicDnsName') AS "publicDnsName",
    ((nodes.properties ->> 'launchTime'))::timestamp with time zone AS "launchTime"
   FROM nodes
  WHERE (nodes.labels @> '{Instance}');

CREATE VIEW vpcs AS
 SELECT nodes.id,
    (nodes.properties ->> 'resourceARN') AS "arn",
    (nodes.properties ->> 'vpcId') AS "vpcId",
    (nodes.properties ->> 'ownerId') AS "ownerId",
    (nodes.properties ->> 'region') AS "region",
    ( SELECT tags.value
           FROM tags
          WHERE ((tags.id = nodes.id) AND (tags.key = 'Name'))) AS name,
    (nodes.properties ->> 'state') AS state,
    (nodes.properties ->> 'cidrBlock') AS "cidrBlock"
   FROM nodes
  WHERE (nodes.labels @> '{Vpc}');

CREATE VIEW subnets AS
 SELECT nodes.id,
    (nodes.properties ->> 'resourceARN') AS "arn",
    (nodes.properties ->> 'subnetId') AS "subnetId",
    (nodes.properties ->> 'ownerId') AS "ownerId",
    (nodes.properties ->> 'region') AS "region",
    ( SELECT tags.value
           FROM tags
          WHERE ((tags.id = nodes.id) AND (tags.key = 'Name'))) AS name,
    (nodes.properties ->> 'state') AS state,
    (nodes.properties ->> 'vpcId') AS "vpcId",
    (nodes.properties ->> 'cidrBlock') AS "cidrBlock",
    (nodes.properties ->> 'availabilityZone') AS "availabilityZone"
   FROM nodes
  WHERE (nodes.labels @> '{Subnet}');
