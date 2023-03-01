CREATE TYPE tag AS (
	key text,
	value text
);

CREATE TABLE nodes (
    id serial NOT NULL PRIMARY KEY,
    keyspace text,
    key text,
    labels text[],
    properties jsonb
);

CREATE TABLE edges (
    id serial NOT NULL PRIMARY KEY,
    keyspace text,
    key text,
    labels text[],
    properties jsonb,
    a int REFERENCES nodes(id) ON DELETE CASCADE ON UPDATE CASCADE,
    b int REFERENCES nodes(id) ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE UNIQUE INDEX node_keys ON nodes (keyspace, key);
CREATE INDEX node_labels ON nodes USING gin (labels);
CREATE INDEX node_props ON nodes USING gin (properties);

CREATE UNIQUE INDEX edge_keys ON edges (keyspace, key);
CREATE INDEX edge_labels ON edges USING gin (labels);
CREATE INDEX edge_props ON edges USING gin (properties);
CREATE INDEX edge_a ON edges (a);
CREATE INDEX edge_b ON edges (b);

-- CREATE VIEW nodemeta AS
--  SELECT nodes.id,
--     COALESCE(( SELECT tags.value
--            FROM tags
--           WHERE tags.id = nodes.id AND tags.key = 'Name'::text), nodes.properties ->> 'functionName'::text) AS name
--    FROM nodes;

CREATE VIEW tags AS
 SELECT a.id,
    (a.tag).key AS key,
    (a.tag).value AS value
   FROM ( SELECT nodes.id,
            jsonb_populate_recordset(NULL::tag, nodes.properties -> 'resourceTags'::text) AS tag
           FROM nodes
          WHERE nodes.labels @> '{Resource}'::text[]
        UNION
         SELECT nodes.id,
            jsonb_populate_recordset(NULL::tag, nodes.properties -> 'tagList'::text) AS tag
           FROM nodes
          WHERE nodes.labels @> '{DbInstance}'::text[] AND jsonb_typeof(nodes.properties -> 'tagList'::text) = 'array'::text
        UNION
         SELECT nodes.id,
            jsonb_populate_recordset(NULL::tag, nodes.properties -> 'tags'::text) AS tag
           FROM nodes
          WHERE jsonb_typeof(nodes.properties -> 'tags'::text) = 'array'::text) a;

CREATE VIEW environments AS
 SELECT a.id,
    (a.env).key AS key,
    (a.env).value AS value
   FROM ( SELECT nodes.id,
            jsonb_each(nodes.properties -> 'environment' -> 'variables') AS env
           FROM nodes
          WHERE (jsonb_typeof((nodes.properties -> 'environment' -> 'variables')) = 'object')) a;

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
    (nodes.properties ->> 'launchTime') AS "launchTime"
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

CREATE FUNCTION jsonb_default(a jsonb, dflt jsonb) RETURNS jsonb AS $$
BEGIN
  RETURN CASE
    WHEN a='null'::jsonb THEN dflt
    WHEN a IS NULL THEN dflt
    ELSE a
  END;
END;
$$ IMMUTABLE LANGUAGE plpgsql;

CREATE VIEW security_groups AS
 SELECT nodes.id,
    (nodes.properties ->> 'resourceARN') AS "arn",
    (nodes.properties ->> 'groupId') AS "groupId",
    (nodes.properties ->> 'ownerId') AS "ownerId",
    (nodes.properties ->> 'region') AS "region",
    ( SELECT tags.value
           FROM tags
          WHERE ((tags.id = nodes.id) AND (tags.key = 'Name'))) AS name,
    (nodes.properties ->> 'description') AS description,
    perms."fromPort",
    perms."toPort",
    perms."ipProtocol",
    ranges."cidrIp"
   FROM nodes, 
        jsonb_to_recordset(jsonb_default(nodes.properties->'ipPermissions', '[]')) AS perms("toPort" int, "fromPort" int, "ipRanges" jsonb, "ipProtocol" text),
        jsonb_to_recordset(jsonb_default(perms."ipRanges", '[]')) AS ranges("cidrIp" text)
  WHERE (nodes.labels @> '{SecurityGroup}');

CREATE VIEW lambdas AS
 SELECT nodes.id,
    (nodes.properties ->> 'resourceARN') AS "arn",
    (nodes.properties ->> 'codeSize') AS "codeSize",
    (nodes.properties ->> 'description') AS "description",
    (nodes.properties ->> 'functionName') AS "functionName",
    (nodes.properties ->> 'handler') AS "handler",
    (nodes.properties ->> 'memorySize') AS "memorySize",
    (nodes.properties ->> 'packageType') AS "packageType",
    (nodes.properties ->> 'role''') AS "role",
    (nodes.properties ->> 'runtime') AS "runtime",
    (nodes.properties ->> 'timeout') AS "timeout",
    (nodes.properties -> 'vpcConfig' ->> 'vpcId') AS "vpcId"
   FROM nodes
  WHERE (nodes.labels @> '{Lambda}');

CREATE VIEW db_instances AS
 SELECT nodes.id,
    (nodes.properties ->> 'resourceARN') AS "arn",
    (nodes.properties ->> 'dbInstanceIdentifier') AS "dbInstanceIdentifier",
    (nodes.properties ->> 'dbName') AS "dbName",
    ( SELECT tags.value
           FROM tags
          WHERE ((tags.id = nodes.id) AND (tags.key = 'Name'))) AS name,
    (nodes.properties ->> 'engine') AS "engine",
    (nodes.properties ->> 'engineVersion') AS "engineVersion",
    (nodes.properties -> 'endpoint' ->> 'address') AS "endpointAddress",
    (nodes.properties -> 'endpoint' ->> 'port') AS "endpointPort",
    (nodes.properties -> 'listenerEndpoint' ->> 'address') AS "listenerEndpointAddress",
    (nodes.properties -> 'listenerEndpoint' ->> 'port') AS "listenerEndpointPort",
    (nodes.properties ->> 'dbInstanceClass') AS "dbInstanceClass",
    (nodes.properties ->> 'dbInstanceStatus') AS "dbInstanceStatus",
    (nodes.properties ->> 'dbClusterIdentifier') AS "dbClusterIdentifier",
    (nodes.properties ->> 'availabilityZone') AS "availabilityZone",
    (nodes.properties ->> 'multiAZ') AS "multiAZ",
    (nodes.properties ->> 'iops') AS "iops"
   FROM nodes
  WHERE (nodes.labels @> '{DbInstance}');
