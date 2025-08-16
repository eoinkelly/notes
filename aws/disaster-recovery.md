S3 objects are stored redundantly across multiple facilities _within a region_
(11 9's of uptime)

DR toolbox

Assets _ Are already replicated across a region _ can be copied to another
region and put in galcier?

EBS volume snapshots are stored as objects in S3 so they get the same garuantees
except (maybe??) they are replicated within an AZ only not a region

RDS lets you snapshot from one region to another or have a read replica running
in another region

When depployed in "multi AZ" mode, RDS will use synchronous replication to
duplicate data in a second AZ

> When operations such as DB Instance scaling or system upgrades like OS
> patching are initiated for Multi-AZ deployments, they are applied first on the
> standby prior to an automatic failover. As a result, your availability impact
> is limited only to the time required for automatic failover to complete.
