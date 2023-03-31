# Permissions Checker Utility

Starting with DBB Version 2.0, DBB is leveraging your SAF product to determine the DBB role when accessing it's metadata store, which was previously handled by the DBB WebApp server using LDAP or the basicUserRegistry.

This script helps you to determine if a user is member of the DBB groups.

You can provide a user via the cli using `--userId <userName>` . If you don't specify a user it will check the current user, which executes the `checkDBBPermission` script. 

## Sample invocation

```
groovyz checkDBBPermission.groovy --userId GITLAB

-- DBB Permission Checker --

-- Checking UserId provided via CLI
- Check DBB Role for user GITLAB:
- Member of SAF Group
 DBBADMNS : false
 DBBUSERS : true
 DBBGUEST : false

-- Checking Current User
- Check DBB Role for user DBEHM:
- Member of SAF Group
 DBBADMNS : true
 DBBUSERS : true
 DBBGUEST : true

```