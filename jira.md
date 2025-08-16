# JIRA

Everything is scoped under an _Atlassian account_

- Site administration UI
    - https://rabidtech.atlassian.net/admin/
    - all site admin seems tob e under this URI
- My own user's dashboard
    - https://rabidtech.atlassian.net/home/
    - thin, mostly links to other places (projects, site admin, people admin
      etc.)
- User management UI:
    - https://rabidtech.atlassian.net/people/
- JIRA homepage
    - https://rabidtech.atlassian.net/secure/Dashboard.jspa

## Site administration

An Atlassian account

- has many users
    - each user has access to 0-many applications within the account
    - users get "application access" outside of a group (as a checkbox)
    - they get put in a default `jira-software-users` group when they sign-up
- has many groups
    - group name cannot be edited after creation (I think???)
    - group has 0-many users (called members)
    - groups have access to apps
- has applications
    - e.g. JIRA, Bitbucket, Confluence

Question: it seems like access is granted to users through groups?

There are 3 administrator groups

1. administrators
    - Grants access to all applications and their administration features
      (excluding Site administration)
1. jira-administrators
    - can do most things
    - Grant access to the administration features of Jira
1. site-admins
    - Grants access to all applications, their administration features and Site
      administration, which includes managing users and bills

WTF?

JIRA user management redirects you back to the site-admin user management

JIRA has

- Project roles
- Global permissions which apply to all projects
    - these permssions are applied to groups (groups are defined in the
      site-admin)
- Permissions which only apply to

## Project roles

1. Administrators
    - Default members (users or groups):
        - group: jira-administrators
2. atlassian-addons-project-access
    - Default members (users or groups):
        - a bunch of "users" (which seem to represent addons like Trello,
          Github, checklists etc.)
3. Client
    - Default members (users or groups): NONE
4. Developers
    - Default members (users or groups): NONE
5. Product Owner
    - Default members (users or groups): NONE
6. Service Desk Customers
    - Default members (users or groups): NONE
7. Service Desk Team
    - Default members (users or groups): NONE
8. Team Members
    - Default members (users or groups): NONE

## Permission schemes

Groupings of project permissions

1. Project permissions
2. Issue permissions
3. Voters and watchers permissions
4. Comments permissions
5. Attachements permissions
6. Time tracking permissions

Permissions are grouped into "schemes" A project uses a particular "scheme" Each
new JIRA project also creates a permission scheme Q: can projects share a
scheme? @: can i edit a scheme independent of a project? A JIRA account has a
collection of _permission schemes_

Jira Software projects use either the Jira workflow or the Simplified Workflow,
to control the transition of issues from one status to another

Default groups

https://confluence.atlassian.com/cloud/manage-groups-744721627.html

- jira-users (and example of PRODUCTNAME-users naming scheme)
    - the default group that new users are added to
- jira-developers

1. administrators
    - default permissions depend on which products you have
    - Grants access to all applications and their administration features
      (excluding Site administration)
    - can also access JIRA as a user
    - can have it's access tweaked
1. jira-administrators
    - can only administer JIRA, can't actually access JIRA as a user
    - I think they might not take up a license seat
1. site-admins
    - everything that `administrators` can do plus:
        - billing
        - user management
    - you cannot tweak the access that this group has
    - Grants access to all applications, their administration features and Site
      administration, which includes managing users and bills

In addition to these groups, there are two default groups that are used by
Atlassian support staff. You can't edit these groups or add users to them:
confluence-administrators system-administrators
