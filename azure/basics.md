# Azure

Choosing compute

https://docs.microsoft.com/en-us/azure/architecture/guide/technology-choices/compute-decision-tree


Compute options on Azure

1. Virtual machine
    * VMs run inside Azure virtual network
    * Is IaaS
    * can autoscale using _Virtual Machine scale sets_
1. Azure Batch
    * managed service
    * for HPC and large-scale parallel workloads
1. App Service
    * Managed service for hosting web apps
    * Is a PaaS
    * the app runs in an _App Service Plan_
    * has a free tier (1GB mem) but has limited features  e.g. no custom domain
1. App Service Containers
    * Is a PaaS
1. Azure functions
    * managed serverless functions
    * akin to lambda
    * Is FaaS
    * _can_ run in an _App Service Plan_
1. Container instances
    * easiest way to run a container in Azure
    * ++ you don't have to provision a VM
    * sort of a middle ground between _Virtual machine_ and _App Service_
1. Service Fabric
    * .Net only (I think)
    * distributed platform
    * can run on Azure and/or on-prem
1. Azure Kubernetes Service


Terminology

* Azure Devops
    * An umbrella term for a number of other services
        * Pipelines (similar to GH actions)
        * Kanban boards
        * Artifacts (store npm packages etc.)
        * Repos (seems to be Github esque)
        * Azure test plans
    * Azure DevOps represents the evolution of Visual Studio Team Services (VSTS). VSTS users will be upgraded into Azure DevOps projects automatically
* Microsoft Deployment
    * seems to be a bit like CloudFormation
* App service plan
    * A set of compute resources for a web app to run
    * analogous to a server farm in on-prem
    * 1+ apps can run on in the same _App service plan_
    * exists in a single region (I think)
    * Defines
        1. Region
        1. Number of VM instances
        1. Size of Vm instances
        1. Pricing tier
            * one of:
                * Shared compute: Free
                    * your apps share compute resources with other customers
                    * each app gets a CPU quota
                    * app cannot scale out
                * Shared
                    * your apps share compute resources with other customers
                    * each app gets a CPU quota
                    * app cannot scale out
                * Dediated compute: Basic, Standard, Premium, PremiumV2, PremiumV3
                    * apps run on dedicated Azure VMs
                * Isolated
                    * Provides network isolation as well as compute isolation


## Azure Active Directory

a tenant == an organisation

Azure roles vs AD roles
    * you can assign a user in your AD an _Azure role_ and/or _Directory roles_
* Devices (windows machines) can join an Azure AD

    How are administrative units diff from groups?
    how do I require mfa for a user?

### Pricing

https://azure.microsoft.com/en-us/pricing/details/active-directory/

* Azure Active Directory comes in four editions
    * Free,
        * The Free edition is included with a subscription of a commercial online service, e.g. Azure, Dynamics 365, Intune, and Power Platform.
    * Office 365 apps,
        * Office 365 subscriptions include the Free edition, but Office 365 E1, E3, E5, F1 and F3 subscriptions also include the features listed under the Office 365 apps column.
        * Same features as free but adds self-service password resets, branding, device objects sync between on-prem AD and Azure AD
    * Premium P1,
        * NZD $9.072 user/month
    * Premium P2.
        * NZD $13.608 user/month
* The Premium editions are available through
    * your Microsoft representative,
    * the Open Volume License Program,
    * the Cloud Solution Providers program.
    * Azure and Office 365 subscribers can also buy Azure Active Directory Premium P1 and P2 online.


### App registrations

* By default, a user with no role can create an app regisration
    * You can turn this off for all non-admins in the AD Tenant _User Settings_
* The azure cli accesses your AD tenant via an app registration
* seem to be similar to the concept in google cloud
* allow a web app (or other kinds) to login a user with oauth2
* the webapp can then use _Microsoft Graph API_ which provides a graph API (JSON over HTTP) to query stuff about a user e.g. their calendar

### Roles

* Every AD tenant comes with 100ish built-in _Directory roles_ you can assign to a user
    * Each built-in role has 1+ role permissions assigned (which you cannot edit)
    * e.g. `microsoft.directory/applications/create`
* Roles can be assigned to users or groups
* The user who created the AD Tenant is a _Global Administrator_
    * Aside: "tenant" seems to mean "instance of"
* The default role a user has when created is `User`
    * they can read info about other users and groups in the Tenant
* You need a Premium P1 or P2 plan to create custom roles

Types of account

1. _Personal Microsoft_ Account (e.g. Skype, Xbox, Outlook.com, Microsoft.com)
2. _Work or School_ account
   * I think this includes Microsoft 365 accounts
   * I think it means "any account in any azure AD tenant"
3. Social identity account (Google, Facebooke etc.)

Tenancy

https://docs.microsoft.com/en-us/azure/active-directory/develop/single-and-multi-tenant-apps

> Azure Active Directory (Azure AD) organizes objects like users and apps
> into groups called tenants. Tenants allow an administrator to set policies on
> the users within the organization and the apps that the organization owns to
> meet their security and operational policies.

An app can be _single tenant_ or _multi tenant_ when you create it

WHen you create an _app registration_ you actually create an application object in the AD
An _applicaiton object_ is the definition of an app - it describes the app to Azure AD
An applicaiton object has a "home AD directory" - only one instance of it exists even if it can be used by multiple directories
* Service Principal object
    * A "service principal" objct is the representation of the application object in another directory -
    * there will be one service principal objec tin each directory that can access the application object
    * it stores the records of local user and group application-role assignments and permissions granted to the app

An app can use Azure AD for roles as well as auth

> by default all users in your directory have rights to register application objects that they are developing and discretion over which applications they share/give access to their organizational data through consent. If a person is the first user in your directory to sign in to an application and grant consent, that will create a service principal in your tenant; otherwise, the consent grant information will be stored on the existing service principal.

https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-how-applications-are-added

A user can be a member of up to 20 directories
MS seem to suggest having a "production" directory and then other directories for pre-prod etc.

> When you add a user from one directory into a new directory, that user is an "external user" in the new directory. Initially, the display name and user name are copied from the user's "home directory" and stamped onto the "external user" resource in the other directory. From then on, those and other properties of the external user object are entirely independent: if you make a change to the user in the home directory, such as changing the user's name, adding a job title, etc. those changes are not propagated to the external user account in the other directory.
>
> The only linkage between the two objects is that the user always authenticates against the home directory. That's why you don't see an option to reset the password or enable multi factor authentication for an external user account: currently the authentication policy of the home directory is the only one that's evaluated when the user signs in.
>
> If a user is deleted in their home directory, the user resource still exists in the other directory. However, the user can't access resources in the other directory since the user can't authenticate to that directory.
>
> A user who is an administrator of multiple directories can manage each of those directories in the management portal. However, other applications such as Office 365 do not currently provide experiences to assign and access services as an external user in another directory. Going forward, we'll provide guidance to developers how their apps can work with users who are members of multiple directories.
>
> https://techcommunity.microsoft.com/t5/azure-active-directory-identity/creating-and-managing-multiple-windows-azure-active-directories/ba-p/243428