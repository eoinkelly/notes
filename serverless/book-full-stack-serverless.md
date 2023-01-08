
Amplify Framework
Cognito for user management
S3 for file storage
DynamoDB for database
AWS AppSync for ???

Backend as a Service

* Managed databases
  * Firebase
  * DynamoDB
* Managed Auth
  * Auth0
  * AWS Cognito
  * Okta

> The term serverless is commonly associated with FaaS. Though you will find
> varying definitions as to what it means, the term has recently grown to
> encompass more of a philosophy than a shared definition.

Things which are 'serverless'

* Decreased operational responsibioities
* Heavy use of managed services

> Why is serverless becoming so popular?
Marketing. Not necessarily a bad thing but let's be real.


Benefits of serverless

* Scalability
* Cost
* Development velocity
* Experimentation
* Security and scalability
* Less code


Frameworks

* Serverless framework
* AWS SAM (Serverless Application Model)
  * seems to be an AWS version of Serverless
* Amplify Framework
  * Parts
    * CLI
    * Client libs
      * For JS, native iOS, native Android, React native
      * these libs simplify interacting with the hosted platform
      * UI components for popular JS libs e.g. Vue, React
    * Toolchain
    * Web hosting platform
  * Uses Cognito for users
  * ?? provides it's own CI
* Vercel
* Cloudflare Workers
* Netlify functions
* Vercel

### AWS AppSync

* Managed API layer providing a GraphQL interface and forwards on requests to various backend services e.g.
  * DynamoDB,
  * Lambdas,
  * HTTP servers
  * OpenSearch
* https://aws.amazon.com/appsync/
* Looks pretty interesting
* Has a pub/sub feature where various services can push notifications into it and it will send them to your browser/mobile clients

This book focuses on Amplify

## Amplify framework

* has a CLI tool
* it looks like the server is easier to commoditize than the client
* which is why pushing more of the logic into a JS app is favoured because it allows you to commoditize the server
* they are trying to remove complexity from the system by pushing it to the client
* I think you should look at the overall system
* is that "write no server code" serverless a bit of a local minimum?