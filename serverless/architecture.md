

Key questions

How will you route requests to the function that serves them?
How many kinds of request will each function serve?
How will caching work?

Keep in mind

* Functions used less often may have cold start issues
* It


Terminolgy

* Two common flavours
    * Backend as a service
        * Examples
            * AWS Cognito
            * Google Firebase
            * Auth0
    * Function as a service
        * Examples
            * AWS Lambda
Sources

* https://www.serverless.com/blog/serverless-architecture-code-patterns
* https://martinfowler.com/articles/serverless.html

Questions

* What are good auth patterns in FaaS?
    * Idea:
        * client hits an "auth service" (made up of 1+ functions) which signs them in and gives them a cookie
            * this auth service could be a BaaS
        * client presents the cookie to the other "do/get things" functions

Case Study: BBC

* seem to use a single _index.php style_ lambda as entry point
* which dispatches to other lambdas to build other parts of the page
* redis used for caching throughout
* front-end traffic not via API Gateway but via EC2 instances running varnish/nginx/etc.

Pros

* Scale to 0 is easy
* Naturally handles very spiky traffic - can be made to scale really well
    * Scaling isn't free because there will be other parts of the architecture which won't be so elastic e.g. database
* If you have multiple dev teams then, assuming the architecture supports it, they _may_ be able to overlap less and deploy more easily

Cons

* Security can be more complex e.g. every lambda has an IAM policy which can be too broad
* Packaging complexity is a lot higher
* Deployment complexity is a lot higher
* Deployment speed could go either way
    * Could be faster if you have many teams contending for a monolith
    * But in general will probably be slower
* Dependency management will be more complex
    * Examples
        * Security patch for a lib has to be deployed into every function which uses it
* You need distributed monitoring, can't really rely on just reading the log file anymore


Unclears

* Cost may or may not be better than a trad architecture - depends very much on use case.

Cost

* EC2 instances need to have quite a bit of head room anyway (e.g. 20% utilisation as a target) so the extra cost of serverless per request can be balanced against not paying for that headroom
