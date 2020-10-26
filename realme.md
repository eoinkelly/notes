# RealMe

* Saml2 service

RealMe is actually two separate SAML2 services

1. RealMe Login Service
    * https://developers.realme.govt.nz/how-realme-works/how-realme-works-2/
    * Available to govt but not businesses
    * Logins are pseudonoymyus - RealMe returns just a 35 char _Federated Logon Tag_ which is unique to the tuple `(user, service-they-are-logging-on-to)`
        * It does not return any other info about the user.
        * Privacy perserving
    * Only does authentication, your app has to do authorization itself
2. RealMe Assertion Service (also called _RealMe Verified_ in marketing)
    * https://developers.realme.govt.nz/how-realme-works/how-realme-works-4/
    * currently offers verified info about:
        * name
        * date and place of birth
        * gender
        * verified residential address
    * id's users via a _Federated Identity Tag_
    * available to organisations in the wider government sector and to approved organisations in the private sector such as financial institutions
    * when this service gets a request it will play the SP role to the Login Services IDP and log the user in there. then it will get the successful login response and decorate it with verified attributes before returning it to our app

RealMe Verified (you got your photo taken at post office)



Message testing service

* a sandbox environment where you can prove your solution works before doing the actual onboarding to the uat and prod versions of RealMe
