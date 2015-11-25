# Payment gateways

Important features of a payment gateway

1. Authorize
    * validate the credit card and reserve the money for later collection
1. Capture
    * references a previous Authorize step and requests that the money be drawn down
    * some areas require that you not capture until the goods have been shipped
1. Purchase
    * Authorize + Capture in a single transaction
1. Void
    * Entirely void a transaction
1. Credit
    * Refund money to a card
    * Not all gateways support this
    * Those that do may have to explicitly eanble it for your account
    * typically hard to get permission to do this (void is more usual)
1. Recurring
1. Card Store
  * tokenizes a payment method in a gateway's vault
  * often has 3 endpoints
    1. store
    1. unstore
    1. update

AM makes distinction between a "Gateway" and an "Integration"

* gateway
    * ecomm app uses payment provider to process the payment but they hold all the details
    * ecomm app holds CC details so needs PCI compliance
* integration e.g. paypal
    1. ecomm app redirects user to the provider with details of payment
    1. user authenticates with provider and makes the payment
    1. provider will often hit a known callback URL on the ecomm app to verify that the payment details are correct
    1. provider sends user back to ecomm iste
* hybrid
    * tries to avoid having the ecomm app knowing payment details but retain the flexibility of a gateway

Ideally gateways have a "verify" endpoint that allows us to verify the card with a 0 dollar transaciton. If the gateway doesn't support it it is supported via an authorize and immediate void

Braintree provides only "integrations" because it does not expose an API

## Luhn mod 10 algorithm

* a checksum algorithm
* not cryptographically secure
* can find _accidental_ errors in credit card numbers
* can find
  * any single digit errors
  * transposition of adjacent digits
It is 16 numeric characters and conforms to a Luhn "mod 10" algorithm

