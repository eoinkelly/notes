# AWS reserved instances

- can include a capacity reserveation
- be be scoped to an AZ
    - causes AWS to reserve the capacity for the instance types in the RI within
      the AZ
- can be scoped to a region
    - automatically applies the discount to instances in all AZs and instance
      sizes in the region

THere are 3 types:

1. Standard RI
    - best suited to steady-state usage
    - provides the biggest discounts
    - can be sold on the _Reserved instance marketplace_ if you don't need them
      anymore
    - has _instance size flexibility_ so the discount applies to any size of
      server **within the same family** e.g. within the T family you can combine
      nano's to make a large but you can't use them for an M instance
        - only applies if you are running linux (not windows)
        - only applies if you have default tenancy
        - details:
          https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/apply_ri.html
1. Convertible RI
    - best suited to steady-state usage
    - smaller discount than _standard RI_
    - allows you to change the attributes of the RI but you cannot decrease the
      value of the RI (ratchets up)
1. Scheduled RI
    - available to launch within the time windows you reserve
    - allows you to match capacity reservation to your schedule

All types

- can be shared between multiple accounts within a consolidated billing "family"
- available as 1yr or 3yr reservations
- available with with 3 levels of up-front payment:
    1. none
    1. partial
    1. all
