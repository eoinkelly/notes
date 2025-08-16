# Quality of service

- CIR = Committed information rate
    - the bandwidth commited to work under normal circumstances
    - usually expressed in kbps
    - the bandwidth below which available bandwidth should not fall
- CDR = Commited data rate
    - the payload part of the CIR
- PIR = Peak information rate
    - the absolute value of the allowed bandwidth
- EIR = Excess information rate
    - the allowance of burstable bandwidht above CIR
- CBS = commited buffer size
- MBS = maximum buffer size
    - as a percentage of the buffer pool

```
CIR + EIR = PIR
```
