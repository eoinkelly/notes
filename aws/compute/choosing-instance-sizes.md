# How to choose the right EC2 instance size

* According to AWS, the tX and mX sizes are "General purpose"

Questions

QUESTION: Do we care about "EC2 Networking performance" for rails app instances?
  low vs moderate vs high?

QUESTION: What is the appropriate instance size for a given no. of reqs to a rails app?

QUESTION: how does tX and mX compare on cost?

### ECU (EC2 compute unit)

* a way of comparing CPU power across instances

    One EC2 compute unit ~= CPU capacity of a 1.0-1.2 GHz 2007 Opteron or 2007 Xeon processor

* tX instances have variable ECUs because they can burst
* mX instances have approximately 3 ECU per vCPU (ECUs scale directly with the number of vCPUs)
