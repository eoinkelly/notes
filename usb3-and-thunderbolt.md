# USB 3 vs Thunderbolt 3 vs wtf

* There is now one **cable** with "USB Type C" connectors on either end which can carry any of these data protocols:
    1. USB 3.1 Gen 1 - 5 Gbps
    1. USB 3.1 Gen 2 - 10 Gbps
    1. Thunderbolt 3 - 20-40 Gbps (depending on how many PCIe lanes are dedicated to the port by the device)
* On the Late 2016 Macbook pro 13" with 4 Thunderbolt ports, only two of them are full speed (two are 20Gbp/s, two are 40Gbp/s)
* _USB 3.0_ is almost identical to _USB 3.1 Gen 1_ - they have identical transfer rate of 5 Gbps
* USB Type C connectors supports power delivery up to 100W (I think - check this)
* A connector port can be identified with the tuple (usb-data-protocol, type, format)
* The "type" refers to the physicality (shape of the connector on the end of the cable)
    * Type + format defines the exact shape of the connector
    * Type + format defines the shape of "the pipe" while USB-{1,2,3} defines a data protocol which can flow through the pipe
* Each type can have many "format"
* Examples
    ```
    # This is incomplete, TODO
    USB 1
        Type: USB Type A
            Format: Regular
            Format: Micro
            Format: Mini
        Type: USB Type B
            Format: Regular
            Format: Micro
            Format: Mini
    USB 2
        Type: USB Type A
            Format: Regular
            Format: Micro
            Format: Mini
        Type: USB Type B
            Format: Regular
            Format: Micro
            Format: Mini
    USB 3.0
        ???
    USB 3.1 Gen 1 (aka USB 3.0)
        Type: USB Type C
            (Only one format)
    USB 3.1 Gen 2
        Type: USB Type C
            (Only one format)
    ```

TODO: I think Type B differs across USB variations
