# MQTT

Concepts

* https://en.wikipedia.org/wiki/MQTT
* Message Queue Telemetry Transport
* Developed by IBM
* Uses hub/spoke model
* version 3.11 has become a Oasis standard
* Publish/subscribe
    * you publish to a named topic
    * you subscribe to a named topic
* Topics
    * The way you register interest in a
    * Represented as strings separated by `/` - each section is called a "topic level" e.g. `home/lounge/lamp`
    * are case sensitive
* Broker
    * receives all messages
    * filters messages
    * publish received messages to subscribed clients
    * Popular borkers
        * mosquitto
* Messages
    1. Command
    1. Data
* 8 byte header with an arbitrary payload
* PRAM consistent i.e. garuantees in-order delivery per publisher
    * this lets you use it for chat (messages will arrive in order)
* Runs on multiple transport
	* TLS
	* TCP
	* Websockets
	* v5 will support more transports
* Topics can be used as implicit key-value storage
* Built-in QoS quality of service
	* QoS 0
		* fire and forget
		* if the device is online it will receive it if not it will be thrown away
		* at most once
	* QoS 1
		* At least once
        * garuanteed delivery **at the protocol level**
	* QoS 2
		* Exactly once
        * garuanteed delivery **at the protocol level**
        * two phase commit
* Supports "persistent session"
    * Ideal for intermittent connectivity (sessions may last weeks/months/years)
    * Automatic keepalive messages
    * QoS 1 and 2 messages are queued for clients which may be offline but have not timed out
* Birth message
        * a message the client sends when it connects
* Supports a "last will and testement" message
    * Lets a client register a message that the broker should send to a particular topic if that client disconnects without a DISCONNECT message i.e. an unintentional disconnect
* Supports "retained message"
    * A message stored by the broker against a topic
    * The message will be sent to each n* Supports "retained message"
        * A message stored by the broker against a topic
        * The message will be sent to each new client which subscribes to that topic
* Facebook messenger uses MQTT to some extent
* There are 14 defined message types used to achieve the following 6 goals
    1. connect a client to a broker
    2. disconnect a client from a broker
    3. to publish data,
    4. acknowledge receipt of data,
    5. supervise the connection between client and server.
* MQTT sends connection credentials in plain text format and does not include any measures for security or authentication

Message anatomy

1. Message
1. Topic
1. QoS

* When clients connect to a broker they provide a "Client ID"
* The broker tells the client how frequently to send its keepalive messages (default is 60 sec)


    How much queueing will an MQTT broker do?
    Does it set the "Retain" flag so taht the borker will hold onto it?
    WHich QoS does it use?

