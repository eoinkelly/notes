# Go Game

## Tutorials

* http://playgo.to/iwtg/en/
* http://www.britgo.org/intro/intro.html
* http://www.gokgs.com/tutorial/

* board is 19x19 or 13x13, 9x9 traditionally
* black moves first
* stones put on the intersection of the squares

## Terminology

* Client: lets a human play against something
* Engine: a computer program that knows how to play


## Go Text Protocol (GTP)

* version 2 is the one in common use now
* spec: http://www.lysator.liu.se/~gunnar/gtp/

## Servers

Long game servers

* Dragon go server (DGS)
    * not realtime (go games "drag-on" on this server)
    * sort of an improvement on playing go by email

Real time servers

* KGS 
    * http://www.gokgs.com/
    * has cgoban java client
        * client is super ugly
        * integrates chatrooms/irc alike features
* IGS (Pandanet)
    * http://pandanet-igs.com/communities/pandanet
    * aka PandaGo
    * has a client built with node-webkit
        * client is super ugly
        * integrates chatrooms/irc alike features
