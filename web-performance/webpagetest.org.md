# webpagetest.org

5 kinds of tests available:

1. Site performance
2. Core web vitals
3. Lighthouse
4. Visual comparison
5. Traceroute
    * give it a hostname/IP and it runs a traceroute from one of their available locations (AWS & Azure data centers around the world)
    * repeats the traceroute N times


* paid plans are reasonable
* has npm CLI package

* Locations to run the test in
  * many AWS and Azure data centers available
* Browsers
  * Chrome (all release channels)
  * Firefox (all release channels)
  * Brave
  * ?? safari maybe only on paid plan?
  * Mobile devices available through Chrome emulation
* it repeats the run N times
  * results shown are based on the "median run" - presumably the media
* You can
  * simulate failure of certain domains
  * inject HTTP headers and scripts
  * block the browser from loading certain scripts and domains
  * run a lighthouse audit as part of the test
  * capture low level chrome profiling data
  * capture network packets via tcpdump
  * set a custom user agent string
  * save response bodies
  * ignore TLS errors