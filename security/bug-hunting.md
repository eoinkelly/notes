# Bug Hunting

https://www.youtube.com/watch?v=-FAjxUOKbdI

Pentesting

- looks for common-ish vuln
- incentivised based on number of vulns
- racing the clock (pen test will be for a fixed duration)

Bug bounties

- paid based on impact not number of vulns
- incentivised to find unique vulns
- racing other testers

Recommended books for web app testing

- OWASP testing guide
- Web application hackers handbook 2

To succeed at bounties you need to find the "road less travelled"

Consider testing `acme.com`

1. Find subdomains `*.acme.com` scope is your friend
1. Find other domains they own
    - via Google
    - automate via `recon-ng` or other tools
1. Port scan for obscure web services on all domains you find
   `nmap -sS -A -PN -p- --script=http-title acme.com` (I might have typos in
   that command) _ SYN scan _ OS + service fingerprint _ no ping _ all ports \*
   http titles
1. Find aquisitinos the company might have made recently (note bounties will
   have rules about this)
    - you can find info in wikipedia about this
1. Map the site
    - Directory brute forcing (unlinked content discovery)
    - Mapping tools
        - Google
        - https://github.com/danielmiessler/SecLists/tree/master/Discovery/DNS
            - Raft lists (made by spidering robots.txt files on the net)
            - SVN Digger
            - Git Digger
    - Platform identifier
        1. Use Tools to find what platform/frameworks/libs are in use
            - Wappalyzer
            - Builtwith
            - retire.js https://retirejs.github.io/retire.js/
        1. Check for CVEs in those
        1. If you find CMSes use
            - https://wpscan.org/
            - https://github.com/Dionach/CMSmap
1. Look for functionality changes or re-designs (places which might not have
   been well tested yet)
1. Mobile websites
1. New mobile app versions
1. Look for existing known vulns which might not have already been reported
   using OSINT
    - examples
        - xssed.com
        - redit /r/xss
        - Punkspider
        - xss.cs
        - xssposed.org
        - twitter searching
    - Even if these issues have already been reported they might indicate where
      some flaws are

## Tools

- https://github.com/jhaddix/domain
    - wrapper around `recon-ng` and `Alt-DNS` to make it faster to find
      subdomains of a given domain
