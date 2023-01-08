# SafeStack Secure Dev

Threat actor types

1. Automated attack
  * Scripted
  * Attack known vulnerable technologies
  * They attack the technology rather than the org
  * Traits
    * Noisy and loud (server load, logs noisy)
    * Attack pattern
    * Attack length
      * can be quite sustained
    * Sophistication: often not very
1. Insider
  * People within or associated with the org
  * **These are the most likely attacker for any org**
  * Forms:
    * Intentional
      * Act with malice
    * Unintentional
      * Cause harm by inaction or accidentally
    * Traits
      * very hard to detect (they know a lot about how the systems work)
1. Advanced Persistent threat
  * Traits
    * Sophisticated attackers
    * Bespoke attacks
    * Often detectable only over sustained periods or after breach
    * Period of info gathering
    * can vary in sophistication
1. Lone Gunman/Lone wolf
   * attacker acting along
   * all traits vary depending on the attacker
   * motivation can be personal
2. Organised Crime
  * Traits
    * Motivated by money
    * Can hire experts to
    * Sophistication: varies

Each actor has:

* Motivation
  * Attackers rarely focus on the technology, the tech is just a route to achieving their goal
  * Common
    1. Financial (primary object to get money)
       1. Direct (stealing money)
       1. Indirect (stealing items which can be sold for money e.g. data, things)
    1. Political
      * It doesn't matter if their politics don't make sense to you, only that they will take action based on it
    1. Egotistical
      * Proving they are smarter, more correct etc.
    1. Personal
      * For love or hate
* Resources
	* Money (buy access to devices, people, equipment, skills you don't have)
	* Access to buildings and physical equipment
	* Access to knowledge of how the org works
	* People
  * More resources can mean
    * longer attack
    * more sophisticated attacks
* Skills
  * their ability to act undetected
  * how many mistakes they make


SQL injection


1. Regular SQLi
  * Results are visible on page
2. Blind SQLi
  * get true/false answers from the DB, can eventually get data out via these
  * true = page rendered
  * false = error rendered
3. Super-blind (timed) SQLi
  * get true/false via page load time
  * true = fast, false = slow (or could be vice-versa)