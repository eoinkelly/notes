
## How to set up a baseline of site performance

### Core web vitals

* Lab testing
  * Lighthouse in dev tools
    * reports on web vitals in a "lab setting" (my laptop artificially set up to mimic other users bandwidth, CPU etc.)
* Field testing
  * Chrome web vitals extension
    * technically field testing but only testing my setup
    * Reports on web vitals using my CPU and network and browser (not necessarily matching other users)
* Field & Lab
  * page speed insights
    * https://pagespeed.web.dev/
    * you plug in a URL and it creates a report similar to what lighthouse dev tools does
    * shows a mix of lab and field data
      * field data
        * 28 day sampel from CRUX report of the core web vital metrics
        * PSI gets CRUX data daily. if you use bigquery on CRUX you get data monthly
      * lab data
        * it runs a lighthouse report for you (seems similar to what dev tools would do
        * > simulates the page load conditions of a mid-tier device (Moto G4) device on a mobile network for mobile, and an emulated-desktop with a wired connection for desktop.
* TODO
  * Core web vitals report in search console

### webpagetest.org

* paid plans are reasonable
* has npm CLI package


## Which of the vitals do you care about most?

Which improvements should the site prioritise?
Maybe site is fine in some already?

## Tools

* http://www.webpagetest.org/
* https://developers.google.com/speed/pagespeed/insights/
* https://github.com/Huddle/PhantomCSS
* https://github.com/giakki/uncss
* https://github.com/tkadlec/grunt-perfbudget
    * uses webpagetest API to check your current perf
* https://github.com/addyosmani/psi
    * command line tool for page speed insights
* https://github.com/macbre/phantomas
    * makes nice graphs in a local page of your perf
* https://github.com/t32k/stylestats
    * web page that analyises your CSS
* https://github.com/katiefenn/parker
    * css analysis tool

Resources

* https://www.youtube.com/watch?v=FEs2jgZBaQA
    * Addy Osmani video


