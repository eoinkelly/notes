# Products

New relic offer 7 productions for monitoring your appliations

1. APM
    - classic app monitoring
2. Servers
    - basic server monitoring: cpu usage, uptime, bandwidth usage, disk i/o etc.
      ("activity monitor" for server)
    - currently free
3. Mobile
    - mobile app "monitoring"
    - mobile crash reporting
        - includes stack traces
    - tag certain interactions as special and monitor those more closely
    - monitor what resrouces apps are consuming
    - monitor network times, timeouts etc.
4. Insights
    - Seems like a competitor to google analytics
    - NR do have a lot of info
    - uses the existing agents that run on server or in app to send data to
      "insights"
    - Can send it custom events ala google analytics
    - gives you graphs and a "SQL esque" query language for generating reports
5. Browser
    - link a JS file in head
    - gives you similar stats you see on server side e.g. ajax calls, page load
      times
    - gives stats similar to those provided by browser dev tools
    - overlaps with google analytics a bit: tells you geography, browser
      versions etc.
6. Platform
    - plugins for a bunch of services that sends their logging and perf data to
      new relic
    - e.g. postgres, mysql, sendgrid, couchdb, redis
    - the ideas is that you can see data from all those other sources in the
      same dashboard as everything else
    - reading some of the comments on plugins it seems many are not well
      maintained
7. Synthetics
    - a world-wide colleciton of boxes that can run selenium tests against your
      site
    - can do basic uptime checks as well as scripted work flows (e.g. user login
      and purchase item)
    - you get req/response headers, waterfall of assets, error screenshots to
      diagnose what went wrong
    - also get integration with APM so you can tie it up with the server POV
    - the data from synthetics is also fed into "insights" product
    - so this overlaps with with what services like Pingdom do

# Appdex score

You set 3 time durations for the app in settings:

T = satisfying _ default: 0.0 - 0.5 sec t = tolerating _ default: 0.5 - 2.0 sec
f = frustrating \* default: 2.0+ sec

and new relic gives you a graph of how your requests have performed.

You can define "key transactions" which have unique values for (T,t,f) and these
will contribute to the graph based on these values.
