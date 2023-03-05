# Web performance

> When you want to be fast you have to give up the things which are keeping you slow

Human perception research:

- 1-100ms feels instant
- 300-1000ms feels sluggish
- 1sec+ feels slow
- 10sec+ feels terrible

So we care most about getting to the 0-1000ms range

Example performance budget:

- 1s page load (showing the above the fold content)
- Speed index under 1000
- Max 200mx server response time

## Tools

-   http://www.webpagetest.org/
- Pagespeed insights (uses CRUX data under the hood)
-   https://developers.google.com/speed/pagespeed/insights/
  - is good baseline but 1 month out of date
  - is a generalisation of what I get in lighthouse
- Google search console
  - uses CRUX data under the hood too