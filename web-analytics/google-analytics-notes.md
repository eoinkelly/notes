# Inbox
http://searchengineland.com/google-puts-a-price-on-privacy-98029
http://24ways.org/2011/getting-the-most-out-of-google-analytics
research how to measxure page load times in GA http://analytics.blogspot.co.nz/2011/05/measure-page-load-time-with-site-speed.html
http://support.google.com/analytics/bin/answer.py?hl=en&answer=1683971#utm_medium=email&utm_source=newsletter&utm_campaign=apr2012&utm_content=Social_text
http://www.google.com/analytics/features/attribution.html#utm_medium=email&utm_source=newsletter&utm_campaign=apr2012&utm_content=Attribution_text
recommended altertative to funnels by avanish guy http://paditrack.com/

Dimension
The visitor or campaign attribute (e.g. source, medium). A good way to distinguish dimensions from metrics is to remember that metrics will always have number values, whereas dimensions are usually text.

Metric
Refers to the values or numbers you select for your report.

You can choose any metric to build your custom report with. You don't have to pair them with dimensions, which means there are no restrictions to which metrics you can use. However when they are paired with dimensions, metrics are subject to the definitions that dimensions define for the dimension-metric combinations. Please see this chart to understand which metric and dimension pairings are possible when designing a custom report.


defn list of metrics: http://support.google.com/googleanalytics/bin/answer.py?hl=en&answer=99118
defn. list of dimensions: http://support.google.com/googleanalytics/bin/answer.py?hl=en&answer=99021
valid combinations of metrics and dimensions: http://support.google.com/googleanalytics/bin/answer.py?hl=en&answer=99174
how time on site is calc'd: http://www.conceptcurry.com/web-analytics/time-spent-on-site-and-page-google-analytics/

Funnel analytics - there seem to be 4 choices.

Google,
Kissmetrics,
MixPanel, or
build your own.

Anything I'm missing? No real-time options?

if you don't know what the outcomes of your website are then no amount of analysis will tell you what to do.
always provide "context" for your data - give the numbers relatvie to a previous timeframe
don't look at a single metric by itself - compare it to the past or compare it to other stuff

NEVER LOOK AT A SINGLE METRIC BY ITSELF in GA
# Account structure
google account eoin.p.kelly@gmail.com
	can contain multiple analytics accounts (each identified by a unique tracking code and domain)
	each "analytics account" can be accessed by multiple google accounts
	1-many google analytics accounts
			an account is a collection of profiles (all use the same tracking code)
		1-many website profiles
			a profile is a group of rules & settings that define the reports I see
			can have multiple profiles for a domain that use the same tracking code - they just filter & display the data differently

# Google Conversion University
I can have max 25 analytics accounts in a single google account

GA profile is just a set of
	1. user access
	1. goals
	1. filter settings
that define which data appears in the reports

	a profile
	Interpreting the code I embed: UA-<analytics acc no.>-<property no.>
	all profiles that track the same domain will have the same property no.
	profiles that have the same account and property no (i.e. they track the same domain) are called duplicate profiles
		you create duplicate profiels to contol user access to reports & goals

GA Goal
	An activity or level of interaction that represents value to the business greater than a page view
	3 types
		url destination goal
		time on site goal
		pages/visit goal
a goal conversion can only happen once per visit
ecommerce transactions can occur multiple times per visit so if you might have multiple goal conversions in a visit use this!

Filters
	keep one unfiltered profile as it's not possible to "unfilter" afterwards
	2 types of filter
		predefined
		custom
	if more than one filter is applied to a profile they are executed sequentially in the order theya ppear in GA

Cookies
	1st party: set by the site you are visiting
		allowed by most people
		GA sets 4 persistent & 1 temporary 1st party cookies
			__utma: expires 2 years
				content is: <domain hash>.<random unique id>.<time of initial visit>.<beginning of previous session>.<beginning of ucrrent session>.<session counter>
					<random unique id>.<time of initial visit> = visitor ID

			__utmb: expires after 30 mins of inactivity
				content: <domain hash>
			__utmc: session identifier: termorary cookie, destoryed when you quit browser
				content: <domain hash>
			__utmz: campaign values: expires after 6 months
			__utmv| visitor segmentation (only set if setvar method called) | exprires 2 years
				content: <domain hash>.<value you set via setvar>
		*You can change the timeout by modifying the page tracking code*

	3rd party: set by partners of sites you visit
	can be set with/without expiration date
	persistent cookie
		lives beyond browser session
		can be read by the website when you re-open the browser

	temporary cookie
		deleted when browser session ends

The googly guys says
	10/90 rule - if you have $100 you should spend $10 on the tools and $90 on people to interpret it and extract value
	the web is very complex so you need to invest in people who understand your business and the data
	reporting is not analysis
		reporting = providing data
		analysis = providing insights
		you don't gejt insights just by implementing an analytics tool

	avoid the data quality trap - you data will never be perfect - "the web has been deliberately built to screw with you"
	ads in magazine is a "faith based initiative"
		don't argue about your % level of confidence in the data - spend your time making a decision
		"an educated mistake is better than no action at all" - stuart godin
		"decision making is a journey, not a destination"
	experiment or go home!
	HIPPO = highest paid person's opinion.
	learn to be wrong. quickly.

other tools
	omniture
	web trends
	coremetrics?
	piviot?

$ Index = (revenue + goal value) / unique views of page before conversion
	it is most useful as a comparison metric rather than a stand-alone number

"average time on page" does not count bounce (0 sec) page views
"average time on site" does count bounces

search engine traffic can be "organic" = free or it can be "paid"







# Advanced Web Metrics with Google Analytics book

## Chapter 1
A bounced visitor
	a visitor who views a single page on your website and has no further actions

Goal conversion
	a desired aciton on the site defined as being more valuable than a pageview e.g. purchase confirmation, download confirmation, newsletter registration
	a goal is pretty much anything that happens on your site that is more valuable than a pageview

session timeout is 30 mins

funnel
	a well defined process (usually pages) leading to a goal conversion

medium
	the means by which the visitor to your site got the link
		organic
		cost-per-click
		email
		pdf

offsite vs onsite metrics
	offsite
		measure size of potential audience (opportunity) and your share of voice(visibility) and the buzz (comments/sentiment) happening on the Internet
		measures the potential no. of customers out there
		measures the bigger picture of how your website compares to others
		examples
			google search insights
			microsoft adlab

	onsite
		measures the visitors journey on your site and its performance
		measures what happens when they start to become a customer
		examples
			google analytics
			yahoo web analytics

offsite and onsite metrics will rarely match-up - they measure different things

basic stuff to measure
	how many daily visitors you get
	your average conversion rate (% of those visitors who "converted" (whatever you have defined that to mean)
	average time on site
	how often they come back
	average visit page depth and how this varies by referer
	how "sticky" your pages are - how likely are they to bounce off them?

more complex questions
	what is the value of a visitor and how does this vary depending on where they come from?
	how do existing customers use the site compared to new members?
	how do visits/conversions vary by referer type or campaign source
	how does the bounce rate vary by page viewed or referring source
	is the site engaging visitors
	is the internal site search helping or hindering conversions
	how many visits and how much time does it take for a visitor to become a customer

average conversion rates are approx 2-3% http://index.fireclick.com/
but others say you can get up to 12% conversion rate for online stuff: http://www.clickz.com/clickz/column/1718099/the-average-conversion-rate-is-it-myth
	amazon converts 9.6%

analytics is only worth looking at if you intend to do something with what you find - or at least put it in front of the people who can do something
web analytics do not tell you "why" things happen
GA is free but implementing, analyzing, interpreting and making website changes all require resources

analyzing your web analytics reports is simi- lar to visiting the gym. Unless
you go regularly, don’t waste your time there, because you will only become
frustrated at the little impact made from previous sessions.

you are trying to understand the value of the website in monetary terms either directly as an e-commerce site or indirectly from lead gener
identifying goals is the most step of building website - it enables you to define success



## CHAPTER 2
data collection can be on the server or via page tags (on the client)
analytics software that includes both is called "hybrid"

page tags
	advantages
		breaks through proxy & caching servers
		tracks client-side events
		captuers client side ecommerce data
		collects data in nearly real time
		the hard work is done by the vendor

	disadvantages
		setup errors lead to data loss
		firewalls can mangle or restrict tags
		cannot track bandwidth or completed downloads - tags are set when the file is requested
		cannot track search engine spiders - robots ignore page tags

logfile analysis software
	advantages
		historical data can be reprocessed easily
		no firewall issue
		can differentiate between completed and partial downloads
		tracks search engine spiders and robots by default
		tracks visitors w/o javascript e.g. legacy mobile visitors by default

	disadvantages
		proxy & caching inaccuracies
		no event tracking
		requires you to do program updates, data storage, archiving (the hard work)
		robots multiply visit counts
maximum cookie size is 4k

how do you check that all your pages have GA enabled???
large sites rarely have 100% of their pages tagged

Javascript errors before the GA code will stop it loading

users owning and sharing multiple computers can make analytics data inaccurate
I am the same person even if browser on laptop or iphone
I might share my home computer with many different people
cookies are also often deleted deliberately


things that affect the number of unique visitors
	people deleting cookies
	sharing computers
	using multiple computers

you can try to estimate what the error factor is here by comparing with simlar sites that require a user login - 30% is true for some large online sites but be wary of generalising

all analytics vendors use different metrics to compute visitors from visits - usually it's activity within 60 mins. also the GA cookies usuaslly expire after 6 months so you won't see any repeat visits outside that

tracking "unique visitors" is really difficult and inaccurate with current web analytics tech - don't use it as a KPI - at best focus on the trend rather than the absolute number
cookies get regularly deleted so comparing year on year is very error prone

I need to do a page tag audit? automated way?
