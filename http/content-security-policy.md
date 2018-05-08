# Content security policy

Sources
* https://content-security-policy.com/

Basics

* a single HTTP header `Content-Security-Policy`
    * there were other headers in the past but they are deprecated now
* two versions of the spec:
    * CSP 1.0 (all browsers except IE)
        * https://caniuse.com/#feat=contentsecuritypolicy
		* IE 11 supports the `X-Content-Security-Policy` header but using that header with the standard one causes bugs in other browsers so is not recommended
    * CSP Level 2 (all recent browsers (Firefox had a niggle)
        * https://caniuse.com/#feat=contentsecuritypolicy2
		* lets you whitelist scripts with a hash in the header

```
Content-Security-Policy: {directive}; {directive};
```

* header value is one or more directives separated by `;`
* there are 15 directives documented in the 1.0 spec
* A single directive can have multiple sources
* If a directive has multiple sources, they are ??? together to make the rule


* Never add `unsafe-inline` to the `script-src` directive - it pretty much defeats the purpose of CSP
	* this means Google Analytics and friends will not work in their default configuration

To get Google Analytics working

1. Move it to a separate script file and load it into HTML via `<script src="whatever/ga.js"></script>` tag
2. Set `Content-Security-Policy: default-src 'none';script-src 'self' www.google-analytics.com;img-src www.google-analytics.com;`


Sources

* `'self'` (the single quotes are a required part of the value)
	* allows loading resources from the same origin (same scheme, same host, same port)
		* note that this does not include path!
* `https:` (the colon is part of the value)
	* allows loading resources over HTTPS on any domain i.e. it locks down the URL scheme but nothing else

## CSP 2.0 Spec

> Individual inline scripts and stylesheets may be whitelisted via nonces and hashes


Terms

* Security policy
	* the HTTP header value as a whole creates a policy
	* contains a set of "security policy directives"
* Security policy directive
	* declares restrictions for a particular resource type e.g. `script-src`
	* each directive has a name and a value

Security policies are applied by user-agents (browsers) to a specific "resource representation" known as a "protected resource"


Policy delivery can happen in two ways:

1. Via one of two HTTP headers:
	* Enforce the policy:
		```
		Content-Security-Policy: script-src 'self'
	  	```
	* Warn about violations but do not enforce:
		```
		Content-Security-Policy-Report-Only: script-src 'self'
	  	```
1. Via HTML meta tag
	* `<meta http-equiv="Content-Security-Policy" content="script-src 'self'">`




If not specified explicitly in the policy, the directives listed below will use the `default-src` sources as their source list.

	child-src
	connect-src
	font-src
	img-src
	media-src
	object-src
	script-src
	style-src
















