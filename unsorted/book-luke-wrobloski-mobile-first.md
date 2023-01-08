

What "defines" the mobile context

	Partial Attention ("One eyeball and one thumb")
		we need clear focussed designs
		user mistakes are more likely
		user confusion is more likely
	Latency
	Bandwidth
	Sensor Access
	Camera Access

We still need to stick to the basics of IA
	balance breadth & depth
	clear labeling
	appropriate mental models

3 types of mobile behaviour:

* Lookup/find (urgent now)
* Explore/plan (bored)
* Checkin/status (urgent change)

Placement of navigation
=======================
Content should take precedence over navigation on mobile
Keep nav concise and out of the way
Put nav underneath content on the  page
Each page can also have a "related nav" contextual list


LW believes sofware "back" button is unnecessary as devices have them already - we should conside a label other than "back"

"explore and pivot"

Touch targets
==================
* Make touch appropriate affordances e.g make touch targets big enough
* Touch targes specified in pt/mm to avoid confusion caused by different pixel density screens
* 70%-90% users right-handed => can optimize for right-handed thumb
* Touch gestures have no affordances but we can add visual hints

Natural User Interface (direct manipulation of objects) Vs GUI (Windows, Icons, Menus, Pointers (WIMP))
Set expectation for guestures: "Does it look swipeable?" If it does, it should swipe

Rethink "hover". If it important enough to show users shouldn't it already be on the screen?
Swipe gestures are less discoverable than tap
Non touch screen devices can use :hover, :focus to show user where they are
I think this emans that :hover state should alwasy be same as :focus?

Labels
=============
Top algined form leabels work best on mobile
Labels should stay visible when virtual keyboard in on screen
We can use HTML5 "placeholder" attribute to put lables *in* the form fields but
	placeholders must be visually distinct from the answer typed by the user
	are not available when user starts to type - will that confuse the user?
Use smart defaults on all forms - we can control autocapitalize and autocorrect on mobile forms

Input Masks
================

* http://www.webresourcesdepot.com/javascript-input-masks/
* https://github.com/digitalBush/jquery.maskedinput
* http://www.lukew.com/ff/entry.asp?756 (good example videos here)
	* show all the formatting up front to not confuse the user

Defining device experience:
	common user posture + primary input method + average display size = device experience

Reading List
===============

* Apple HIG
* Microsoft phone guidelines:
* Book recommend: Tapworthy by JOsh Clark
* Book: LW Touch gesture reference guide
* LW Form Design book
