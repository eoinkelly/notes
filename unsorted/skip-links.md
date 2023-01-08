				Skip-links: http://webaim.org/techniques/skipnav/
					These allow screen users/text browsers to skip the whole navigation section and get straight to the content of the page
					They should be as close to the top as possible. Ideally they should be the first links on the page but at least before your navigation.
				When screen-readers/text-browsers are updated to understand the HTML 5 nav element, we don't need skip-links any more.
				If you decide to put your main content before your nav in source order, you might need a "skip to nav" link which makes that approach a bit pointless

				CSS for skip-links
				There are 2 approaches
					1. Hide link until it gets keyboard focus (a few browser issues ith this)
					2. Hide link from all but screen-readers
				CSS Hiding Options
					Hide from everything:
						display: none;
						visibility: hidden;
						width: 0px; height: 0px; (if element has 0px dimensions it is removed from normal flow, same as the 2 methods above)

					Hide but make available to screen readers
						* text-indent: -1000px; (works but might leave a focus indicator (marching ants) extending from where the element is to the -1000px point)
						* absolute positioning off-screen (best solution):
							.hidden {
								// Remove the content from page flow
								position:absolute;
								// and position it 10000 pixels to the left
								left:-10000px;
								// if we don't specify a top dimension some browsers might ignore the left dimension
								top:auto;

								// Make it 1px x 1px and hide anything that overflows that. This is probably overkill but hey...
								width:1px;
								height:1px;
								overflow:hidden;
							}

					WP TwentyEleven theme doesn't style .skip-link but does style .assistive-text
					Multiple sets of skip-links is generally accepted to be a bad idea.

					FIXME what is the impact of skip-links on SEO?

					Article: http://www.webnauts.net/skip-to-main-content.html
						he says that skip-links should be after the logo so that users know where they are before skipping to the content of where they are

						The WAI ARIA specification defines a set of specialised “landmark” roles. These roles provide a method to programmatically identify commonly found sections of web page content in a consistent way.
						Simply add a role attribute to a container element, using the most appropriate role value for the content of the container, for example:

						<div class="content" role="main">
						Landmark roles are currently supported In JAWS version 10 screen reader, NVDA 2010.1 and VoiceOver on iPhone IOS4.

						The new sectioning elements in HTML5 have some overlap
						with ARIA landmark roles, but in a majority of of cases
						there is no equivalent for the ARIA landmark roles in
						HTML5. It is suggested that where there is a similarity
						the ARIA roles can be used to provide semantic
						identification that has a practical use now, for
						example if you want to use the HTML5 nav element, add
						role="navigation" to it, so supporting Assistive
						Technology (AT) can convey the semantic information to
						users. When HTML5 elements such as nav are supported by
						AT, you can then remove the role as it will no longer
						be required.

						<nav role="navigation">

						List of roles (role = "<role>")
						role="search"
						role"complementary" (<aside>)
						role="navigation"
						role="main"
						role="form"
						role="contentinfo"
						role="application"
						role="banner" (<header>)

						which roles should I use regularly?
						More on live regions http://juicystudio.com/article/wai-aria-live-regions.php (handling AJAX changes for screen readers)

						wp_nav_menu() generates:
						<div class="menu-{menu-slug}-container">
						<ul id="menu-{menu-slug}" class="{menu_class}">
							<li id="menu-item-{id-num}" class="menu-item menu-tiem-type-post_type menu-item-object-page current-menu-item page-itme page-item-{page-id} current-page-item menu-item-{id-num}"
								<a href="<url>">{title}</a>
							</li>
							...
						</ul>