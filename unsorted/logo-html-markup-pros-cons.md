	Logo Markup
	===========

	Method 1: <h1> with a background style (Image replacement)
	---------------------------

	<a href="/" rel="home"> <h1 id="site-title" class="site-title ir"><?php bloginfo( 'name' ); ?></h1> </a>

	.site-title {
		background-image: url('path/to/image.jpg');
	}
	.ir {
		display: block : Without this, image-replacement won't work on inline elements (e.g. span)
		border: 0 : Remove the default border from elements like button
		text-indent: -999em : Hide the text off-screen
		overflow: hidden : Clip the text and prevent the focus outline from extending off-screen in some browsers.
		background-color: transparent : Hide the default background color on elements like button
		background-repeat: no-repeat : Prevent tiling of the background image
		text-align: left : Make sure the text is left aligned for the negative text-indent to work
		direction: ltr : Avoid problems with the off-screen text in rtl settings
		*line-height: 0 : hide text of submit inputs in ie6/7
		.ir br { display: none; } : hide line breaks within the element as these mess up the off-screen positioning of the text
	}

	Pros/Cons
	+	the text that users who can't see images see is in H1 tags to indicate it is important
	+	the logo is clickable
	=	the user can't right-click on the logo to download
	-	the user who has CSS turned on will not see the text
	-	It might be seen by google as a trick; See http://www.youtube.com/watch?v=fBLvn_WkDJ4
	-	http://luigimontanez.com/2010/stop-using-text-indent-css-trick/
	-	http://maileohye.com/html-text-indent-not-messing-up-your-rankings/

	Method 2: Inline image
	----------------------

	<a href="/" rel="home">
		<img src="foo.jpg" alt="<?php bloginfo('name'); ?>">
		<!--
			* Only add this on pages where the foremost title on the page should be the same text as the logo - on other pages the h1 should be separate
		-->
		<h1 class="visuallyhidden focusable"><?php bloginfo('name'); ?>"></h1>
	</a>

	<a  class="visuallyhidden focusable" href="/" rel="home">
		<!--
			* Only add this on pages where the foremost title on the page should be the same text as the logo
			? should the visually hidden be on the a or the h1?
			the blockquote thing is andy malarkey
		-->
		<h1><?php bloginfo('name'); ?>"></h1>
		<blockquote>
			This is my logo tagline
		</blockquote>
	</a>


	Pros/cons
	+	google likes this method
	+	allows the logo to persist regardless of styles
	+	means the logo is resizable (a background image cannot be resized as easily)


	http://csswizardry.com/2010/10/your-logo-is-an-image-not-a-h1/
		The organisation's logo is *content* not *style* - it does not depend on the look & feel of the site
		The <h1> tag is the uppermost, all encompassing title of a page - it might be the same words as the logo on a site homepage but probably not on other pages
		If I use <h1. for the logo, I have to start the page titles on <h2> and this is wrong.
		<h1> is one of
		The homepage might not display a h1 but it should have on
		It needs the logo to be an image but it also needs a h1 tag somewhere - can use the 'visuallyhidden' class here to hide it from all but screen-readers

	Conclusion
	----------

	Inline image is best