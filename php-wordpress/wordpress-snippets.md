These should be best practice

Iterate through a acf repeater field
------------------------------------

			<?php
				$allib_got_stuff_to_show = false; // Create a flag

				// We assume that if the_repeater_field() exists, then ACF plugin is enabled.
				if (function_exists('the_repeater_field')) {
					while(the_repeater_field('photos')) {
						if ( strlen(get_sub_field('image')) > 0) {
							$allib_got_stuff_to_show = true;
							// break; // It messes up the_repeater_field() internal counter if we break early
						}
					}
				}
			?>

			<?php if ($allib_got_stuff_to_show): ?>
				<?php while(the_repeater_field('photos')): ?>
						<?php echo wp_get_attachment_image(get_sub_field('image', $post->ID), 'thumbnail', false, array('class' => '')); ?>
						<div id="caption" class="caption">
							<?php the_sub_field('caption'); ?>
						</div>
				<?php endwhile; ?>
				</ul>
			<?php endif; ?>

			<?php unset($allib_got_stuff_to_show); // Destroy the flag ?>

Do a custom query
-----------------
	<?php

			$allib['paged'] = ( get_query_var( 'paged' ) ) ? get_query_var('paged') : 1;
			$allib['tynews_query'] = new WP_Query( array(
								'post_type' => 'post',
								'category_name' => 'ty-news',
								'posts_per_page' => 2,
								'paged' => $allib['paged']
								) ); ?>
			<?php if ( $allib['tynews_query']->have_posts() ) : ?>

				<?php while (  $allib['tynews_query']->have_posts() ) :  $allib['tynews_query']->the_post(); ?>
					<?php get_template_part( 'content', get_post_format() ); ?>
				<?php endwhile; ?>

				<nav id="">
					<h3 class="assistive-text"><?php _e( 'Post navigation', $allib['text_domain'] ); ?></h3>
					<div class="nav-previous"><?php next_posts_link( __('<span class="meta-nav">&larr;</span> Older posts', $allib['text_domain']), $allib['tynews_query']->max_num_pages); ?></div>
					<div class="nav-next"><?php previous_posts_link( __('Newer posts <span class="meta-nav">&rarr;</span>', $allib['text_domain']), $allib['tynews_query']->max_num_pages); ?></div>
				</nav><!-- # -->

			<?php endif; ?>
			<?php wp_reset_query(); ?>



custom query snippet option 2
====================

<?php
    // Full params in https://gist.github.com/2023628
    $allib_query = new WP_Query(array(
        'page_id' => 123
    ));
?>
<?php if ( $allib_query->have_posts() ) : ?>
    <?php while ( $allib_query->have_posts() ) : $allib_query->the_post(); ?>
        <?php get_template_part( 'content', 'page' ); ?>
    <?php endwhile; ?>
<?php endif; ?>
<?php wp_reset_postdata(); ?>


Load an image
-------------

*	don't put width="" or height="" as IE will do something like make the image 1px x 1px
*	? is it still BP to put values in for width & height?
*	we don't have width or height attributes as they don't work in responsive design

<img id="" class="" src="<?php echo get_stylesheet_directory_uri(); ?>/images/something.png" alt="">

Link to another page (via permalink)
-----------------------------------

<a id="" class="" title="" href="<?php echo home_url( '/permalink-to-thing' ); ?>"></a>


link with no google juice: <a href="" title="" rel="nofollow"></a>

Website Logo
----------------

* http://csswizardry.com/2010/10/your-logo-is-an-image-not-a-h1/
http://csswizardry.com/2011/08/more-logo-markup-tips/


not finished yet


<a href="/" id="logo" class="logo">
<img src="<?php echo get_stylesheet_directory_uri(); ?>/images/logo.png" alt="Company logo">
</a>

#logo{
  display:block;
  width:100px; /* Width of logo */
  height:100px; /* Height of logo */
  background:url(/img/css/sprite.png);
}

#logo img{
  position:absolute;
  left:-9999px;
}

Change Table Prefix
-------------------
to change from 'wp_' to 'prefix_' you need to run
	UPDATE `prefix_usermeta` SET `meta_key` = REPLACE( `meta_key` , 'wp_', 'prefix_' );
In the options table, there is ‘wp_user_roles’, make sure you get that changed into ‘prefix_user_roles’.
	UPDATE `prefix_options` SET `option_name` = 'prefix_user_roles' WHERE `option_name` ='wp_user_roles' AND `blog_id` =0;


Get decent attachment info
--------------------------

JTBCS VERSION IS MUCH NEWER

function allib_get_attachment_info($id, $size = 'thumbnail', $required_info = array()) {

	// what should we di if $id isn't valid? return false or an empty array?
	// what is valid?

	// should check size is valid

	// what is best practice way in PHP to suck args into a function?
	// any WP boilerplate I can make use of?

	// echo '<pre>';
	// print_r($required_info);
	// echo '</pre>';
	/*
		I want *one* function that I can call to get the info I need to retrieve something
		from the image library.

			wp_get_attachment_image_src()
				needs: id, image-size
				returns: url, width, height
			custom query by post ID
				needs: id
				returns: title, alternate text, caption, desciption

			wp_get_attachment_metadata()
				needs: id
				returns: ???

		The things I want to be able to get
		*	URL
		*	width
		*	height
		*	title
		*	alternate text
		*	caption
		*	description

		The embedded info like shutter speed etc. is rarely needed so wp_get_attachment_metadata()
		is fine for that.

		I want one function as interface - it can dispatch to others


		Example Post Object
		-------------------
		Array
		(
		    [0] => stdClass Object
		        (
		            [ID] => 376
		            [post_author] => 1
		            [post_date] => 2011-12-12 14:55:54
		            [post_date_gmt] => 2011-12-12 14:55:54
		            [post_content] =>
		            [post_title] => IMGP2580 (Custom)
		            [post_excerpt] =>
		            [post_status] => inherit
		            [comment_status] => closed
		            [ping_status] => closed
		            [post_password] =>
		            [post_name] => imgp2580-custom
		            [to_ping] =>
		            [pinged] =>
		            [post_modified] => 2011-12-12 14:55:54
		            [post_modified_gmt] => 2011-12-12 14:55:54
		            [post_content_filtered] =>
		            [post_parent] => 420
		            [guid] => http://www.aspenlodge.co.nz/shiny-new-site/wp-content/uploads/2011/12/IMGP2580-Custom.jpg
		            [menu_order] => 0
		            [post_type] => attachment
		            [post_mime_type] => image/jpeg
		            [comment_count] => 0
		            [ancestors] => Array
		                (
		                    [0] => 420
		                )
		            [filter] => raw
		        )

	 */

	$results = array();

	/*
	 *	If we are asked for any of url, width height we fetch
	 *	all of them as it is the same function call. We are careful to only return
	 *	what we are asked for however.
	 */
	if (in_array('url', $required_info) ||
		in_array('width', $required_info) ||
		in_array('height', $required_info)) {

		// echo 'Running wp_get_attachment_image_src()';

		$a = wp_get_attachment_image_src($id, $size);
		$results['url'] = $a[0];
		$results['width'] = $a[1];
		$results['height'] = $a[2];
	}

	/*
	 *	If we are asked for any of title, description, caption, alt-text we fetch
	 *	all of them as it is the same function call. We are careful to only return
	 *	what we are asked for however.
	 */
	if (in_array('title', $required_info) ||
		in_array('description', $required_info) ||
		in_array('caption', $required_info) ||
		in_array('alt-text', $required_info)) {

		// echo 'Running custom query';

		$query = new WP_Query(array(
				'post_type' => 'attachment',
				'p' => $id
				// how to retrieve exactly one post?
				// should prob throw error if not?
		));

		foreach ($query->posts as $post) {
			$results['title'] = $post->post_title;
			$results['caption'] = $post->post_excerpt;
			$results['description'] = $post->post_content;
			$results['alt-text'] = get_post_meta($post->ID, '_wp_attachment_image_alt', true);
		}
	}

	// $results potentially contains more info than we were asked for so trim it
	// down
	// NOT IMPLEMENTED YET

	// echo '<pre>';
	// print_r($results);
	// echo '</pre>';

	// Return an array if we found any results, otherwise return false
	// as this seems to be the WordPress way(TM)...
	if (count($results) > 0) {
		return $results;
	} else {
		return false;
	}
}