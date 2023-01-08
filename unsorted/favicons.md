			If favicon.ico and apple-touch-icon.png are in the root directory you
				don't need this code. More at mathiasbynens.be/notes/touch-icons

			This is the traditional favicon.
				- size: 16x16 or 32x32
				- transparency is OK
				- see wikipedia for info on browser support: http://mky.be/favicon/

			The is the icon for iOS's Web Clip.
				- size: 57x57 for older iPhones, 72x72 for iPads, 114x114 for iPhone4's retina display (IMHO, just go ahead and use the biggest one)
				- To prevent iOS from applying its styles to the icon name it thusly: apple-touch-icon-precomposed.png
				- Transparency is not recommended (iOS will put a black BG behind the icon)
			<link rel="apple-touch-icon" href="<?php echo get_template_directory_uri(); ?>/images/favicons/apple-touch-icon-114x114px.png">