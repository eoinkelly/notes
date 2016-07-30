# Live reload

* https://github.com/phoenixframework/phoenix_live_reload

* phoenix uses a Plug built on its own channels stuff to implement live reloading
	* the channel sends the `assets_change` message with some data about what kind of asset changedto the JS
	* JS then refreshes all loaded assets of that type on the page
* some docs on it at <https://github.com/phoenixframework/phoenix_live_reload/blob/master/lib/phoenix_live_reload/live_reloader.ex>
* it automatically reloads ALL css files on the page but you can disable this for particular files by adding the `data-no-reload` tag e.g.
	```
	<link rel="stylesheet" href="http://example.com/style.css" data-no-reload>
	```
