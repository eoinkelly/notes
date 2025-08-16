# Centered element without a containing div

this should make a link or piece of text center itself this is related but diff
to .center-text this will not work in IE 6/7 so don't use it for very important
stuff I don't know how safe this is to be a helper class. i don't know how its
children react. it is safe for leaf nodes

.about-us .centered { display: table; text-align: center; margin-left: auto;
margin-right: auto; }
