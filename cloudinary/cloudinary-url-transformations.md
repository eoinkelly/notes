# Cloudinary URL transformations

https://cloudinary.com/documentation/transformation_reference

```
https://res.cloudinary.com/<cloud_name>/<asset_type>/<delivery_type>/<transformations>/<version>/<public_id>.<extension>
```

c_crop,g_face,h_400,w_400/r_max/c_scale,w_200

_transformation parameters_ are comma separated
_transformations_ are `/` separated


* transformation parameters
* two kinds
	1. action parameters
		* these trigger the actual transforming of the image
		* each transformation must have exactly one action parameter
	2. qualifier parameters
		* these tweak the settings of a previous action param
		* a given action parameter will have documented required and optional qualifier parameters


a single transformation = action parameter + 0-N qualifier parameters

can chain transformations with `/`

	<transform-1>/<transform-2>/<transform-3>

creates a pipeline

	<transform-1> -> <transform-2> -> <transform-3>

modeled in the ruby API as an array of hashes (each hash a collection of transformation params), each transformation applied in order

Pataka embedded crops are a c_crop followed by a c_scale

## What happens if a c_crop is applied twice in a URL?

Each c_crop works on the *output* of the previous transformation - they form a pipeline. It is not the case that "last crop wins".

```bash
# given a public ID
4RR4Q9P_eoin_test_truck

# image with no transformations:
http://demo-ressh.cloudinary.com/image/upload/4RR4Q9P_eoin_test_truck

# take a crop the bottom left corner
http://demo-ressh.cloudinary.com/image/upload/c_crop,w_500,h_500,g_south_west/4RR4Q9P_eoin_test_truck

# then take a crop of the crop
http://demo-ressh.cloudinary.com/image/upload/c_crop,w_500,h_500,g_south_west/c_crop,w_200,h_200,g_south_east/4RR4Q9P_eoin_test_truck

# and compare that to just the second crop applied to the original image
http://demo-ressh.cloudinary.com/image/upload/c_crop,w_200,h_200,g_south_east/4RR4Q9P_eoin_test_truck

# Conclusion: the images are different
```

```bash
# original
https://demo-ressh.cloudinary.com/image/upload/c_scale,f_auto,q_auto,w_576,c_crop,h_291,w_291,x_909,y_0/c_scale,h_291,w_291/4M0WN2J_the_dragon_magazine_jpeg

# try putting scale after embedded
https://demo-ressh.cloudinary.com/image/upload/f_auto,q_auto,c_crop,h_291,w_291,x_909,y_0/c_scale,h_291,w_291/c_scale,w_576/4M0WN2J_the_dragon_magazine_jpeg
```
