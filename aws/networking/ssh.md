To jump to an EC2 instance in a private subnet through an instance in a public
subnet:

```
# * 'ubuntu' user for ubuntu boxes only
# * -v makes ssh verbos which is nice if it doesn't work first time
# * assumes you use the same key for both the jump box and the target
#   box
ssh -v -i <PATH_TO_PRIVATE_KEY.pem> -J ubuntu@<PUBLIC_IP_OF_JUMP_BOX> ubuntu@<PRIVATE_IP_OF_TARGET_BOX>
```
