
In ubuntu:

1. Go to /usr/local/share/ca-certificates/
1. Create a new folder, i.e. "sudo mkdir school"
1. Copy the . crt file into the school folder.
1. Make sure the permissions are OK (755 for the folder, 644 for the file)
1. Run "sudo update-ca-certificates"
