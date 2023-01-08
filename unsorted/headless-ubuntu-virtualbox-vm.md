# Headless Ubuntu

start:
vboxheadless -s Ubuntu

stop:
VBoxManage controlvm [nameofmachine] savestate       # saves the state of the VM like in suspend
VBoxManage controlvm [nameofmachine] poweroff        # simply "unplugs" the VM
VBoxManage controlvm [nameofmachine] acpipowerbutton # sends ACPI poweroff signal
