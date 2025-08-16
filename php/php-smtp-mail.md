php.ini mail configuration:

```bash
# used on Windows only
SMTP	localhost	localhost

# used on Windows only
smtp_port	25	25

# Add X-PHP-Originating-Script header with value being the UUID of the script followed by filename
mail.add_x_header	Off

# force extra params to the 5th arg to mail()
mail.force_extra_parameters	no value

# path to log file which will log all mail() calls (logs headers but not body)
mail.log	no value

# the sendmail path, configure will attempt to find it itself, this is an override
# this usually needs to be an absolute path to the binary
sendmail_path	/usr/sbin/sendmail -t -i

# what From: address should sendmail use
sendmail_from	no value
```

A "never send mail" config:

```ini
; needed?
[sendmail]

[mail function]
; window sonly
SMTP = ""
smtp_port = ""

; Unixen
sendmail_from = ""
sendmail_path = ""
```
