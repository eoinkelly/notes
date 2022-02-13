FROM kalilinux/kali-rolling

RUN apt-get update && apt-get upgrade -yy

# https://www.kali.org/docs/general-use/metapackages/

RUN apt-get install -y kali-linux-core htop vim

# gets stuck on a yes/no question about capturing packers
# kali-linux-arm

# kismet asks about being suid root
# RUN apt-get install kali-linux-headless -yy

CMD ["/usr/bin/tail", "-f", "/dev/null"]
