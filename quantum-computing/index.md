# Quantum computing

https://docs.microsoft.com/en-us/azure/quantum/concepts-overview

What is it for?

- computational tasks which would either be too slow or require too much memory
  using a conventional computer

> quantum algorigthmic thinking quantum algorithms are very different from
> classical algorithms we compute by manipulating quantum interference

you need to know vectors and matrices to understand quantum computing

## The SDK

The SDK depends on dotnet 3. dotnet 5 is latest and is what homebrew has.

1. Open VS Code
1. From blue menu, choose "Re-open in container" (or accept the prompt on bottom
   left)

This directory is mapped to `/workspace` in the container Extensions are
installed when the container is created by vs code VS code runs the docker
commands for you VS-Code has a docker-compose.yml that can override your normal
one

- project names allow you to use kebab case but it breaks things - stick to
  PascalCase

This is the log from the start up

Interesting things in the log:

- docker-compose supports being passed multiple docker-compose.yml files
- docker-compose has a command to show you the merged config of multiple config
  files

    ```
    docker-compose -f /Users/eoinkelly/Dropbox/Eoin/Notes/on-github/quantum-computing/docker-compose.yml -f /Users/eoinkelly/Dropbox/Eoin/Notes/on-github/quantum-computing/.devcontainer/docker-compose.yml config
    ```

- it creates files in /home/jovyan/.vscode-server /var/vscode-server

```
[440 ms] Remote-Containers 0.202.5 in VS Code 1.61.2 (6cba118ac49a1b88332f312a8f67186f7f3c1643).
[440 ms] Start: Resolving Remote
[444 ms] Setting up container for folder or workspace: /Users/eoinkelly/Dropbox/Eoin/Notes/on-github/quantum-computing
[447 ms] Start: Check Docker is running
[447 ms] Start: Run: docker version --format {{.Server.APIVersion}}
[700 ms] Server API version: 1.41
[715 ms] Start: Run: docker-compose version --short
[1924 ms] Start: Run: docker ps -q -a --filter label=com.docker.compose.project=quantum-computing --filter label=com.docker.compose.service=app
[2149 ms] Start: Run: docker inspect --type container e5906bc70c01
[2370 ms] Start: Run: docker-compose -f /Users/eoinkelly/Dropbox/Eoin/Notes/on-github/quantum-computing/docker-compose.yml -f /Users/eoinkelly/Dropbox/Eoin/Notes/on-github/quantum-computing/.devcontainer/docker-compose.yml config
[2946 ms] services:
  app:
    build:
      context: /Users/eoinkelly/Dropbox/Eoin/Notes/on-github/quantum-computing
    command: /bin/sh -c "while sleep 1000; do :; done"
    init: true
    tty: true
    volumes:
    - /Users/eoinkelly/Dropbox/Eoin/Notes/on-github/quantum-computing:/workspace:cached
version: '3'

[2946 ms]
[2949 ms] Start: Run: docker events --format {{json .}} --filter event=start
[2952 ms] Start: Run: docker-compose --project-name quantum-computing -f /Users/eoinkelly/Dropbox/Eoin/Notes/on-github/quantum-computing/docker-compose.yml -f /Users/eoinkelly/Dropbox/Eoin/Notes/on-github/quantum-computing/.devcontainer/docker-compose.yml up -d
Starting quantum-computing_app_1 ... done
[5686 ms] Start: Run: docker ps -q -a --filter label=com.docker.compose.project=quantum-computing --filter label=com.docker.compose.service=app
[5935 ms] Start: Run: docker inspect --type container e5906bc70c01
[6151 ms] Start: Inspecting container
[6151 ms] Start: Run: docker inspect --type container e5906bc70c01d8bbd812ca58bb483983069fb1847bf99e640a478a262e332ba0
[6365 ms] Start: Run in container: /bin/sh
[6367 ms] Start: Run in container: uname -m
[6725 ms] x86_64
[6726 ms]
[6726 ms] Start: Run in container: (cat /etc/os-release || cat /usr/lib/os-release) 2>/dev/null
[6732 ms] PRETTY_NAME="Debian GNU/Linux 10 (buster)"
NAME="Debian GNU/Linux"
VERSION_ID="10"
VERSION="10 (buster)"
VERSION_CODENAME=buster
ID=debian
HOME_URL="https://www.debian.org/"
SUPPORT_URL="https://www.debian.org/support"
BUG_REPORT_URL="https://bugs.debian.org/"
[6733 ms]
[6733 ms] Start: Run in container: cat /etc/passwd
[6737 ms] Start: Setup shutdown monitor
[6738 ms] Forking shutdown monitor: /Users/eoinkelly/.vscode/extensions/ms-vscode-remote.remote-containers-0.202.5/dist/shutdown/shutdownMonitorProcess /var/folders/my/t3x7q7yn1lnds82qkfspj2z00000gn/T/vscode-remote-containers-bca05409e97ea5dbcd24ca637030d5c85d6d0262.sock dockerCompose Debug /Users/eoinkelly/Library/Application Support/Code/logs/20211027T094436/exthost9/ms-vscode-remote.remote-containers 1635540255585
[6743 ms] Start: Run in container: test -d /home/jovyan/.vscode-server
[6758 ms]
[6759 ms]
[6759 ms] Start: Run in container: test -f /var/vscode-server/.patchEtcEnvironmentMarker
[6770 ms]
[6772 ms]
[6773 ms] Start: Run in container: test -f /var/vscode-server/.patchEtcProfileMarker
[6778 ms]
[6778 ms]
[6779 ms] Start: Run in container: test ! -f '/home/jovyan/.vscode-server/data/Machine/.writeMachineSettingsMarker' && set -o noclobber && mkdir -p '/home/jovyan/.vscode-server/data/Machine' && { > '/home/jovyan/.vscode-server/data/Machine/.writeMachineSettingsMarker' ; } 2> /dev/null
[6791 ms]
[6792 ms]
[6792 ms] Exit code 1
[6792 ms] Start: Run in container: cat /home/jovyan/.vscode-server/data/Machine/settings.json
[6804 ms]
[6804 ms] cat: /home/jovyan/.vscode-server/data/Machine/settings.json: No such file or directory
[6804 ms] Exit code 1
[6805 ms] Start: Run in container: test -d /home/jovyan/.vscode-server/bin/6cba118ac49a1b88332f312a8f67186f7f3c1643
[6811 ms]
[6811 ms]
[6811 ms] Start: Launching Remote-Containers helper.
[6813 ms] ssh-agent: SSH_AUTH_SOCK in container (/tmp/vscode-ssh-auth-a3ae9e5df125550d41e1c9406ba0903aece2427e.sock) forwarded to host (/private/tmp/com.apple.launchd.GWODSqC1hf/Listeners).
[6814 ms] Start: Run: gpgconf --list-dir agent-extra-socket
[6915 ms] /Users/eoinkelly/.gnupg/S.gpg-agent.extra
[6915 ms]
[6915 ms] Start: Run in container: gpgconf --list-dir agent-socket
[7008 ms] /home/jovyan/.gnupg/S.gpg-agent
[7008 ms]
[7009 ms] Start: Run in container: gpgconf --list-dir homedir
[7016 ms] /home/jovyan/.gnupg
[7016 ms]
[7016 ms] Start: Run in container: ls '/home/jovyan/.gnupg/private-keys-v1.d' 2>/dev/null
[7023 ms]
[7023 ms]
[7024 ms] Exit code 2
[7024 ms] Start: Run in container: mkdir -p -m 700 '/home/jovyan/.gnupg'
[7025 ms] userEnvProbe: loginInteractiveShell (default)
[7026 ms] userEnvProbe shell: /bin/bash
[7032 ms]
[7032 ms]
[7032 ms] Start: Run in container: cat <<'EOF-/tmp/vscode-remote-containers-a3ae9e5df125550d41e1c9406ba0903aece2427e.js' >/tmp/vscode-remote-containers-a3ae9e5df125550d41e1c9406ba0903aece2427e.js
[7033 ms] Start: Run: gpgconf --list-dir homedir
[7040 ms]
[7040 ms]
[7040 ms] Start: Run in container: # Test for /home/jovyan/.ssh/known_hosts and ssh
[7046 ms] /home/jovyan/.ssh/known_hosts exists
[7046 ms]
[7046 ms] Exit code 1
[7046 ms] Start: Run in container: cat <<'EOF-/tmp/vscode-remote-containers-server-a3ae9e5df125550d41e1c9406ba0903aece2427e.js' >/tmp/vscode-remote-containers-server-a3ae9e5df125550d41e1c9406ba0903aece2427e.js
[7047 ms] Start: Run in container: /bin/sh
[7050 ms] Start: Run in container: command -v git >/dev/null 2>&1 && git config --system credential.helper '!f() { /home/jovyan/.vscode-server/bin/6cba118ac49a1b88332f312a8f67186f7f3c1643/node /tmp/vscode-remote-containers-a3ae9e5df125550d41e1c9406ba0903aece2427e.js $*; }; f' || true
[7072 ms]
[7072 ms]
[7072 ms] Start: Run in container: /home/jovyan/.vscode-server/bin/6cba118ac49a1b88332f312a8f67186f7f3c1643/node /tmp/vscode-remote-containers-server-a3ae9e5df125550d41e1c9406ba0903aece2427e.js
[7587 ms]
[7587 ms]
[7588 ms] Start: Preparing Extensions
[7588 ms] Start: Run in container: test ! -f '/home/jovyan/.vscode-server/data/Machine/.installExtensionsMarker' && set -o noclobber && mkdir -p '/home/jovyan/.vscode-server/data/Machine' && { > '/home/jovyan/.vscode-server/data/Machine/.installExtensionsMarker' ; } 2> /dev/null
[7591 ms]
[7591 ms]
[7591 ms] Exit code 1
[7594 ms] Extensions cache, install extensions: None
[7594 ms] Start: Run in container: test -d /home/jovyan/.vscode-server/extensionsCache && ls /home/jovyan/.vscode-server/extensionsCache || true
[7598 ms]
[7599 ms]
[7600 ms] Extensions cache, copy to remote: None
[7600 ms] Start: Run in container: for pid in `cd /proc && ls -d [0-9]*`; do { echo $pid ; readlink /proc/$pid/cwd ; readlink /proc/$pid/ns/mnt ; cat /proc/$pid/stat | tr "
[7724 ms] Start: Starting VS Code Server
[7724 ms] Start: Run in container: /home/jovyan/.vscode-server/bin/6cba118ac49a1b88332f312a8f67186f7f3c1643/server.sh --log debug --force-disable-user-env --use-host-proxy --port 0 --extensions-download-dir /home/jovyan/.vscode-server/extensionsCache --start-server --disable-websocket-compression
[7950 ms] userEnvProbe PATHs:
Probe:     '/usr/local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/jovyan/dotnet:/home/jovyan/.dotnet/tools:/home/jovyan/.dotnet/tools'
Container: '/usr/local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/jovyan/dotnet:/home/jovyan/.dotnet/tools'
[7960 ms]

*
* Visual Studio Code Server
*
* Reminder: You may only use this software with Visual Studio family products,
* as described in the license https://aka.ms/vscode-remote/license
*

Extension host agent listening on 39459

[7961 ms] Start: Run in container: echo 39459 >'/home/jovyan/.vscode-server/data/Machine/.devport-6cba118ac49a1b88332f312a8f67186f7f3c1643'
[7964 ms]
[7964 ms]
[7964 ms] Port forwarding for container port 39459 starts listening on local port.
[7965 ms] Port forwarding local port 39459 to container port 39459
[7965 ms] Start: Run in container: test ! -f '/home/jovyan/.vscode-server/data/Machine/.onCreateCommandMarker' && set -o noclobber && mkdir -p '/home/jovyan/.vscode-server/data/Machine' && { > '/home/jovyan/.vscode-server/data/Machine/.onCreateCommandMarker' ; } 2> /dev/null
[7969 ms]
[7969 ms]
[7969 ms] Exit code 1
[7970 ms] Start: Run in container: test ! -f '/home/jovyan/.vscode-server/data/Machine/.updateContentCommandMarker' && set -o noclobber && mkdir -p '/home/jovyan/.vscode-server/data/Machine' && { > '/home/jovyan/.vscode-server/data/Machine/.updateContentCommandMarker' ; } 2> /dev/null
[7974 ms]
[7974 ms]
[7974 ms] Exit code 1
[7974 ms] Start: Run in container: test ! -f '/home/jovyan/.vscode-server/data/Machine/.postCreateCommandMarker' && set -o noclobber && mkdir -p '/home/jovyan/.vscode-server/data/Machine' && { > '/home/jovyan/.vscode-server/data/Machine/.postCreateCommandMarker' ; } 2> /dev/null
[7980 ms]
[7980 ms]
[7980 ms] Exit code 1
[7989 ms] Start: Run in container: # Test for /home/jovyan/.gitconfig and git
[7996 ms] Port forwarding connection from 59293 > 39459 > 39459 in the container.
[7996 ms] Start: Run in container: /home/jovyan/.vscode-server/bin/6cba118ac49a1b88332f312a8f67186f7f3c1643/node -e
[8040 ms] /home/jovyan/.gitconfig exists
[8040 ms]
[8040 ms] Exit code 1
[8042 ms] Start: Run in container: command -v git >/dev/null 2>&1 && git config --global credential.helper '!f() { /home/jovyan/.vscode-server/bin/6cba118ac49a1b88332f312a8f67186f7f3c1643/node /tmp/vscode-remote-containers-a3ae9e5df125550d41e1c9406ba0903aece2427e.js $*; }; f' || true
[8091 ms]
[8091 ms]
[8092 ms] Start: Run in container: mkdir -p '/home/jovyan/.vscode-server/data/Machine' && [ "$(cat '/home/jovyan/.vscode-server/data/Machine/.postStartCommandMarker' 2>/dev/null)" != '2021-10-29T20:44:21.1518586Z' ] && echo '2021-10-29T20:44:21.1518586Z' > '/home/jovyan/.vscode-server/data/Machine/.postStartCommandMarker'
[8137 ms]
[8137 ms]
[8777 ms] [20:44:24] Extension host agent started.
[8886 ms] Port forwarding connection from 59319 > 39459 > 39459 in the container.
[8889 ms] Start: Run in container: /home/jovyan/.vscode-server/bin/6cba118ac49a1b88332f312a8f67186f7f3c1643/node -e
[9360 ms] [20:44:24] [::ffff:127.0.0.1][7e8169b6][ManagementConnection] New connection established.
[9977 ms] [20:44:25] [::ffff:127.0.0.1][1a434295][ExtensionHostConnection] New connection established.
[10028 ms] [20:44:25] [::ffff:127.0.0.1][1a434295][ExtensionHostConnection] <228> Launched Extension Host Process.
[16106 ms] /Users/eoinkelly/.gnupg
[16106 ms]
[16106 ms] Start: Run in container: gpgconf --list-dir homedir
[16150 ms] /home/jovyan/.gnupg
[16150 ms]
[16152 ms] Start: Run in container: # Test for /home/jovyan/.gnupg/trustdb.gpg and gpg
[16157 ms] /home/jovyan/.gnupg/trustdb.gpg exists
[16158 ms]
[16159 ms] Exit code 1
[16159 ms] Start: Run: gpg-connect-agent updatestartuptty /bye
[17630 ms] Extensions cache, remote removals: None
[19864 ms] Start: Run in container: cat /proc/273/environ
[77497 ms] [20:45:33] Installing extension: quantum.quantum-devkit-vscode
[77582 ms] [20:45:33] Installing extension: ms-dotnettools.csharp
[77641 ms] Extensions cache, remote removals: None
[79032 ms] [20:45:34] Downloaded extension: ms-dotnettools.csharp /home/jovyan/.vscode-server/extensionsCache/ms-dotnettools.csharp-1.23.16
[79321 ms] [20:45:34] Extracted extension to /home/jovyan/.vscode-server/extensions/.03acc521-2d38-4a39-9dfe-bf03b135d8ec: ms-dotnettools.csharp
[79358 ms] [20:45:34] Renamed to /home/jovyan/.vscode-server/extensions/ms-dotnettools.csharp-1.23.16
[79398 ms] [20:45:34] Extracting completed. ms-dotnettools.csharp
[79401 ms] [20:45:34] Extension installed successfully: ms-dotnettools.csharp
[80126 ms] [20:45:35] Downloaded extension: quantum.quantum-devkit-vscode /home/jovyan/.vscode-server/extensionsCache/quantum.quantum-devkit-vscode-0.20.2110171573
[103884 ms] [20:45:59] Extracted extension to /home/jovyan/.vscode-server/extensions/.94550163-1066-4c4a-9345-da1a07c72d5d: quantum.quantum-devkit-vscode
[103987 ms] [20:45:59] Renamed to /home/jovyan/.vscode-server/extensions/quantum.quantum-devkit-vscode-0.20.2110171573
[103994 ms] [20:45:59] Extracting completed. quantum.quantum-devkit-vscode
[103998 ms] [20:45:59] Extension installed successfully: quantum.quantum-devkit-vscode
[137649 ms] Extensions cache, remote removals: None
[137650 ms] Start: Run in container: tar c extensionsCache/ms-dotnettools.csharp-1.23.16 extensionsCache/quantum.quantum-devkit-vscode-0.20.2110171573
[198952 ms] Extensions cache, remote removals: None
[258970 ms] Extensions cache, remote removals: None
[318985 ms] Extensions cache, remote removals: None
[378997 ms] Extensions cache, remote removals: None
```
