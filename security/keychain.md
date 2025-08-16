I didn't know it when I first started using this but you don't need this
Keychain on macOS because macOS automatically runs `ssh-agent` for you and
integrates with OSXKeychain - see my notes on ssh for more details.

## Keychain (funtoo one, not macos)

- came out of funtoo linux project
- keychain is a front-end to `ssh-agent` and `ssh-add` - only have to enter
  password when machine is booted
- Normally you have one `ssh-agent` per login session so you have to enter
  passwords every time you open a session
- booted from .bashrc/.zshrc

Sample setup for zsh:

```zsh
# Keychain Setup

keychain ~/.ssh/id_rsa
. ~/.keychain/$(hostname)-sh
```

Explanation of options:

```
keychain <options> <path-to-key>
--nocolor
--eval          # print lines to be evaluated by the shell (papers over diff between shells)
--agents ssh
--inherit any   # inherit SSH key passphrases stored in MacOS Keychain
```

- keeps my decrypted private key cached in memory - `keychain --clear` to remove
  them
- when you start a session keychain adds your private key to the agents
  (gpg-agent and ssh-agent) (starts the agents if required)

```
# stop all agents
keychain -k all
```

Environment variables that keychain uses:

SSH_AGENT_PID

- pid of `ssh-agent`

SSH_AUTH_SOCK

- socket that MacOS Keychain controls (I think!)
