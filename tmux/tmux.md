```plain
tmux new -s SESSION_NAME # create named session (protip: always name session)

# create new session and name the window it creates
tmux new -s SESSION_NAME -n WINDOW_NAME

tmux new -s SESSION_NAME -d # create named session but do not attach

prefix d # detach from current session (within the session

tmux ls # list available sessions

tmux attach -t SESSION_NAME # attach to an existing named session
tmux kill-session -t SESSION_NAME # kill a session from outside


# windows
# #######

prefix c # create window
prefix n # next window
prefix p # previous window


# panes
# #####
prefix %  # vertical split
prefix " # horizontal split

prefx ARROW_KEY # moves between panes
prefix o # cycle through panes

prefix spacebar # cycle through pane layouts

prefix x # close pane (with prompt)


# status line
# ###########

prefix : # run command on status line

# commands
# ########

new-window -n WIN_NAME "COMMAND TO RUN TO START SHELL"
new-window -n status "top"

# help
# ####

prefix ? # show help
```

- keybindings are just shortcuts for tmux commands
- you can execte commands from
    1. terminal itself
    1. the command area of the tmux status line
- when you start a window with an initial command the window will close when
  that command exits - if you want the window to persist you have to create it
  without an initial command and run the command in the shell

Things I want

maximize current pane ? can i name panes? add a nice theme
