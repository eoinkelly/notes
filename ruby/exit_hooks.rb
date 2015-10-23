puts 'hangin out, just being a program'

# use `echo $? after running this script to check its return value in the shell

trap('EXIT') { puts 'trapping EXIT' } # 1.
at_exit { puts 'in at_exit' }         # 2. Kernel#at_exit
END { puts 'in END' }                 # 3.

# stops execution at this line. ruby will not find any code after this line so
# make sure exit hooks are defined above it
# abort 'blast'

# abort('blast') is the same as:
# warn 'blast'
# exit(1)

# exit! will skip all exit hooks and "finalisers" (TODO: what are they?)
exit! # same as exit!(1)
exit!(3) # exits with 1
