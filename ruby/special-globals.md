
# Ruby's special globals

__FILE__

$: $LOAD_PATH
    * Array of dirs that ruby will search for filenames passed to `load`
    * `.` is not on the load path. `load` is hardwired to search it first anyway

$/
$stdin
