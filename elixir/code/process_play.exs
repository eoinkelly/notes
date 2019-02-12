# This minimal exs script starts 57 processes
# Starts 3 applications: elixir, kernel, logger
# The top process in the application in observer
# * is linked to its children in the application
# * also has a link to the "application_controller" process
# * seems to be the "group leader" for all processes in the application
#
#
# The top process in 'kernel, 'logger' and 'elixir' are started by application_controlle process
# The 'application_controller' process does not appear in the 'applications' tab
# * it is a GenServer
#
# Observer will show the proceese "registered name" if it has one
Process.list() |> Enum.count() |> IO.inspect()

:observer.start()

Process.sleep(:infinity)
