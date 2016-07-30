defmodule InfoGetter do
  defmacro definfo do
    IO.puts "In macro context (#{__MODULE__})"
    quote do
      IO.puts "In caller context (#{__MODULE__})"

      def friendly_info do
        # IO.inspect(thing) is equivalent to IO.puts(Kernel.inspect(thing))
        IO.puts """
        My name is #{__MODULE__}
        My functions are #{Kernel.inspect __info__(:functions)}
        """
      end
    end
  end
end


defmodule RealDoesWorkModule do
  require InfoGetter
  InfoGetter.definfo
  # friendly_info # this fails. why???
end
