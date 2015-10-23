defmodule V2015_10_04Test do
  use ExUnit.Case

  import ExUnit.CaptureIO

  doctest V2015_10_04

  @input_name "Foo bar"
  @expected_prompt "What is your name? "
  @expected_output "#{@expected_prompt}Hello, #{@input_name}, nice to meet you!\n"
  @subject &V2015_10_04.echo/0

  test "it echos the input back with a welcome message" do
    assert capture_io([input: @input_name, capture_prompt: true], @subject) == @expected_output
  end

  test "it strips newline off the end on input" do
    input_name_w_newline = "#{@input_name}\n"
    assert capture_io([input: input_name_w_newline, capture_prompt: true], @subject) == @expected_output
  end

  test "it shows help message when it gets the --help command line arg" do
    command = "--help"
    expected_output = "This is a simple echo program"

    # options
    # 1. spawn the process and communicate via a port - is that possible from a test? how do tests work?
    # 2. simulate it by invoking the main function the same way it would be invoked in an escript
  end
end
