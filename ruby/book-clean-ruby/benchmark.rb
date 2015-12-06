require 'rubygems'
require 'benchmark/ips'

class ExampleClass
  def foo; 42; end
end

module ExampleMixin
  def foo; 43; end
end

Benchmark.ips do |bm|
  bm.report("without dci") { ExampleClass.new.foo }
  bm.report("with dci") do
    obj = ExampleClass.new
    obj.extend(ExampleMixin)
    obj.foo
  end
end

