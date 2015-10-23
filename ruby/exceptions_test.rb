class Foo
  def do_thing
    raise 'Bad thing'
  end
end

class Bar
  def do_other_thing
    Foo.new.do_thing
  end
end

begin
  Bar.new.do_other_thing
rescue => e
  p e
  p e.stacktrace
end

