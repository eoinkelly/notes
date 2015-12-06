require 'autotest'

class Autotest::Rubykoan < Autotest
  def initialize
    super
    @exceptions = /\.txt|Rakefile|\.rdoc/

    self.order = :alpha
    self.add_mapping(/^about_.*rb$/) do |filename, _|
      filename
    end 

  end

  def make_test_cmd files_to_test
    "#{ruby}  'path_to_enlightenment.rb'"
  end 

  # quiet test/unit chatter
  def handle_results(results)
  end

end

