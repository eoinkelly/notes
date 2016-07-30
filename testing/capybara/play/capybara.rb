require 'capybara/rspec'
require 'capybara/poltergeist'
require 'pry'

Capybara.register_driver :poltergeist do |app|
  options = { js_errors: false }
  Capybara::Poltergeist::Driver.new(app, options)
end

Capybara.default_driver = :poltergeist

describe "Capybara and RSpec setup" do
  it "fails because we did not pass the type: :feature option to Rspec" do
    expect {
      visit 'http://wikinewzealand.org/'
    }.to raise_error(NoMethodError)
  end
end

# NOTE: { type: :feature } is required by rspec to load capybara
describe "A simple smoke test", type: :feature do
  before :each do
  end

  it "can read content off the page" do
    visit 'http://wikinewzealand.org/'
    # within("#session") do
    #   fill_in 'Email', :with => 'user@example.com'
    #   fill_in 'Password', :with => 'password'
    # end
    # click_button 'Sign in'
    # NOTE: have_content is case sensitive
    expect(page).to have_content 'ABOUT US'
  end
end
