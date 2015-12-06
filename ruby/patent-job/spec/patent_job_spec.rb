require_relative 'spec_helper'

describe PatentDownloader do
  it "should download the csv file from the ftp server" do
    upload_test_file('localhost', 'foo', 'foopw' 'patents.csv', '/')

    # @job = PatentJob.new
    @conn = PatentDownloader.new
    f = File.read(@conn.download_file)
    f.should have(250).characters
    f.include("just 3 mintues").should be_true
  end
end


describe PatentJob do

  it "should replace existing patents with new patents" do
    downldr = mock("Downloader") # a mock is a stunt double
    f = fixture_path + "/patents.csv"
    downldr.should_receive(:download_file).once_and_return(f)

    @job = PatentJob.new(downldr)
    @job.run

    Patent.find(:all).should have(3).rows
    Patent.find_by_name('Anti-Gravity Simulator').should be
    Patent.find_by_name('Exo-Skello Jello').should be
    Patent.find_by_name('Nap Compressor').should be
  end

  # which implies that PatentJob is doing more than one thing

end