require 'csv'

class PatentJob

  attr_reader :downloader
  private :downloader # make it private

  def initialize(downloader = PatentDownloader.new)
    @downloader = downloader
  end

  def run
    temp = downloader.download_file
    rows = parse(temp)
    update_patents(rows)
  end


  def parse(temp)
    FasterCSV.read(temp, headers: true)
  end

  def update_patents(rows)
    Patent.connection.transaction do
      Patent.delete_all
      rows.each do |r|
        Patent.create!(r.to_hash)
      end
    end
  end
end

puts "hello"
hello

