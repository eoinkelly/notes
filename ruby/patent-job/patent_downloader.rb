require 'net/ftp'
require 'tempfile'

class PatentDownloader
  def download_file
    temp = Tempfile.new('patents')
    tempname = temp.path
    temp.close
    Net::FTP.open('localhost', 'foo', 'pass') do |ftp|
      ftp.getbinaryfile('/stuff.csv', tempname)
    end
    tempname
  end
end