require "fileutils"

INPUT_PATH = File.join(__dir__, "index.src.md")
BACKUP_PATH = File.join(__dir__, "index.md.gitignored.backup")
OUTPUT_PATH = File.join(__dir__, "index.md")

def do_build # rubocop:disable Metrics/MethodLength
  puts "[Building]"
  output = ""

  File.read(INPUT_PATH).each_line do |line|
    if line.start_with?("INCLUDE_FILE")
      _token, rel_path = line.chomp.split(/\s+/)
      contents = File.read(rel_path)
      output << "# #{rel_path}\n"
      output << contents
    else
      output << line
    end
  end

  FileUtils.cp(OUTPUT_PATH, BACKUP_PATH)
  File.write(OUTPUT_PATH, output)
rescue StandardError => e
  warn "Rescued build error: #{e.inspect}"
end

def main
  do_build

  puts "Watching for changes in #{INPUT_PATH}"
  old_mtime = File.mtime(INPUT_PATH)

  loop do
    if File.mtime(INPUT_PATH) > old_mtime
      do_build
      old_mtime = File.mtime(INPUT_PATH)
    else
      sleep 1
    end
  end
end

main
