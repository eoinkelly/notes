

## Experiment 1

```
❯ EDITOR=code brew create --go --set-name osv-detector -v https://github.com/G-Rath/osv-detector/releases/download/v0.7.1/osv-detector_0.7.1_darwin_arm64
==> Downloading https://github.com/G-Rath/osv-detector/releases/download/v0.7.1/osv-detector_0.7.1_darwin_arm64
==> Downloading from https://objects.githubusercontent.com/github-production-release-asset-2e65be/465015144/f27f17a8-ad7e-4862-b304-a37a08ed26b6?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIWNJYAX4CSVEH53A%2F20220909%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20220909T195412Z&X-Amz-Expires=300&X-Amz-Signature=2836908290c790c10b82628534805f2871411595a7a10d8a7d3a6e7b107bfae7&X-Amz-SignedHeaders=host&actor_id=0&key_id=0&repo_id=465015144&response-content-disposition=attachment%3B%20filename%3Dosv-detector_0.7.1_darwin_arm64&response-content-type=application%2Foctet-stream
/opt/homebrew/Library/Homebrew/shims/shared/curl --disable --cookie /dev/null --globoff --show-error --user-agent Homebrew/3.6.0\ \(Macintosh\;\ arm64\ Mac\ OS\ X\ 12.5.1\)\ curl/7.79.1 --header Accept-Language:\ en --fail --retry 3 --location --remote-time --output /Users/eoinkelly/Library/Caches/Homebrew/downloads/9fffbd3e0469f09e00cd576d938e669d5311cc9422911e516c8a10459d20e65e--osv-detector_0.7.1_darwin_arm64.incomplete https://objects.githubusercontent.com/github-production-release-asset-2e65be/465015144/f27f17a8-ad7e-4862-b304-a37a08ed26b6\?X-Amz-Algorithm=AWS4-HMAC-SHA256\&X-Amz-Credential=AKIAIWNJYAX4CSVEH53A\%2F20220909\%2Fus-east-1\%2Fs3\%2Faws4_request\&X-Amz-Date=20220909T195412Z\&X-Amz-Expires=300\&X-Amz-Signature=2836908290c790c10b82628534805f2871411595a7a10d8a7d3a6e7b107bfae7\&X-Amz-SignedHeaders=host\&actor_id=0\&key_id=0\&repo_id=465015144\&response-content-disposition=attachment\%3B\%20filename\%3Dosv-detector_0.7.1_darwin_arm64\&response-content-type=application\%2Foctet-stream
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 6067k  100 6067k    0     0  3178k      0  0:00:01  0:00:01 --:--:-- 3190k
==> Verifying checksum for '9fffbd3e0469f09e00cd576d938e669d5311cc9422911e516c8a10459d20e65e--osv-detector_0.7.1_darwin_arm64'
Warning: Cannot verify integrity of '9fffbd3e0469f09e00cd576d938e669d5311cc9422911e516c8a10459d20e65e--osv-detector_0.7.1_darwin_arm64'.
No checksum was provided for this resource.
For your reference, the checksum is:
  sha256 "ae2b735f240754ea2e62022d3516eb02a9a73c662f21b416700f19a6d008f80b"
Please run `brew audit --new osv-detector` before submitting, thanks.
Editing /opt/homebrew/Library/Taps/homebrew/homebrew-core/Formula/osv-detector.rb
code /opt/homebrew/Library/Taps/homebrew/homebrew-core/Formula/osv-detector.rb
```

```ruby
# Documentation: https://docs.brew.sh/Formula-Cookbook
#                https://rubydoc.brew.sh/Formula
# PLEASE REMOVE ALL GENERATED COMMENTS BEFORE SUBMITTING YOUR PULL REQUEST!
class OsvDetector < Formula
  desc ""
  homepage ""
  url "https://github.com/G-Rath/osv-detector/releases/download/v0.7.1/osv-detector_0.7.1_darwin_arm64"
  sha256 "ae2b735f240754ea2e62022d3516eb02a9a73c662f21b416700f19a6d008f80b"
  license ""

  depends_on "go" => :build

  def install
    # ENV.deparallelize  # if your formula fails when building in parallel
    system "go", "build", *std_go_args(ldflags: "-s -w")
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! For Homebrew/homebrew-core
    # this will need to be a test that verifies the functionality of the
    # software. Run the test with `brew test osv-detector`. Options passed
    # to `brew install` such as `--HEAD` also need to be provided to `brew test`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "false"
  end
end
```

# Experiment 2

```
❯ EDITOR=code brew create  --set-name osv-detector-ex-1 -v https://github.com/G-Rath/osv-detector/releases/download/v0.7.1/osv-detector_0.7.1_darwin_arm64
==> Downloading https://github.com/G-Rath/osv-detector/releases/download/v0.7.1/osv-detector_0.7.1_darwin_arm64
Already downloaded: /Users/eoinkelly/Library/Caches/Homebrew/downloads/9fffbd3e0469f09e00cd576d938e669d5311cc9422911e516c8a10459d20e65e--osv-detector_0.7.1_darwin_arm64
==> Verifying checksum for '9fffbd3e0469f09e00cd576d938e669d5311cc9422911e516c8a10459d20e65e--osv-detector_0.7.1_darwin_arm64'
Warning: Cannot verify integrity of '9fffbd3e0469f09e00cd576d938e669d5311cc9422911e516c8a10459d20e65e--osv-detector_0.7.1_darwin_arm64'.
No checksum was provided for this resource.
For your reference, the checksum is:
  sha256 "ae2b735f240754ea2e62022d3516eb02a9a73c662f21b416700f19a6d008f80b"
Please run `brew audit --new osv-detector-ex-1` before submitting, thanks.
Editing /opt/homebrew/Library/Taps/homebrew/homebrew-core/Formula/osv-detector-ex-1.rb
code /opt/homebrew/Library/Taps/homebrew/homebrew-core/Formula/osv-detector-ex-1.rb
```
```ruby
# Documentation: https://docs.brew.sh/Formula-Cookbook
#                https://rubydoc.brew.sh/Formula
# PLEASE REMOVE ALL GENERATED COMMENTS BEFORE SUBMITTING YOUR PULL REQUEST!
class OsvDetectorEx1 < Formula
  desc ""
  homepage ""
  url "https://github.com/G-Rath/osv-detector/releases/download/v0.7.1/osv-detector_0.7.1_darwin_arm64"
  sha256 "ae2b735f240754ea2e62022d3516eb02a9a73c662f21b416700f19a6d008f80b"
  license ""

  # depends_on "cmake" => :build

  def install
    # ENV.deparallelize  # if your formula fails when building in parallel
    # Remove unrecognized options if warned by configure
    # https://rubydoc.brew.sh/Formula.html#std_configure_args-instance_method
    system "./configure", *std_configure_args, "--disable-silent-rules"
    # system "cmake", "-S", ".", "-B", "build", *std_cmake_args
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! For Homebrew/homebrew-core
    # this will need to be a test that verifies the functionality of the
    # software. Run the test with `brew test osv-detector-ex-1`. Options passed
    # to `brew install` such as `--HEAD` also need to be provided to `brew test`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "false"
  end
end
```
