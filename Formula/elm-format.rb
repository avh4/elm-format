class ElmFormat < Formula
  desc "Source code formatter for Elm (with teleport-imports)"
  homepage "https://github.com/perkee/elm-format"
  version "0.8.7-teleport"
  license "BSD-3-Clause"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/perkee/elm-format/releases/download/v0.8.7-teleport/elm-format-v0.8.7-teleport-arm64-apple-darwin.tar.gz"
      sha256 "bc6a2ca8eec8fda0d2164de75d514df6e291a12de5eed92fd13aafb4ec544c41"
    end
  end

  on_linux do
    url "https://github.com/perkee/elm-format/releases/download/v0.8.7-teleport/elm-format-v0.8.7-teleport-x86_64-linux.tar.gz"
    sha256 "12eb671c8f3345fb2e302b30cfe7724dffde06ba02063e7583b79529ab9363b3"
  end

  def install
    bin.install "elm-format"
  end

  test do
    assert_match "elm-format", shell_output("#{bin}/elm-format --help")
  end
end
