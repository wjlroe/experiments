#!/usr/bin/env ruby

require 'readline'

stty_save = `stty -g`.chomp
trap('INT') { system('stty', stty_save); exit }

COMMANDS = [
            'nick', 'whois', 'who'
].sort

comp = proc { |s| COMMANDS.grep( /^#{Regexp.escape(s)}/ ) }

Readline.completion_proc = comp
Readline.emacs_editing_mode

while line = Readline.readline('> ', true)
  puts "You typed: #{line}"
end
