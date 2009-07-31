#!/usr/bin/env ruby

require 'rubygems'
require 'flickr'

flickr = Flickr.new('f9cbdebffb9c269192841aae0ba9dbcb', 'flickr@wjlr.org.uk', '13dfb74249a8f7b6')

set='72157620417041389'

photoset = Photoset.new(set)
puts photoset.getInfo
