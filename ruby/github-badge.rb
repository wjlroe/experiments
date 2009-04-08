#!/usr/bin/ruby

require 'net/http'
require 'uri'
require 'json'

def getUsersInfo(username)
  Net::HTTP.get_print URI.parse("http://github.com/api/v1/json/" + username)
end

getUsersInfo "wjlroe"
