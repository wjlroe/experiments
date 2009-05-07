#!/usr/bin/ruby

require 'net/http'
require 'uri'
require 'json'

def getUsersInfo(username)
  JSON.parse Net::HTTP.get(URI.parse("http://github.com/api/v1/json/" + username))
end
