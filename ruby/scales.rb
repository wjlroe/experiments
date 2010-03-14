#!/usr/bin/env ruby

keys=('a'..'g')
majm=['major','minor']
accs=['#','b']

puts keys.map{|key|
  accs.map{|acc|
    majm.map{|maj|
      "#{key}#{acc} #{maj}"
    }
  }
}.join(',')
