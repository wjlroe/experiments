#/usr/bin/env ruby

require 'bigdecimal'

def fact(n)
 if n == 0
   1
 else
   n * fact(n-1)
 end
end

4.times do 
  numbers=[]
  6.times do
    num=rand(49)+1
    while numbers.include?(num)
      num=rand(49)+1
    end
    numbers << num
  end
  puts numbers.sort.map { |num| format("%02d", num) }.join(" ")
end

posibility = BigDecimal.new('0.0')
posibility = posibility + fact(49) / (fact(49-6) * fact(49))

puts "Posibility of winning: #{sprintf("%.10f", posibility)}"
