require "rubygems"
require "uuid"
module Summer
  puts "hello"
  def self.sum(max)
    puts UUID.generate
    raise "Invalid maximum #{max}" if max < 0
    (max*max + max)/2
  end
end
