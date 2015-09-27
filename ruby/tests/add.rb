require "rubygems"
require "uuid"
module Add
  puts "hello"
  def self.sum(max)
    puts UUID.generate
    (max + max)
  end
end
