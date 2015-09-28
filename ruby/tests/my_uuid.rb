require "rubygems"
require "uuid"
module MyUUID
  def self.generate()
    UUID.generate
  end
  def self.calc(x)
    x * x
  end
end
