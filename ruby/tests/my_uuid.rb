require "rubygems"
require "uuid"
module MyUUID
  def self.generate()
    UUID.generate
  end
  def self.calc(x)
    x * x
  end
  def self.re(x)
    x
  end
end
