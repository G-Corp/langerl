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
  def self.tuple()
    begin
      Erlang::Tuple.new([:un, 2, "trois"])
    rescue => e
      e.message
    end
  end
  def self.array()
    [:un, 2, "trois"]
  end
end
