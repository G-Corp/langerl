require "rubygems"
require "uuid"
module MyUUID
  def self.generate()
    UUID.generate
  end
end
