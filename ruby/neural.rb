class Neuron

  class << self
    attr_accessor :inputs
    attr_accessor :outputs
  end

  def self.sin
  end

  def set_inputs(inputs)
    @inputs = inputs
  end

  def initialize(num_inputs)
    @inputs = Array.new(num_inputs)
  end

end
