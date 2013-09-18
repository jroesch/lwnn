class TestCase
  attr_reader :status

  def initialize(file_name)
    @file_name = file_name
    @status = :pending
  end

  def condition
    case @status
    when :success
      '+'
    when :parse_error
      'p'
    when :typechecking_error
      't'
    when :false_positive
      'x'
    end
  end

  def dump
    @result
  end
end

class SuccessfulTestCase < TestCase
  def run
    @result = result = `sbt #{@file_name}`
    if result =~ /Parse error: .*\nAt line: .*/
      @status = :parse_error
    elsif result =~ /cs260.lwnn.Illtyped.*/
      @status = :typechecking_error
    else
      @status = :success
    end
    print condition; self
  end
end

class FailingTestCase < TestCase
  def run
    @result = result = `sbt #{@file_name}`
    if result =~ /Parse error: .*\nAt line: .*/ || result =~ /cs260.lwnn.Illtyped/
      @status = :success
    else
      @status = :false_positive
    end
    print condition; self
  end
end


class TestRunner
  def initialize(passing_tests, failing_tests)
    @should_pass = passing_tests.map { |x| SuccessfulTestCase.new(passing_tests) }
    @should_fail = failing_tests.map { |x| FailingTestCase.new(failing_tests) }
    @results = [] 
  end

  def run
    puts "-" * 10
    passing = @should_pass.map(&:run)
    failing = @should_fail.map(&:run)
    passing.each do |tc|
      tc.dump if tc.status =~ /error/
    end
    
    failing.each do |tc|
      tc.dump if tc.status == :false_positive
    end
    puts "\n" + "-" * 10
  end
end

tr = TestRunner.new(Dir.foreach(Dir.pwd + '/test/success/').to_a, Dir.foreach(Dir.pwd + '/test/failure/').to_a)
tr.run


