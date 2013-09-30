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
    puts @result
  end
end

class SuccessfulTestCase < TestCase
  def run
    @result = result = `sbt "run #{@file_name}"`
    if result =~ /Parse error: .*\nAt line: .*/
      @status = :parse_error
    elsif result =~ /TypeError.*/
      @status = :typechecking_error
    else
      @status = :success
    end
    print condition; self
  end
end

class FailingTestCase < TestCase
  def run
    @result = result = `sbt "run #{@file_name}"`
    if result =~ /Parse error: .*\nAt line: .*/ || result =~ /TypeError/
      @status = :success
    else
      @status = :false_positive
    end
    print condition; self
  end
end


class TestRunner
  def initialize(passing_tests, failing_tests)
    @should_pass = passing_tests.map { |x| SuccessfulTestCase.new(x) }
    @should_fail = failing_tests.map { |x| FailingTestCase.new(x) }
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

test_success = 'test/success/'
test_failure = 'test/failure/'
good = Dir.foreach(Dir.pwd + '/' + test_success).to_a.select { |f| f[0] != '.' }.map { |f| test_success + f }
bad = Dir.foreach(Dir.pwd + '/' + test_failure).to_a.select { |f| f[0] != '.' }.map { |f| test_failure + f }
tr = TestRunner.new(good, bad)
tr.run


