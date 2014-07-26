#!/usr/bin/env ruby

module Assembler
  INSTRUCTIONS = {  # and number of args
    "mov" => 2,
    "inc" => 1,
    "dec" => 1,
    "add" => 2,
    "sub" => 2,
    "mul" => 2,
    "div" => 2,
    "and" => 2,
    "or"  => 2,
    "xor" => 2,
    "jlt" => 3,
    "jeq" => 3,
    "jgt" => 3,
    "int" => 1,
    "hlt" => 0,
  }
  DIRECTIONS = ["up", "right", "down", "left"]
  VITALITY = ["standard", "fright", "invisible"]
  BLOCKS = ["wall", "empty", "pill", "power", "fruit", "lambdastart", "ghoststart"]
  INTERRUPTS = ["setdir", "getlambdapos", "getlambda2pos", "getindex", "getghoststart", "getghostpos", "getvitdir", "getblock", "trace"]

  class Line
    attr_accessor :type, :comment, :label, :instruction, :args

    def initialize(str)
      if match = /^\s*(?<main>[^;]*)\s*(;(?<comment>.*))?/.match(str)
        if match[:comment] && !match[:comment].empty?
          @comment = match[:comment].chomp
          @type = :comment
        end

        main = match[:main].chomp.downcase
        if main.empty?
          return
        elsif match = /^(?<label>\w+):/.match(main)
          @label = match[:label]
          @type = :label
        elsif match = /^(?<instruction>\w+)(\s+(?<args>(\w+|\[\w+\])(,\s*(\w+|\[\w+\])){0,2}))?/.match(main)
          unless INSTRUCTIONS.include?(match[:instruction])
            raise "invalid instruction: #{main}"
          end
          @instruction = match[:instruction]
          if match[:args]
            @args = match[:args].split(/\s*,\s*/)
          else
            @args = []
          end
          if args.length != INSTRUCTIONS[@instruction]
            puts match.inspect
            raise "invalid number of arguments: #{main}"
          end
          @type = :instruction
        else
          raise "invalid syntax: #{main}"
        end
      end
    end

    def instruction?
      @type == :instruction
    end

    def label?
      @type == :label
    end

    def sub_label(labels)
      if instruction? && @instruction.start_with?("j")
        if labels.include?(@args[0])
          @args[0] = labels[@args[0]]
        end
      end
    end

    def to_s
      s = ""
      if instruction?
        s << @instruction
        s << " " + @args.join(",") unless @args.empty?
      end
      return s
    end
  end

  def self.assemble(file)
    lines = []
    labels = {}

    File::open(file) do |f|
      index = 0
      f.readlines.each do |l|
        line = Line.new(l)
        if line.instruction?
          lines << line
          index += 1
        elsif line.label?
          labels[line.label] = index
        end
      end
    end

    lines.each do |line|
      line.sub_label(labels)
    end

    puts lines.join "\n"
  end
end

Assembler.assemble(ARGV[0])
