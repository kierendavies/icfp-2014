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
    "def" => 2,
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
        elsif match = /^(?<instruction>\w+)(\s+(?<args>(\w+|\[\w+\])((,|\s)\s*(\w+|\[\w+\])){0,2}))?/.match(main)
          unless INSTRUCTIONS.include?(match[:instruction])
            raise "invalid instruction: #{main}"
          end
          @instruction = match[:instruction]
          if match[:args]
            @args = match[:args].split(/\s*[,\s]\s*/)
          else
            @args = []
          end
          if args.length != INSTRUCTIONS[@instruction]
            puts match.inspect
            raise "invalid number of arguments: #{main}"
          end
          args.each_with_index do |arg, i|
            arg = arg.tr("_", "")
            [DIRECTIONS, VITALITY, BLOCKS, INTERRUPTS].each do |sublist|
              if sublist.include?(arg)
                @args[i] = sublist.find_index(arg)
              end
            end
          end
          if @instruction == "def"
            @type = :definition
          else
            @type = :instruction
          end
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

    def definition
      if @type == :definition
        return args
      else
        nil
      end
    end

    def substitute!(subs)
      if instruction?
        @args.each_with_index do |arg, i|
          brackets = false
          if match = /^\[(?<arg>.*)\]$/.match(arg.to_s)
            arg = match[:arg]
            brackets = true
          end
          if subs.include?(arg)
            arg = subs[arg]
            if brackets
              arg = "[#{arg}]"
            end
            @args[i] = arg
          end
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

  def self.assemble(source)
    lines = []
    labels = {}
    definitions = {}

    index = 0
    source.each_line do |l|
      line = Line.new(l)
      if line.instruction?
        lines << line
        index += 1
      elsif line.label?
        labels[line.label] = index
      elsif d = line.definition
        definitions[d[0]] = d[1]
      end
    end

    lines.each do |line|
      line.substitute!(labels)
      line.substitute!(definitions)
    end

    return lines.join("\n") + "\n"
  end
end

out = ""
File::open(ARGV[0]) do |f|
  out = Assembler.assemble(f.read)
end
File::open(ARGV[0].sub(/.\w*$/, ".ghc"), "w") do |f|
  f.write(out)
end
