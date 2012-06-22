#!/usr/bin/env ruby

class LanguageSyntaxError < StandardError  
end

class Tokenizer
  def initialize s
    @string = s
    @text = nil
    @type = nil
    scan_impl
  end

  def text
    return @text
  end

  def type
    return @type
  end

  def set_token(text, type)
    #puts "token '#{text}' of type '#{type}'; string now '#{@string}'"
    @text = text
    @type = type
  end

  def scan
    scan_impl
    return @text
  end

  def scan_impl
    m = /^\s+(.*?)$/.match @string
    @string = m[1] if not m.nil?

    if @string.empty?
      set_token(nil, nil)
      return
    end

    # check for any of: (, ), comma, return as single token
    m = /^([(),])(.*?)$/.match @string
    if m
      @string = m[2]
      set_token(m[1], 'seperator')
      return
    end

    # check for strings of: >, <, =, !,
    m = /^([<>=!]+)(.*?)$/.match @string
    if m
      @string = m[2]
      set_token(m[1], 'relop')
      return
    end

    # check for strings of "word" characters
    m = /^(\w+)(.*?)$/.match @string
    if m
      @string = m[2]
      set_token(m[1], 'atom')
      return
    end

    set_token(nil, nil)
  end

  def consume s
    if @text == s
      scan
      true
    else
      false
    end
  end

  def expect s
    if @text == s
      scan
    else
      raise LanguageSyntaxError, "expected '#{s}', found '#{@text}'"
    end
  end
end

# Expr ::= Atom | "(" Expr {"," Expr} ")".

class Parser
  def initialize s
    @tokenizer = Tokenizer.new(s)
  end

  def expr
    if @tokenizer.consume "("
      exprs = []
      e = expr
      exprs.push e
      while @tokenizer.consume ","
        e = expr
        exprs.push e
      end
      @tokenizer.expect ")"
      r = List.new(exprs)
      #puts "Returning #{r}"
      return r
    else
      if @tokenizer.type == 'atom'
        r = Atom.new(@tokenizer.text)
        @tokenizer.scan
        #puts "Returning #{r}"
        return r
      else
        raise LanguageSyntaxError, "expected atom, found #{@tokenizer.type}"
      end
    end
  end
end

class Atom
  def initialize t
    @t = t
  end
  
  def to_s
    return ".#{@t}"
  end
end

class List
  def initialize l
    @l = l
  end
  
  def to_s
    s = "("
    for e in @l
      s += e.to_s + " "
      end
    s += ")"
    return s
  end
end

### Main ###

p = Parser.new("(a, b, (c, d, e), f, ((g)))")
print p.expr
