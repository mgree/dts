signature RUNTIME =
  sig
  
  structure Boolean: BOOLEAN
  structure Pair: PAIR
  structure List: LIST
  structure Symbol: SYMBOL
  structure Number: NUMBER
  structure Character: CHARACTER
  structure String: STRING
  structure Vector: VECTOR
  structure Control: CONTROL
  structure InputOutput: INPUTOUTPUT
  structure Dynamic: DYNAMIC
  structure Environment: ENVIRONMENT
  structure Basis: BASIS

  sharing type Boolean.boolean = Dynamic.boolean
  and type Pair.pair = Dynamic.pair
  and type List.slist = Dynamic.slist
  and type Symbol.symbol = Dynamic.symbol
  and type Number.number = Dynamic.number
  and type Character.char = Dynamic.char
  and type String.sstring = Dynamic.sstring
  and type Vector.vector = Dynamic.vector
  and type Control.procedure = Dynamic.procedure
  and type InputOutput.input_port = Dynamic.input_port
  and type InputOutput.output_port = Dynamic.output_port
  and type Environment.value = Dynamic.dynamic = Basis.value
  and type Environment.key = string
  end

structure Runtime: RUNTIME =
  struct

  structure Boolean = Boolean
  structure Pair = Pair
  structure List = List
  structure Symbol = Symbol
  structure Number = Number
  structure Character = Character
  structure String = String
  structure Vector = Vector
  structure Control = Control
  structure InputOutput = InputOutput
  structure Dynamic = Dynamic
  structure Environment = Environment
  structure Basis = Basis

  end
