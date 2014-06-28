kLPar = '('
kRPar = ')'
kQuote = '\''

type Nil
  dummy
end
kNil = Nil("nil")

type Num
  data
end

type Sym
  data
end

sym_table = Dict()
function makeSym(str)
  if str == "nil"
    return kNil
  elseif !haskey(sym_table, str)
    merge!(sym_table, {str => Sym(str)})
  end
  get(sym_table, str, kNil)
end

type Error
  data
end

type Cons
  car
  cdr
end

type Subr
  fn
end

type Expr
  args
  body
  env
end

function safeCar(obj)
  if (isa(obj, Cons))
    obj.car
  else
    kNil
  end
end

function safeCdr(obj)
  if (isa(obj, Cons))
    obj.cdr
  else
    kNil
  end
end

makeExpr(args, env) = Expr(safeCar(args), safeCdr(args), env)

isDelimiter(c) = c == kLPar || c == kRPar || c == kQuote || isspace(c)

skipSpaces(str) = lstrip(str)

function makeNumOrSym(str)
  try
    Num(int(str))
  catch
    makeSym(str)
  end
end

function readAtom(str)
  next = ""
  for i in 1:length(str)
    if isDelimiter(str[i])
      next = str[i:]
      str = str[1:i-1]
      break
    end
  end
  makeNumOrSym(str), next
end

parseError(str) = Error(str), ""

function read1(str)
  str = skipSpaces(str)
  if str == ""
    return parseError("empty input")
  elseif str[1] == kRPar
    return parseError(string("invalid syntax: ", str))
  elseif str[1] == kLPar
    return parseError("noimpl")
  elseif str[1] == kQuote
    return parseError("noimpl")
  else
    return readAtom(str)
  end
end

while true
  print("> ")
  line = readline(STDIN)
  if line == "" break end
  println(read1(line))
end
