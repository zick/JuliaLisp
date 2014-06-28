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

sym_t = makeSym("t")
sym_quote = makeSym("quote")

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
  if isa(obj, Cons)
    obj.car
  else
    kNil
  end
end

function safeCdr(obj)
  if isa(obj, Cons)
    obj.cdr
  else
    kNil
  end
end

makeExpr(args, env) = Expr(safeCar(args), safeCdr(args), env)

function nreverse(lst)
  ret = kNil
  while isa(lst, Cons)
    tmp = lst.cdr
    lst.cdr = ret
    ret = lst
    lst = tmp
  end
  ret
end

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
    return readList(str[2:])
  elseif str[1] == kQuote
    elm, next = read1(str[2:])
    return Cons(sym_quote, Cons(elm, kNil)), next
  else
    return readAtom(str)
  end
end

function readList(str)
  ret = kNil
  while true
    str = skipSpaces(str)
    if str == ""
      return parseError("unfinished parenthesis")
    elseif str[1] == kRPar
      break
    end
    elm, next = read1(str)
    if isa(elm, Error)
      return elm, next
    end
    ret = Cons(elm, ret)
    str = next
  end
  nreverse(ret), str[2:]
end

function printObj(obj)
  if isa(obj, Nil)
    return "nil"
  elseif isa(obj, Num)
    return string(obj.data)
  elseif isa(obj, Sym)
    return obj.data
  elseif isa(obj, Error)
    return string("<error: ", obj.data, ">")
  elseif isa(obj, Cons)
    return printList(obj)
  elseif isa(obj, Subr)
    return "<subr>"
  elseif isa(obj, Expr)
    return "<expr>"
  else
    return "<unknown>"
  end
end

function printList(obj)
  ret = ""
  first = true
  while isa(obj, Cons)
    if first
      first = false
    else
      ret = string(ret, " ")
    end
    ret = string(ret, printObj(obj.car))
    obj = obj.cdr
  end
  if obj == kNil
    return string("(", ret, ")")
  end
  string("(", ret, " . ", printObj(obj), ")")
end

function findVar(sym, env)
  while isa(env, Cons)
    alist = env.car
    while isa(alist, Cons)
      if alist.car.car == sym
        return alist.car
      end
      alist = alist.cdr
    end
    env = env.cdr
  end
  kNil
end

g_env = Cons(kNil, kNil)

addToEnv(sym, val, env) = env.car = Cons(Cons(sym, val), env.car)

function eval1(obj, env)
  if isa(obj, Nil) || isa(obj, Num) || isa(obj, Error)
    return obj
  elseif isa(obj, Sym)
    bind = findVar(obj, env)
    if bind == kNil
      return Error(string(obj.data, " has no value"))
    end
    return bind.cdr
  end
  Error("noimpl")
end

addToEnv(sym_t, sym_t, g_env)

while true
  print("> ")
  line = readline(STDIN)
  if line == "" break end
  println(printObj(eval1(read1(line)[1], g_env)))
end
