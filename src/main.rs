use std::io;
use std::num::ParseFloatError;


enum RispAtom {
  Symbol(String),
  Number(f64),
}

enum RispExp {
  List(Vec<RispExp>),
  Atom(RispAtom),
}

enum RispErr {
  Reason(String),
}

/*
def eval(x: Exp, env=global_env) -> Exp:
    "Evaluate an expression in an environment."
    if isinstance(x, Symbol):        # variable reference
        return env[x]
    elif isinstance(x, Number):      # constant number
        return x                
    elif x[0] == 'if':               # conditional
        (_, test, conseq, alt) = x
        exp = (conseq if eval(test, env) else alt)
        return eval(exp, env)
    elif x[0] == 'define':           # definition
        (_, symbol, exp) = x
        env[symbol] = eval(exp, env)
    else:                            # procedure call
        proc = eval(x[0], env)
        args = [eval(arg, env) for arg in x[1:]]
        return proc(*args)

def parse(program: str) -> Exp:
    "Read a Scheme expression from a string."
    return read_from_tokens(tokenize(program))

def read_from_tokens(tokens: list) -> Exp:
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF')
    token = tokens.pop(0)
    if token == '(':
        L = []
        while tokens[0] != ')':
            L.append(read_from_tokens(tokens))
        tokens.pop(0) # pop off ')'
        return L
    elif token == ')':
        raise SyntaxError('unexpected )')
    else:
        return atom(token)
def atom(token: str) -> Atom:
    "Numbers become numbers; every other token is a symbol."
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            return Symbol(token)
*/

fn parse(tokens: Vec<String>, pos: usize) -> Result<(RispExp, usize), RispErr> {
  let token = tokens
    .get(pos)
    .ok_or(RispErr::Reason("token and pos mismatch".to_string()))
    ?;
  let to_match = &token[..];
  match to_match {
    "(" => Err(RispErr::Reason("implement lists!".to_string())),
    ")" => Err(RispErr::Reason("unexpected `)`".to_string())),
    _ => {
      let potential_float: Result<f64, ParseFloatError> = to_match.parse();
      return match potential_float {
        Ok(v) => Ok(
          (
            RispExp::Atom(RispAtom::Number(v)),
            pos + 1,
          )
        ),
        Err(_) => Ok(
          (
            RispExp::Atom(RispAtom::Symbol(to_match.to_string())),
            pos + 1,
          )
        ),
      }
    },
  }
}

fn tokenize(expr: String) -> Vec<String> {
  return expr
    .replace("(", " ( ")
    .replace(")", " ) ")
    .split(" ")
    .map(|x| x.to_string())
    .collect();
}

fn slurp_expr() -> String {
  let mut expr = String::new();
  
  io::stdin().read_line(&mut expr)
    .expect("Failed to read line");

  return expr;
}

fn main() {
  loop {
    println!("risp >");
    let _res = parse(
      tokenize(slurp_expr()),
      0
    );
  }
}