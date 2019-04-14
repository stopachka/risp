use std::collections::HashMap;
use std::io;
use std::num::ParseFloatError;

/* 
  Types
*/

#[derive(Debug)]
enum RispAtom {
  Symbol(String),
  Number(f64),
}

#[derive(Debug)]
enum RispExp {
  List(Vec<RispExp>),
  Atom(RispAtom),
}

#[derive(Debug)]
enum RispErr {
  Reason(String),
}

struct RispEnv<'a> {
  data: HashMap<String,  &'a Fn(RispExp) -> RispExp>,
}

/*

*/

// fn eval(exp: RispExp, env: RispEnv) -> () {
//   match exp {
//     RispExp::Atom(atom) => {
//       match atom {
//         RispAtom::Symbol(sym) => {
//           return env
//             .data
//             .get(sym)
//             .ok_or(RispErr::Reason("could not find symbol {}"))
//           ;
//         },
//         RispAtom::Number(num) => {
//           return num;
//         }
//       }
//     }
//     RispExp::List(list) => {

//     }
//   }
// }

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

/* 
  Eval
*/

/* 
  Parse
*/

fn read_seq(tokens: &Vec<String>, start: usize) -> Result<(RispExp, usize), RispErr> {
  let mut res: Vec<RispExp> = vec![];
  let mut next = start;
  loop {
    let next_token = tokens.get(next).ok_or(RispErr::Reason("could not find closing `)`".to_string()))?;
    if next_token == ")" {
      return Ok((RispExp::List(res), next + 1))
    }
    let (exp, new_next) = parse(&tokens, next)?;
    res.push(exp);
    next = new_next;
  }
}

fn parse_atom(token: String) -> Result<RispExp, RispErr> {
  let potential_float: Result<f64, ParseFloatError> = token.parse();
  return match potential_float {
    Ok(v) => Ok(RispExp::Atom(RispAtom::Number(v))),
    Err(_) => Ok(RispExp::Atom(RispAtom::Symbol(token))),
  }
}

fn parse(tokens: &Vec<String>, pos: usize) -> Result<(RispExp, usize), RispErr> {
  let token = tokens
    .get(pos)
    .ok_or(RispErr::Reason("token and pos mismatch".to_string()))
    ?;
  let to_match = &token[..];
  match to_match {
    "(" => read_seq(tokens, pos + 1),
    ")" => Err(RispErr::Reason("unexpected `)`".to_string())),
    _ => {
      return match parse_atom(to_match.to_string()) {
        Ok(v) => Ok((v, pos + 1)),
        Err(e) => Err(e)
      }
    },
  }
}

fn tokenize(expr: String) -> Vec<String> {
  return expr
    .replace("(", " ( ")
    .replace(")", " ) ")
    .trim()
    .split(" ")
    .map(|x| x.to_string())
    .collect();
}

/*
  REPL
*/

fn slurp_expr() -> String {
  let mut expr = String::new();
  
  io::stdin().read_line(&mut expr)
    .expect("Failed to read line");

  return expr;
}

fn main() {
  loop {
    println!("risp >");
    let res = parse(
      &tokenize(slurp_expr()),
      0
    );
    println!("{:?}", res);
  }
}