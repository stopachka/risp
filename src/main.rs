use std::collections::HashMap;
use std::io;
use std::num::ParseFloatError;

/* 
  Types
*/

#[derive(Clone)]
enum RispAtom {
  Symbol(String),
  Number(f64),
}

#[derive(Clone)]
enum RispExp {
  List(Vec<RispExp>),
  Atom(RispAtom),
  Func(fn(&RispExp) -> Result<RispExp, RispErr>),
}

#[derive(Debug)]
enum RispErr {
  Reason(String),
}

struct RispEnv {
  data: HashMap<String, RispExp>,
}

/* 
  Eval
*/

fn parse_single_float(x: &RispExp) -> Result<f64, RispErr> {
  return match x {
    RispExp::Atom(v) => {
      return match v {
        RispAtom::Number(num) => Ok(*num),
        RispAtom::Symbol(_) => Err(RispErr::Reason("expected numb".to_string())),
      }
    },
    _ => Err(RispErr::Reason("expected a single number".to_string()))
  }
}

fn parse_list_of_floats(x: &RispExp) -> Result<Vec<f64>, RispErr> {
  match x {
    RispExp::List(list) => {
      let xs = list.iter().map(|x| parse_single_float(x)).map(Result::unwrap).collect();
      return Ok(xs);
    },
    _ => {
      let x = parse_single_float(x)?;
      return Ok(vec![x]);
    }
  }
}

fn default_env() -> RispEnv {
  let mut data: HashMap<String, RispExp> = HashMap::new();
  data.insert(
    "+".to_string(), 
    RispExp::Func(
      |x: &RispExp| -> Result<RispExp, RispErr> {
        let sum = parse_list_of_floats(x)?.iter().fold(0.0, |sum, a| sum + a);
        return Ok(RispExp::Atom(RispAtom::Number(sum)));
      }
    )
  );
  data.insert(
    "-".to_string(), 
    RispExp::Func(
      |x: &RispExp| -> Result<RispExp, RispErr> {
        let floats = parse_list_of_floats(x)?;
        let first = *floats.get(0).ok_or(RispErr::Reason("expected at least one number".to_string()))?;
        let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

        return Ok(RispExp::Atom(RispAtom::Number(first - sum_of_rest)));
      }
    )
  );
  return RispEnv {data: data}
}

fn eval(exp: &RispExp, env: &RispEnv) -> Result<RispExp, RispErr> {
  match exp {
    RispExp::Atom(atom) => match atom {
      RispAtom::Symbol(k) => match env.data.get(k) {
        Some(v) => Ok(v.clone()),
        _ => Err(RispErr::Reason("uh oh unexpected symbol".to_string()))
      }
      _ => Ok(exp.clone()),
    },
    RispExp::List(list) => {
      let first = list.get(0).ok_or(RispErr::Reason("uh oh could not get first item".to_string()))?;    
      let first_res = eval(first, env)?;
      let evaled_rest: Vec<RispExp> = list[1..].iter().map(|x| eval(x, env).unwrap()).collect();
      let evaled_exp = RispExp::List(evaled_rest);
      return match first_res {
        RispExp::Func(f) => f(&evaled_exp),
        _ => Err(RispErr::Reason("uh oh unexpected first arg".to_string())),
      }
    },
    RispExp::Func(_) => Err(RispErr::Reason("uh oh unexpected input".to_string())),
  }
}

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

fn to_str(exp: &RispExp) -> Result<String, RispErr> {
  match exp {
    RispExp::Atom(a) => {
      return match a {
        RispAtom::Symbol(s) => Ok(s.clone()),
        RispAtom::Number(n) => Ok(n.to_string()),
      }
    },
    RispExp::List(list) => {
      let str_items: Vec<String> = list
        .iter()
        .map(|x| to_str(x))
        .map(Result::unwrap)
        .collect();
      return Ok(format!("({})", str_items.join(",")));
    },
    _ => Err(RispErr::Reason("uh oh can't print str".to_string()))
  }
}

fn parse_eval_print(expr: String) -> Result<String, RispErr> {
  let (parsed_exp, _) = parse(&tokenize(expr), 0)?;
  let evaled_exp = eval(&parsed_exp, &default_env())?;
  return to_str(&evaled_exp);
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
    let expr = slurp_expr();
    let res = parse_eval_print(expr).unwrap();
    println!("//=> {}", res);
  }
}