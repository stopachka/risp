use std::collections::HashMap;
use std::io;
use std::num::ParseFloatError;

/* 
  Types
*/

#[derive(Debug, Clone)]
enum RispAtom {
  Bool(bool),
  Symbol(String),
  Number(f64),
}

#[derive(Clone)]
enum RispExp {
  List(Vec<RispExp>),
  Atom(RispAtom),
  Func(fn(&Vec<RispExp>) -> Result<RispExp, RispErr>),
}

#[derive(Debug)]
enum RispErr {
  Reason(String),
}

struct RispEnv {
  data: HashMap<String, RispExp>,
}

impl PartialEq for RispAtom {
  fn eq(&self, other: &RispAtom) -> bool {
    match (self, other) {
      (RispAtom::Bool(ref a), RispAtom::Bool(ref b)) => a == b,
      (RispAtom::Symbol(ref a), RispAtom::Symbol(ref b)) => a == b,
      (RispAtom::Number(ref a), RispAtom::Number(ref b)) => a == b,
      _ => false,
    }
  }
}

impl PartialEq for RispExp {
  fn eq(&self, other: &RispExp) -> bool {
    match (self, other) {
      (RispExp::Atom(ref a), RispExp::Atom(ref b)) => a == b,
      (RispExp::List(ref a), RispExp::List(ref b)) => a == b,
      (RispExp::Func(ref _a), RispExp::Func(ref _b)) => false,
      _ => false,
    }
  }
}

/*
  Print
*/

fn to_str(exp: &RispExp) -> String {
  match exp {
    RispExp::Atom(a) => {
      return match a {
        RispAtom::Symbol(s) => s.clone(),
        RispAtom::Number(n) => n.to_string(),
        RispAtom::Bool(b) => b.to_string(),
      }
    },
    RispExp::List(list) => {
      let xs: Vec<String> = list
        .iter()
        .map(|x| to_str(x))
        .collect();
      return format!("({})", xs.join(","));
    },
    RispExp::Func(_) => "Function {}".to_string(),
  }
}

/* 
  Env
*/

fn parse_single_float(exp: &RispExp) -> Result<f64, RispErr> {
  match exp {
    RispExp::Atom(RispAtom::Number(num)) => Ok(*num),
    _ => Err(
      RispErr::Reason(
        format!("expected a number, got form='{}'", to_str(exp))
      )
    ),
  }
}

fn parse_list_of_floats(args: &Vec<RispExp>) -> Result<Vec<f64>, RispErr> {
  return args
    .iter()
    .map(|x| parse_single_float(x))
    .collect::<Result<Vec<f64>, RispErr>>();
}

fn default_env() -> RispEnv {
  let mut data: HashMap<String, RispExp> = HashMap::new();
  data.insert(
    "+".to_string(), 
    RispExp::Func(
      |args: &Vec<RispExp>| -> Result<RispExp, RispErr> {
        let sum = parse_list_of_floats(args)?.iter().fold(0.0, |sum, a| sum + a);
        return Ok(RispExp::Atom(RispAtom::Number(sum)));
      }
    )
  );
  data.insert(
    "-".to_string(), 
    RispExp::Func(
      |args: &Vec<RispExp>| -> Result<RispExp, RispErr> {
        let floats = parse_list_of_floats(args)?;
        let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
        let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

        return Ok(RispExp::Atom(RispAtom::Number(first - sum_of_rest)));
      }
    )
  );
  data.insert(
    "=".to_string(), 
    RispExp::Func(
      |args: &Vec<RispExp>| -> Result<RispExp, RispErr> {
        let first = args.first().ok_or(RispErr::Reason("expected at least one arg".to_string()))?;
        return Ok(
          RispExp::Atom(
            RispAtom::Bool(args[1..].iter().all(|x| first == x))
          )
        );
      }
    )
  );
  data.insert(
    ">".to_string(), 
    RispExp::Func(
      |args: &Vec<RispExp>| -> Result<RispExp, RispErr> {
        let floats = parse_list_of_floats(args)?;
        let first = floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
        let rest = &floats[1..];
        fn f (prev: &f64, xs: &[f64]) -> bool {
          match xs.first() {
            Some(x) => prev > x && f(x, &xs[1..]),
            None => true,
          }
        };
        return Ok(
          RispExp::Atom(
            RispAtom::Bool(f(first, rest))
          )
        );
      }
    )
  );
  return RispEnv {data: data}
}

/* 
  Eval
*/

fn eval_if_args(args_to_eval: &[RispExp], env: &RispEnv) -> Result<RispExp, RispErr> {
  let test_form = args_to_eval.first().ok_or(
    RispErr::Reason(
      "expected test form".to_string(),
    )
  )?;
  let test_eval = eval(test_form, env)?;
  match test_eval {
    RispExp::Atom(RispAtom::Bool(b)) => {
      let form_idx = if b { 1 } else { 2 };
      let res_form = args_to_eval.get(form_idx)
        .ok_or(RispErr::Reason(
          format!("expected form idx={}", form_idx)
        ))?;
      let res_eval = eval(res_form, env);
      
      return res_eval;
    },
    _ => Err(
      RispErr::Reason(format!("unexpected test form='{}'", to_str(test_form)))
    )
  }
}

fn eval_built_in_symbol(sym: String, args_to_eval: &[RispExp], env: &RispEnv) -> Result<RispExp, RispErr> {
  match sym.as_ref() {
    "if" => eval_if_args(args_to_eval, env),
    _ => Err(RispErr::Reason(format!("unknown built-in symbol='{}'", sym))),
  }
}

fn eval(exp: &RispExp, env: &RispEnv) -> Result<RispExp, RispErr> {
  match exp {
    RispExp::Atom(RispAtom::Symbol(k)) =>
      env.data.get(k)
        .or(Some(exp))
        .ok_or(
          RispErr::Reason(
            format!("unexpected symbol k='{}'", k)
          )
        )
        .map(|x| x.clone())
    ,
    RispExp::Atom(_a) => Ok(exp.clone()),
    RispExp::List(list) => {
      let first_to_eval = list
        .first()
        .ok_or(RispErr::Reason("expected a non-empty list".to_string()))?;
      let args_to_eval = &list[1..];
      let first = eval(first_to_eval, env)?;
      return match first {
        RispExp::Atom(RispAtom::Symbol(sym)) => eval_built_in_symbol(sym, args_to_eval, env),
        RispExp::Func(f) => {
          let args = args_to_eval
            .iter()
            .map(|x| eval(x, env))
            .collect::<Result<Vec<RispExp>, RispErr>>()?;
          return f(&args);
        },
        _ => Err(
          RispErr::Reason(
            format!("first form must be a function, but got form='{}'", to_str(&first))
          )
        ),
      }
    },
    RispExp::Func(_) => Err(
      RispErr::Reason(
        format!("unexpected form='{}'", to_str(exp))
      )
    ),
  }
}

/* 
  Parse
*/

fn read_seq(tokens: &Vec<String>, start: usize) -> Result<(RispExp, usize), RispErr> {
  let mut res: Vec<RispExp> = vec![];
  let mut next = start;
  loop {
    let next_token = tokens
      .get(next)
      .ok_or(RispErr::Reason("could not find closing `)`".to_string()))
      ?;
    if next_token == ")" {
      return Ok((RispExp::List(res), next + 1)) // skip `)`, head to the token after
    }
    let (exp, new_next) = parse(&tokens, next)?;
    res.push(exp);
    next = new_next;
  }
}

fn parse_atom(token: &String) -> RispAtom {
  match token.as_ref() {
    "true" => RispAtom::Bool(true),
    "false" => RispAtom::Bool(false),
    _ => {
      let potential_float: Result<f64, ParseFloatError> = token.parse();
      return match potential_float {
        Ok(v) => RispAtom::Number(v),
        Err(_) => RispAtom::Symbol(token.clone()),
      }
    }
  }
}

fn parse(tokens: &Vec<String>, pos: usize) -> Result<(RispExp, usize), RispErr> {
  let token = tokens
    .get(pos)
    .ok_or(
      RispErr::Reason(format!("could not get token for pos='{}'", pos))
    )?;
  let to_match = &token[..];
  match to_match {
    "(" => read_seq(tokens, pos + 1),
    ")" => Err(RispErr::Reason("unexpected `)`".to_string())),
    _ => Ok(
      (RispExp::Atom(parse_atom(token)), pos + 1)
    ),
  }
}

fn tokenize(expr: String) -> Vec<String> {
  return expr
    .replace("(", " ( ")
    .replace(")", " ) ")
    .split(" ")
    .map(|x| x.trim().to_string())
    .filter(|x| !x.is_empty())
    .collect();
}

/*
  REPL
*/

fn parse_eval_print(expr: String) -> Result<String, RispErr> {
  let (parsed_exp, _) = parse(&tokenize(expr), 0)?;
  let evaled_exp = eval(&parsed_exp, &default_env())?;
  return Ok(to_str(&evaled_exp));
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
    match parse_eval_print(expr) {
      Ok(res) => println!("// 🔥 => {}", res),
      Err(e) => match e {
        RispErr::Reason(msg) => println!("// 🙀 => {}", msg),
      },
    }
  }
}