use std::collections::HashMap;
use std::io;
use std::num::ParseFloatError;

/* 
  Types
*/

#[derive(Clone)]
enum RispExp {
  Bool(bool),
  Symbol(String),
  Number(f64),
  List(Vec<RispExp>),
  Func(fn(&[RispExp]) -> Result<RispExp, RispErr>),
}

#[derive(Debug)]
enum RispErr {
  Reason(String),
}

struct RispEnv {
  data: HashMap<String, RispExp>,
}

impl PartialEq for RispExp {
  fn eq(&self, other: &RispExp) -> bool {
    match (self, other) {
      (RispExp::Bool(ref a), RispExp::Bool(ref b)) => a == b,
      (RispExp::Symbol(ref a), RispExp::Symbol(ref b)) => a == b,
      (RispExp::Number(ref a), RispExp::Number(ref b)) => a == b,
      (RispExp::List(ref a), RispExp::List(ref b)) => a == b,
      _ => false,
    }
  }
}

/*
  Print
*/

fn to_str(exp: &RispExp) -> String {
  match exp {
    RispExp::Symbol(s) => s.clone(),
    RispExp::Number(n) => n.to_string(),
    RispExp::Bool(b) => b.to_string(),
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
    RispExp::Number(num) => Ok(*num),
    _ => Err(
      RispErr::Reason(
        format!("expected a number, got form='{}'", to_str(exp))
      )
    ),
  }
}

fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
  return args
    .iter()
    .map(|x| parse_single_float(x))
    .collect::<Result<Vec<f64>, RispErr>>();
}

macro_rules! ensure_tonicity {
  ($check_fn:expr) => {{
    |args: &[RispExp]| -> Result<RispExp, RispErr> {
      let floats = parse_list_of_floats(args)?;
      let first = floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
      let rest = &floats[1..];
      fn f (prev: &f64, xs: &[f64]) -> bool {
        match xs.first() {
          Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
          None => true,
        }
      };
      return Ok(RispExp::Bool(f(first, rest)));
    }
  }};
}

fn default_env() -> RispEnv {
  let mut data: HashMap<String, RispExp> = HashMap::new();
  data.insert(
    "+".to_string(), 
    RispExp::Func(
      |args: &[RispExp]| -> Result<RispExp, RispErr> {
        let sum = parse_list_of_floats(args)?.iter().fold(0.0, |sum, a| sum + a);
        return Ok(RispExp::Number(sum));
      }
    )
  );
  data.insert(
    "-".to_string(), 
    RispExp::Func(
      |args: &[RispExp]| -> Result<RispExp, RispErr> {
        let floats = parse_list_of_floats(args)?;
        let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
        let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

        return Ok(RispExp::Number(first - sum_of_rest));
      }
    )
  );
  data.insert(
    "=".to_string(), 
    RispExp::Func(ensure_tonicity!(|a, b| a == b))
  );
  data.insert(
    ">".to_string(), 
    RispExp::Func(ensure_tonicity!(|a, b| a > b))
  );
  data.insert(
    ">=".to_string(), 
    RispExp::Func(ensure_tonicity!(|a, b| a >= b))
  );
  data.insert(
    "<".to_string(), 
    RispExp::Func(ensure_tonicity!(|a, b| a < b))
  );
  data.insert(
    "<=".to_string(), 
    RispExp::Func(ensure_tonicity!(|a, b| a <= b))
  );
  
  return RispEnv {data: data}
}

/* 
  Eval
*/

fn eval_if_args(arg_forms: &[RispExp], env: &RispEnv) -> Result<RispExp, RispErr> {
  let test_form = arg_forms.first().ok_or(
    RispErr::Reason(
      "expected test form".to_string(),
    )
  )?;
  let test_eval = eval(test_form, env)?;
  match test_eval {
    RispExp::Bool(b) => {
      let form_idx = if b { 1 } else { 2 };
      let res_form = arg_forms.get(form_idx)
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

fn eval_built_in_symbol(sym: String, arg_forms: &[RispExp], env: &RispEnv) -> Result<RispExp, RispErr> {
  match sym.as_ref() {
    "if" => eval_if_args(arg_forms, env),
    _ => Err(RispErr::Reason(format!("unknown built-in symbol='{}'", sym))),
  }
}

fn eval(exp: &RispExp, env: &RispEnv) -> Result<RispExp, RispErr> {
  match exp {
    RispExp::Symbol(k) =>
      env.data.get(k)
        .or(Some(exp))
        .ok_or(
          RispErr::Reason(
            format!("unexpected symbol k='{}'", k)
          )
        )
        .map(|x| x.clone())
    ,
    RispExp::Bool(_a) => Ok(exp.clone()),
    RispExp::Number(_a) => Ok(exp.clone()),
    RispExp::List(list) => {
      let first_form = list
        .first()
        .ok_or(RispErr::Reason("expected a non-empty list".to_string()))?;
      let arg_forms = &list[1..];
      let first_eval = eval(first_form, env)?;
      return match first_eval {
        RispExp::Symbol(sym) => eval_built_in_symbol(sym, arg_forms, env),
        RispExp::Func(f) => {
          let arg_evals = arg_forms
            .iter()
            .map(|x| eval(x, env))
            .collect::<Result<Vec<RispExp>, RispErr>>()?;
          return f(&arg_evals);
        },
        _ => Err(
          RispErr::Reason(
            format!("first form must be a function, but got form='{}'", to_str(&first_eval))
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

fn read_seq(tokens: &[String], start: usize) -> Result<(RispExp, usize), RispErr> {
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

fn parse_atom(token: &str) -> RispExp {
  match token.as_ref() {
    "true" => RispExp::Bool(true),
    "false" => RispExp::Bool(false),
    _ => {
      let potential_float: Result<f64, ParseFloatError> = token.parse();
      return match potential_float {
        Ok(v) => RispExp::Number(v),
        Err(_) => RispExp::Symbol(token.to_string().clone())
      }
    }
  }
}

fn parse(tokens: &[String], pos: usize) -> Result<(RispExp, usize), RispErr> {
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
      (parse_atom(token), pos + 1)
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