mod lang;

fn main() {
  let expression_1 = "((if (= (+ 3 (/ 9 3))
         (* 2 3))
     *
     /) 
  456 123)";
  println!("\"{}\"\nevaled gives us: {:?}", expression_1, lang::parser::eval_from_str(expression_1));
}