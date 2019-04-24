use std::fs;
use std::time::Instant;
pub fn main()
{
  let start = Instant::now();
  let data = fs::read_to_string("input_day3")
  .unwrap()
  .lines()
  .map( |line| 
    line.chars().filter(|c| *c != '#' && *c != '@' && *c != ':')
    .collect::<String>()
    .split(|c| c == ' ' || c == ',' || c == 'x')
    .filter(|s| *s != "")
    .map(|s| s.parse::<i32>().unwrap())
    .collect::<Vec<_>>()
  )
  .map( |v| 
    (v[0],v[1],v[2],v[3],v[4])
  ).
  collect::<Vec<_>>();
  println!("ID of remaining square: {:?}",
  data.iter()
  .find(|a| 
    !data.iter().any(|b|
      (a.0 != b.0) && (intersect( (a.1,a.2,a.3,a.4) , (b.1,b.2,b.3,b.4) )  || intersect( (b.1,b.2,b.3,b.4) , (a.1,a.2,a.3,a.4))) 
      )
    ).map(|(x,_,_,_,_)| x).unwrap_or(&-1)
  );

  println!("Time elaspsed {}ms",start.elapsed().as_millis());
}

fn intersect (a:(i32,i32,i32,i32),b:(i32,i32,i32,i32)) -> bool
{
  let a_x = a.0;
  let a_y = a.1;
  let a_x2 = a.0+a.2-1;
  let a_y2 = a.1+a.3-1;

  let b_x = b.0;
  let b_y = b.1;
  let b_x2 = b.0+b.2-1;
  let b_y2 = b.1+b.3-1;

  ((b_x >= a_x && b_x <= a_x2) && (b_y >= a_y && b_y <= a_y2))
  || ((b_x >= a_x && b_x <= a_x2) && (b_y2 >= a_y && b_y2 <= a_y2))
  || ((b_x2 >= a_x && b_x2 <= a_x2) && (b_y >= a_y && b_y <= a_y2))
  || ((b_x2 >= a_x && b_x2 <= a_x2) && (b_y2 >= a_y && b_y <= a_y2))
}
