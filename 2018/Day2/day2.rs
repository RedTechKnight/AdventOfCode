use std::fs;
use std::time::Instant;
pub fn main()
{
  let input = fs::read_to_string("input_day2").unwrap_or_default();
  let start = Instant::now();

  println!("The checksum is {}",
  input.lines().filter(|l| 
    l.chars().any( |ch| 
      l.chars().filter(|c| *c == ch).count() == 2)
  ).count()
  *
  input.lines().filter(|l| 
   l.chars().any( |ch| 
     l.chars().filter(|c| *c == ch).count() == 3)
  ).count()
  );
  
  println!("Time elapsed: {}ms",start.elapsed().as_millis());

  let start = Instant::now();

  let ans = input.lines().filter( |line_a| 
    input.lines().any( |line_b| 
    
      line_a.chars().zip(line_b.chars()).filter(|(a,b)| a == b).count() == line_a.len()-1
    )
  ).fold(String::new(),|acc,l|
    if acc == ""
    {
      String::from(l)
    }
    else
    {
      acc.chars().zip(l.chars()).filter(|(a,b)| a == b).map(|(a,_)| a).collect::<String>()
    }
  );
  
  println!("Ans: {:?}",ans);
  println!("Time elapsed: {}ms",start.elapsed().as_millis());
}