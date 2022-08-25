use std::fs;
use std::collections::HashMap;
use std::time::{Duration,Instant};
pub fn main() 
{
  let start = Instant::now();
  let input = fs::read_to_string("input_day1").unwrap();
  println!("Final frequency: {:?}",

  input.
  lines()
  .map( |l| 
    l.parse::<i32>().unwrap()
  )
  .sum::<i32>()
  );
  println!("Time elapsed: {}us",start.elapsed().as_micros());
  let mut freqs: HashMap<i32,i32> = HashMap::new();
  let mut freq = 0;
  let start = Instant::now();
  for i in input.lines().map(|l| l.parse::<i32>().unwrap()).cycle()
  {
    freq += i;
    match freqs.insert(freq,1)
    {
      None => (),
      Some(_) => 
        {
        println!("Duplicate frequency: {}",freq);
        println!("Time elapsed: {}us",start.elapsed().as_micros());
        break
        }
    }
  }
}
