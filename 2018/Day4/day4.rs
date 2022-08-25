use std::time::{Instant};
use std::collections::HashMap;

pub fn main()
{
  let sum_times = |acc:i32,x:&(i32,i32)| -> i32
  {
    acc + (x.1 - x.0)
  };

  let accum_minutes = |acc:Vec<i32>,x:&(i32,i32)|
  {
    acc.into_iter().chain( (x.0..x.1).into_iter() )
  };

  let most_common_minute = |v:Vec<i32>|
  {
    
      v.into_iter()
      .fold(HashMap::new(),|mut acc,x|{ acc.entry(x).and_modify(|e| *e += 1).or_insert(1); acc} )
      .into_iter()
      .max_by(|(ak,av),(bk,bv)|
        av.cmp(bv)
      )
      .unwrap()
  };

  let start = Instant::now();

  let mut data = 
  std::fs::read_to_string("input_day4")
  .map( |d| 
    d.lines()
    .map( |line| {
      let out = line
      .split(|ch| 
        ch == ' ' || ch == '-' || ch == ':' || ch == '[' || ch == ']'
      )
      .map(|str_slice| str_slice.to_string())
      .filter(|x| !x.is_empty())
      .skip(1)
      .take(6);

      let t = out.clone().take(4).map(|x| x.parse::<i32>().unwrap()).collect::<Vec<_>>();
      let e = out.clone().skip(4).take(2).fold(String::new(),|acc,st| acc + " " + &st);

      EventData
      {
        time:(t[0],
        if t[2] == 0 {t[1]} else {t[1]+1},
        if t[2] == 0 {t[3]} else {0}
      ),
      event:e
      }
    }
    )
    .collect::<Vec<_>>()
  ).unwrap();

  let mut current_guard = 0;

  data.sort();
  
  let guards = 
  data
  .into_iter()
  .fold(HashMap::new(), |mut acc:HashMap<i32,Vec<i32>>,event|  {
    if event.event.contains("Guard")  {
     current_guard = event.event
      .split(|c| c == ' ' || c == '#')
      .map(|id| id.parse::<i32>())
      .filter(|id| id.is_ok())
      .map(|id| id.unwrap())
      .collect::<Vec<_>>()[0];
    }
    if event.event.contains("asleep") {
      acc.entry(current_guard).and_modify(|v| v.push(event.time.2)).or_insert(vec![event.time.2]);
    }
    if event.event.contains("wakes") {
      acc.entry(current_guard).and_modify(|v| v.push(event.time.2)).or_insert(vec![event.time.2]);
    }
    acc
   }
  )
  .into_iter()
  .fold(HashMap::new(),|mut acc,guard| {
    acc.insert(guard.0,
      guard.1.clone().into_iter().step_by(2).zip(guard.1.into_iter().skip(1).step_by(2)).collect::<Vec<_>>()
    );
    acc
  }
  );
  println!("Part 1: {:?} \n Part 2: {:?}",
  guards
  .iter()
  .max_by(|guard_a,guard_b| {
    (guard_a.1.iter().fold(0,sum_times).cmp(&guard_b.1.iter().fold(0,sum_times)))
  })
  .map(|(k,v)| 
    (k,most_common_minute( v.into_iter().fold(Vec::new(),|acc,x| accum_minutes(acc,&x).collect::<Vec<_>>() ) ) )
  )
  .map(|(k,v)|
    k*v.0
  )
  ,
  guards
  .iter()
  .max_by(|guard_a,guard_b| {
    most_common_minute(guard_a.1.clone().into_iter().fold(Vec::new(),|acc,x| accum_minutes(acc,&x).collect::<Vec<_>>() ) ).1
    .cmp(&most_common_minute(guard_b.1.clone().into_iter().fold(Vec::new(),|acc,x| accum_minutes(acc,&x).collect::<Vec<_>>())).1 )
  })
  .map(|(k,v)| 
    (k,most_common_minute( v.into_iter().fold(Vec::new(),|acc,x| accum_minutes(acc,&x).collect::<Vec<_>>() ) ) )
  )
  .map(|(k,v)|
    k*v.0
  )
  );
    println!("Time elapsed: {}ms",start.elapsed().as_millis());
}

#[derive(Debug)]
struct EventData
{
  time:(i32,i32,i32),
  event:String
}

impl std::cmp::PartialEq for EventData
{
  fn eq(&self,rhs: &EventData) -> bool
  {
    self.time.0 == rhs.time.0
    && self.time.1 == rhs.time.1
    && self.time.2 == rhs.time.2
  }
}

impl std::cmp::Eq for EventData
{
  
}

impl std::cmp::PartialOrd for EventData
{
  fn partial_cmp(&self, rhs: &EventData) -> Option<std::cmp::Ordering>
  {
    Some(
      self.time.0.cmp(&rhs.time.0)
      .then(self.time.1.cmp(&rhs.time.1))
      .then(self.time.2.cmp(&rhs.time.2))
    )
  }
}

impl std::cmp::Ord for EventData
{
  fn cmp(&self,rhs: &EventData) -> std::cmp::Ordering
  {
    self.partial_cmp(rhs).unwrap()
  }
}