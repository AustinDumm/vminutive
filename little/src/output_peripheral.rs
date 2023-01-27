use std::collections::VecDeque;

use crate::{PeripheralHandler, Peripheral};


pub struct OutputPeripheral {}

impl OutputPeripheral {
    pub fn new() -> Peripheral {
        Peripheral { 
            priority: crate::PeripheralPriority::Normal, 
            handler: Box::new(Self {}), 
            message_queue: VecDeque::new() 
        }
    }
}
impl PeripheralHandler for OutputPeripheral {
    fn send(&mut self, data: Vec<u64>) {
        for value in data {
            if let Some(c) = char::from_u32(value as u32) {
                print!("{}", c);
            }
        }
    }

    fn run(&mut self) -> Option<Vec<u64>> { None }

    fn close(&mut self) {
        println!();
    }
}
