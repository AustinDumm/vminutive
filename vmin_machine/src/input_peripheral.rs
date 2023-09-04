use std::{sync::mpsc, io::Read, thread, collections::VecDeque};

use crate::{PeripheralHandler, Peripheral};


pub struct InputPeripheral {
    input_channel: mpsc::Receiver<u8>
}

impl InputPeripheral {
    pub fn new() -> Peripheral {
        let (tx, rx) = mpsc::channel::<u8>();
        thread::spawn(move || loop {
            let mut buf = [0u8; 1];
            std::io::stdin().read_exact(&mut buf).expect("Failed to read byte in from stdin");
            tx.send(buf[0]).expect("Failed to send byte across channel");
        });

        Peripheral {
            priority: crate::PeripheralPriority::Interrupt,
            handler: Box::new(Self { input_channel: rx }),
            message_queue: VecDeque::new(),
        }
    }
}

impl PeripheralHandler for InputPeripheral {
    fn send(&mut self, _: Vec<u64>) {}

    fn run(&mut self) -> Option<Vec<u64>> {
        let mut received = vec![];
        while let Ok(next) = self.input_channel.try_recv() {
            received.push(next as u64);
        }

        if received.len() > 0 {
            Some(received)
        } else {
            None
        }
    }

    fn close(&mut self) {}
}
