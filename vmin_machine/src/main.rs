use std::{fs::File, io::Read};

use vmin_machine::{Machine, InputPeripheral, OutputPeripheral};


fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let filename = &args[0];
    let mut file_data = File::open(filename).expect("Failed to open file");
    let mut raw_instructions = String::new();
    file_data.read_to_string(&mut raw_instructions).expect("Failed to read file");

    let instructions = raw_instructions[1..raw_instructions.len()-1]
        .split_whitespace()
        .map(|instruction_text| instruction_text.parse::<u8>()
                .expect("Failed to convert text to instructions"))
        .collect::<Vec<u8>>();

    let machine = Machine::new(
        instructions,
        vec![
            InputPeripheral::new(),
            OutputPeripheral::new(),
        ],
    );

    let result = machine.run()?;
    println!("FINISHED: {:x}", result);

    Ok(())
}
