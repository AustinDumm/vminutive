
use std::{collections::{HashMap, VecDeque, HashSet}, hash::Hash};

use crate::instruction::Instruction;

#[derive(PartialEq, Clone, Debug)]
enum HoldReason {
    RegisterPort(u16),
    Port(u16),
    Expect,
    Observe(HashSet<u32>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct HoldingProcess {
    process_id: u32,
    reason: HoldReason,
}

impl Hash for HoldingProcess {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.process_id.hash(state);
    }
}

impl Eq for HoldingProcess {}

pub struct Process {
    program_counter: usize,
    stack: Vec<u64>,
    addr_stack: Vec<u32>,
    heap_map: HashMap<u32, u8>,
    registered_port: Option<u16>,
    incoming_messages: VecDeque<Vec<u64>>,
}

impl Process {
    pub fn new(
        initial_program_counter: usize,
    ) -> Process {
        Process { 
            program_counter: initial_program_counter,
            stack: Vec::new(),
            addr_stack: Vec::new(),
            heap_map: HashMap::new(),
            registered_port: None,
            incoming_messages: VecDeque::new(),
        }
    }

    pub fn perform_integer_math(
        &mut self,
        operation: &dyn Fn(i64, i64) -> i64
    ) -> MachineResult<()> {
        let top = self.stack.pop()
            .ok_or(format!("Failed to pop top value for integer math"))?;
        let next = self.stack.pop()
            .ok_or(format!("Failed to pop next value for integer math"))?;
        
        let result = operation(top as i64, next as i64);
        self.stack.push(result as u64);

        Ok(())
    }

    pub fn perform_floating_math(
        &mut self,
        operation: &dyn Fn(f64, f64) -> f64
    ) -> MachineResult<()> {
        let top = self.stack.pop()
            .ok_or(format!("Failed to pop top value for integer math"))?;
        let next = self.stack.pop()
            .ok_or(format!("Failed to pop next value for integer math"))?;
        
        let top_float = f64::from_bits(top);
        let next_float = f64::from_bits(next);
        
        let result = operation(top_float, next_float);
        self.stack.push(result.to_bits());

        Ok(())
    }

    pub fn perform_jump(
        &mut self,
    ) -> MachineResult<()> {
        let addr = self.addr_stack.pop()
            .ok_or(format!("Failed to pop top addr value for jump"))?;
        
        self.program_counter = addr as usize;

        Ok(())
    }

    pub fn perform_conditional_jump(
        &mut self,
        predicate: &dyn Fn(u64) -> bool
    ) -> MachineResult<usize> {
        let value = self.stack.pop()
            .ok_or(format!("Failed to pop top value for conditional jump"))?;

        let should_jump = predicate(value);

        if should_jump {
            self.perform_jump()?;
            Ok(0)
        } else {
            _ = self.addr_stack.pop()
                .ok_or(format!("Failed to pop top addr value for failed jump"))?;
            Ok(1)
        }
    }

    pub fn perform_call(
        &mut self,
    ) -> MachineResult<()> {
        let addr = self.addr_stack.pop()
            .ok_or(format!("Failed to pop top addr value for jump"))?;
        let from_addr = self.program_counter as u32;
    
        self.program_counter = addr as usize;
        self.addr_stack.push(from_addr + 1);

        Ok(())
    }

    pub fn perform_store(
        &mut self,
        byte_count: u8,
        offset: u8,
    ) -> MachineResult<()> {
        assert!(byte_count <= 8);

        let value = self.stack.pop()
            .ok_or(format!("Failed to pop top value for store"))?;
        let store_start_addr = self.addr_stack.pop()
            .ok_or(format!("Failed to pop addr value for store"))? as u64;
        
        let store_start_addr = store_start_addr + offset as u64;
        let store_limit_addr = store_start_addr + byte_count as u64;
        let store_range = store_start_addr .. store_limit_addr;

        let bytes = value.to_le_bytes();
        for (store_addr, byte) in store_range.zip(bytes) {
            self.heap_map.insert(store_addr as u32, byte);
        }

        Ok(())
    }

    pub fn perform_load(
        &mut self,
        instructions: &Vec<u8>,
        byte_count: u8,
        offset: u8,
    ) -> MachineResult<()> {
        assert!(byte_count <= 8);

        let load_start_addr = self.addr_stack.pop()
            .ok_or(format!("Failed to pop top addr value for load"))? as u64;

        let load_start_addr = load_start_addr + offset as u64;
        let load_limit_addr = load_start_addr + byte_count as u64;
        let load_range = load_start_addr .. load_limit_addr;

        let mut byte_buffer = [0u8; 8];

        let load_bytes = load_range
            .map(|addr| {
                *self.heap_map
                    .get(&(addr as u32))
                    .unwrap_or_else(|| {
                        if (addr as usize) < instructions.len() {
                            &instructions[addr as usize]
                        } else {
                            &0
                        }
                    })
            })
            .collect::<Vec<u8>>();
        
        byte_buffer[..load_bytes.len()]
            .copy_from_slice(&load_bytes[..load_bytes.len()]);

        self.stack.push(u64::from_le_bytes(byte_buffer));

        Ok(())
    }
}

pub enum PeripheralPriority {
    Interrupt,
    Normal,
}

pub struct Peripheral {
    pub priority: PeripheralPriority,
    pub handler: Box<dyn PeripheralHandler>,
    pub message_queue: VecDeque<Vec<u64>>,
}

pub trait PeripheralHandler {
    fn send(&mut self, data: Vec<u64>);
    fn run(&mut self) -> Option<Vec<u64>>;
    fn close(&mut self);
}

pub struct Machine {
    instructions: Vec<u8>,
    next_process_id: u32,
    processes: HashMap<u32, Process>,
    registered_ports: HashSet<u16>,
    active_process_id: Option<u32>,
    pending_processes: VecDeque<u32>,
    holding_processes: HashMap<u32, HoldingProcess>,
    peripherals: Vec<Peripheral>,
    instructions_since_switch: u32,
    execution_count: HashMap<usize, u64>,
}

type MachineResult<Success> = Result<Success, String>;

impl Machine {
    fn main_process() -> Process {
        Process::new(
            0,
        )
    }

    pub fn new(
        instructions: Vec<u8>,
        peripherals: Vec<Peripheral>,
    ) -> Machine {
        let main_process = Machine::main_process();

        Machine {
            instructions,
            next_process_id: 1,
            processes: HashMap::from([(0, main_process)]),
            active_process_id: Some(0),
            registered_ports: HashSet::new(),
            pending_processes: VecDeque::new(),
            holding_processes: HashMap::new(),
            peripherals,
            instructions_since_switch: 0,
            execution_count: HashMap::new(),
        }
    }

    pub fn step(&mut self) -> MachineResult<Option<u64>> {
        let active_process_id =
            if let Some(id) = self.active_process_id {
                self.instructions_since_switch += 1;
                id
            } else {
                self.instructions_since_switch = 0;
                if let Some(next_process) = self.pending_processes.pop_front() {
                    self.active_process_id = Some(next_process);
                    next_process
                } else {
                    return Ok(None);
                }
            };

        let mut active_process = self
            .processes
            .get_mut(&active_process_id)
            .ok_or(format!("Failed to find active process of id: {}", active_process_id))?;
        
        let active_program_counter = active_process.program_counter;
        ((active_program_counter as usize) < self.instructions.len())
            .then_some(()).ok_or(format!("Out of bounds program counter value: {}", active_program_counter))?;
        
        let instruction_code = *active_process
            .heap_map
            .get(&(active_program_counter as u32))
            .unwrap_or_else(|| &self.instructions[active_program_counter as usize]);
        
        *self.execution_count
            .entry(active_program_counter)
            .or_insert(0) += 1;
        
        let instruction = Instruction::from_repr(instruction_code)
            .ok_or(format!(
                "Failed to parse instruction code ({}) at address: {}",
                instruction_code,
                active_program_counter
            ))?;

        // println!(
        //     "!!! PId: {},\n\tinstruction: {:?}@{},\n\tstack: {:?}\n\taddr-stack: {:?}",
        //     active_process_id,
        //     instruction,
        //     active_program_counter,
        //     active_process.stack.iter().map(|i| *i as i64).collect::<Vec<i64>>(),
        //     active_process.addr_stack.iter().map(|i| *i as i64).collect::<Vec<i64>>(),
        // );
        // let start = 729;
        // let end = start + 128;
        // let mem = (start..end).step_by(8)
        //     .map(|addr| {
        //         let mut bytes = vec![];
        //         for byte_addr in addr..(addr+8) {
        //             let byte = active_process
        //                 .heap_map
        //                 .get(&byte_addr)
        //                 .unwrap_or_else(||
        //                     if (byte_addr as usize) < self.instructions.len() {
        //                         &self.instructions[byte_addr as usize]
        //                     } else {
        //                         &0
        //                     }
        //                 );
        //             bytes.push(*byte);
        //         }

        //         i64::from_le_bytes(bytes.try_into().unwrap())
        //     });
        // for (value, addr) in mem.zip((start..end).step_by(8)) { println!("!!! \t{}: [{}]", addr, value) }
        // println!("!!! ====================");
        // let start = 0xFFFFFF80;
        // let end = 0xFFFFFFFF;
        // let mem = (start..end).step_by(8)
        //     .map(|addr| {
        //         let mut bytes = vec![];
        //         for byte_addr in addr..=(addr+7) {
        //             let byte = active_process
        //                 .heap_map
        //                 .get(&byte_addr)
        //                 .unwrap_or_else(||
        //                     if (byte_addr as usize) < self.instructions.len() {
        //                         &self.instructions[byte_addr as usize]
        //                     } else {
        //                         &0
        //                     }
        //                 );
        //             bytes.push(*byte);
        //         }

        //         u64::from_le_bytes(bytes.try_into().unwrap())
        //     });
        // for (value, addr) in mem.zip((start..end).step_by(8)).rev() { println!("!!! \t{:0x}: [{}]", addr, value) }

        let pc_offset: usize =
            match instruction {
                Instruction::Push => {
                    let push_start = active_program_counter + 1;
                    let push_end = push_start + 8;
                    let push_val = u64::from_le_bytes(
                        self.instructions[push_start .. push_end]
                            .try_into()
                            .map_err(|e| format!("Failed to convert bytes to Push value: {}", e))?
                    );

                    active_process.stack.push(push_val);

                    1 + 8
                },
                Instruction::PushStackCount => {
                    let stack_count = active_process.stack.len();
                    active_process.stack.push(stack_count as u64);

                    1
                },
                Instruction::PushAddrCount => {
                    let addr_count = active_process.addr_stack.len();
                    active_process.stack.push(addr_count as u64);

                    1
                },
                Instruction::PushAddr => {
                    let push_start = active_program_counter + 1;
                    let push_end = push_start + 4;
                    let push_val = u32::from_le_bytes(
                        self.instructions[push_start .. push_end]
                            .try_into()
                            .map_err(|e| format!("Failed to convert bytes to PushAddr value: {}", e))?
                    );

                    active_process.addr_stack.push(push_val);

                    1 + 4
                },
                Instruction::Pop => {
                    active_process.stack.pop();

                    1
                },
                Instruction::PopAddr => {
                    active_process.addr_stack.pop();

                    1
                },
                Instruction::Duplicate => {
                    let top_value = active_process
                        .stack
                        .last()
                        .ok_or(format!("Failed to find top value on stack to duplicate"))?;

                    active_process.stack.push(*top_value);

                    1
                },
                Instruction::DuplicateAddr => {
                    let top_value = active_process
                        .addr_stack
                        .last()
                        .ok_or(format!("Failed to find top value on addr-stack to duplicate"))?;

                    active_process.addr_stack.push(*top_value);

                    1
                },
                Instruction::Swap => {
                    (active_process.stack.len() >= 2)
                        .then_some(())
                        .ok_or(format!("Attempted to swap stack with less than 2 values"))?;

                    let top_index = active_process.stack.len() - 1;
                    let second_from_top_index = active_process.stack.len() - 2;
                    active_process.stack.swap(top_index, second_from_top_index);

                    1
                },
                Instruction::SwapAddr => {
                    (active_process.addr_stack.len() >= 2)
                        .then_some(())
                        .ok_or(format!("Attempted to swap addr-stack with less than 2 values"))?;

                    let top_index = active_process.addr_stack.len() - 1;
                    let second_from_top_index = active_process.addr_stack.len() - 2;
                    active_process.addr_stack.swap(top_index, second_from_top_index);

                    1
                },
                Instruction::PopToStack => {
                    let top_addr = active_process
                        .addr_stack
                        .pop()
                        .ok_or(format!("Attempted to push from addr-stack with less than 1 addr_stack value"))?;

                    active_process
                        .stack
                        .push(top_addr.into());

                    1
                },
                Instruction::PopToAddr => {
                    let top_value = active_process
                        .stack
                        .pop()
                        .ok_or(format!("Attempted to push from stack with less than 1 addr_stack value"))?;

                    active_process
                        .addr_stack
                        .push(top_value as u32);

                    1
                },
                Instruction::DuplicateFromAddr => {
                    let top_addr = active_process
                        .addr_stack
                        .last()
                        .ok_or(format!("Attempted to push from addr-stack with less than 1 addr_stack value"))?;

                    active_process
                        .stack
                        .push((*top_addr).into());

                    1
                },
                Instruction::DuplicateToAddr => {
                    let top_value = active_process
                        .stack
                        .last()
                        .ok_or(format!("Attempted to push from stack with less than 1 addr_stack value"))?;

                    active_process
                        .addr_stack
                        .push((*top_value) as u32);

                    1
                },
                Instruction::AddI => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs + lhs)?;

                    1
                },
                Instruction::AddF => {
                    active_process.perform_floating_math(&|lhs, rhs| rhs + lhs)?;

                    1
                },
                Instruction::SubtractI => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs - lhs)?;

                    1
                },
                Instruction::SubtractF => {
                    active_process.perform_floating_math(&|lhs, rhs| rhs - lhs)?;

                    1
                },
                Instruction::MultiplyI => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs * lhs)?;

                    1
                },
                Instruction::MultiplyF => {
                    active_process.perform_floating_math(&|lhs, rhs| rhs * lhs)?;

                    1
                },
                Instruction::DivideI => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs / lhs)?;

                    1
                },
                Instruction::DivideF => {
                    active_process.perform_floating_math(&|lhs, rhs| rhs / lhs)?;

                    1
                },
                Instruction::Modulus => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs % lhs)?;

                    1
                },
                Instruction::ShiftRight => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs >> lhs)?;

                    1
                },
                Instruction::ShiftLeft => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs << lhs)?;

                    1
                },
                Instruction::RotateRight => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs.rotate_right(lhs as u32))?;

                    1
                },
                Instruction::RotateLeft => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs.rotate_left(lhs as u32))?;

                    1
                },
                Instruction::BitwiseAnd => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs & lhs)?;

                    1
                },
                Instruction::BitwiseOr => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs | lhs)?;

                    1
                },
                Instruction::BitwiseXor => {
                    active_process.perform_integer_math(&|lhs, rhs| rhs ^ lhs)?;

                    1
                },
                Instruction::BitwiseNot => {
                    let top = active_process.stack.pop()
                        .ok_or(format!("Failed to pop top value for bitwise not"))?;

                    let result = !top;

                    active_process.stack.push(result);

                    1
                },
                Instruction::Jump => {
                    active_process.perform_jump()?;

                    0
                },
                Instruction::JumpEqZI => {
                    active_process.perform_conditional_jump(
                        &|value| value == 0
                    )?
                },
                Instruction::JumpEqZF => {
                    active_process.perform_conditional_jump(
                        &|value| f64::from_bits(value) == 0.0
                    )?
                },
                Instruction::JumpLtZI => {
                    active_process.perform_conditional_jump(
                        &|value| (value as i64) < 0
                    )?
                },
                Instruction::JumpLtZF => {
                    active_process.perform_conditional_jump(
                        &|value| f64::from_bits(value) < 0.0
                    )?
                },
                Instruction::JumpGtZI => {
                    active_process.perform_conditional_jump(
                        &|value| (value as i64) > 0
                    )?
                },
                Instruction::JumpGtZU => {
                    active_process.perform_conditional_jump(
                        &|value| value > 0
                    )?
                },
                Instruction::JumpGtZF => {
                    active_process.perform_conditional_jump(
                        &|value| f64::from_bits(value) > 0.0
                    )?
                },
                Instruction::JumpLeZI => {
                    active_process.perform_conditional_jump(
                        &|value| (value as i64) <= 0
                    )?
                },
                Instruction::JumpLeZF => {
                    active_process.perform_conditional_jump(
                        &|value| f64::from_bits(value) <= 0.0
                    )?
                },
                Instruction::JumpGeZI => {
                    active_process.perform_conditional_jump(
                        &|value| (value as i64) >= 0
                    )?
                },
                Instruction::JumpGeZF => {
                    active_process.perform_conditional_jump(
                        &|value| f64::from_bits(value) >= 0.0
                    )?
                },
                Instruction::JumpNeZI => {
                    active_process.perform_conditional_jump(
                        &|value| value != 0
                    )?
                },
                Instruction::JumpNeZF => {
                    active_process.perform_conditional_jump(
                        &|value| f64::from_bits(value) != 0.0
                    )?
                },
                Instruction::JumpInfF => {
                    active_process.perform_conditional_jump(
                        &|value| f64::from_bits(value).is_infinite()
                    )?
                },
                Instruction::JumpNanF => {
                    active_process.perform_conditional_jump(
                        &|value| f64::from_bits(value).is_nan()
                    )?
                },
                Instruction::Call => {
                    active_process.perform_call()?;

                    0
                },
                Instruction::Store1 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_store(
                        1,
                        offset,
                    )?;

                    2
                },
                Instruction::Store2 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_store(
                        2,
                        offset,
                    )?;

                    2
                },
                Instruction::Store3 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_store(
                        3,
                        offset,
                    )?;

                    2
                },
                Instruction::Store4 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_store(
                        4,
                        offset,
                    )?;

                    2
                },
                Instruction::Store5 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_store(
                        5,
                        offset,
                    )?;

                    2
                },
                Instruction::Store6 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_store(
                        6,
                        offset,
                    )?;

                    2
                },
                Instruction::Store7 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_store(
                        7,
                        offset,
                    )?;

                    2
                },
                Instruction::Store8 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_store(
                        8,
                        offset,
                    )?;

                    2
                },
                Instruction::Load1 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_load(
                        &self.instructions,
                        1,
                        offset,
                    )?;

                    2
                },
                Instruction::Load2 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_load(
                        &self.instructions,
                        2,
                        offset,
                    )?;

                    2
                },
                Instruction::Load3 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_load(
                        &self.instructions,
                        3,
                        offset,
                    )?;

                    2
                },
                Instruction::Load4 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_load(
                        &self.instructions,
                        4,
                        offset,
                    )?;

                    2
                },
                Instruction::Load5 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_load(
                        &self.instructions,
                        5,
                        offset,
                    )?;

                    2
                },
                Instruction::Load6 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_load(
                        &self.instructions,
                        6,
                        offset,
                    )?;

                    2
                },
                Instruction::Load7 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_load(
                        &self.instructions,
                        7,
                        offset,
                    )?;

                    2
                },
                Instruction::Load8 => {
                    let offset = self.instructions[active_program_counter + 1 as usize];
                    active_process.perform_load(
                        &self.instructions,
                        8,
                        offset,
                    )?;

                    2
                },
                Instruction::PortRegister => {
                    let value = active_process.stack.pop()
                        .ok_or(format!("Failed to pop port id off of stack for register"))?;

                    (value != 0)
                        .then_some(())
                        .ok_or(format!("Attempted to register invalid port of id 0"))?;

                    let port_id = value as u16;

                    if self.peripherals.len() < port_id as usize {
                        Err(format!("Attempted to connect to port not associated to peripheral: {}", port_id))?;
                    }

                    if self.registered_ports.contains(&port_id) {
                        active_process.stack.push(0);
                    } else {
                        active_process.stack.push(1);
                    }

                    self.registered_ports.insert(port_id);
                    active_process.registered_port = Some(port_id);

                    1
                },
                Instruction::IsPortRegistered => {
                    let value = active_process.stack.pop()
                        .ok_or(format!("Failed to pop port id off of stack for register"))?;

                    let port_id = value as u16;

                    let push_value = if self.registered_ports.contains(&port_id) {
                        1
                    } else {
                        0
                    };

                    active_process.stack.push(push_value);

                    1 
                },
                Instruction::PortWaitRegister => {
                    let value = active_process.stack.pop()
                        .ok_or(format!("Failed to pop port id off of stack for register"))?; 
                    let port_id = value as u16;

                    if self.registered_ports.contains(&port_id) {
                        self.holding_processes
                            .insert(
                                active_process_id,
                                HoldingProcess {
                                    process_id: active_process_id,
                                    reason: HoldReason::RegisterPort(port_id)
                                }
                            );
                        active_process.program_counter += 1;
                        self.active_process_id = None;

                        0
                    } else {
                        self.registered_ports.insert(port_id);
                        active_process.registered_port = Some(port_id);

                        1
                    }
                },
                Instruction::PortSend => {
                    let port_id = active_process.registered_port
                        .ok_or(format!("Attempted to send with no registered port"))?;

                    let arity = active_process.stack.pop()
                        .ok_or(format!("Failed to pop arity to send to port"))?;

                    let mut values = vec![];
                    for _ in 0..arity {
                        values.push(
                            active_process.stack.pop()
                                .ok_or(format!("Failed to pop argument to send to port"))?
                        );
                    }

                    ((port_id as usize) <= self.peripherals.len())
                        .then_some(())
                        .ok_or(format!("Invalid port id for sending to peripheral"))?;

                    let peripheral = &mut self.peripherals[port_id as usize - 1];

                    peripheral.handler.send(values);

                    1
                },
                Instruction::PortRead => {
                    let port_id = active_process.registered_port
                        .ok_or(format!("Attempted to read with no registered port"))?;

                    ((port_id as usize) <= self.peripherals.len())
                        .then_some(())
                        .ok_or(format!("Invalid port id for sending to peripheral"))?;

                    active_process.program_counter += 1;

                    self.holding_processes.insert(
                        active_process_id, 
                        HoldingProcess { 
                            process_id: active_process_id, 
                            reason: HoldReason::Port(port_id)
                        }
                    );

                    self.active_process_id = None;

                    0
                },
                Instruction::PortDisconnect => {
                    let port_id = active_process.registered_port
                        .ok_or(format!("Attempted to disconnected with no registered port"))?;

                    active_process.registered_port = None;

                    let mut process_id_to_update = None;
                    for holding_process in self.holding_processes.values() {
                        match holding_process.reason {
                            HoldReason::RegisterPort(holding_port_id) if port_id == holding_port_id =>
                                process_id_to_update = Some(holding_process.process_id),
                            _ => (),
                        }
                    }

                    if let Some(process_id_to_update) = process_id_to_update {
                        if let Some(process) = self.processes.get_mut(&process_id_to_update) {
                            process.registered_port = Some(port_id);
                            self.holding_processes.remove(&process_id_to_update);
                            self.pending_processes.push_back(process_id_to_update);
                        }
                    }

                    1
                },
                Instruction::PushPort => {
                    if let Some(port_id) = active_process.registered_port {
                        active_process.stack.push(port_id as u64);
                    } else {
                        active_process.stack.push(0);
                    }

                    1
                },
                Instruction::Die => {
                    let maybe_result = active_process.stack.pop();

                    self.processes.remove(&active_process_id);

                    if self.processes.len() == 0 {
                        let result = maybe_result
                            .ok_or(format!("Final process died without result in stack"))?;

                        for peripheral in &mut self.peripherals {
                            peripheral.handler.close();
                        }

                        return Ok(Some(result));
                    }

                    let mut process_ids_to_update = HashSet::<u32>::new();
                    for holding in self.holding_processes.values() {
                        match &holding.reason {
                            HoldReason::Observe(hold_ids)
                                if hold_ids.contains(&active_process_id) => {
                                    process_ids_to_update.insert(holding.process_id);
                                },
                            _ => (),
                        }
                    }

                    let new_holding = self
                        .holding_processes
                        .clone()
                        .into_iter()
                        .filter(|(_, holding)| !process_ids_to_update.contains(&holding.process_id))
                        .collect();
                    self.holding_processes = new_holding;

                    for process_id_to_update in process_ids_to_update {
                        let process = self.processes.get_mut(&process_id_to_update)
                            .ok_or(format!("Failed to find process to update: {}", process_id_to_update))?;
                        
                        process.stack.push(active_process_id as u64);

                        self.pending_processes.push_back(process_id_to_update);
                    }

                    self.active_process_id = None;

                    0
                },
                Instruction::Spawn => {
                    let process_pc = active_process.addr_stack.pop()
                        .ok_or(format!("Failed to pop addr stack for process spawn"))?;
                    let arity = active_process.stack.pop()
                        .ok_or(format!("Failed to pop arity for the spawn process"))?;

                    let mut arguments = vec![];
                    for _ in 0..arity {
                        arguments.push(
                            active_process.stack.pop()
                                .ok_or(format!("Failed to pop argument to spawn process"))?
                        );
                    }
                    
                    let mut new_process = Process::new(
                        process_pc as usize,
                    );
                    
                    while let Some(argument) = arguments.pop() {
                        new_process.stack.push(argument);
                    }
                    
                    let next_id = self.next_process_id;
                    self.next_process_id += 1;

                    active_process.stack.push(next_id as u64);

                    self.processes.insert(next_id, new_process)
                        .is_none()
                        .then_some(())
                        .ok_or(format!("Attempted to start process with existing id: {}", next_id))?;
                    self.pending_processes.push_back(next_id);

                    1
                },
                Instruction::Send => {
                    let send_id = active_process.stack.pop()
                        .ok_or(format!("Failed to pop send process-id from stack"))?;
                    let arity = active_process.stack.pop()
                        .ok_or(format!("Failed to pop arity from stack for send"))?;
                    
                    let mut args = vec![arity];
                    for _ in 0..arity {
                        args.push(
                            active_process.stack.pop()
                                .ok_or(format!("Failed to pop argument for sending"))?
                        );
                    }

                    let receiver_process = self.processes
                        .get_mut(&(send_id as u32))
                        .ok_or(format!("Failed to find process to send. Id: {}", send_id))?;

                    receiver_process.incoming_messages
                        .push_back(args);

                    if let Some(sent_process) = self.holding_processes.get(&(send_id as u32)) {
                        match &sent_process.reason {
                            HoldReason::Expect => {
                                self.holding_processes.remove(&(send_id as u32));
                                self.pending_processes.push_back(send_id as u32);
                            },
                            _ => ()
                        }
                    }
                    
                    1
                },
                Instruction::Expect => {
                    if let Some(next_message) = active_process.incoming_messages.pop_front() {
                        for value in next_message.into_iter().rev() {
                            active_process.stack.push(value);
                        }

                        1
                    } else {
                        self.holding_processes.insert(
                            active_process_id, 
                            HoldingProcess { 
                                process_id: active_process_id, 
                                reason: HoldReason::Expect 
                            },
                        );
                        self.active_process_id = None;

                        0
                    }
                },
                Instruction::Observe => {
                    let arity = active_process.stack.pop()
                        .ok_or(format!("Failed to pop arity for observe instruction"))?;
                    
                    let mut observed_processes = HashSet::new();
                    for _ in 0..arity {
                        observed_processes.insert(
                            active_process.stack.pop()
                                .ok_or(format!("Failed to pop process id argument for observation"))? as u32
                        );
                    }

                    self.holding_processes
                        .insert(
                            active_process_id, 
                            HoldingProcess {
                                process_id: active_process_id,
                                reason: HoldReason::Observe(observed_processes),
                            }
                        );

                    active_process.program_counter += 1;
                    self.active_process_id = None;

                    0
                },
            };
        
        if let Some(active_process_id) = self.active_process_id {
            if let Some(active_process) = self.processes.get_mut(&active_process_id) {
                active_process.program_counter += pc_offset;
            }

            if self.instructions_since_switch > 10000 {
                self.pending_processes.push_back(active_process_id);
                self.active_process_id = None;
            }
        }

        for peripheral in &mut self.peripherals {
            if let Some(result) = peripheral.handler.run() {
                peripheral.message_queue.push_back(result);
            }
        }

        let mut interrupt_handler = None;
        let mut process_ids_to_pend = vec![];
        for holding in self.holding_processes.values_mut() {
            match holding.reason {
                HoldReason::Port(port_id) => {
                    (port_id as usize <= self.peripherals.len())
                        .then_some(())
                        .ok_or(format!("Attempted to send port data for id out of bounds"))?;

                    let peripheral = &mut self.peripherals[port_id as usize - 1];
                    let next_values = peripheral.message_queue.pop_front();

                    if let Some(mut next_values) = next_values {
                        let arity = next_values.len();
                        let process = self.processes.get_mut(&holding.process_id)
                            .ok_or(format!("Attempted to update port value for non-existant process"))?;

                        while let Some(next) = next_values.pop() {
                            process.stack.push(next);
                        }

                        process.stack.push(arity as u64);

                        match peripheral.priority {
                            PeripheralPriority::Interrupt => {
                                interrupt_handler = Some(holding.process_id);
                            },
                            _ => {
                                process_ids_to_pend.push(holding.process_id);
                            }
                        }
                    }
                },
                _ => (),
            }
        }

        for process_id_to_pend in process_ids_to_pend {
            self.holding_processes.remove(&process_id_to_pend);
            self.pending_processes.push_back(process_id_to_pend);
        }

        if let Some(interrupt_handler_id) = interrupt_handler {
            self.holding_processes.remove(&interrupt_handler_id);
            self.instructions_since_switch = 0;
            self.pending_processes.push_back(active_process_id);
            self.active_process_id = Some(interrupt_handler_id);
        }
        
        Ok(None)
    }

    pub fn run(mut self) -> MachineResult<u64> {
        // use std::io::Write;
        loop {
            if let Some(final_result) = self.step()? {
                let mut execution_pairs = self.execution_count
                    .into_iter()
                    .collect::<Vec<(usize, u64)>>();
                execution_pairs.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));

                // writeln!(
                //     std::io::stderr(), "Line Execution Counts:\n{:?}", execution_pairs
                // ).unwrap();
                return Ok(final_result)
            }
        };
    }
}
