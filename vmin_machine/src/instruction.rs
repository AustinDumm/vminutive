
use enum_repr::EnumRepr;

#[derive(Debug)]
#[EnumRepr(type = "u8", implicit = true)]
pub enum Instruction {
    /// Push the next 8 bytes as a 64 bit value onto the stack
    Push,
    /// Push the number of elements in the stack onto the stack
    PushStackCount,
    /// Push the number of elements in the addr-stack onto the stack
    PushAddrCount,
    /// Push the next 4 bytes as a 32 bit value onto the addr-stack
    PushAddr,
    /// Pop and discard the top stack value
    Pop,
    /// Pop and discard the top addr-stack value
    PopAddr,

    /// Push a copy of the top value onto the stack
    Duplicate,
    /// Push a copy of the top value onto the addr-stack
    DuplicateAddr,

    /// Swap the positions of the top two values of the stack
    Swap,
    /// Swap the positions of the top two values of the addr-stack
    SwapAddr,

    /// Pop the top value of the addr-stack and push onto the stack
    PopToStack,
    /// Pop the top value of the stack and push onto the addr-stack
    PopToAddr,
    /// Pushes the top value of the addr-stack onto the stack
    DuplicateFromAddr,
    /// Pushes the top value of the stack onto the addr-stack
    DuplicateToAddr,

    /// Math and Logical Instructions
    /// Pop the top two values off of the stack and push the result of the operation
    /// onto the stack.
    ///
    /// Overflow or underflow in operation results in overflow bit set for checking
    /// with corresponding jump instructions.
    /// 
    /// Instructions appended with I are 64-bit integer, F are 64-bit IEEE floating point
    /// 
    /// Ex. If stack top -> a -> b -> c,
    ///     on Subtract instruction,
    ///     result: top -> a - b -> c
    AddI,
    AddF,
    SubtractI,
    SubtractF,
    MultiplyI,
    MultiplyF,
    DivideI,
    DivideF,
    Modulus,
    ShiftRight,
    ShiftLeft,
    RotateRight,
    RotateLeft,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,

    /// Jump instructions
    /// Pops the top value off of the addr-stack and sets it as the next program counter value
    Jump,

    /// Conditional jumps
    /// Pops the top value off of the stack and pops the top value off of the addr-stack,
    /// if the condition is true with the stack value, sets the addr-stack value as the
    /// next program counter value
    /// 
    /// Appended with I indicates integer comparison
    /// Appended with U indicates unsigned comparison
    /// Appended with F indicates IEEE floating comparison
    JumpEqZI,
    JumpEqZF,
    JumpLtZI,
    JumpLtZF,
    JumpGtZI,
    JumpGtZU,
    JumpGtZF,
    JumpLeZI,
    JumpLeZF,
    JumpGeZI,
    JumpGeZF,
    JumpNeZI,
    JumpNeZF,
    JumpInfF,
    JumpNanF,
    
    /// Call
    /// Pops the top value off of the addr-stack and jumps unconditionally while pushing
    /// the current program counter value onto the top of the addr-stack register
    Call,
    
    /// Store#
    /// Pops the top value off of the stack and stores # bytes of it in memory, little-endian
    /// starting at the address on the addr-stack, indexed by the next byte in
    /// instruction code.
    Store1,
    Store2,
    Store3,
    Store4,
    Store5,
    Store6,
    Store7,
    Store8,

    /// Load#
    /// Reads the # bytes of little-endian memory starting at the address on the addr-stack,
    /// indexed by the next 4 pushes the value onto the top of the stack
    Load1,
    Load2,
    Load3,
    Load4,
    Load5,
    Load6,
    Load7,
    Load8,

    /// Port Instructions
    /// 
    /// External devices are communicated with via ports. Only one port number can be registered
    /// to a process at a time. processs can only be registered to one port at a time.
    /// Ports have a 16-bit identifier and take and return 64 bit values.
    ///
    /// PortRegister
    /// Pops the top value off of the stack and treats the lower 2 bytes as port id to register with.
    /// Pushes 0 on the stack if registration fails and pushes non-0 on the stack if registration succeeds.
    PortRegister,

    /// IsPortRegistered
    /// Pops the top value off of the stack and pushs a 0 if the port id popped is not registered. Pushes a 
    /// non-zero value if it is registered
    IsPortRegistered,

    /// PortWaitRegister
    /// Behaves the same as PortRegister but holds this process until registration succeeds.
    PortWaitRegister,

    /// PortSend
    /// Pops the top value off of the stack and sends it to the port registered with this process. In the
    /// case this process does not have a port it is registered with, the machine panics.
    PortSend,


    /// PortRead
    /// Holds this process until a 64-bit value is available on the port with which this process is
    /// registered. Once the value arrives, the value is pushed onto this process's stack. In the
    /// case this process does not have a port it is registered with, the machine panics.
    PortRead,

    /// PortDisconnect
    /// Separates this process from the port it is registered with. In the case this process does not
    /// have a port it is registered with, the process dies.
    PortDisconnect,

    /// PushPort
    /// Pushes the port id onto the stack as the 2 lower bytes of a 64 bit number. In the case this
    /// process does not have a port it is registered with, 0 is pushed onto the stack as 0 is an
    /// invalid value for a port
    PushPort,


    /// Die
    /// Kills this process and sends the 0 message to all processs waiting on this process
    Die,

    /// Spawn
    /// Pops the top value off of the addr-stack and creates and runs a new process starting from the
    /// popped instruction addr.
    /// Pops the next value as the arity of successive values to push onto the new process's stack
    /// Pops the next ARITY values and places them into the new process's stack same as they existed
    /// in the source process.
    /// Pushes the ID value for the new process onto this process's stack.
    Spawn,

    /// Send
    /// Pops the top value off of the stack as id number of the process to send the data to
    /// Pops the next value off of the stack as arity, the number of values to pop and send
    /// Pops the next "arity" values off of the stack and sends them to the other process
    /// as a message.
    Send,

    /// Expect
    /// Pops the top value off of the stack as arity=N
    /// Pops the next N values off of the stack as expected N values starting the message
    /// to wake this process with.
    /// 
    /// Holds this process until a message is sent to this process whose first N values
    /// match the N popped off of the stack when expecting. Upon waking, the message is
    /// copied into this process's stack the same as it existed on the stack of the sending
    /// process, arity of the message is pushed on top, and continues execution.
    Expect,

    /// Observe
    /// Pops the top value off of the stack as the number of process IDs to read
    /// Pops the next N IDs off of the stack and holds this process until a die signal is
    /// sent from one of the IDs being observed. In such a case, the ID of the dead process
    /// is pushed onto this process's stack and execution resumes.
    Observe,
}
