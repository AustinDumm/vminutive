# vMinutive

A small virtual machine and instruction set meant for personal experimentation and education related to low level computing, the actor model, and multitasking/concurrency with non-blocking message passing.

## Overview

The vMinutive instruction set is based around a stack machine with one value stack containing 64-bit values on which operations can be performed and an address stack containing 32-bit values on which jumps, stores, and loads can be performed.
vMinutive also contains support for interacting with code outside of the machine via ports each of which connect via a Rust trait for implementation in the Rust host language.

Ports can be sent data of variable arity all of 64-bit integers and can be read from causing the reading process to halt until data arrives from the port.
Ports can only be associated with one process at a time and attempting to register to a port already connected to a process will result in a connection failure.

Processes can only be associated with one port at a time and attempting to register for a different port while already connected will result in a connection failure.
vMinutive supports experimenting with something resembling the actor model via spawning new processes which run functions as if in a completely new and isolated vMinutive machine.
New processes start running with values in their stack being those passed to the process on spawn and a completely clean section of memory containing none of the data which was contained by the spawning process.
Processes communicate by sending messages of data of variable arity and expecting messages, halting until one arrives.

## Assembling

The current assembler is written in Racket and supports labels, constants, literals, embedded compile-time Racket scripts, and including assembly as textual replacements from other files. To assemble, either run the vmin-assembly.rkt file in a Racket environment or compile to an executable using a tool such as DrRacket. The assembler reads assembly via stdin.
