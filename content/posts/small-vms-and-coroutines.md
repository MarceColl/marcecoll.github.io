+++
title = "Small VMs & Coroutines"
date = 2023-01-17
author = "Marce Coll"
+++

# TL;MDWR

In this post I'll talk about the implementation of the coroutine system in x64 asm for a game I'm writing.

(Disclaimer: I'm very new at x64 assembly programming, so I may be doing some very stupid things)


# Background

I've had this idea for a while of a programmable game. I'm not gonna go into details for now but the main idea
is a Real-time strategy game (RTS) where you can program every unit. The code of the units runs in a very simple 
fantasy stack virtual machine, and every tick each unit gets to execute a certain amount of instructions  
based on their virtual CPU speed.

As this is an RTS, lots of units are running at the same time and so we need a way to efficiently run all of these
units at a speed reasonable for an interactive videogame (60fps). Some years ago I did some tests around this idea and the VMs 
I came up with were underperforming messes of either rust or C. There was no way I could deliver the performance I 
had in mind with that.

Recently while reading the first volume of [The Art of Computer Programming](https://www-cs-faculty.stanford.edu/~knuth/taocp.html) 
I came across a section when Knuth implements coroutines in his MIX machine. And, while I had heard and used 
coroutines informaly all the time in my career I had never seen it like this. For those who don't know, a coroutine
is a kind of subroutine that can be suspended and resumed, they are like concurrent-less threads. Each coroutine
has it's own stack and instruction pointer. You may have encountered asymmetrical coroutines in the form of python 
generators for example. If you need more context please check the [coroutine page at wikipedia](https://en.wikipedia.org/wiki/Coroutine)

So I had two ideas at about the same time, convert the code of the Fantasy VM to x64 asm code and run to the code for the
unit VMs as tiny individual coroutines. This both avoids expensive thread context switches, and completely removes the need for
coordinating. I still have to run some benchmarks but I believe splitting the VMs by power (lots of small units in a thread,
some powerful units in another) may bring the best performance results. I may create a separate blogpost once I benchmark this
properly.

So let's get started!

# Fantasy ASM to x64 ASM

The first one seems very obvious in hindsight, why spend time implementing the VM when the CPU it is running on already
implements most of the things I want? With some simple macros I can even mostly feed the Fantasy ISA program into an assembler and it
will come out as expected.

Let's work with a very reduced version of the VM instruction set, just three simple instructions:

|Instr | Description                                               |
|------|-----------------------------------------------------------|
| PUSH | Push immediate to stack                                   |
| ADD  | Add the top two items of the stack. Place result on stack |
| JMP  | Jump to label                                             |

Let's create a very dumb program with this:

```ASM
_co_start:
	PUSH 0x01
loop:
	PUSH 0x01
	ADD
	JMP loop
```

It basically adds `0x01` constantly to a value on the stack, forever. It's an infinite loop.

Now we can use an assembler with macro support to implement these operations in x64. I'll be using [fasm](https://flatassembler.net/) for this.

```asm
macro JMP target {
	inc rax
	jmp target
}

macro ADD {
	inc rax
	pop WORD bx
	add WORD [rsp], bx
}

macro PUSH number {
	inc rax
	push WORD number
}
```

Several things to note, I'm using `inc rax` in every instruction because I want a way to know how many virtual instructions have executed in order
to impose limits on number of instructions per tick. `rax` holds this counter. `JMP` and `PUSH` are exactly the same instruction as the x64 counterpart, they map 1 to 1. 
`ADD` is very simple as well: use `bx` as a temporary storage for the top value of the stack, add the top value of the stack with `bx` and store in 
back to the top of the stack.

With these macros in place, passing the Fantasy ISA input to fasm already gives us a valid x64 program. The result of the macro expansion would look
like this:

```asm
_co_start:
	inc rax
	push WORD 0x01
loop:
	inc rax
	push WORD 0x01
	inc rax
	pop WORD bx
	add WORD [rsp], bx
	inc rax
	jmp target
```

In very few lines we have an assembler for our fantasy VM to native x64 code! Now, currently this will loop forever and we don't want that, this is where
coroutines come and save the day.

# Coroutines

How do we make sure that the unit vm only executes N instructions before it suspends itself? A pretty lightweight solution that requires very little setup
is to insert a check that we haven't exceeded the number of operations every `K` ops and after every label (in the unit VM you can only jump to labels, 
so we catch all loops, branches and sketchy stuff with this).

> I know that this doesn't ensure that the VMs execute exactly N operations, but I think it is a pretty good tradeoff between code size, execution
> overhead and constraining the number of ops. I'll show one possible mitigation further down.

So how do we implement these lightweight unit coroutines?

First we need to save several things per coroutine:
* current stack pointer (`rsp`)
* current instruction pointer (`rip`)
* current `rax` where we save the amount of instructions executed
* current `rbx` that is used for some temporal values, this one is not strictly needed and there is a chance I remove it in the future

Usually these things are saved in the stack with normal coroutines but we don't want to taint the stack of the unit vms with unrelated data.
First because that means their stack gets reduced for reasons unrelated to their code, second because they may be able to manipulate the stack
to the point where they override the values, and then all hell breaks loose. 

Let's start with a simple example where we have 10 units, so let's allocate 40 (4 fields * 10 coroutines) 64-bit values and 800 bytes of 
total stack (80 per coroutine).

```asm
; co-routine data, 40 quad-word sized (8 bytes), zeroed buffer
_co_data:	dq 40 dup 0

data_stack:
	; Allocate 400 word sized (2 bytes), zeroed buffer
	dw 400 dup 0
end_data_stack:
```

and initialize the data in them:

```asm
_start:         ; Main VM start
	xor rax, rax                ; Initialize rax to 0
	; Set rdx to the end of the stack, we will use this to 
	; assign stacks to each coroutine, we start from the end
	mov rdx, end_data_stack     
	; rcx will be the moving pointer through the _co_data 
	; structure as we save the data
	mov rcx, _co_data           
	; Loop over all 10 coroutines to...
init_co_loop:
	; Initialize the rsp
	mov QWORD [rcx], rdx
	; Initialize the rip
	mov QWORD [rcx + 24], _co_start
	; Move to the next structure in _co_data
	add rcx, 32
	; Move back to the next coroutine start of the stack
	; Each coroutine in this example gets 80 bytes
	sub rdx, 80
	; Loop stuff
	inc rax
	cmp rax, 10
	jl init_co_loop
```

Now let's loop over the coroutines, when we `yield` to the first one we will wait until it `yield`s control back to the main loop, then it will
continue on to the second, and so on. While you read the next code fragment, always keep in mind that all across the execution *`rcx` always points to the
currently active coroutine data.* 


```asm
co_initialized:
	; i = 0
	xor rax, rax
	; we will start with the first co-routine
	mov QWORD rcx, _co_data
_loop:
	; yield to the coroutine
	YIELD
	; i += 1
	inc rax
	; move onto the next coroutine
	add rcx, 32
	; Loop things
	cmp rax, 10
	jl _loop
	jmp _exit
```

Now all the magic seems to happen in this `YIELD` macro, let's take a look:

```asm
macro YIELD reset_rax {
	; local turns these labels into local labels, so multiple uses of
	; YIELD don't create repeated labels.
	local next_inst, after_yield
	; swap rsp, by the stored value in rsp (this means we just saved
	; the value of the main program rsp to the co_data structure pointed
	; by rcx)
	xchg rsp, QWORD [rcx]
	; swap rax, same as above
	xchg rax, QWORD [rcx + 8]
	; swap rbx, same as above
	xchg rbx, QWORD [rcx + 16]
	; now we push onto the stack the instruction pointer for the coroutine
	; which will be used by the "ret" instruction further down
	push QWORD [rcx + 24]	
	; we call next_inst to get the instruction pointer of this coroutine.
	; We don't wanna return to the next "pop" so we will need to increment
	; this value so that a yield to this coroutine returns right after the
	; "ret". The call will put this rip into the stack
	call next_inst
next_inst:
	; move the just save rip into the rip of the _co_data[coroutine_id]
	pop QWORD [rcx + 24]
	; increment it so a yield from the other coroutine continues from right
	; after the ret
	add QWORD [rcx + 24], (after_yield - next_inst)
	; jump, using the value pushed 4 instructions above, into the coroutine
	ret
after_yield:
}
```

There are probably some better ways to implement this, but this is what I came up with. Each call into a coroutine saves the main coroutine state
into the `_co_data` of the coroutine you `YIELD` into. Once `YIELD` is called from the other coroutine the same thing will happen, the values for 
the main one will be restored and the execution will continue right after the `ret`. This works very well for our case of a main coroutine coordinating
other coroutines.

There is one last piece left, injecting some instructions to check if we've exhausted our instruction limit this tick. To do so I created this macro:

```asm
macro CHECK_LIMITS {
	local no_yield
	; in this case the 15k instruction limit is hardcoded, it could
	; be passed as an argument to the macro as when you generate the
	; code, you can set a limit per unit depending on their virtual cpu
	cmp rax, 15000		
	jl no_yield
	; If we are above, YIELD
	YIELD 
	; Initialize rax to 0 to reset the counter, here is where the mitigation
	; that I talked about before could happen, here you could subtract by 15k
	; and you'd compensate going over the instruction limit last iteration
	; by having less instructions this loop. This is still not perfect but I
	; think it's mostly fine. If people found a way to exploit this, I'd
	; be happy to let them.
	xor rax, rax
no_yield:
}
```

and we can modify the original program like so:

```ASM
_co_start:
	PUSH 0x01
loop:
	; On every loop start, check if we've exceeded the max
	CHECK_LIMITS
	PUSH 0x01
	ADD
	JMP loop
```


# Some small notes

This coroutine implementation is very specific. For general purpose coroutines you'd have to save some more registers than `rip`, `rsp`, `rax` and `rbx`.
This works in this case because I've assigned a very specific meaning to each register that works all across the implementation, this allows for a smaller
and faster switching between coroutines than a general version.

Another thing is that right now this is very static, but I think it's fairly simple to see how to make it dynamic on the number of coroutines and having
different starting labels for each coroutine.

Right now with the implementation I've shown there is no stack security to simplify the idea, but of course the `PUSH` and `POP` instructions should have
boundary checks.

Another point is that the `inc rax` for every operation may add too much overhead, another option that I need to explore is adding an `add rax, <immediate>`
right before any jump operation. I believe this should be enough to reduce the overhead and it should cover all cases but I need to investigate further.

# The End

Hope this was informative and clear, if you have any question or you wanna talk, please let me know at `marce [at] dziban.net`.
