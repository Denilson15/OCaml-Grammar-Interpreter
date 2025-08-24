# 🧠 OCaml Stack-Based Interpreter  

A lightweight stack-based interpreter written in OCaml. This project implements a small virtual machine that parses and executes a minimal instruction set. It was designed as a way to practice functional programming concepts like parsing, recursion, and evaluation, while keeping everything in a single file.  

The interpreter currently supports the following instructions:  
- `PUSH <int>` → push an integer onto the stack  
- `POP` → remove the top value  
- `ADD`, `SUB`, `MUL`, `DIV` → perform arithmetic on the top two values  
- `PRINT` → display the top value  

### Example Program  
PUSH 4
PUSH 5
MUL
PRINT

Output:  
20


### How to Run  
Make sure you have [OCaml](https://ocaml.org/) installed. Save your program as a `.txt` file (e.g. `program.txt`), then compile and run:  
ocamlc -o interpreter interpreter.ml
./interpreter program.txt


This project was built to deepen my understanding of functional programming, parsing, and evaluation in OCaml. It serves as a foundation for future extensions such as control flow, variables, and more advanced instruction sets.  
