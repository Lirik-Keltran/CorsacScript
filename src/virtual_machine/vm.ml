open Types

type command = {
  op: op_code;
  arg: int32;
}

type vm = {
  mutable stack_command : command array;
  mutable stack         : int array;
  mutable memory        : int Stack.t;
  mutable cursor        : int;
  mutable memory_cursor : int
}

let create_vm program = {
  stack_command = program;
  stack = Array.make 10000 0;
  memory = Stack.create ();
  cursor = 0;
  memory_cursor = 0;
}

let run_vm vm = 
  let execute_command vm = 
    let command = Array.get vm.stack_command vm.cursor in 
      match command.op with
      | Push ->
  in
  let array_length = Array.length vm.stack_command in 
  while vm.cursor < array_length do
    let _ = execute_command vm in
    vm.cursor <- vm.cursor + 1
  done


let push arg = {
  op = Push;
  arg
}

let add = {
  op = Add;
  arg = 0l;
}

let sub = {
  op = Add;
  arg = 0l;
}

let jmp arg = {
  op = JMP;
  arg;
}

let goto arg = {
  op = Goto;
  arg;
}

let eq = {
  op = EQ;
  arg = 0l;
}












