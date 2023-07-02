open Types

type command = {
  op: op_code;
  arg: int32;
}

type vm = {
  mutable stack_command   : command array;
  mutable stack           : int32 array;
  mutable heap            : int32 Stack.t;
  mutable command_cursor  : int;
  mutable stack_cursor    : int;
}

let create_vm program = {
  stack_command = program;
  stack = Array.make 10000 0l;
  heap = Stack.create ();
  command_cursor = 0;
  stack_cursor = 0;
}

let run_vm vm =
  let get_command vm = Array.get vm.stack_command vm.command_cursor in
  let execute_command vm = 
    let command = get_command vm in 
      match command.op with
      | Push  -> Stack.push command.arg vm.heap
      | PushVar -> 
        let var = Array.get vm.stack (Int32.to_int command.arg)  in
        Stack.push var vm.heap
      | SetVar -> 
        let var_value = Stack.pop vm.heap in
        let _ = Array.set vm.stack (Int32.to_int command.arg) var_value in
        ()
      | Add   -> 
        let left = Stack.pop vm.heap in 
        let right = Stack.pop vm.heap in
        let _ = Stack.push (Int32.add left right) vm.heap in ()
      | Sub   -> 
        let left = Stack.pop vm.heap in 
        let right = Stack.pop vm.heap in
        let _ = Stack.push (Int32.sub left right) vm.heap in ()
      | EQ    ->
        let left = Stack.pop vm.heap in 
        let right = Stack.pop vm.heap in
        let _ = if Int32.equal left right then Stack.push 1l vm.heap else Stack.push 0l vm.heap in ()
      | JMP   ->
        let jmpcond = Stack.pop vm.heap in 
        if Int32.equal jmpcond 0l then vm.command_cursor <- Int32.to_int command.arg
      | Goto -> vm.command_cursor <- Int32.to_int command.arg
      | End  -> vm.command_cursor <- -1
      | Call -> 
        let arg = Stack.pop vm.heap in
        let _ = Array.set vm.stack (Int32.to_int command.arg) arg in
        let _ = Stack.push vm.heap vm.command_cursor in
        failwith "Todo"
      | _ -> failwith "Todo"
  in
  let array_length = Array.length vm.stack_command in 
  while vm.command_cursor < array_length && vm.command_cursor >= 0 do
    let _ = execute_command vm in
    vm.command_cursor <- vm.command_cursor + 1
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












