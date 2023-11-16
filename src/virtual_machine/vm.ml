open Types

type command = { op : op_code; arg : int32 }

type vm = {
  mutable stack_command : command array;
  mutable stack : int32 array;
  mutable heap : int32 Stack.t;
  mutable command_cursor : int32;
  mutable stack_cursor : int;
}

let create_vm program =
  {
    stack_command = program;
    stack = Array.make 10000 0l;
    heap = Stack.create ();
    command_cursor = 0l;
    stack_cursor = 0;
  }

let print_vm_heap vm =
  Stack.iter (fun n -> Int32.to_string n |> print_endline) vm.heap

let run_vm vm =
  let get_command vm =
    Array.get vm.stack_command (Int32.to_int vm.command_cursor)
  in
  let rec execute_command vm =
    let command = get_command vm in
    let _ =
      match command.op with
      | Push -> Stack.push command.arg vm.heap
      | PushVar ->
          let var = Array.get vm.stack (Int32.to_int command.arg) in
          Stack.push var vm.heap
      | SetVar ->
          let var_value = Stack.pop vm.heap in
          let _ = Array.set vm.stack (Int32.to_int command.arg) var_value in
          ()
      | Add ->
          let left = Stack.pop vm.heap in
          let right = Stack.pop vm.heap in
          let _ = Stack.push (Int32.add left right) vm.heap in
          ()
      | Sub ->
          let left = Stack.pop vm.heap in
          let right = Stack.pop vm.heap in
          let _ = Stack.push (Int32.sub left right) vm.heap in
          ()
      | EQ ->
          let left = Stack.pop vm.heap in
          let right = Stack.pop vm.heap in
          let _ =
            if Int32.equal left right then Stack.push 1l vm.heap
            else Stack.push 0l vm.heap
          in
          ()
      | JMP ->
          let jmpcond = Stack.pop vm.heap in
          if Int32.equal jmpcond 0l then vm.command_cursor <- command.arg
      | Goto -> vm.command_cursor <- command.arg
      | End -> vm.command_cursor <- -10l
      | Call -> vm.command_cursor <- Int32.sub command.arg 1l
      | Ret -> vm.command_cursor <- Int32.sub command.arg 1l
    in
    let array_length = Array.length vm.stack_command in
    let _ = vm.command_cursor <- Int32.add vm.command_cursor 1l in
    if Int32.to_int vm.command_cursor < array_length && vm.command_cursor >= 0l
    then execute_command vm
    else ()
  in
  execute_command vm

let push arg = { op = Push; arg }
let add = { op = Add; arg = 0l }
let sub = { op = Add; arg = 0l }
let jmp arg = { op = JMP; arg }
let goto arg = { op = Goto; arg }
let eq = { op = EQ; arg = 0l }
let ret adr = { op = Ret; arg = adr }
let end_op = { op = End; arg = 0l }
let call adr = { op = Call; arg = adr }
let push_var adr = { op = PushVar; arg = adr }
let set_var adr = { op = SetVar; arg = adr }

let program =
  [|
    push 2l;
    push 3l;
    set_var 0l;
    set_var 1l;
    call 6l;
    end_op;
    push_var 0l;
    push_var 0l;
    add;
    push_var 1l;
    push_var 1l;
    add;
    push_var 0l;
    push_var 1l;
    add;
    ret 5l;
  |]

let virt = create_vm program
