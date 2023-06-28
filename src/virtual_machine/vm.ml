open Types

type vm = {
  stack_command : command list;
  stack_memory : value array;
  cursor : int;
}

let create_vm stack_command =
  { stack_command; stack_memory = Array.make 150 (Num 0); cursor = 0 }

let add_command vm command =
  { vm with stack_command = vm.stack_command @ [ command ] }
