type message_to_status = [
  | `Status_change of string * string
]

type message_to_modules = unit