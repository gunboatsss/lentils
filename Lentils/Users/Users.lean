/-
Users — IO wrapper for the `users` utility. 0BSD -/
import Lentils.Users.Logic

namespace Lentils.Users

open Logic

/-- FFI: get list of logged-in usernames from utmpx. -/
@[extern "lean_coreutils_users"]
opaque getLoggedInUsers : IO (Array String)

def run (_args : List String) : IO UInt32 := do
  let users ← getLoggedInUsers
  let userList := users.toList
  if userList.isEmpty then
    -- Still exit 0 but print nothing
    return 0
  else
    IO.println (formatUsers userList)
    return 0

end Lentils.Users
