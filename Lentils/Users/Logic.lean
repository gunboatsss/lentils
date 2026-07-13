/-
Users.Logic — Pure user-listing logic for `users`. 0BSD -/
namespace Lentils.Users.Logic

/--
Format a list of usernames space-separated.
-/
def formatUsers (usernames : List String) : String :=
  String.intercalate " " usernames

end Lentils.Users.Logic
