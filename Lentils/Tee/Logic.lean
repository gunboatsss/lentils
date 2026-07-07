/-
Tee.Logic — Pure spec for `tee`. 0BSD -/
namespace Lentils.Tee.Logic
open ByteArray
def processBytes (ba : ByteArray) : ByteArray := ba
def parseAppend (args : List String) : Bool := args.any (λ arg => arg = "-a" || arg = "--append")
def parseFilenames (args : List String) : List String := args.filter (λ arg => arg ≠ "-a" && arg ≠ "--append")
theorem processBytes_id (ba : ByteArray) : processBytes ba = ba := rfl
end Lentils.Tee.Logic
