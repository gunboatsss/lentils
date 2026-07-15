/-
Sync — IO wrapper for the `sync` utility.
0BSD

Calls `sync(2)` via the C FFI to flush all filesystem buffers to disk.

Provenance: POSIX.1-2017, Section "sync — synchronise cached writes to disk".
No GPL source was consulted.
-/

import Lentils.Sync.Logic
import Lentils.Common.IO.Native

namespace Lentils.Sync

open Logic
open Lentils.Common.IO.Native

/--
Run the `sync` utility.

Flushes all filesystem write caches to disk. Always returns exit code 0
(sync(2) has no failure return, though it may block on I/O).
-/
def run (_args : List String) : IO UInt32 := do
  sync
  return 0

end Lentils.Sync
