/-
Uname.Logic — Pure system-info formatting for `uname`. 0BSD -/
namespace Lentils.Uname.Logic

structure UnameInfo where
  sysname  : String := ""
  nodename : String := ""
  release  : String := ""
  version  : String := ""
  machine  : String := ""
deriving Inhabited

def formatAll (info : UnameInfo) : String :=
  s!"{info.sysname} {info.nodename} {info.release} {info.version} {info.machine}"

def formatSysname (info : UnameInfo) : String := info.sysname
def formatNodename (info : UnameInfo) : String := info.nodename
def formatRelease (info : UnameInfo) : String := info.release
def formatVersion (info : UnameInfo) : String := info.version
def formatMachine (info : UnameInfo) : String := info.machine

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- formatSysname returns the sysname field. -/
theorem formatSysname_eq : formatSysname { sysname := "Linux", nodename := "", release := "", version := "", machine := "" } = "Linux" := by
  native_decide

/-- formatNodename returns the nodename field. -/
theorem formatNodename_eq : formatNodename { sysname := "", nodename := "myhost", release := "", version := "", machine := "" } = "myhost" := by
  native_decide

/-- formatRelease returns the release field. -/
example : formatRelease { sysname := "", nodename := "", release := "6.5.0", version := "", machine := "" } = "6.5.0" := by
  native_decide

end Lentils.Uname.Logic
