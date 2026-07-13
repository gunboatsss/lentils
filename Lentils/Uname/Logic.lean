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

end Lentils.Uname.Logic
