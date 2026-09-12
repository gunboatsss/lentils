/-
Mktemp.Logic — Verified pure logic for `mktemp`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Mktemp.Logic

open Lentils.Common.Spec

structure Options where
  directory : Bool := false
  tmpdir : String := ""
  suffix : String := ""
  dryRun : Bool := false
  quiet : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure MktempInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : MktempInput := { args := [] }

def defaultTemplate : String := "tmp.XXXXXXXXXX"

def parseArgs (args : List String) : Options × String :=
  let rec go (remaining : List String) (opts : Options) (template : String) : Options × String :=
    match remaining with
    | [] => (opts, if template.isEmpty then defaultTemplate else template)
    | "--" :: _ => (opts, if template.isEmpty then defaultTemplate else template)
    | "-d" :: rest => go rest { opts with directory := true } template
    | "-u" :: rest => go rest { opts with dryRun := true } template
    | "-q" :: rest => go rest { opts with quiet := true } template
    | "-p" :: dir :: rest => go rest { opts with tmpdir := dir } template
    | "--suffix" :: suf :: rest => go rest { opts with suffix := suf } template
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        (opts, if template.isEmpty then defaultTemplate else template)
      else go rest opts s
  go args {} ""

def buildTemplate (opts : Options) (userTemplate : String) : String :=
  let base :=
    if userTemplate.contains '/' then userTemplate
    else if opts.tmpdir.isEmpty then userTemplate
    else
      let dir := opts.tmpdir
      let dir' := if dir.endsWith "/" then dir.dropRight 1 else dir
      dir' ++ "/" ++ userTemplate
  let withSuffix := base ++ opts.suffix
  withSuffix

def spec (input : MktempInput) : Options × String := parseArgs input.args

/-- Unfolding lemma: `spec` delegates to `parseArgs`. -/
theorem i_spec_unfold (input : MktempInput) : spec input = parseArgs input.args := by
  simp [spec]

theorem i_empty : spec defaultInput = ({}, defaultTemplate) := by native_decide

example : (parseArgs []).2 = defaultTemplate := by native_decide
example : (parseArgs ["-d"]).1.directory = true := by native_decide
example : (parseArgs ["mytemp.XXXXXX"]).2 = "mytemp.XXXXXX" := by native_decide
example : buildTemplate {} "tmp.XXXXXX" = "tmp.XXXXXX" := by native_decide
example : buildTemplate { tmpdir := "/tmp", suffix := "" } "tmp.XXXXXX" = "/tmp/tmp.XXXXXX" := by native_decide
example : buildTemplate { tmpdir := "/tmp/", suffix := "" } "tmp.XXXXXX" = "/tmp/tmp.XXXXXX" := by native_decide
example : buildTemplate { suffix := ".bak" } "tmp.XXXXXX" = "tmp.XXXXXX.bak" := by native_decide
example : buildTemplate { tmpdir := "/tmp", suffix := ".txt" } "tmp.XXXXXX" = "/tmp/tmp.XXXXXX.txt" := by native_decide

end Lentils.Mktemp.Logic
