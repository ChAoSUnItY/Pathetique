open! Belt
open Data
open Parser

let a = "
# format of a line in this file:
# <instruction name> <args> <opcode>
#
# <opcode> is given by specifying one or more range/value pairs:
# hi..lo=value or bit=value or arg=value (e.g. 6..2=0x45 10=1 rd=0)
#
# <args> is one of rd, rs1, rs2, rs3, imm20, imm12, imm12lo, imm12hi,
# shamtw, shamt, rm
# rv32i
beq     bimm12hi rs1 rs2 bimm12lo 14..12=0 6..2=0x18 1..0=3
bne     bimm12hi rs1 rs2 bimm12lo 14..12=1 6..2=0x18 1..0=3
blt     bimm12hi rs1 rs2 bimm12lo 14..12=4 6..2=0x18 1..0=3
bge     bimm12hi rs1 rs2 bimm12lo 14..12=5 6..2=0x18 1..0=3
bltu    bimm12hi rs1 rs2 bimm12lo 14..12=6 6..2=0x18 1..0=3
bgeu    bimm12hi rs1 rs2 bimm12lo 14..12=7 6..2=0x18 1..0=3
"

let operands: array<string> = [
  "rd",
  "rs1",
  "rs2",
  "rs3",
  "bimm12hi",
  "bimm12lo",
  "imm12hi",
  "imm12lo",
  "imm12",
  "jimm20",
  "imm20",
  "fm",
  "pred",
  "succ",
  "rm",
  "shamtw",
  "shamt",
]

let rec merge = (
  a: InsnTree.t,
  b: InsnTree.t,
  ~path: array<string>=[],
): InsnTree.t => {
  open Belt.MutableMap.String

  b->forEach((key, bVal) => {
    switch a->get(key) {
      | Some(aVal) => switch (aVal, bVal) {
        | (Node(aVal), Node(bVal)) => a->set(key, Node(merge(aVal, bVal, ~path=[...path, key])))
        | (aVal, bVal) => if aVal == bVal {
          ()
        } else {
          raise(Failure(`Conflict at ${path->Array.joinWith(".", s => s)}`))
        }
      }
      | None => a->set(key, bVal)
    }
  })

  a
}

let decode = (src: string): InsnTree.t => {
  let result: InsnTree.t = MutableMap.String.make()
  let lines = src->String.split("\n")
  
  lines->Array.forEach(line => {
    
    ()
  })

  result
}

Console.log(Parser.args()("rs1 rd rs2"))
