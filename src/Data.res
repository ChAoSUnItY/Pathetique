module ISAFormat = {
  module Arg = {
    type t =
      | Rd
      | Rs1
      | Rs2
      | Rs3
      | Imm20
      | Imm12
      | Imm12lo
      | Imm12hi
      | Shamtw
      | Shamt
      | Rm
  }

  module Opcode = {
    module Range = {
      type t = {
        hi: int,
        lo: int
      }
    }

    module Bit = {
      type t = {
        bit: int,
      }
    }

    module Arg = {
      type t = {
        arg: Arg.t
      }
    }
    
    type tag =
      | Range(Range.t)
      | Bit(Bit.t)
      | Arg(Bit.t)

    type t = {
      tag: tag,
      val: int
    }
  }

  type t = {
    insn_name: string,
    args: array<Arg.t>,

  }
}

module DecodeNode = {
  type rec t =
    | Node(MutableMap.String.t<t>)
    | Leaf(ISAFormat.t)
}

module InsnTree = {
  type t = MutableMap.String.t<DecodeNode.t>
}