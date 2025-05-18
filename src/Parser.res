open Data

module Parser = {
  module Err = {
    type t = {msg: string}
  }

  module Result = {
    type t<'a> = result<('a, string), Err.t>
  }

  type t<'a> = string => Result.t<'a>

  let split = (str: string, length: int): (string, string) => {
    (str->String.substring(~start=0, ~end=length), str->String.substringToEnd(~start=length))
  }

  let satisfy = (predicate: char => bool): t<char> => {
    input => {
      switch input->split(1) {
      | ("", _) => Error({Err.msg: "Empty input"})
      | (val, remain) if val->OCamlCompat.String.get(0)->predicate =>
        Ok((val->OCamlCompat.String.get(0), remain))
      | (val, _) => Error({Err.msg: `Mismatched character, "${val}" does not satisfy`})
      }
    }
  }

  let identifier = (id: string): t<string> => {
    input => {
      switch input->split(id->String.length) {
      | (val, remain) if val === id => Ok((val, remain))
      | (val, _) => Error({Err.msg: `Mismatched identifier "${val}", expects "${id}"`})
      }
    }
  }

  let identifierConst = (id: string, val: 'a): t<'a> => {
    input => {
      switch identifier(id)(input) {
      | Ok((_, remain)) => Ok((val, remain))
      | Error(err) => Error(err)
      }
    }
  }

  let andThen = (a: t<'a>, b: t<'b>): t<'b> => {
    input => {
      let result = a(input)

      switch result {
      | Ok((_, remain)) => b(remain)
      | Error(err) => Error(err)
      }
    }
  }

  let orElse = (a: t<'a>, b: t<'a>): t<'a> => {
    input => {
      let result = a(input)

      switch result {
      | Ok(_) => result
      | Error(_) => b(input)
      }
    }
  }

  let oneOf = (parsers: array<t<'a>>): t<'a> => {
    parsers->Belt.Array.reduce(_ => Error({Err.msg: "Empty parser"}), orElse)
  }

  let rec zeroOrMore = (parser: t<'a>): t<array<'a>> => {
    input => {
      switch parser(input) {
      | Ok((val, remain)) => switch zeroOrMore(parser)(remain) {
        | Ok((others, remain)) => Ok(([val, ...others], remain))
        | Error(_) => Ok(([val], remain))
        }
      | Error(_) => Ok(([], input))
      }
    }
  }

  let whitespace = (): t<()> => {
    input => {
      switch zeroOrMore(satisfy(c => c == ' '))(input) {
        | Ok((_, remain)) => Ok(((), remain))
        | Error(_) => Ok(((), input))
      }
    }
  }

  let arg = (): t<ISAFormat.Arg.t> => {
    open ISAFormat.Arg

    let argParser = oneOf([
      identifierConst("rd", Rd),
      identifierConst("rs1", Rs1),
      identifierConst("rs2", Rs2),
      identifierConst("rs2", Rs3),
      identifierConst("imm20", Imm20),
      identifierConst("imm12", Imm12),
      identifierConst("imm12lo", Imm12lo),
      identifierConst("imm12hi", Imm12hi),
      identifierConst("shamtw", Shamtw),
      identifierConst("shamt", Shamt),
      identifierConst("rm", Rm),
    ])

    whitespace()->andThen(argParser)
  }

  let args = (): t<array<ISAFormat.Arg.t>> => {
    zeroOrMore(arg())
  }

  let runParser = (parser: t<'a>, input: string): Result.t<'a> => {
    parser(input)
  }
}
