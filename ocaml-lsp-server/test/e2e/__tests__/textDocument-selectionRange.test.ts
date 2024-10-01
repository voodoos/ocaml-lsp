import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/selectionRange", () => {
  let languageServer: LanguageServer.LanguageServer;

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  function openDocument(source: string) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(
          "file:///test.ml",
          "ocaml",
          0,
          source,
        ),
      },
    );
  }

  async function selectionRange(positions: Types.Position[]) {
    return await languageServer.sendRequest(
      Protocol.SelectionRangeRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        positions: positions,
      },
    );
  }

  it("returns a selection range for modules", async () => {
    openDocument(outdent`
      let foo a b =
        let min_ab = min a b in
        let max_ab = max a b in
        min_ab * max_ab
        `);

    let result = await selectionRange([Types.Position.create(1, 17)]);
    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "parent": Object {
          "parent": Object {
            "parent": Object {
              "range": Object {
                "end": Object {
                  "character": 17,
                  "line": 3,
                },
                "start": Object {
                  "character": 0,
                  "line": 0,
                },
              },
            },
            "range": Object {
              "end": Object {
                "character": 17,
                "line": 3,
              },
              "start": Object {
                "character": 8,
                "line": 0,
              },
            },
          },
          "range": Object {
            "end": Object {
              "character": 17,
              "line": 3,
            },
            "start": Object {
              "character": 2,
              "line": 1,
            },
          },
        },
        "range": Object {
          "end": Object {
            "character": 22,
            "line": 1,
          },
          "start": Object {
            "character": 2,
            "line": 1,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 22,
          "line": 1,
        },
        "start": Object {
          "character": 15,
          "line": 1,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 18,
        "line": 1,
      },
      "start": Object {
        "character": 15,
        "line": 1,
      },
    },
  },
]
`);
  });
  it("returns a selection range for deeply nested modules", async () => {
    openDocument(outdent`
      module Math = struct
        let add x y = x + y
        let multiply a b = a * b
        module Nested = struct
          let square z = multiply z z
        end
      end
    `);

    let result = await selectionRange([Types.Position.create(3, 20)]);
    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "parent": Object {
          "parent": Object {
            "range": Object {
              "end": Object {
                "character": 3,
                "line": 6,
              },
              "start": Object {
                "character": 0,
                "line": 0,
              },
            },
          },
          "range": Object {
            "end": Object {
              "character": 3,
              "line": 6,
            },
            "start": Object {
              "character": 14,
              "line": 0,
            },
          },
        },
        "range": Object {
          "end": Object {
            "character": 5,
            "line": 5,
          },
          "start": Object {
            "character": 2,
            "line": 1,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 5,
          "line": 5,
        },
        "start": Object {
          "character": 2,
          "line": 3,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 5,
        "line": 5,
      },
      "start": Object {
        "character": 18,
        "line": 3,
      },
    },
  },
]
`);
  });
  it("returns a selection range for module types", async () => {
    openDocument(outdent`
      module type MathOps = sig
        val add : int -> int -> int
        val subtract : int -> int -> int
      end

      module Math : MathOps = struct
        let add x y = x + y
        let subtract x y = x - y
      end
    `);

    let result = await selectionRange([Types.Position.create(0, 15)]);
    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "range": Object {
          "end": Object {
            "character": 3,
            "line": 8,
          },
          "start": Object {
            "character": 0,
            "line": 0,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 3,
          "line": 3,
        },
        "start": Object {
          "character": 0,
          "line": 0,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 19,
        "line": 0,
      },
      "start": Object {
        "character": 12,
        "line": 0,
      },
    },
  },
]
`);
  });

  it("returns a selection range for pattern matching", async () => {
    openDocument(outdent`
      let describe_shape = function
        | Circle r -> Printf.sprintf "Circle with radius %f" r
        | Rectangle (w, h) -> Printf.sprintf "Rectangle %f x %f" w h
        | _ -> "Unknown shape"
    `);

    let result = await selectionRange([Types.Position.create(1, 10)]);
    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "range": Object {
        "end": Object {
          "character": 24,
          "line": 3,
        },
        "start": Object {
          "character": 0,
          "line": 0,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 24,
        "line": 3,
      },
      "start": Object {
        "character": 21,
        "line": 0,
      },
    },
  },
]
`);
  });

  it("returns a selection range for nested let bindings", async () => {
    openDocument(outdent`
      let rec factorial n =
        let rec aux acc n =
          if n <= 1 then acc else aux (acc * n) (n - 1)
        in aux 1 n
    `);

    let result = await selectionRange([Types.Position.create(2, 10)]);
    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "parent": Object {
          "parent": Object {
            "parent": Object {
              "parent": Object {
                "parent": Object {
                  "range": Object {
                    "end": Object {
                      "character": 12,
                      "line": 3,
                    },
                    "start": Object {
                      "character": 0,
                      "line": 0,
                    },
                  },
                },
                "range": Object {
                  "end": Object {
                    "character": 12,
                    "line": 3,
                  },
                  "start": Object {
                    "character": 18,
                    "line": 0,
                  },
                },
              },
              "range": Object {
                "end": Object {
                  "character": 12,
                  "line": 3,
                },
                "start": Object {
                  "character": 2,
                  "line": 1,
                },
              },
            },
            "range": Object {
              "end": Object {
                "character": 49,
                "line": 2,
              },
              "start": Object {
                "character": 2,
                "line": 1,
              },
            },
          },
          "range": Object {
            "end": Object {
              "character": 49,
              "line": 2,
            },
            "start": Object {
              "character": 14,
              "line": 1,
            },
          },
        },
        "range": Object {
          "end": Object {
            "character": 49,
            "line": 2,
          },
          "start": Object {
            "character": 4,
            "line": 2,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 13,
          "line": 2,
        },
        "start": Object {
          "character": 7,
          "line": 2,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 11,
        "line": 2,
      },
      "start": Object {
        "character": 9,
        "line": 2,
      },
    },
  },
]
`);
  });

  it("handles GADTs with phantom types", async () => {
    openDocument(outdent`
      type _ typ =
        | TInt : int typ
        | TBool : bool typ
      type packed = Pack : 'a typ * 'a -> packed
      let print : type a. a typ -> a -> string = function
        | TInt -> string_of_int
        | TBool -> string_of_bool
      let show (Pack (t, v)) = print t v
    `);

    let result = await selectionRange([Types.Position.create(7, 8)]);
    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
    "parent": Object {
      "parent": Object {
        "range": Object {
          "end": Object {
            "character": 34,
            "line": 7,
          },
          "start": Object {
            "character": 0,
            "line": 0,
          },
        },
      },
      "range": Object {
        "end": Object {
          "character": 34,
          "line": 7,
        },
        "start": Object {
          "character": 0,
          "line": 7,
        },
      },
    },
    "range": Object {
      "end": Object {
        "character": 8,
        "line": 7,
      },
      "start": Object {
        "character": 4,
        "line": 7,
      },
    },
  },
]
`);
  });
});

