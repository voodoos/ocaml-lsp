import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("ocamllsp/merlinCallCompatible", () => {
  let languageServer: LanguageServer.LanguageServer;

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

  async function sendMerlinCallCompatible(
    command: string,
    args: string[],
    resultAsSexp: boolean,
  ) {
    return languageServer.sendRequest("ocamllsp/merlinCallCompatible", {
      uri: "file:///test.ml",
      command,
      args,
      resultAsSexp,
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("case-analysis on simple example", async () => {
    openDocument(outdent`
type t = {a: int * int; b: string}
let f ({a; b} : t) = assert false
`);
    let r = await sendMerlinCallCompatible(
      "case-analysis",
      ["-start", "2:9", "-end", "2:9"],
      false,
    );
    expect(r).toMatchInlineSnapshot(`
      Object {
        "result": "{\\"class\\":\\"return\\",\\"value\\":[{\\"start\\":{\\"line\\":2,\\"col\\":8},\\"end\\":{\\"line\\":2,\\"col\\":9}},\\"a = (_, _)\\"]}",
        "resultAsSexp": false,
      }
    `);
  });

  it("case-analysis on empty example", async () => {
    openDocument(outdent``);
    let r = await sendMerlinCallCompatible(
      "case-analysis",
      ["-start", "2:9", "-end", "2:9"],
      false,
    );
    expect(r).toMatchInlineSnapshot(`
      Object {
        "result": "{\\"class\\":\\"exception\\",\\"value\\":\\"Merlin_analysis.Destruct.Nothing_to_do\\"}",
        "resultAsSexp": false,
      }
    `);
  });

  it("case-analysis on simple example with result as sexp", async () => {
    openDocument(outdent`
type t = {a: int * int; b: string}
let f ({a; b} : t) = assert false
`);
    let r = await sendMerlinCallCompatible(
      "case-analysis",
      ["-start", "2:9", "-end", "2:9"],
      true,
    );
    expect(r).toMatchInlineSnapshot(`
      Object {
        "result": "((assoc) (class . \\"return\\") (value ((assoc) (start (assoc) (line . 2) (col . 8)) (end (assoc) (line . 2) (col . 9))) \\"a = (_, _)\\"))",
        "resultAsSexp": true,
      }
    `);
  });
});
