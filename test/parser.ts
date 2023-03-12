import assert from "assert";
import parser, { parse } from "../lib/parser";
import { getReprinter } from "../lib/patcher";
import { Printer } from "../lib/printer";
import { fromString } from "../lib/lines";
import * as types from "ast-types";
const namedTypes = types.namedTypes;
import FastPath from "../lib/fast-path";
import { EOL as eol } from "os";
import { comparePos } from "../lib/util";
import { randomInt } from "crypto";
const nodeMajorVersion = parseInt(process.versions.node, 10);

// Esprima seems unable to handle unnamed top-level functions, so declare
// test functions with names and then export them later.

describe("parser", function() {
  [
    "../parsers/acorn",
    "../parsers/babel",
    "../parsers/esprima",
    "../parsers/flow",
    "../parsers/typescript",
  ].forEach(runTestsForParser);

  it("AlternateParser", function() {
    const b = types.builders;
    const parser = {
      parse: function() {
        const program = b.program([
          b.expressionStatement(b.identifier("surprise")),
        ]);
        program.comments = [];
        return program;
      },
    };

    function check(options?: any) {
      const ast = parse("ignored", options);
      const printer = new Printer();

      types.namedTypes.File.assert(ast, true);
      assert.strictEqual(printer.printGenerically(ast).code, "surprise;");
    }

    check({ esprima: parser });
    check({ parser: parser });
  });
});

function runTestsForParser(parserId: string) {
  const parserName = parserId.split("/").pop();

  if (
    nodeMajorVersion < 6 &&
    (parserName === "babel" ||
      parserName === "flow" ||
      parserName === "typescript")
  ) {
    // Babel 7 no longer supports Node 4 or 5.
    return;
  }

  if (!parserName) {
    return;
  }

  const parser = require(parserId);

  it("[" + parserName + "] empty source", function() {
    const printer = new Printer();

    function check(code: string) {
      const ast = parse(code, { parser });
      assert.strictEqual(printer.print(ast).code, code);
    }

    check("");
    check("/* block comment */");
    check("// line comment");
    check("\t\t\t");
    check(eol);
    check(eol + eol);
    check("    ");
  });

  const lineCommentTypes: { [name: string]: string } = {
    acorn: "Line",
    babel: "CommentLine",
    esprima: "Line",
    flow: "CommentLine",
    typescript: "CommentLine",
  };

  it("[" + parserName + "] parser basics", function testParser(done) {
    const code = testParser + "";
    const ast = parse(code, { parser });

    namedTypes.File.assert(ast);
    assert.ok(getReprinter(FastPath.from(ast)));

    const funDecl = ast.program.body[0];
    const funBody = funDecl.body;

    namedTypes.FunctionDeclaration.assert(funDecl);
    namedTypes.BlockStatement.assert(funBody);
    assert.ok(getReprinter(FastPath.from(funBody)));

    const lastStatement = funBody.body.pop();
    const doneCall = lastStatement.expression;

    assert.ok(!getReprinter(FastPath.from(funBody)));
    assert.ok(getReprinter(FastPath.from(ast)));

    funBody.body.push(lastStatement);
    assert.ok(getReprinter(FastPath.from(funBody)));

    assert.strictEqual(doneCall.callee.name, "done");

    assert.strictEqual(lastStatement.comments.length, 2);

    const firstComment = lastStatement.comments[0];

    assert.strictEqual(firstComment.type, lineCommentTypes[parserName]);

    assert.strictEqual(firstComment.leading, true);
    assert.strictEqual(firstComment.trailing, false);
    assert.strictEqual(
      firstComment.value,
      " Make sure done() remains the final statement in this function,",
    );

    const secondComment = lastStatement.comments[1];

    assert.strictEqual(secondComment.type, lineCommentTypes[parserName]);

    assert.strictEqual(secondComment.leading, true);
    assert.strictEqual(secondComment.trailing, false);
    assert.strictEqual(
      secondComment.value,
      " or the above assertions will probably fail.",
    );

    // Make sure done() remains the final statement in this function,
    // or the above assertions will probably fail.
    done();
  });

  it("[" + parserName + "] LocationFixer", function() {
    const code = ["function foo() {", "    a()", "    b()", "}"].join(eol);
    const ast = parse(code, { parser });
    const printer = new Printer();

    types.visit(ast, {
      visitFunctionDeclaration: function(path) {
        if (namedTypes.BlockStatement.check(path.node.body)) {
          path.node.body.body.reverse();
        }
        this.traverse(path);
      },
    });

    const altered = code
      .replace("a()", "xxx")
      .replace("b()", "a()")
      .replace("xxx", "b()");

    assert.strictEqual(altered, printer.print(ast).code);
  });

  it("[" + parserName + "] TabHandling", function() {
    function check(code: string, tabWidth: number) {
      const lines = fromString(code, { tabWidth: tabWidth });
      assert.strictEqual(lines.length, 1);

      function checkId(s: any, loc: types.namedTypes.SourceLocation) {
        const sliced = lines.slice(loc.start, loc.end);
        assert.strictEqual(s + "", sliced.toString());
      }

      types.visit(
        parse(code, {
          tabWidth: tabWidth,
          parser,
        }),
        {
          visitIdentifier(path) {
            const ident = path.node;
            checkId(ident.name, ident.loc!);
            this.traverse(path);
          },

          visitLiteral(path) {
            const lit = path.node;
            checkId(lit.value, lit.loc!);
            this.traverse(path);
          },
        },
      );
    }

    for (let tabWidth = 1; tabWidth <= 8; ++tabWidth) {
      check("\t\ti = 10;", tabWidth);
      check("\t\ti \t= 10;", tabWidth);
      check("\t\ti \t=\t 10;", tabWidth);
      check("\t \ti \t=\t 10;", tabWidth);
      check("\t \ti \t=\t 10;\t", tabWidth);
      check("\t \ti \t=\t 10;\t ", tabWidth);
    }
  });

  it("[" + parserName + "] Only comment followed by space", function() {
    const printer = new Printer();

    function check(code: string) {
      const ast = parse(code, { parser });
      assert.strictEqual(printer.print(ast).code, code);
    }

    check("// comment");
    check("// comment ");
    check("// comment\n");
    check("// comment\n\n");
    check(" // comment\n");
    check(" // comment\n ");
    check(" // comment \n ");

    check("/* comment */");
    check("/* comment */ ");
    check(" /* comment */");
    check("\n/* comment */");
    check("\n/* comment */\n");
    check("\n /* comment */\n ");
    check("/* comment */\n ");
    check("/* com\n\nment */");
    check("/* com\n\nment */ ");
    check(" /* com\n\nment */ ");
  });
}

// Testing the tree copier "findTokenRange" algorithm.
// Changed from linear search to binary search because of performance issues.
describe("TreeCopier", function() {

  const TreeCopier = parser[Symbol.for('TreeCopier')]
  type Position = { line: number; column: number; token: unknown };
  type Location = { loc: { start: Position; end: Position } }

  function loc(startLine: number, endLine: number): Location {
    return { loc: { start: position(startLine, 0), end: position(endLine, 0) } }
  }

  function position(line: number, column: number): Position {
    return { line, column, token: undefined }
  }

  randomCheckFindTokenRange;
  function randomCheckFindTokenRange(tokens: Location[]) {

    let min = tokens[0], max = tokens[0];
    for (const token of tokens) {
      if (comparePos(min.loc.start, token.loc.start) > 0) min = token;
      if (comparePos(max.loc.end, token.loc.end) < 0) max = token;
    }

    const startLine = randomInt(max.loc.start.line - min.loc.start.line) + min.loc.start.line;
    const endLine = randomInt(max.loc.start.line - startLine) + startLine;
    const column = 0;
    let sample = {
      tokens,
      start: { line: startLine, column, token: undefined },
      end: { line: endLine, column, token: undefined },
    }

    new OldTreeCopier().linearFindTokenRange(sample)
    const expectedOutput = { startIndex: sample.start.token, endIndex: sample.end.token }

    sample = {
      tokens,
      start: { line: startLine, column, token: undefined },
      end: { line: endLine, column, token: undefined },
    }

    new TreeCopier([], []).findTokenRange(sample)
    const actualOutput = { startIndex: sample.start.token, endIndex: sample.end.token }

    assert.equal(actualOutput.startIndex, expectedOutput.startIndex, JSON.stringify({
      message: `[random] old tree copier mismatch - expected start to be the first token in node`,
      sample,
    }, null, 2))
    assert.equal(actualOutput.endIndex, expectedOutput.endIndex, JSON.stringify({
      message: `[random] old tree copier mismatch - expected start to be the first token after node`,
      sample,
    }, null, 2))

  }

  function createCheckFindTokenRange(tokens: Location[]) {
    return (name: string, input: Location, output: { startIndex: number, endIndex: number }) => {

      let sample = {
        tokens,
        ...input.loc,
      }

      new OldTreeCopier().linearFindTokenRange(sample)
      assert.equal(sample.start.token, output.startIndex, `[${name}] BAD TEST - old tree copier mismatch - expected start to be the first token in node`)
      assert.equal(sample.end.token, output.endIndex, `[${name}] BAD TEST - old tree copier mismatch - expected start to be the first token after node`)

      sample = {
        tokens,
        ...input.loc,
      }

      new TreeCopier([], []).findTokenRange(sample)
      assert.equal(sample.start.token, output.startIndex, `[${name}] expected start to be the first token in node`)
      assert.equal(sample.end.token, output.endIndex, `[${name}] expected start to be the first token after node`)

    }
  }

  it("[findTokenRange] with duplications", function() {

    const tokens = [loc(1, 1), loc(2, 2), loc(2, 2), loc(2, 2), loc(2, 2), loc(2, 2), loc(3, 4)]

    const checkTokenRange = createCheckFindTokenRange(tokens)
    checkTokenRange;

    checkTokenRange('(2,2)', loc(2, 2), { startIndex: 1, endIndex: 1 })
    checkTokenRange('(1,1)', loc(1, 1), { startIndex: 0, endIndex: 0 }) // node contains no tokens
    checkTokenRange('(1,3)', loc(1, 3), { startIndex: 0, endIndex: 6 })
    checkTokenRange('(2,4)', loc(2, 4), { startIndex: 1, endIndex: 6 })
    checkTokenRange('(3,3)', loc(3, 3), { startIndex: 6, endIndex: 6 }) // node contains no tokens
    checkTokenRange('(4,4)', loc(4, 4), { startIndex: 6, endIndex: 6 }) // node contains no tokens

    for (let i = 0; i < 1000; i++) {
      randomCheckFindTokenRange(tokens)
    }

    for (let i = 0; i < 10000; i++) {
      randomCheckFindTokenRange([
        loc(1, 1), loc(2, 2), loc(2, 2), loc(2, 2), loc(2, 2), loc(2, 2),
        loc(3, 4), loc(4, 4), loc(4, 4), loc(4, 4), loc(4, 4), loc(4, 4),
        loc(4, 7), loc(7, 9), loc(9, 9), loc(9, 9), loc(9, 10), loc(10, 111),
      ])
    }

  });

  it("[findTokenRange] without duplications", function() {

    const tokens = [loc(1, 1), loc(2, 3), loc(3, 4), loc(4, 5), loc(5, 6)]

    const checkTokenRange = createCheckFindTokenRange(tokens)
    checkTokenRange;

    checkTokenRange('(1,4)', loc(1, 4), { startIndex: 0, endIndex: 2 })
    checkTokenRange('(1,1)', loc(1, 1), { startIndex: 0, endIndex: 0 }) // node contains no tokens
    checkTokenRange('(5,6)', loc(5, 6), { startIndex: 4, endIndex: 4 })
    checkTokenRange('(3,3)', loc(3, 3), { startIndex: 1, endIndex: 1 }) // node contains no tokens
    checkTokenRange('(4,4)', loc(4, 4), { startIndex: 2, endIndex: 2 }) // node contains no tokens

    for (let i = 0; i < 10000; i++) {
      randomCheckFindTokenRange(tokens)
    }

  });

})


// This is the old implementation taken from the original "recast" library.
// Comparing against the old implementation.
class OldTreeCopier {

  startTokenIndex: number;
  endTokenIndex: number;

  constructor() {
    this.startTokenIndex = 0;
    this.endTokenIndex = 0;
  }

  linearFindTokenRange(loc: any) {
    // In the unlikely event that loc.tokens[this.startTokenIndex] starts
    // *after* loc.start, we need to rewind this.startTokenIndex first.
    while (this.startTokenIndex > 0) {
      const token = loc.tokens[this.startTokenIndex];
      if (comparePos(loc.start, token.loc.start) < 0) {
        --this.startTokenIndex;
      } else break;
    }

    // In the unlikely event that loc.tokens[this.endTokenIndex - 1] ends
    // *before* loc.end, we need to fast-forward this.endTokenIndex first.
    while (this.endTokenIndex < loc.tokens.length) {
      const token = loc.tokens[this.endTokenIndex];
      if (comparePos(token.loc.end, loc.end) < 0) {
        ++this.endTokenIndex;
      } else break;
    }

    // Increment this.startTokenIndex until we've found the first token
    // contained by this node.
    while (this.startTokenIndex < this.endTokenIndex) {
      const token = loc.tokens[this.startTokenIndex];
      if (comparePos(token.loc.start, loc.start) < 0) {
        ++this.startTokenIndex;
      } else break;
    }

    // Index into loc.tokens of the first token within this node.
    loc.start.token = this.startTokenIndex;

    // Decrement this.endTokenIndex until we've found the first token after
    // this node (not contained by the node).
    while (this.endTokenIndex > this.startTokenIndex) {
      const token = loc.tokens[this.endTokenIndex - 1];
      if (comparePos(loc.end, token.loc.end) < 0) {
        --this.endTokenIndex;
      } else break;
    }

    // Index into loc.tokens of the first token *after* this node.
    // If loc.start.token === loc.end.token, the node contains no tokens,
    // and the index is that of the next token following this node.
    loc.end.token = this.endTokenIndex;
  };

}
