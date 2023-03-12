import assert from "assert";
import * as types from "ast-types";
const b = types.builders;
const isObject = types.builtInTypes.object;
const isArray = types.builtInTypes.array;
import { normalize as normalizeOptions } from "./options";
import { fromString } from "./lines";
import { attach as attachComments } from "./comments";
import * as util from "./util";
import { Options } from "./options";
import { Direction, SortedArray } from 'search-sorted-array';

export function parse(source: string, options?: Partial<Options>) {
  options = normalizeOptions(options);

  const lines = fromString(source, options);

  const sourceWithoutTabs = lines.toString({
    tabWidth: options.tabWidth,
    reuseWhitespace: false,
    useTabs: false,
  });

  let comments: any[] = [];
  const ast = options.parser.parse(sourceWithoutTabs, {
    jsx: true,
    loc: true,
    locations: true,
    range: options.range,
    comment: true,
    onComment: comments,
    tolerant: util.getOption(options, "tolerant", true),
    ecmaVersion: 6,
    sourceType: util.getOption(options, "sourceType", "module"),
  });

  // Use ast.tokens if possible, and otherwise fall back to the Esprima
  // tokenizer. All the preconfigured ../parsers/* expose ast.tokens
  // automatically, but custom parsers might need additional configuration
  // to avoid this fallback.
  const tokens: any[] = Array.isArray(ast.tokens)
    ? ast.tokens
    : require("esprima").tokenize(sourceWithoutTabs, {
      loc: true,
    });

  // We will reattach the tokens array to the file object below.
  delete ast.tokens;

  // Make sure every token has a token.value string.
  tokens.forEach(function(token) {
    if (typeof token.value !== "string") {
      token.value = lines.sliceString(token.loc.start, token.loc.end);
    }
  });

  if (Array.isArray(ast.comments)) {
    comments = ast.comments;
    delete ast.comments;
  }

  if (ast.loc) {
    // If the source was empty, some parsers give loc.{start,end}.line
    // values of 0, instead of the minimum of 1.
    util.fixFaultyLocations(ast, lines);
  } else {
    ast.loc = {
      start: lines.firstPos(),
      end: lines.lastPos(),
    };
  }

  ast.loc.lines = lines;
  ast.loc.indent = 0;

  let file;
  let program;
  if (ast.type === "Program") {
    program = ast;
    // In order to ensure we reprint leading and trailing program
    // comments, wrap the original Program node with a File node. Only
    // ESTree parsers (Acorn and Esprima) return a Program as the root AST
    // node. Most other (Babylon-like) parsers return a File.
    file = b.file(ast, options.sourceFileName || null);
    file.loc = {
      start: lines.firstPos(),
      end: lines.lastPos(),
      lines: lines,
      indent: 0,
    } as any;
  } else if (ast.type === "File") {
    file = ast;
    program = file.program;
  }

  // Expose file.tokens unless the caller passed false for options.tokens.
  if (options.tokens) {
    file.tokens = tokens;
  }

  // Expand the Program's .loc to include all comments (not just those
  // attached to the Program node, as its children may have comments as
  // well), since sometimes program.loc.{start,end} will coincide with the
  // .loc.{start,end} of the first and last *statements*, mistakenly
  // excluding comments that fall outside that region.
  const trueProgramLoc: any = util.getTrueLoc(
    {
      type: program.type,
      loc: program.loc,
      body: [],
      comments,
    },
    lines,
  );
  program.loc.start = trueProgramLoc.start;
  program.loc.end = trueProgramLoc.end;

  // Passing file.program here instead of just file means that initial
  // comments will be attached to program.body[0] instead of program.
  attachComments(comments, program.body.length ? file.program : file, lines);

  // Return a copy of the original AST so that any changes made may be
  // compared to the original.
  return new TreeCopier(lines, tokens).copy(file);
}

interface TreeCopierType {
  lines: any;
  tokens: any[];
  startTokenIndex: number;
  endTokenIndex: number;
  indent: number;
  seen: Map<any, any>;
  copy(node: any): any;
  findTokenRange(loc: any): any;
}

interface TreeCopierConstructor {
  new(lines: any, tokens: any): TreeCopierType;
}

const TreeCopier = function TreeCopier(
  this: TreeCopierType,
  lines: any,
  tokens: any,
) {
  assert.ok(this instanceof TreeCopier);
  this.lines = lines;
  this.tokens = tokens;
  this.startTokenIndex = 0;
  this.endTokenIndex = tokens.length;
  this.indent = 0;
  this.seen = new Map();
} as any as TreeCopierConstructor;

const TCp: TreeCopierType = TreeCopier.prototype;

TCp.copy = function(node) {
  if (this.seen.has(node)) {
    return this.seen.get(node);
  }

  if (isArray.check(node)) {
    const copy: any = new Array(node.length);
    this.seen.set(node, copy);
    node.forEach(function(this: any, item: any, i: any) {
      copy[i] = this.copy(item);
    }, this);
    return copy;
  }

  if (!isObject.check(node)) {
    return node;
  }

  util.fixFaultyLocations(node, this.lines);

  const copy: any = Object.create(Object.getPrototypeOf(node), {
    original: {
      // Provide a link from the copy to the original.
      value: node,
      configurable: false,
      enumerable: false,
      writable: true,
    },
  });

  this.seen.set(node, copy);

  const loc = node.loc;
  const oldIndent = this.indent;
  let newIndent = oldIndent;

  const oldStartTokenIndex = this.startTokenIndex;
  const oldEndTokenIndex = this.endTokenIndex;

  if (loc) {
    // When node is a comment, we set node.loc.indent to
    // node.loc.start.column so that, when/if we print the comment by
    // itself, we can strip that much whitespace from the left margin of
    // the comment. This only really matters for multiline Block comments,
    // but it doesn't hurt for Line comments.
    if (
      node.type === "Block" ||
      node.type === "Line" ||
      node.type === "CommentBlock" ||
      node.type === "CommentLine" ||
      this.lines.isPrecededOnlyByWhitespace(loc.start)
    ) {
      newIndent = this.indent = loc.start.column;
    }

    // Every node.loc has a reference to the original source lines as well
    // as a complete list of source tokens.
    loc.lines = this.lines;
    loc.tokens = this.tokens;
    loc.indent = newIndent;

    // Set loc.start.token and loc.end.token such that
    // loc.tokens.slice(loc.start.token, loc.end.token) returns a list of
    // all the tokens that make up this node.
    this.findTokenRange(loc);
  }

  const keys = Object.keys(node);
  const keyCount = keys.length;
  for (let i = 0; i < keyCount; ++i) {
    const key = keys[i];
    if (key === "loc") {
      copy[key] = node[key];
    } else if (key === "tokens" && node.type === "File") {
      // Preserve file.tokens (uncopied) in case client code cares about
      // it, even though Recast ignores it when reprinting.
      copy[key] = node[key];
    } else {
      copy[key] = this.copy(node[key]);
    }
  }

  this.indent = oldIndent;
  this.startTokenIndex = oldStartTokenIndex;
  this.endTokenIndex = oldEndTokenIndex;

  return copy;
};

TCp.findTokenRange = function(loc) {

  const sortedTokensByStart = SortedArray.unsafe<{ loc: { start: unknown; end: unknown } }>(loc.tokens, (a, b) => util.comparePos(a.loc.start, b.loc.start))
  const sortedTokensByEnd = SortedArray.unsafe<{ loc: { start: unknown; end: unknown } }>(loc.tokens, (a, b) => util.comparePos(a.loc.end, b.loc.end))

  // In the unlikely event that loc.tokens[this.startTokenIndex] starts
  // *after* loc.start, we need to rewind this.startTokenIndex first.
  {
    const { index } = sortedTokensByStart.searchLeft({ loc }, { inclusive: false, fromIndex: this.startTokenIndex }) ?? { index: this.startTokenIndex }
    this.startTokenIndex = index;
  }

  // In the unlikely event that loc.tokens[this.endTokenIndex - 1] ends
  // *before* loc.end, we need to fast-forward this.endTokenIndex first.
  {
    const { index } = sortedTokensByEnd.searchRight({ loc }, { inclusive: true, fromIndex: this.endTokenIndex }) ?? { index: this.endTokenIndex }
    this.endTokenIndex = index;
  }

  // Increment this.startTokenIndex until we've found the first token
  // contained by this node.
  {
    const { index } = sortedTokensByStart.search({ loc }, { inclusive: true, range: { left: this.startTokenIndex, right: this.endTokenIndex + 1 }, direction: Direction.Right }) ?? { index: this.startTokenIndex }
    this.startTokenIndex = index;
  }

  // Index into loc.tokens of the first token within this node.
  loc.start.token = this.startTokenIndex;

  // Decrement this.endTokenIndex until we've found the first token after
  // this node (not contained by the node).
  {
    const { index } = sortedTokensByEnd.search({ loc }, { inclusive: true, range: { left: this.startTokenIndex, right: this.endTokenIndex }, direction: Direction.Left }) ?? { index: this.endTokenIndex - 1 }
    this.endTokenIndex = index + 1;
  }

  // Index into loc.tokens of the first token *after* this node.
  // If loc.start.token === loc.end.token, the node contains no tokens,
  // and the index is that of the next token following this node.
  loc.end.token = this.endTokenIndex;

};

// Export the TreeCopier for testing purposes.
export default {
  [Symbol.for('TreeCopier')]: TreeCopier
}
