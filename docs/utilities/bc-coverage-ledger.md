# bc coverage decisions and open obligations

This inventory accompanies #214 and #215. It records source conditions rather
than allowing a blanket exemption for a file, diagnostic, assertion or template.
No adjusted denominator is calculated here. The raw LLVM exports and summaries
remain authoritative for what the compiler actually instruments. Conditions
which LLVM folds or excludes are tool scope, not successful MC/DC tests.

The entries below identify the current function and expression. Recheck them
whenever the implementation, compiler, extension option or template changes.
Test intention alone is not evidence that LLVM observed an independence pair;
inspect the fresh per-decision MC/DC report as well as the asserted test outcome.

## Frontend decisions

| Source decision | Configuration / evidence | Status and obligation |
| --- | --- | --- |
| `Location` equality in `reader.cc`: file, line and column equality | Both extension modes; `Location saturation and comparison independence` holds two fields equal while changing the third, plus the all-equal case. | Feasible; asserted pairs for each of three conditions. |
| `FileReader::peek`: EOF and not end-of-file | Both modes; empty/normal files exercise EOF, and a directory is a controlled read/open error depending on platform. | Feasible on POSIX where a directory opens but cannot be read; preserve the Windows open-error distinction. |
| `Lexer::lex_number`: space or tab after escaped newline | ON only; ordinary following character, space, tab and mixed continuation fixtures. OFF bypasses the extension. | Feasible in ON; inspect both condition pairs. |
| `Lexer::lex`: extensions, first character and `#` shebang gate | ON/OFF; initial shebang, ordinary first character, delayed/indented shebang and non-shebang `#`. | Extension predicate is constant within a binary; do not combine ON/OFF profiles to claim an independence pair for it. The other conditions remain eligible in ON. |
| `Lexer::lex`: shebang loop not EOF and not newline | ON; unterminated shebang at EOF, newline-terminated shebang and nonempty body. | Feasible pairs; EOF and newline are distinct termination fixtures. |
| `Parser::ExprIndex` equality: missing type or same index | Both modes; explicit comparison matrix across all six types and two indices. | Feasible; missing indices compare equal independently of position, other same-type indices require equal position. |
| `Parser::ExprIndex` versus index: not missing and same index | Both modes; same comparison matrix. | Feasible; hold index equal while changing missing/type, and hold nonmissing type while changing index. |
| `Parser::parse_input_item`: not interactive and not error | Both modes; interactive stepping and batch CLI tests. | Interactive condition is feasible. Review the error condition against `insert_error` unwinding; parser reuse after a fatal error belongs to the #185 recovery contract. No error-condition exemption is applied here. |
| `Parser::parse_semicolon_list`, `parse_statement_list`, `parse_opt_define_list`, `parse_opt_argument_list`: continuation and not error | Both modes; separators, parameter/argument lists and malformed inputs. | Continuation outcomes are feasible. `insert_error` throws to `parse` before a first-parse loop can observe error=true; review any proposed exemption with the #185 reuse contract, not just current counts. |
| `Parser::parse_statement_list`: semicolon or newline | Both modes; empty and nonempty blocks, semicolon and newline separators, closing brace. | Feasible three-vector short-circuit set. |
| `Parser::parse_return_statement`: found opening parenthesis or optional parentheses | Both modes; bare return, empty parentheses, valued return; ON also permits an unparenthesized value. | Optional-parentheses condition is constant in one binary; inspect LLVM's folded-condition scope separately. Parenthesis outcomes are feasible when syntax requires them. |
| `Parser::parse_opt_auto_define_list`: neither semicolon nor newline | Both modes; semicolon/newline auto declarations and malformed `auto a 1`. | Feasible three-vector short-circuit set. Malformed input must exit 1, not abort while formatting a diagnostic. |
| `Parser::parse_opt_primary_expression`: missing element and slices disabled | Both modes; indexed array, array argument, forbidden standalone empty subscript. | Feasible three-vector set; valid array-argument execution also requires #241. |
| `Parser::ensure_expr_loaded`: named or array slice | Both modes; scalar expressions and array arguments; direct instruction tests in #215 supplement parse shape. | Feasible; ordinary unnamed values supply the false result. |

## Invariant and public-boundary review

| Exact source / condition | Evidence and rationale | Classification |
| --- | --- | --- |
| `VariableMask::for_each_internal` in `bc.hh`: `letter != letters.end()` while `mask != 0` | Public constructors/add operations accept only the 26 encoded letters and set bits 0..25. After 26 shifts a valid mask is zero. An end iterator with a nonzero mask therefore requires invalid bits that the public API cannot create. Empty, last-letter and all-letter masks remain directly testable. | Candidate infeasible independence pair for the iterator condition under the valid-mask invariant; keep raw counts and review under #215. Do not inject high bits or change the invariant. |
| `Token::type` generic visitor fallback | The variant has exactly the five alternatives handled by explicit visitors. A new variant alternative would change this proof. | Infeasible for a valid present variant; retain the defensive code and raw mapping. |
| Token constructor assertions requiring payload-compatible type | Every valid token kind/payload is exercised explicitly. Passing a number/string/error/letter kind to the wrong constructor breaks its documented precondition. | Assertion-failure outcomes are invariant/death-test scope, not ordinary malformed source. Malformed source still must produce a valid error token. |
| `Letter::encode` assertion for an unknown letter | Public semantic inputs are the 26 bc letters; the checked constructors and valid-mask tests enumerate them without assuming contiguous character codes. | Invalid-constructor precondition scope; normal lexer rejection of other input stays eligible. |
| Parser `insert_load`/`insert_store` assertions on expression kind | Valid frontend expressions and array parameters exercise their accepted kinds. | Review individual assertion arms; no blanket parser/assert exclusion. |
| VM `assert_error` paths | These are active runtime checks, including result type checks. Valid array parameters already exposed incorrect checks (#241). | Eligible until an individual verifier/path proof establishes otherwise; #187/#215 track the stream contract. |
| Location saturation | Direct constructors reach maximum line/column values without enormous input files. | Feasible and asserted, never exempt for impractical natural file size. |
| Numeric continuation and delimiter diagnostics | Malformed continuations (#240) and missing builtin/function delimiters (#242) exposed real bugs. | Feasible normal error handling, not invariant scope. |

## Remaining measurements

Keep ON and OFF reports separate, and publish both the broad native/test-trait
header scope and the application-mappings view. Current arithmetic (#199) and VM,
formatting, interrupt and verifier (#215/#187) obligations remain open. Add exact
per-decision feasibility evidence as those tests land; this inventory does not
claim the first 95%/90%/90% eligible milestone or ultimate feasible coverage goal.

## Exact frontend gaps reviewed at revision 7794ec8

These entries supplement the decision inventory with the observed line/branch
locations. They are not automatic exclusions and do not subtract from LLVM's
raw totals.

| Source location | Evidence / constraint | Review outcome |
| --- | --- | --- |
| `bc.cc:66`, `!instructions` | `Parser::parse` creates its shared instruction vector at entry and returns that vector on every normal return. Allocation failure throws; it cannot return a null vector. | Null-result arm is a defensive future-contract guard in the current application. The VM continuation condition remains feasible and tested. |
| `bc.cc:97`, getopt `':'` result | Current option strings are `:glq` and `:l`; no option takes an argument. A missing-required-argument result is therefore unavailable. Unknown-option handling still executes and is asserted. | Current option-schema constraint; revisit if options gain arguments. |
| `bc.cc` `case 'g'` and `case 'q'`, OFF | The OFF option string does not recognize either option; the switch cases remain mapped by LLVM. OFF tests assert their rejection. | Inactive extension behavior; keep raw mapping and report OFF separately. |
| `lexer.cc:38-40`, absent token after `lex()` | Every normal return from private `lex` creates a token, including EOF and error cases; comment/continuation paths continue until a token or throw. An exception does not reach this check. | Defensive invariant-failure arm. The preceding chew-before-peek call is feasible and now asserted. |
| `parser.cc:534-536`, `update_loop_exits` fallback | The only recorded exit instructions are the branch inserted for `break` and the branch-zero inserted by loop parsing. The fallback requires corruption of the private exit list or instruction vector. | Internal branch-kind invariant; keep checks and raw mapping. |
| `parser.cc:547-549`, nested `parse_function` call | Only `parse_input_item` calls private `parse_function`. Function bodies use statement parsing, where another `define` is rejected before calling it. Each public `parse` resets function context. | Defensive nesting guard; malformed nested definitions remain ordinary eligible diagnostics. |
| `parser.cc:589-591`, required function newline, ON | The error is guarded by `!extensions_enabled()`. ON accepts an omitted newline; OFF explicitly tests rejection. | Constant extension configuration, not a missed ON input. |
| `parser.cc:763-764`, relational default | The preceding `is_rel_op` gate admits exactly the six handled relational token kinds; all six comparisons are asserted. | Token-class invariant; preserve fallback. |
| `parser.cc:834-835`, compound-assignment default | The preceding assignment classification admits the seven assignment kinds; plain assignment is handled outside this switch and the other six inside it. | Token-class invariant; preserve fallback. |
| `parser.cc:891-893`, no-argument `parse_add_expression` | This private overload has a declaration and definition but no call sites; the active path uses the overload taking an already parsed left operand. | Uncalled implementation path; retain raw mapping rather than remove code to change the denominator. |
| `parser.cc:911-912` and `942-943`, add/multiply defaults | The loop gates admit exactly the operators handled by their switches. The explicit precedence/assignment tests assert the admitted operators. | Token-class invariants; do not inject an invalid token into private parser state. |
| `parser.cc:169-170`, loop-stack cleanup, and `206-207`, persistent error | A fatal parse can leave these bookkeeping paths for a subsequent public `parse` call. The CLI executes the fatal diagnostic and exits first. | Parser-reuse/recovery contract is still an open #185 review; do not exempt merely because normal CLI execution terminates earlier. |
| `parser.cc:288`, closing brace after unconditional return | The string-statement arm returns on its preceding line. The brace has a zero-count mapping although it is not a separately executable semantic operation. | Source-mapping artifact; retain raw count and distinguish it from an untested statement. |

The mask iterator guard deserves the same distinction: an end iterator with a
nonzero valid 26-bit mask is impossible, but LLVM may still count a masking pair
from `{false, unevaluated}` and `{true, true}`. Last-letter variable and array
parameters assert real end-iterator traversal and local restoration. Preserve
LLVM's recorded pairs; they do not demonstrate construction of an impossible
end/nonzero concrete state. The directly callable empty formatter parse-context
boundary remains feasible and is covered separately by #215.
