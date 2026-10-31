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

## Arithmetic decisions reviewed at revision cd0f96e

These are scoped source/call-graph proofs and open obligations, not adjustments
to the raw denominator. The checkpoint in `bc-coverage-results.md` retains both
broad and application mappings. Revisit these entries when traits, public limits,
callers, multiplication dispatch or extension configurations change.

| Exact source / decision | Evidence and constraint | Classification |
| --- | --- | --- |
| `number.hh:694`, quotient-estimate equality or second-limb correction | Width-parametric exact quotient/remainder fixtures have both recorded MC/DC conditions covered separately in Traits8/16/32, ON/OFF. | Feasible and measured; the algebraic oracle remains independent of the branch evidence. |
| `number.hh:1302`, radix-power chunk limit and input scale limit | Short fractions stop at scale; exact 35-bit binary fractions exceed the largest native chunk and exercise the radix-bound exit. Both conditions are recorded covered at each width. | Feasible and measured without a huge input or floating-point tolerance. |
| `number.hh:376`, reverse-end and high-zero limb tests | Public `mul_pow10`/`mac` operations create high zeros and all-zero storage without private injection. Exact length tests exposed #266, fixed before this checkpoint; recorded pairs now cover both conditions. | Feasible, asserted and measured. Do not exempt unnormalized public-operation results as invalid storage. |
| `number.hh:1753` and `1760`, zero/sign comparison decisions | Native fixed-point comparison tables vary zero, sign, magnitude and the two scales independently. They exposed #263; the recorded conditions are now covered. | Feasible, asserted and measured. |
| `number.hh:247` and `321`, extension predicate combined with omitted integer output | ON makes the first condition false and the corresponding nonextension behavior inactive. OFF makes it true; whole and fractional outputs exercise the second condition. | Configuration constant/inactive scope; retain each binary's raw counts rather than combine builds to manufacture independence. |
| `number.hh:1269`, numeric-token constructor assertion | Digits are selected from `0` through `F`; the final less-than-16 condition cannot be false for a selected digit. POSIX bc leaves out-of-input-base digits undefined except its single-digit register assignment exception. The constructor accepts the project's single-digit convention; normal invalid source tokens still belong to lexer/parser diagnostics. | Valid-token/precondition review for assertion failure outcomes, not a blanket exemption for numeric input or conversion errors. |
| `number.hh:924` and `982`, private `for_each` pointer assertions | Each private helper explicitly requires non-null operand storage. Normal Number entry paths handle zero operands before calling it; the mutable helper initializes/detaches its destination. Raw mappings repeat template/macro obligations. | Candidate invariant-failure scope for those private helpers. Audit any new caller and direct digit API boundary separately; do not infer an adjusted count by summing repeated raw records. |
| `number.hh:1029`, empty private multiplication subranges | The public Number threshold contract is `[1, base_]`, default 1; the tuner and tests use positive thresholds. Entering Karatsuba therefore requires both lengths at least 2. Its split is at least 1 and strictly smaller than both lengths; recursive low/high ranges remain nonempty. The Number multiplication entry handles zero values first. | Defensive empty-range guard is infeasible through those supported callers. #273 was corrected after finding the existing precondition; setting zero is not a valid way to fill this gap. Retain the guard. |
| `number.hh:1132` and `1138–1141`, private half-sum loop and leftover LHS | Its only callers add a low half of length `half` to a high half of length `size-half`, with `half <= size/2`. Thus the RHS cannot end while LHS remains, and no leftover LHS iteration occurs. | Coupled range-length invariant; RHS termination cannot independently decide the first loop while LHS is live. Unequal full multiplication operands remain eligible and are tested. |
| `number.hh:1640–1645`, modulo scale-restoration fallback | For supported bounded scales, division gives scale `t`, multiplication gives `t + scale(rhs)`, and the corrected subtraction preserves the maximum operand scale, including zero. That maximum is the selected remainder scale. | Defensive fallback after #233's scale preservation; no private scale corruption to force it. Revisit when #184/#186 change size/scale domains. |

The remaining zero-count numeric lines are not automatically exempt. In
particular: native integer construction (#275), allocated-empty parity (#274),
positive fractional register/index conversion and overflow (#276), formatter
parse/debug/error contexts, output positions, strict digit conversion boundaries,
fraction splitting and practical small-trait power-scale folding remain eligible
or under test. The known wider length/scale work in #184/#186 is not claimed
implemented. No first eligible milestone or ultimate feasible-coverage completion
is asserted by this review.

## Fatal-state review after #281

The terminal diagnostic cache supersedes the earlier open parser-reuse review.
A fatal `parse()` result is re-emitted on later calls before grammar parsing;
`insert_error` still unwinds immediately from grammar code on the first error.
Thus a normally entered grammar method cannot observe `error_ == true` in a
subsequent loop iteration. This applies to the error predicates in
`parse_input_item`, `parse_semicolon_list`, `parse_statement_list`,
`parse_opt_define_list` and `parse_opt_argument_list`, and to the old error EOF
footer in `parse_program`. Keep their raw counts and defensive code; recovery
work in #185 must revisit this proof before changing the terminal-state model.
Continuation/separator/interactive outcomes remain feasible and eligible.

The loop cleanup itself remains feasible: unfinished nested constructs leave
contexts which the fatal emitter clears. Repeated public parses now assert the
same terminal diagnostic and one EOF, while independent nonfatal interactive
parses execute both input values normally. No private state is injected, no
boolean is rewritten to fill a denominator, and no adjusted percentage is
inferred from this control-flow proof.


## Arithmetic boundary follow-up at revision ce52323

The 184-entry ON/OFF checkpoint includes the conversion, parity, native
constructor, formatter, output-position, padded-fraction and bounded power-scale
regressions. Their formerly missing ordinary paths now execute; #274, #275,
#276, #281, #283 and #285 have fixes with independent assertions. Retain the
following exact residual constraints, rather than removing defensive code.

| Source decision / lines | Scope and proof | Classification |
| --- | --- | --- |
| `number.hh:345`, `digits_printed > break_at` after writing the radix point | `output_base10` normalizes the initial counter below `break_at`. Each emitted integer digit and the OFF leading zero wraps exactly when reaching that bound. Before the radix point the counter is therefore at most `break_at - 1`; adding one cannot exceed `break_at`. Boundary tests exercise both sides of every feasible neighboring counter. | Defensive overflow fallback after #283; retain its three raw zero-count body lines. |
| `number.hh:1359-1361`, Number fractional-conversion diagnostic | `BasicNumber::to_unsigned` requests truncation explicitly. `BasicDigits::to_unsigned(..., true)` skips fractional limbs and cannot return its fractional sentinel. The strict digit API still rejects nonzero fractions, with exact tests. | Defensive compatibility guard after #276, not an exempt positive-fraction input. Negative and overflow diagnostics remain ordinary tested paths. |
| `number.hh:249-250` and `335-340`, OFF leading integer zero | ON compiles the predicate false; OFF executes both leading-zero paths, including boundary wrapping. | Per-binary extension constant. Do not combine configurations to create a fictitious independence pair. |
| `number.hh:1157-1160`, half-sum leftover LHS | The earlier low/high-half range proof remains unchanged at the new line numbers. | Coupled private range invariant; retain raw counts. |
| `number.hh:1295-1297`, invalid numeric character abort | The constructor consumes numeric-token spellings from `0..F` and a radix point. An unrecognized character violates that token contract; lexer/parser rejection remains eligible and tested. | Invalid constructor precondition, separate from normal malformed source. |
| `number.hh:1665-1669`, modulo scale restoration | The earlier exact subtraction/scale proof still applies to supported bounded scales. | Defensive scale invariant; #184/#186 remain separate limits work. |

This table is evidence for individual obligations. It does not calculate an
eligible denominator, erase assertion mappings, or claim a raw 100% result.
