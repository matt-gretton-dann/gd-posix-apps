/** \subsection Accepted grammar:
 *
 * ```
 * program              : EOF
 *                      | input_item program
 *
 * input_item           : semicolon_list NEWLINE
 *                      | function
 *
 * semicolon_list       : // empty
 *                      | statement
 *                      | semicolon_list ';' statement
 *                      | semicolon_list ';'
 *
 * statement_list       : // empty
 *                      | statement
 *                      | statement_list NEWLINE
 *                      | statement_list NEWLINE statement
 *                      | statement_list ';'
 *                      | statement_list ';' statement
 *
 * statement            : expression
 *                      | STRING
 *                      | Break
 *                      | Quit
 *                      | Return
 *                      | Return '(' return_expression ')'
 *                      | For '(' expression ';' relational_expression ';' expression ')' statement
 *                      | If '(' relational_expression ')' statement
 *                      | While '(' relational_expression ')' statement
 *                      | '{' statement_list '}'
 *
 * function             : Define LETTER '(' opt_parameter_list ')' '{' NEWLINE opt_auto_define_list
 *                        statement_list '}'
 *
 * opt_parameter_list   : // empty
 *                      | parameter_list
 *
 * parameter_list       : LETTER
 *                      | define_list ',' LETTER
 *
 * opt_auto_define_list : // empty
 *                      | Auto define_list NEWLINE
 *                      | Auto define_list ';'
 *
 * define_list          : LETTER
 *                      | LETTER '[' ']'
 *                      | define_list ',' LETTER
 *                      | define_list ',' LETTER '[' ']'
 *
 * opt_argument_list    : // empty
 *                      | argument_list
 *
 * argument_list        : expression
 *                      | LETTER '[' ']' ',' argument_list
 *
 * relational_expression : expression
 *                      | expression REL_OP expression
 *
 * return_expression    : // empty
 *                      | expression
 *
 * expression           : assign_expression
 *
 * incr_decr_expression : INCR_DECR named_expression
 *                      | named_expression INCR_DECR
 *                      | primary_expression
 *
 * unary_minus_expr     : '-' incr_decr_expression
 *                      : incr_decr_expression
 *
 * power_expression     : unary_minus_expression '^' power_expression
 *                      | unary_minus_expression
 *
 * mul_expression       : mul_expression MUL_OP power_expression
 *                      | power_expression
 *
 * add_expression       : mul_expression ADD_OP add_expression
 *                      | mul_expression
 *
 * assign_expression    : named_expression ASSIGN_OP assign_expression
 *                      | add_expression
 *
 * primary_expression   : named_expression
 *                      | NUMBER
 *                      | '(' expression ')'
 *                      | LETTER '(' opt_argument_list ')'
 *                      | Length '(' expression ')'
 *                      | Sqrt '(' expression ')'
 *                      | Scale '(' expression ')'
 *
 * named_expression     : LETTER
 *                      | LETTER '[' expression ']'
 *                      | Scale
 *                      | Ibase
 *                      | Obase
 */
