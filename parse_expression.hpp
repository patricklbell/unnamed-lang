#pragma once

#include "ast.hpp"

struct ExpressionParsingContext {
    bool in_parenthesis = false;
    bool in_comma_list = false;
    bool in_square = false;
};

void parse_expression_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti, ExpressionParsingContext settings = ExpressionParsingContext{}, int parent_precedence = -1);

bool match_compile_time_directive(const Lexer& lexer);