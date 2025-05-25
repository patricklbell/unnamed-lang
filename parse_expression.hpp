#pragma once

#include "ast.hpp"

void parse_expression_add_child(AST& ast, Lexer& lexer, Logger& logger, int parenti, int parent_precedence = -1, bool in_parenthesis = false, bool in_comma_list = false);