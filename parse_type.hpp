#pragma once

#include "ast.hpp"

TypeExpression parse_type(AST& ast, Lexer& lexer, Logger& logger);

bool add_generic_parameter_list(TypeOrValueExpression& first, size_t& num, ASTNode* node, AST& ast, Lexer& lexer, Logger& logger);
bool match_generic_parameter_list(const Lexer& lexer);