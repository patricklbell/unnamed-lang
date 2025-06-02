#pragma once

#include "ast.hpp"

int parse_type(AST& ast, Lexer& lexer, Logger& logger);

int parse_generic_parameter_list(AST& ast, Lexer& lexer, Logger& logger);
bool match_generic_parameter_list(const Lexer& lexer);