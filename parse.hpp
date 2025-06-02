#pragma once

#include "ast.hpp"
#include "lexer.hpp"
#include "logging.hpp"

void parse_module_ast(AST& ast, std::string name, Lexer& lexer, Logger& logger);