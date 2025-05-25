#pragma once

#include "ast.hpp"

int make_node(AST& ast, ASTNodeType type, Lexer& lexer);
bool expect_token(const TokenType& expected, Lexer& lexer);
bool expect_token(ASTNode& node, const TokenType& expected, Lexer& lexer);
bool not_end(Lexer& lexer, Logger& logger, AST& ast, int nodei, std::string context = "");