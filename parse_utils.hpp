#pragma once

#include "ast.hpp"

int make_node(AST& ast, ASTNodeType type, Lexer& lexer);
bool expect_token(const TokenType& expected, Lexer& lexer);
bool expect_token(const TokenType& expected, Lexer& lexer, Logger& logger);
bool expect_token(ASTNode& node, const TokenType& expected, Lexer& lexer);
bool expect_token(ASTNode& node, const TokenType& expected, Lexer& lexer, Logger& logger, std::string message = "");
void absorb_and_consume(AST& ast, int nodei, Lexer& lexer);
name_id add_name(AST& ast, int nodei, Lexer& lexer);
name_id get_name(AST& ast, int nodei);
bool not_end(Lexer& lexer, Logger& logger, AST& ast, int nodei, std::string context = "");
bool not_end(Lexer& lexer, Logger& logger, std::string context = "");
bool not_matching(TokenType t, Lexer& lexer, Logger& logger, AST& ast, int nodei, std::string context = "");

std::string unescape_string(const std::string& input);