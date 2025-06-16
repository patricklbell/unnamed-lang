#pragma once

#include "ast.hpp"

bool expect_token(const TokenType& expected, Lexer& lexer);
bool expect_token(const TokenType& expected, Lexer& lexer, Logger& logger);
bool expect_token(ASTNode* node, const TokenType& expected, Lexer& lexer);
bool expect_token(ASTNode* node, const TokenType& expected, Lexer& lexer, Logger& logger, std::string message = "");
void absorb_and_consume(ASTNode* node, Lexer& lexer);
name_id get_next_name(AST& ast, Lexer& lexer);
bool not_end(Lexer& lexer, Logger& logger, ASTNode* node);
bool not_end(Lexer& lexer, Logger& logger, ASTNode* node);
bool not_matching(TokenType t, Lexer& lexer, Logger& logger, ASTNode* node);
bool not_matching(TokenType t, Lexer& lexer, Logger& logger);

std::string unescape_string(const std::string& input);