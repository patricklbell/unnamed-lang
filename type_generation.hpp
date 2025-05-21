#pragma once

#include "typing.hpp"
#include "ast.hpp"
#include <unordered_map>

void typegen(AST& ast, TypeInfo& types, Logger& logger);