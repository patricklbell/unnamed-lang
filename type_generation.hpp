#pragma once

#include "typing.hpp"
#include "ast.hpp"
#include "logging.hpp"

#include <unordered_map>

void typegen(AST& ast, TypeInfo& types, Logger& logger);