#include "type_generation.hpp"
#include "ast.hpp"

static Type* resolve_type(TypeInfo& types, std::string type_name, const TextSpan& span, Logger& logger) {
  {
    auto lu = types.global_types.find(type_name);
    if (lu != types.global_types.end())
      return lu->second;
  }
  
  logger.log(Errors::Type, "Unresolved type \"" + type_name + "\".", span);
  return nullptr;
}

static Type* resolve_variable_type(const std::vector<ScopeTypeInfo*>& scopes, std::string name, const TextSpan& span, Logger& logger) {
  for (auto& scope : scopes) {
    auto lu = scope->locals.find(name);
    if (lu != scope->locals.end())
      return lu->second;
  }
  
  logger.log(Errors::Type, "Undefined variable \"" + name + "\".", span);
  return nullptr;
}

// @todo overloading
static FunctionType* resolve_function_type(TypeInfo& types, std::string name, const TextSpan& span, Logger& logger) {
  {
    auto lu = types.functions.find(name);
    if (lu != types.functions.end())
      return &lu->second;
  }
  
  logger.log(Errors::Type, "Undefined function \"" + name + "\".", span);
  return nullptr;
}

// @note assumes function defintions and prototypes are all top level
static void register_functions_in_module(AST& ast, int modulei, TypeInfo& types, Logger& logger) {
  for (auto& node : ASTChildrenIterator(ast, modulei)) {
    ASTPrototypeData* prototype_data = nullptr;

    if (node.type == ASTNodeType::FunctionDefinition) {
      static_assert((int)ASTFunctionDefinitionData::Children::Prototype == 1);
      ASTNode& prototype = ast.nodes[node.id + 1];
      prototype_data = prototype.cast_data<ASTPrototypeData>();
    } else if (node.type == ASTNodeType::Prototype) {
      prototype_data = node.cast_data<ASTPrototypeData>();
    }

    if (prototype_data == nullptr)
      continue;

    FunctionType func_type {
      .name = prototype_data->name,
      .return_type = resolve_type(types, prototype_data->return_type, node.span, logger),
    };
    for (auto& arg_prototype : prototype_data->args) {
      FunctionArgumentType arg {
        .name = arg_prototype.name,
        .type = resolve_type(types, arg_prototype.type, node.span, logger),
      };
      func_type.args.emplace_back(std::move(arg));
    }
    types.functions.emplace(prototype_data->name, std::move(func_type));
  }
}

static ScopeTypeInfo* make_scope(TypeInfo& types, int node_id) {
  ScopeTypeInfo new_scope {
    .node_id = node_id,
  };
  types.scopes.emplace(node_id, std::move(new_scope));
  return &types.scopes[node_id];
}

static void check_parameters_match_function(FunctionType* ft, const std::vector<Type*>& parameters, const TextSpan& span, Logger& logger) {
  if (ft->args.size() != parameters.size()) {
    logger.log(Errors::Type, "Number of parameters does not match function definition.", span);
    return;
  }

  // @todo span for specific parameter
  for (int i = 0; i < parameters.size(); ++i) {
    if (parameters[i] != ft->args[i].type)
      logger.log(Errors::Type, "Parameter " + std::to_string(i+1) + "'s type does not match function definition.", span);
  }
}

static Type* infer_expression_type(AST& ast, ASTNode& node, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger);

static void add_subexpression_types(std::vector<Type*>& args, AST& ast, ASTNode& node, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger) {
  for (auto& child : ASTChildrenIterator(ast, node.id)) {
    args.push_back(infer_expression_type(ast, child, types, scopes, logger));
  }
}

static Type* infer_expression_type_impl(AST& ast, ASTNode& node, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger) {
  switch (node.type)
  {
  case ASTNodeType::IntLiteral:
  {
    return &types.int_type;
  }
  case ASTNodeType::FloatLiteral:
  {
    return &types.float_type;
  }
  case ASTNodeType::Variable:
  {
    ASTVariableData* data = node.cast_data<ASTVariableData>();
    return resolve_variable_type(scopes, data->name, node.span, logger);
  }
  case ASTNodeType::Call:
  {
    ASTCallData* data = node.cast_data<ASTCallData>();
    FunctionType* ft = resolve_function_type(types, data->name, node.span, logger);

    if (ft == nullptr)
      return nullptr;

    std::vector<Type*> args;
    add_subexpression_types(args, ast, node, types, scopes, logger);
    check_parameters_match_function(ft, args, node.span, logger);

    return ft->return_type;
  }
  case ASTNodeType::UnaryOperator:
  {
    std::vector<Type*> args;
    add_subexpression_types(args, ast, node, types, scopes, logger);

    LANG_ASSERT((int)ASTUnaryOperatorData::Children::Expression == 1);
    return args[0];
  }
  case ASTNodeType::BinaryOperator:
  {
    std::vector<Type*> args;
    add_subexpression_types(args, ast, node, types, scopes, logger);

    // implicit casting is not allowed
    // @todo eg. division
    LANG_ASSERT((int)ASTBinaryOperatorData::Children::RHS == 2);
    if (args[0] != args[1])
      logger.log(Errors::Type, "Operator types do not match.", node.span);

    return args[0];
  }
  default:
    LANG_ASSERT(false, "Unhandled expression type encountered during type inferrence");
    break;
  }

  return nullptr;
}

static Type* infer_expression_type(AST& ast, ASTNode& node, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger) {
  ASTExpressionData* data = node.cast_data<ASTExpressionData>();
  Type* t = infer_expression_type_impl(ast, node, types, scopes, logger);
  data->resolved_type = t;
  return t;
}

static void infer_variable_type(AST& ast, ASTNode& variable, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger) {
  LANG_ASSERT(variable.type == ASTNodeType::VariableDefinition);
  ASTVariableDefinitionData* variable_data = variable.cast_data<ASTVariableDefinitionData>();
  
  auto scope = scopes.back();

  static_assert((int)ASTVariableDefinitionData::Children::Expression == 1);

  Type* inferred_t = infer_expression_type(ast, ast.nodes[variable.id + 1], types, scopes, logger);
  scope->locals[variable_data->name] = inferred_t;

  Type* conditioned_t = inferred_t;
  if (!variable_data->type.empty()) {
    conditioned_t = resolve_type(types, variable_data->type, variable.span, logger);
  }

  if (conditioned_t != inferred_t) {
    if (conditioned_t && inferred_t) {
      logger.log(Errors::Type, "Cannot assign " + inferred_t->name + " to " + conditioned_t->name, variable.span);
    }
  }

  variable_data->resolved_type = conditioned_t;
}

static void infer_local_types(AST& ast, TypeInfo& types, Logger& logger) {
  std::vector<ScopeTypeInfo*> scopes;

  for (int node_id = 0; node_id < ast.nodes.size();) {
    auto& node = ast.nodes[node_id];

    switch (node.type)
    {
    case ASTNodeType::Prototype:
    {
      ScopeTypeInfo* prototype_scope = make_scope(types, node_id);
      
      ASTPrototypeData* prototype_data = node.cast_data<ASTPrototypeData>();
      
      FunctionType* ft = resolve_function_type(types, prototype_data->name, node.span, logger);

      if (ft != nullptr) {
        prototype_data->resolved_return_type = ft->return_type;
  
        LANG_ASSERT(ft->args.size() == prototype_data->args.size());
        for (int i = 0; i < ft->args.size(); ++i) {
          auto& arg = ft->args[i];
          auto& proto_arg = prototype_data->args[i];
  
          prototype_scope->locals.emplace(arg.name, arg.type);
          proto_arg.resolved_type = arg.type;
        }
      }

      scopes.push_back(prototype_scope);
      break;
    }
    case ASTNodeType::Block:
    {
      scopes.push_back(make_scope(types, node_id));
      break;
    }
    case ASTNodeType::VariableDefinition:
    {
      if (scopes.empty()) {
        logger.log(Errors::Syntax, "Variable definition outside a block.", node.span);
      } else {
        infer_variable_type(ast, node, types, scopes, logger);
      }

      // @note skips recursing into expressions
      node_id += node.size;
      continue;
    }
    case ASTNodeType::IntLiteral:
    case ASTNodeType::FloatLiteral:
    case ASTNodeType::Variable:
    case ASTNodeType::Call:
    case ASTNodeType::UnaryOperator:
    case ASTNodeType::BinaryOperator:
    {
      // checks that expression is consistent (can be inferred)
      infer_expression_type(ast, node, types, scopes, logger);

      // @note skips recursing into expressions
      node_id += node.size;
      continue;
    }
    default:
      break;
    }

    // depth first search, top to bottom
    node_id += 1;
  }
}

void typegen(AST& ast, TypeInfo& types, Logger& logger) {
  for (int modulei = 0; modulei < ast.nodes.size();) {
    auto& module = ast.nodes[modulei];

    LANG_ASSERT(module.type == ASTNodeType::Module, "expected only modules at top-level");
    ASTModuleData* module_data = module.cast_data<ASTModuleData>();
  
    register_functions_in_module(ast, module.id, types, logger);

    modulei += module.size;
  }

  infer_local_types(ast, types, logger);
}

TypeInfo::TypeInfo() {
  this->int_type.name = "int";
  this->int_type.form = Type::Form::Int;

  this->float_type.name = "float";
  this->float_type.form = Type::Form::Float;
  
  this->global_types[int_type.name]   = &this->int_type;
  this->global_types[float_type.name] = &this->float_type;
}