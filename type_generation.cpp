#include "type_generation.hpp"
#include "ast.hpp"

// static void register_types_in_module(AST& ast, int modulei, TypeInfo& types, Logger& logger);
// static void infer_local_types(AST& ast, TypeInfo& types, Logger& logger);

void typegen(AST& ast, TypeInfo& types, Logger& logger) {
  // for (int modulei = 0; modulei < ast.nodes.size();) {
  //   auto& module = ast.nodes[modulei];
  //   LANG_ASSERT(module.type == ASTNodeType::Module, "expected only modules at root");
  //   modulei += module.size;
  
  //   register_types_in_module(ast, module.id, types, logger);
  // }

  // infer_local_types(ast, types, logger);
}


// static Type* resolve_type(TypeInfo& types, std::string type_name, const TextSpan& span, Logger& logger) {
//   {
//     auto lu = types.global_types.find(type_name);
//     if (lu != types.global_types.end())
//       return lu->second;
//   }
  
//   logger.log(Errors::Type, "Unresolved type \"" + type_name + "\".", span);
//   return nullptr;
// }

// static Type* find_variable_type_from_scopes(const std::vector<ScopeTypeInfo*>& scopes, std::string name) {
//   for (auto& scope : scopes) {
//     auto lu = scope->locals.find(name);
//     if (lu != scope->locals.end())
//       return lu->second;
//   }

//   return nullptr;
// }

// static Type* resolve_variable_type(const std::vector<ScopeTypeInfo*>& scopes, std::string name, const TextSpan& span, Logger& logger) {
//   Type* t = find_variable_type_from_scopes(scopes, name);
//   if (t == nullptr)
//     logger.log(Errors::Type, "Undefined variable \"" + name + "\".", span);

//   return t;
// }

// // @todo overloading
// static FunctionType* resolve_function_type(TypeInfo& types, std::string name, const TextSpan& span, Logger& logger) {
//   {
//     auto lu = types.functions.find(name);
//     if (lu != types.functions.end())
//       return &lu->second;
//   }
  
//   logger.log(Errors::Type, "Undefined function \"" + name + "\".", span);
//   return nullptr;
// }

// // @note assumes function defintions and prototypes are all top level
// static void register_types_in_module(AST& ast, int modulei, TypeInfo& types, Logger& logger) {
//   for (auto& node : ASTChildrenIterator(ast, modulei)) {
//     ASTPrototypeData* prototype_data = nullptr;

//     if (node.type == ASTNodeType::FunctionDefinition) {
//       static_assert((int)ASTFunctionDefinitionData::Children::Prototype == 1);
//       ASTNode& prototype = ast.nodes[node.id + 1];
//       prototype_data = prototype.cast_data<ASTPrototypeData>();
//     } else if (node.type == ASTNodeType::Prototype) {
//       prototype_data = node.cast_data<ASTPrototypeData>();
//     }

//     if (prototype_data == nullptr)
//       continue;

//     FunctionType func_type {
//       .name = prototype_data->name,
//       .return_type = resolve_type(types, prototype_data->return_type, node.span, logger),
//     };
//     prototype_data->resolved_return_type = func_type.return_type;

//     for (auto& arg_prototype : prototype_data->args) {
//       FunctionArgumentType arg {
//         .name = arg_prototype.name,
//         .type = resolve_type(types, arg_prototype.type, node.span, logger),
//       };
//       arg_prototype.resolved_type = arg.type;
//       func_type.args.emplace_back(std::move(arg));
//     }
//     types.functions.emplace(prototype_data->name, std::move(func_type));
//   }
// }

// static ScopeTypeInfo* make_scope(TypeInfo& types, int node_id) {
//   ScopeTypeInfo new_scope {
//     .node_id = node_id,
//   };
//   types.scopes.emplace(node_id, std::move(new_scope));
//   return &types.scopes[node_id];
// }

// static void check_parameters_match_function(FunctionType* ft, const std::vector<Type*>& parameters, const TextSpan& span, Logger& logger) {
//   if (ft->args.size() != parameters.size()) {
//     logger.log(Errors::Type, "Number of parameters does not match function definition.", span);
//     return;
//   }

//   // @todo span for specific parameter
//   for (int i = 0; i < parameters.size(); ++i) {
//     if (parameters[i] != ft->args[i].type)
//       logger.log(Errors::Type, "Parameter " + std::to_string(i+1) + "'s type does not match function definition.", span);
//   }
// }

// static Type* infer_expression_type(AST& ast, ASTNode& node, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger);

// static void add_subexpression_types(std::vector<Type*>& args, AST& ast, ASTNode& node, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger) {
//   for (auto& child : ASTChildrenIterator(ast, node.id)) {
//     args.push_back(infer_expression_type(ast, child, types, scopes, logger));
//   }
// }

// static Type* infer_expression_type_impl(AST& ast, ASTNode& node, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger) {
//   switch (node.type)
//   {
//   case ASTNodeType::IntLiteral:
//   {
//     return &types.int_type;
//   }
//   case ASTNodeType::FloatLiteral:
//   {
//     return &types.float_type;
//   }
//   case ASTNodeType::Variable:
//   {
//     ASTVariableData* data = node.cast_data<ASTVariableData>();
//     return resolve_variable_type(scopes, data->name, node.span, logger);
//   }
//   case ASTNodeType::Call:
//   {
//     ASTCallData* data = node.cast_data<ASTCallData>();
//     FunctionType* ft = resolve_function_type(types, data->name, node.span, logger);

//     if (ft == nullptr)
//       return nullptr;

//     std::vector<Type*> args;
//     add_subexpression_types(args, ast, node, types, scopes, logger);
//     check_parameters_match_function(ft, args, node.span, logger);

//     return ft->return_type;
//   }
//   case ASTNodeType::UnaryOperator:
//   {
//     std::vector<Type*> args;
//     add_subexpression_types(args, ast, node, types, scopes, logger);

//     LANG_ASSERT((int)ASTUnaryOperatorData::Children::Expression == 1);
//     return args[0];
//   }
//   case ASTNodeType::BinaryOperator:
//   {
//     std::vector<Type*> args;
//     add_subexpression_types(args, ast, node, types, scopes, logger);

//     // implicit casting is not allowed
//     // @todo eg. division
//     LANG_ASSERT((int)ASTBinaryOperatorData::Children::RHS == 2);
//     if (args[0] != args[1])
//       logger.log(Errors::Type, "Operator types do not match.", node.span);

//     return args[0];
//   }
//   default:
//     LANG_ASSERT(false, "Unhandled expression type encountered during type inferrence");
//     break;
//   }

//   return nullptr;
// }

// static Type* infer_expression_type(AST& ast, ASTNode& node, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger) {
//   ASTExpressionData* data = node.cast_data<ASTExpressionData>();
//   Type* t = infer_expression_type_impl(ast, node, types, scopes, logger);
//   data->resolved_type = t;
//   return t;
// }

// static void infer_variable_type(AST& ast, ASTNode& variable, TypeInfo& types, const std::vector<ScopeTypeInfo*>& scopes, Logger& logger) {
//   LANG_ASSERT(variable.type == ASTNodeType::VariableDeclaration);
//   ASTVariableDeclarationData* variable_data = variable.cast_data<ASTVariableDeclarationData>();
  
//   auto scope = scopes.back();

//   static_assert((int)ASTVariableDeclarationData::Children::Expression == 1);

//   Type* inferred_t = infer_expression_type(ast, ast.nodes[variable.id + 1], types, scopes, logger);
//   scope->locals[variable_data->name] = inferred_t;

//   Type* conditioned_t = inferred_t;
//   if (!variable_data->type.empty()) {
//     conditioned_t = resolve_type(types, variable_data->type, variable.span, logger);
//   }

//   if (conditioned_t != inferred_t) {
//     if (conditioned_t && inferred_t) {
//       logger.log(Errors::Type, "Cannot assign " + inferred_t->name + " to " + conditioned_t->name, variable.span);
//     }
//   }

//   variable_data->resolved_type = conditioned_t;
// }

// static void infer_local_types_impl(AST& ast, int start_node_id, int size, TypeInfo& types, std::vector<ScopeTypeInfo*> &scopes, Logger& logger) {
//   for (int node_id = start_node_id; node_id < start_node_id + size;) {
//     auto& node = ast.nodes[node_id];

//     switch (node.type)
//     {
//     case ASTNodeType::FunctionDefinition:
//     {
//       auto& prototype = ast.nodes[node.id + 1];
//       static_assert((int)ASTFunctionDefinitionData::Children::Prototype == 1);
//       LANG_ASSERT(prototype.type == ASTNodeType::Prototype);
//       ASTPrototypeData* prototype_data = prototype.cast_data<ASTPrototypeData>();

//       auto& body = ast.nodes[prototype.id + prototype.size];
//       static_assert((int)ASTFunctionDefinitionData::Children::Body == 2);
//       LANG_ASSERT(body.type == ASTNodeType::Block);

//       ScopeTypeInfo* function_scope = make_scope(types, body.id);

//       const FunctionType* ft = resolve_function_type(types, prototype_data->name, node.span, logger);
//       if (ft != nullptr) {
//         LANG_ASSERT(ft->args.size() == prototype_data->args.size());
//         for (int i = 0; i < ft->args.size(); ++i) {
//           auto& arg = ft->args[i];
//           auto& proto_arg = prototype_data->args[i];
  
//           function_scope->locals.emplace(arg.name, arg.type);
//         }
//       }

//       scopes.push_back(function_scope);
//       infer_local_types_impl(ast, body.id + 1, body.size - 1, types, scopes, logger);
//       node_id += node.size;
//       scopes.pop_back();

//       break;
//     }
//     case ASTNodeType::Block:
//     {
//       scopes.push_back(make_scope(types, node_id));
//       infer_local_types_impl(ast, node.id + 1, node.size - 1, types, scopes, logger);
//       node_id += node.size;
//       scopes.pop_back();

//       break;
//     }
//     case ASTNodeType::VariableDeclaration:
//     {
//       LANG_ASSERT(node.type == ASTNodeType::VariableDeclaration);
//       ASTVariableDeclarationData* data = node.cast_data<ASTVariableDeclarationData>();

//       if (scopes.empty()) {
//         logger.log(Errors::Syntax, "Cannot declare a variable outside a block.", node.span);
//       }
//       if (find_variable_type_from_scopes(scopes, data->name) != nullptr) {
//         // @todo show where previously defined
//         logger.log(Errors::Type, data->name + " has already been declared.", node.span);
//       }
      
//       infer_variable_type(ast, node, types, scopes, logger);

//       // @note skips recursing into variable definition
//       node_id += node.size;
//       continue;
//     }
//     case ASTNodeType::Assignment:
//     {
//       LANG_ASSERT(node.type == ASTNodeType::Assignment);
//       ASTAssignmentData* data = node.cast_data<ASTAssignmentData>();

//       Type* variable_t = find_variable_type_from_scopes(scopes, data->name);
//       if (variable_t == nullptr) {
//         logger.log(Errors::Type, data->name + " is undefined.", node.span);
//       }

//       // @note inference also performs consistency checks
//       static_assert((int)ASTAssignmentData::Children::Expression == 1);
//       Type* inferred_t = infer_expression_type(ast, ast.nodes[node.id + 1], types, scopes, logger);

//       if (variable_t != nullptr && inferred_t != nullptr && variable_t != inferred_t) {
//         logger.log(Errors::Type, "Cannot assign " + inferred_t->name + " to " + variable_t->name, node.span);
//       }

//       // @note skips recursing into assignment
//       node_id += node.size;
//       continue;
//     }
//     case ASTNodeType::IntLiteral:
//     case ASTNodeType::FloatLiteral:
//     case ASTNodeType::Variable:
//     case ASTNodeType::Call:
//     case ASTNodeType::UnaryOperator:
//     case ASTNodeType::BinaryOperator:
//     {
//       // checks that expression is consistent (can be inferred)
//       infer_expression_type(ast, node, types, scopes, logger);

//       // @note skips recursing into expressions
//       node_id += node.size;
//       continue;
//     }
//     default:
//       break;
//     }

//     // depth first search, top to bottom
//     node_id += 1;
//   }
// }

// static void infer_local_types(AST& ast, TypeInfo& types, Logger& logger) {
//   std::vector<ScopeTypeInfo*> scopes;
//   infer_local_types_impl(ast, 0, ast.nodes.size(), types, scopes, logger);
// }