#pragma once

#include <string>
#include <vector>
#include <unordered_map>

struct Type {
    std::string name;
  
    enum class Form : char {
      Unknown = 0,
      Int,
      Float,
      Custom
    } form;
  };
  
  struct FunctionArgumentType {
    std::string name;
    Type* type; 
  };
  
  // @todo fold function type into generic type
  struct FunctionType {
    std::string name;
    std::vector<FunctionArgumentType> args;
    Type* return_type;
  };
  
  struct ScopeTypeInfo {
    int node_id;
  
    std::vector<Type> types;
    std::unordered_map<std::string, Type*> locals;
  };
  
  struct TypeInfo {
    std::unordered_map<std::string, FunctionType> functions;
    std::unordered_map<std::string, Type*> global_types;
    Type int_type;
    Type float_type;
    
    std::unordered_map<int, ScopeTypeInfo> scopes;
  
    TypeInfo();
  };