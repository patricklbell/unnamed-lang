#pragma once

#include "helpers.hpp"

#include <string>
#include <vector>
#include <unordered_map>
  
using type_id = int64_t;
struct Type;

struct FunctionArgumentType {
  name_id name;
  type_id type;
  // @note optional
  void* default_value = nullptr;
};

struct FunctionType {
  std::vector<FunctionArgumentType> args;
  // @note optional
  type_id return_type = -1;
};

struct MemberType {
  name_id name;
  type_id type;
  // @note optional
  void* default_value = nullptr;
};

struct StructType {
  std::vector<MemberType> members;
  int size;
};

struct BufferType {
  type_id type;
  int count;
};

struct PtrType {
  // @note optional
  type_id type;
};

enum class PrimitiveType : char {
  Int,
  Float,
  Function,
  Struct,
  Buffer,
  Ptr,
  // @todo enum, union and generic
  Enum,
  Union,
  Generic,
};

struct Type {
  int64_t id;
  PrimitiveType primitive;

  // @todo consolidate
  FunctionType  function;
  StructType    struct_;
  BufferType    buffer;
  PtrType       ptr;
};

struct ScopeTypeInfo {
  int node_id;

  std::unordered_map<name_id, type_id> named;
  // @note relies on scope pointer being stable
  std::vector<ScopeTypeInfo*> children;
};

struct TypeInfo {
  type_id int_;
  type_id float_;
  type_id ptr;

  std::unordered_map<name_id, type_id> builtins;
  std::vector<Type> types;

  TypeInfo();

  // @todo efficiency
  Type& resolve_or_make_struct(StructType type);
  Type& resolve_or_make_ptr(type_id type);
  Type& resolve_or_make_buffer(type_id type, int count);
  Type& resolve_or_make_function(FunctionType type);
  Type& resolve_int();
  Type& resolve_float();
  Type& resolve_ptr();
};