#include "typing.hpp"

static type_id make_primitive_type(TypeInfo& t, PrimitiveType p, name_id id) {
  type_id tid = (type_id )t.types.size();
  Type type {
    .id = tid,
    .primitive = p,
  };
  t.builtins[id] = tid;
  t.types.emplace_back(std::move(type));

  return tid;
}


TypeInfo::TypeInfo() {
  this->int_    = make_primitive_type(*this, PrimitiveType::Int,    name_id_int);
  this->float_  = make_primitive_type(*this, PrimitiveType::Float,  name_id_float);
  this->ptr     = make_primitive_type(*this, PrimitiveType::Ptr,    name_id_ptr);
}

Type& TypeInfo::resolve_or_make_struct(StructType type) {
  for (auto& t : this->types) {
    if (t.primitive != PrimitiveType::Struct)
      continue;
    if (t.struct_.members.size() != type.members.size())
      continue;
    if (t.struct_.size != type.size)
      continue;

    bool members_are_equal = true;
    for (int i = 0; i < type.members.size(); ++i) {
      const auto& m = t.struct_.members[i];
      const auto& tm = type.members[i];

      // @todo compare default values by value
      if (m.name != tm.name || m.default_value != tm.default_value || m.type != tm.type) {
        members_are_equal = false;
        break;
      }
    }

    if (members_are_equal)
      return t;
  }

  return this->types.emplace_back(Type {
    .id = (int64_t)this->types.size(),
    .primitive = PrimitiveType::Struct,
    .struct_ = type,
  });
}
Type& TypeInfo::resolve_or_make_ptr(type_id type) {
  for (auto& t : this->types) {
    if (t.primitive != PrimitiveType::Ptr)
      continue;
    if (t.ptr.type != type)
      continue;
    
    return t;
  }

  return this->types.emplace_back(Type {
    .id = (int64_t)this->types.size(),
    .primitive = PrimitiveType::Ptr,
    .ptr = PtrType{ .type = type },
  });
}
Type& TypeInfo::resolve_or_make_buffer(type_id type, int count) {
  for (auto& t : this->types) {
    if (t.primitive != PrimitiveType::Buffer)
      continue;
    if (t.buffer.type != type)
      continue;
    if (t.buffer.count != count)
      continue;
    
    return t;
  }

  return this->types.emplace_back(Type {
    .id = (int64_t)this->types.size(),
    .primitive = PrimitiveType::Buffer,
    .buffer = BufferType{ .type = type, .count = count },
  });
}
Type& TypeInfo::resolve_or_make_function(FunctionType type) {
  for (auto& t : this->types) {
    if (t.primitive != PrimitiveType::Function)
      continue;
    if (t.function.args.size() != type.args.size())
      continue;
    if (t.function.return_type != type.return_type)
      continue;

    bool args_are_equal = true;
    for (int i = 0; i < type.args.size(); ++i) {
      const auto& m = t.function.args[i];
      const auto& tm = type.args[i];

      // @todo compare default values by value
      if (m.name != tm.name || m.default_value != tm.default_value || m.type != tm.type) {
        args_are_equal = false;
        break;
      }
    }

    if (args_are_equal)
      return t;
  }

  return this->types.emplace_back(Type {
    .id = (int64_t)this->types.size(),
    .primitive = PrimitiveType::Function,
    .function = type,
  });
}
Type& TypeInfo::resolve_int() {
  return this->types[this->int_];
}
Type& TypeInfo::resolve_float() {
  return this->types[this->float_];
}
Type& TypeInfo::resolve_ptr() {
  return this->types[this->ptr];
}