#pragma once

#include "logging.hpp"

#include <memory>
#include <type_traits>

using name_id = int64_t;

// builtin names
constexpr name_id name_id_int          = 1;
constexpr name_id name_id_float        = 2;
constexpr name_id name_id_ptr          = 3;
constexpr name_id name_id_buffer       = 4;
constexpr name_id name_id_num_builtins = 5;

using unique_void_ptr = std::unique_ptr<void, void(*)(void const*)>;

template<typename T>
inline unique_void_ptr unique_void(T * ptr)
{
    return unique_void_ptr(ptr, [](void const * data) {
        T const * p = static_cast<T const*>(data); 
        delete p;
    });
}

template<>
inline unique_void_ptr unique_void<void>(void * ptr)
{
    return unique_void_ptr(ptr, [](void const * data) {
      LANG_ASSERT(data != nullptr, "void* type should only be used with a nullptr");
    });
}
