#pragma once

#include <vector>

// @todo this is a bare minimum implementation
struct Arena {
    std::vector<void*> pages;
    size_t size;

    template<typename T>
    T* allocate() {
        size += sizeof(T);
        void* page = malloc(sizeof(T));
        LANG_ASSERT(page != nullptr, "Ran out of memory");
        pages.push_back(page);
        
        T* v = new (page) T();
        return v;
    };
    
    ~Arena() {
        for (auto& v : pages) {
            free(v);
        }
    }
};