#include <stdlib.h>
#include <stdint.h>

#if defined(_MSC_VER)
    #include <windows.h>
    #define ARC_INC(ptr) InterlockedIncrement64((volatile LONGLONG*)(ptr))
    #define ARC_DEC(ptr) InterlockedDecrement64((volatile LONGLONG*)(ptr))
#else
    #define ARC_INC(ptr) __atomic_add_fetch((size_t*)(ptr), 1, __ATOMIC_RELAXED)
    #define ARC_DEC(ptr) __atomic_sub_fetch((size_t*)(ptr), 1, __ATOMIC_RELAXED)
#endif

// 在堆上分配对象并初始化 ref_count = 1 
void* __obj_alloc(size_t size) { 
    size_t* p = (size_t*)malloc(size); 
    if (!p) { 
        return NULL; 
    } 
    *p = 1;  // 第一个字段是 ref_count 
    return (void*)p; 
} 

void __obj_retain(void* p) {
    if (!p) return;
    ARC_INC(p);
}

void __obj_release(void* p) {
    if (!p) return;
    if (ARC_DEC(p) == 0) {
        free(p);
    }
}
