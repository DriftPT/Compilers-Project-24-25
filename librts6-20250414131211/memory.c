#include "memory.h"
#include "kernel.h"
#include "string.h"
#include "integer.h"

/* This memory allocator uses the buddy system with brk syscall and the 32-bit ABI */

#define PAGE_SIZE 4096     // 4KB page size
#define MIN_ALLOC_SIZE 16  // Minimum allocation size (must be at least sizeof(block_header))
#define MAX_ORDER 10       // Maximum order for buddy system (4MB)

/* Block header structure to track allocations
 * Designed to fit within 32-bit architecture constraints
 */
typedef struct block_header {
    unsigned long size;        // Size of the block (including header)
    bool free;                 // true if free, false if allocated
    struct block_header *next; // Next block in free list or 0
    struct block_header *prev; // Previous block in free list or 0
} block_header;

/* Global memory management variables
 * Memory addresses are 32-bit in x32 ABI
 */
static void* heap_start = nullptr;        // Start of the heap
static void* heap_end = nullptr;          // Current end of the heap
static unsigned long heap_size = 0;     // Total heap size in bytes
static block_header* free_lists[MAX_ORDER+1]; // Array of free lists

/* Calculate the order (power of 2) needed for a given size */
static int calculate_order(unsigned long size) {
    int order = 0;
    unsigned long min_size = MIN_ALLOC_SIZE;
    
    while (min_size < size && order < MAX_ORDER) {
        min_size <<= 1;
        order++;
    }
    
    return order;
}

/* Calculate the size corresponding to an order */
static unsigned long order_to_size(int order) {
    return MIN_ALLOC_SIZE << order;
}

/* Link a block into the appropriate free list */
static void add_to_free_list(block_header* block, int order) {
    block->free = true;
    block->next = free_lists[order];
    block->prev = nullptr;
    
    if (free_lists[order]) {
        free_lists[order]->prev = block;
    }
    
    free_lists[order] = block;
}

/* Remove a block from its free list */
static void remove_from_free_list(block_header* block, int order) {
    if (block->prev) {
        block->prev->next = block->next;
    } else {
        free_lists[order] = block->next;
    }
    
    if (block->next) {
        block->next->prev = block->prev;
    }
    
    block->free = false;
    block->next = nullptr;
    block->prev = nullptr;
}

/* Split a block of a higher order into two blocks of lower order */
static block_header* split_block(block_header* block, int order) {
    unsigned long size = order_to_size(order);
    block_header* buddy = (block_header*)((char*)block + (size >> 1));
    
    // Set up buddy block
    buddy->size = size >> 1;
    buddy->free = 1;
    buddy->next = nullptr;
    buddy->prev = nullptr;
    
    // Update original block
    block->size = size >> 1;
    
    return buddy;
}

/* Request more memory from the system */
static int expand_heap(unsigned long size) {
    unsigned long aligned_size;
    
    // Ensure page alignment
    aligned_size = (size + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
    
    // Calculate new break value
    void* new_heap_end = (void*)((char*)heap_end + aligned_size);
    
    // Request memory from system using sys_brk (syscall 45)
    // Note: sys_brk returns the new program break as a pointer
    void* result = (void*)sys_brk((unsigned int)new_heap_end);
    if (result != new_heap_end) {
        // If result doesn't match our request, the allocation failed
        return -1;
    }
    
    // Create a new free block at the old heap end
    block_header* new_block = (block_header*)heap_end;
    new_block->size = aligned_size;
    new_block->free = true;
    new_block->next = nullptr;
    new_block->prev = nullptr;
    
    // Update heap end and size
    heap_end = new_heap_end;
    heap_size += aligned_size;
    
    // Add the new block to the appropriate free list
    int order = calculate_order(aligned_size);
    if (order > MAX_ORDER) {
        order = MAX_ORDER;
    }
    add_to_free_list(new_block, order);
    
    return 0;
}

/* Find and return a buddy block address
 * Properly calculate buddy address for power-of-2 sized blocks
 */
static block_header* find_buddy(block_header* block, unsigned long size) {
    // Calculate the buddy address
    // For a block at address A of size S, its buddy is at address A ⊕ S
    // (where ⊕ is the bitwise XOR operation)
    unsigned int block_addr = (unsigned int)block;
    unsigned int buddy_addr = block_addr ^ size;
    return (block_header*)buddy_addr;
}

/* Check if a block is the buddy of another block 
 * Note: This is implemented directly in mem_free() using find_buddy()
 */

/**
 * Initializes the memory management system
 * Must be called before any other memory functions
 * @returns 0 on success, -1 on failure
 */
int mem_init(void) {
    for (int i = 0; i <= MAX_ORDER; i++) {
        free_lists[i] = nullptr;
    }
    
    // Get current break value using sys_brk(0)
    // sys_brk(0) returns the current program break as a pointer
    void* current_break = (void*)sys_brk(0);
    if (current_break == 0) {
        return -1;
    }
    
    heap_start = current_break;
    heap_end = heap_start;
    heap_size = 0;
    
    // Allocate initial heap space (1 page)
    if (expand_heap(PAGE_SIZE) < 0) {
        return -1;
    }
    
    return 0;
}

/**
 * Allocates memory of the specified size
 * @param size number of bytes to allocate
 * @returns pointer to allocated memory, 0 on failure
 */
void* mem_alloc(unsigned long size) {
    int order, current_order;
    block_header* block;
    block_header* buddy;
    unsigned long block_size;
    
    // Calculate required size including header
    unsigned long total_size = size + sizeof(block_header);
    
    // Ensure minimum allocation size
    if (total_size < MIN_ALLOC_SIZE) {
        total_size = MIN_ALLOC_SIZE;
    }
    
    // Round to power of 2
    order = calculate_order(total_size);
    block_size = order_to_size(order);
    
    // Look for a free block in free lists
    current_order = order;
    while (current_order <= MAX_ORDER && !free_lists[current_order]) {
        current_order++;
    }
    
    // If no suitable block found, try to expand heap
    if (current_order > MAX_ORDER) {
        if (expand_heap(block_size) < 0) {
            return 0;
        }
        
        // Try again after expansion
        current_order = order;
        while (current_order <= MAX_ORDER && !free_lists[current_order]) {
            current_order++;
        }
        
        if (current_order > MAX_ORDER) {
            return 0; // Should not happen if expand_heap succeeded
        }
    }
    
    // Get a block from the free list
    block = free_lists[current_order];
    remove_from_free_list(block, current_order);
    
    // Split the block if necessary
    while (current_order > order) {
        current_order--;
        
        buddy = split_block(block, current_order + 1);
        add_to_free_list(buddy, current_order);
    }
    
    // Mark block as allocated
    block->free = 0;
    block->size = block_size;
    
    // Return pointer to data area
    return (void*)((char*)block + sizeof(block_header));
}

/**
 * Frees previously allocated memory
 * @param ptr pointer to memory to be freed
 */
void mem_free(void *ptr) {
    if (!ptr) return;
    
    block_header* block = (block_header*)((char*)ptr - sizeof(block_header));
    
    if (block < (block_header*)heap_start || block >= (block_header*)heap_end) {
        return; // Invalid pointer
    }
    
    if (block->free) {
        return; // Already free, avoid double free
    }
    
    unsigned long size = block->size;
    int order = calculate_order(size);
    add_to_free_list(block, order);
    
    // Try to merge with buddies
    while (order < MAX_ORDER) {
        // Calculate buddy address
        block_header* buddy = find_buddy(block, size);
        
        // Check if buddy is valid and free
        if ((char*)buddy < (char*)heap_start || 
            (char*)buddy >= (char*)heap_end ||
            !buddy->free || 
            buddy->size != size) {
            break; // Cannot merge
        }
        
        // Verify buddy relationship (double-check)
        if (find_buddy(buddy, size) != block) {
            break; // Not proper buddies, should not happen in correct implementation
        }
        
        // Remove both blocks from free lists
        remove_from_free_list(block, order);
        remove_from_free_list(buddy, order);
        
        // Determine which is the lower address
        if (buddy < block) {
            block = buddy;
        }
        
        // Update block size
        size *= 2;
        block->size = size;
        
        // Move to next order
        order++;
        
        // Add the merged block to the higher-order free list
        add_to_free_list(block, order);
    }
}

/**
 * Changes the size of the memory block pointed to by ptr
 * @param ptr pointer to the memory block to resize
 * @param size new size in bytes
 * @returns pointer to the resized memory block, 0 on failure
 */
void* mem_realloc(void *ptr, unsigned long size) {
    // Handle special cases
    if (ptr == nullptr) {
        // If ptr is nullptr, behave like mem_alloc
        return mem_alloc(size);
    }
    
    if (size == 0) {
        // If size is 0, behave like mem_free
        mem_free(ptr);
        return nullptr;
    }
    
    // Get block header
    block_header* block = (block_header*)((char*)ptr - sizeof(block_header));
    
    // Basic validation
    if (block < (block_header*)heap_start || block >= (block_header*)heap_end) {
        return nullptr; // Invalid pointer
    }
    
    // Calculate the current data size (excluding header)
    unsigned long current_data_size = block->size - sizeof(block_header);
    
    // If the requested size fits within the current block, just return the same pointer
    if (size <= current_data_size) {
        return ptr;
    }
    
    // We need to allocate a new, larger block
    void* new_ptr = mem_alloc(size);
    if (new_ptr == nullptr) {
        return nullptr; // Allocation failed
    }
    
    // Copy the old data to the new block
    // Use the smaller of the old and new sizes to avoid buffer overrun
    unsigned long copy_size = current_data_size;
    if (size < copy_size) {
        copy_size = size;
    }
    
    // Perform the copy
    char* src = (char*)ptr;
    char* dst = (char*)new_ptr;
    for (unsigned long i = 0; i < copy_size; i++) {
        dst[i] = src[i];
    }
    
    // Free the old block
    mem_free(ptr);
    
    return new_ptr;
}

/**
 * Reports memory usage statistics
 * Prints information about the heap to stdout
 */
void mem_stats(void) {
    prints("Memory Statistics:\n");
    
    // Print heap bounds (32-bit addresses)
    prints("  Heap start: 0x");
    // Convert address to hex string and print
    char buf[16];
    const char* hex = itoa((unsigned int)heap_start, buf, 16);
    prints(hex);
    prints("\n");
    
    prints("  Heap end:   0x");
    hex = itoa((unsigned int)heap_end, buf, 16);
    prints(hex);
    prints("\n");
    
    // Calculate total size
    unsigned int total_size = (unsigned int)((char*)heap_end - (char*)heap_start);
    prints("  Total size: ");
    printi(total_size);
    prints(" bytes\n");
    
    // Count free blocks
    int total_free_blocks = 0;
    unsigned int free_space = 0;
    
    for (int i = 0; i <= MAX_ORDER; i++) {
        int count = 0;
        unsigned int order_size = order_to_size(i);
        block_header* block = free_lists[i];
        
        while (block) {
            count++;
            free_space += order_size;
            block = block->next;
        }
        
        if (count > 0) {
            prints("  Order ");
            printi(i);
            prints(" (");
            printi(order_size);
            prints(" bytes): ");
            printi(count);
            prints(" free blocks\n");
        }
        
        total_free_blocks += count;
    }
    
    prints("  Total free: ");
    printi(free_space);
    prints(" bytes in ");
    printi(total_free_blocks);
    prints(" blocks\n");
    
    prints("  Used: ");
    printi(total_size - free_space);
    prints(" bytes\n");
}
