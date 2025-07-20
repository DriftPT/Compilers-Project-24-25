#pragma once

/**
 * Initializes the memory management system
 * Must be called before any other memory functions
 * @returns 0 on success, -1 on failure
 */
int mem_init(void);

/**
 * Allocates memory of the specified size
 * @param size number of bytes to allocate
 * @returns pointer to allocated memory, 0 on failure
 */
void* mem_alloc(unsigned long size);

/**
 * Frees previously allocated memory
 * @param ptr pointer to memory to be freed
 */
void mem_free(void *ptr);

/**
 * Changes the size of the memory block pointed to by ptr
 * @param ptr pointer to the memory block to resize
 * @param size new size in bytes
 * @returns pointer to the resized memory block, 0 on failure
 */
void* mem_realloc(void *ptr, unsigned long size);

/**
 * Reports memory usage statistics
 * Prints information about the heap to stdout
 */
void mem_stats(void);
