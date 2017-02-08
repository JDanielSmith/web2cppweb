#ifndef __NMTBL_H__
#define __NMTBL_H__

#include "main.h"

class nm_entry : public heap_object {
  public:
    nm_entry*      next;
    int            tag;

    enum { recursive=1, macro=2 }; 
    unsigned char  flags; 

    char           text[1];    enum { h_t_size = 1987 };
    static nm_entry* nm_table[h_t_size];
    static unsigned  hash(const char* name);

    static nm_entry* add(const char* name, int v_tag);
    static nm_entry* find(const char* name);

 //   void* operator new(size_t size, size_t len) { 
	//return heap.alloc(size + len); 
 //   }
	//void operator delete(void* ptr, size_t sz) {
	//}
	static auto alloc(size_t len) {
		return static_cast<nm_entry*>(heap.alloc(sizeof(nm_entry) + len));
	}
};

#endif

