#include "assert.h"
#include "string.h"
#include "stddef.h"
#include <stdlib.h>
#include <clay-dhall/clay-dhall.h>
#include <clay-dhall/macro.h>

typedef struct {
    char* name;
    char* zipCode;
    cdhall_uint age;
} person_t;

static const cdhall_field_spec person_t_flds[] = {
    CDHALL_DEF_FIELD_SIMPLE(person_t, name),
    CDHALL_DEF_FIELD_SIMPLE(person_t, zipCode),
    CDHALL_DEF_FIELD_SIMPLE(person_t, age),
};

static const cdhall_record_spec person_t_rec =
    CDHALL_DEF_RECORD(person_t, person_t_flds);

static const cdhall_type_spec person_t_spec = {
    CDHALL_TYPE_RECORD, 
    &person_t_rec
};

void free_person(person_t* p)
{
    cdhall_free_array(p->name);
    cdhall_free_array(p->zipCode);
}

int main(int argc, char* argv[])
{
    assert(argc == 2);
    cdhall_init();
    
    cdhall_array array;
    cdhall_typed_ptr hArray = {{CDHALL_TYPE_ARRAY, &person_t_spec}, &array};
    assert(cdhall_input_file(argv[1], hArray));

    assert(array.size == 3);
    person_t* aPerson = array.elem;
    assert(strcmp(aPerson[0].name, "Alice") == 0);
    assert(aPerson[2].age == 40u);

    for(int i = 0; i < array.size; ++i) free_person(aPerson + i);
    cdhall_free_array(array.elem);
    
    cdhall_exit();

    return 0;
}
