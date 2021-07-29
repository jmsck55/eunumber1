// Copyright (c) 2016-2021 James Cook

// "Stub" test program.

#include <stdio.h>
#include <float.h>

#include "myeun.h"

// For GCC:
#if __SIZEOF_POINTER__ == 8
// This code is 8-byte aligned, i.e. #pragma pack(8);
#pragma pack(8)
#else
// This code is 4-byte aligned, i.e. #pragma pack(4);
#pragma pack(4)
#endif

int main()
{
    int mant = DBL_MANT_DIG;
#if __SIZEOF_POINTER__ == 8
    printf("Using GCC for 64-bit.\n");
#endif

    myeun_init_library();

    myeun_PrintVersion();

    // Add more code...
    printf("(Informational) Double's mant is: %i\n", mant);

    // Remember to free library before exitting.
    myeun_free_library();

    // At the end of a console program, prompt for user input:
    printf("Press Enter to exit.\n");
    getc(stdin);

    return 0;
}
