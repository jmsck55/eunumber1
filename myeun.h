// Copyright (c) 2016-2021 James Cook

// C header file

#pragma once

#ifndef myeun_h__
#define myeun_h__

#include <stdio.h>
#include <windows.h>

// For GCC:
#if __SIZEOF_POINTER__ == 8
// This code is 8-byte aligned, i.e. #pragma pack(8);
#pragma pack(8)
#else
// This code is 4-byte aligned, i.e. #pragma pack(4);
#pragma pack(4)
#endif

#define HIGHWORD(T) ((T) >> 16)
#define LOWWORD(T) ((T) & 0xffff)

HMODULE libmyeun = 0;

typedef int (*VersionFunc)(void);
//VersionFunc Version;
VersionFunc EunVersion;

void myeun_init_library()
{
    libmyeun = LoadLibraryA("libmyeun.dll");
    if (libmyeun == NULL)
    {
        // an error occurred.
        return;
    }

    //Version = (VersionFunc)GetProcAddress(libmyeun, "Version");
    EunVersion = (VersionFunc)GetProcAddress(libmyeun, "GetVersion");
}

void myeun_free_library()
{
    if (!FreeLibrary(libmyeun))
    {
        // an error occurred.

    }
}
// Remember to use "FreeLibrary()" when exitting application.

void myeun_PrintVersion()
{
    printf("[libmyeun.dll] EunVersion is: %d\n", EunVersion());
}

#endif // myeun_h__
