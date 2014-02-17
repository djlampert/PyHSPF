#include <stdlib.h>
#include <string.h>

char *FileName(char *cpPath)
{
    char        *cpPtr;

    cpPtr = strrchr(cpPath, '/');
    if (cpPtr == NULL)
        cpPtr = cpPath;
    else
        cpPtr++;
    return (cpPtr);
}
