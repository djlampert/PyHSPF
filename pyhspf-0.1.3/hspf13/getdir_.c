/*
 *  + + + SYSTEM + + +
 */
#include <stdlib.h>
#include <string.h>
#include <memory.h>
/*
 *  + + + ENTRY + + +
 */
    void   getdir_
/* In  */         ( ipLen,
/* Out */           cpDir)
/*
 *  + + + PURPOSE + + +
 *  Get a the name of the current working directory
 *
 *  + + + DUMMY ARGUMENTS + + +
 */
    int         *ipLen;
    char        *cpDir;
/*
 *  + + + ARGUMENT DEFINITIONS + + +
 *  ipLen       - Maximum length of directory name
 *  cpDir       - Directory name
 */
    {
    /*
     *  + + + LOCALS + + +
     */
        char         cpNam[1024];
        int          iLen;
    /*
     *  + + + END SPECIFICATIONS + + +
     */
        (void) getcwd (cpNam, 1024);
        iLen = strlen (cpNam);
        if (iLen > *ipLen)
        {
            iLen = *ipLen;
        }
        (void) memcpy (cpDir, cpNam, iLen);
        if (iLen < *ipLen)
        {
            (void) memset (&cpDir[iLen], ' ', *ipLen - iLen);
        }
    }
