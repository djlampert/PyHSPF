/*
 *  + + + SYSTEM + + +
 */
#include <stdlib.h>
/*
 *  + + + ENTRY + + +
 */
    void   *Malloc
/* In  */          (iSize)
/*
 *  + + + PURPOSE + + +
 *  Allocate memory
 *
 *  + + + DUMMY ARGUMENTS + + +
 */
    int          iSize;
/*
 *  + + + ARGUMENT DEFINITIONS + + +
 *  iSize       - Size to allocate
 */
{
/*
 *  + + + LOCALS + + +
 */
    void       *vpPtr;
/*
 *  + + + END SPECIFICATIONS + + +
 */
    vpPtr = malloc (iSize);
    return (vpPtr);
}
/*
 *  + + + SYSTEM + + +
 */
#include <stdlib.h>
/*
 *  + + + ENTRY + + +
 */
    void   Free
/* In  */       (vpPtr)
/*
 *  + + + PURPOSE + + +
 *  Free an array allocated by Malloc
 *
 *  + + + DUMMY ARGUMENTS + + +
 */
    void        *vpPtr;
/*
 *  + + + ARGUMENT DEFINITIONS + + +
 *  vpPtr   - Pointer to array
 */
{
/*
 *  + + + END SPECIFICATIONS + + +
 */
    free (vpPtr);
}
/*
 *  + + + SYSTEM + + +
 */
#include <stdlib.h>
/*
 *  + + + ENTRY + + +
 */
    void   *Realloc
/* In  */           (vpPtr, iSize)
/*
 *  + + + PURPOSE + + +
 *  Reallocate an array allocated by Malloc
 *
 *  + + + DUMMY ARGUMENTS + + +
 */
    void        *vpPtr;
/*
 *  + + + ARGUMENT DEFINITIONS + + +
 *  vpPtr   - Pointer to array
 */
{
/*
 *  + + + LOCALS + + +
 */
    void       *vpNew;
/*
 *  + + + END SPECIFICATIONS + + +
 */
    vpNew = realloc (vpPtr, iSize);
    return (vpNew);
}
