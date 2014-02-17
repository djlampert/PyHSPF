/*
 *  + + + SYSTEM + + +
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/*
 *  + + + ENTRY + + +
 */
    void   dirlis_
/* In  */         ( ipLen, cpWild, ipMaxLen, ipMaxFil,
/* Out */           cpFiles, ipNfile, ipStat )
/*
 *  + + + PURPOSE + + +
 *  Get a directory listing of files which match the wildcard spec
 *
 *  + + + DUMMY ARGUMENTS + + +
 */
    int         *ipLen, *ipMaxLen, *ipMaxFil, *ipNfile, *ipStat;
    char        *cpWild, *cpFiles;
/*
 *  + + + ARGUMENT DEFINITIONS + + +
 *  ipLen       - Length of wildcard spec
 *  cpWild      - Wildcard spec
 *  ipMaxLen    - Maximum filename length
 *  ipMaxFil    - Maximum number of files to return
 *  cpFiles     - List of returned files
 *  ipNfile     - Number of returned files
 *  ipStat      - Status code. 0 = ok.
 */
    {
    /*
     *  + + + LOCALS + + +
     */
        FILE    *spFp;
        char    *cpCmd,
                *cpPtr,
                 cpBuf[1024],
                *FileName();
    int          iLen,
                 iOff,
                 iNnam,
                 iEof,
                 iChar;
    /*
     *  + + + END SPECIFICATIONS + + +
     *
     *  Only proceed if inputs are valid. Otherwise return no match.
     */
        *ipStat = 0;
        if ((*ipLen > 0) && (*ipMaxFil > 0) && (*ipMaxLen > 0))
        {   
        /*
         *  Redirect stderr to a temp file
         */
            (void) freopen("/dev/null", "w", stderr);
        /*
         *  Allocate space for ls command.
         */
            cpCmd = (char *) Malloc(*ipLen + 4);
            if (cpCmd == NULL)
            {
                *ipStat = 1;
            }
            if (!*ipStat)
            {
            /*
             *  Build the ls command.
             */
                (void) strcpy(cpCmd, "ls ");
                for (iChar = 0; iChar < *ipLen; iChar++)
                {
                    cpCmd[iChar + 3] = cpWild[iChar];
                }
                cpCmd[*ipLen + 3] = 0;
            /*
             *  Open a pipe for the ls command.
             */
                spFp = (FILE *) popen(cpCmd, "r");
                if (spFp == NULL)
                {
                    *ipStat = 1;
                }
            /*
             *  Free the command buffer
             */
                Free((void *) cpCmd);
            }

            if (!*ipStat)
            {
            /*
             *  Loop until EOF
             */
                for (iNnam = iEof = iOff = 0; !iEof && (iNnam < *ipMaxFil); )
                {
                    if (fgets(cpBuf, 1024, spFp) != NULL)
                    {
                    /*
                     *  Check for unlikely event that name exceeds 1024
                     *  characters. If so, last character will
                     *  NOT be a newline.
                     */
                        iLen = strlen(cpBuf);
                        if (cpBuf[iLen - 1] != '\n')
                        {
                        /*
                         *  Space past the newline character.
                         *  Skip the long name.
                         */
                            do
                            {
                                iChar = fgetc(spFp);
                            }
                            while ((iChar != EOF) && (iChar != '\n'));
                        }
                        else        /* Name not too long */
                        {
                        /*
                         *  Get filename portion of pathname
                         */
                            iLen--;
                            cpBuf[iLen] = 0;        /* Get rid of newline */
                            cpPtr = FileName(cpBuf);
                            iLen = strlen(cpPtr);
                            if (iLen < *ipMaxLen)
                            {
                                (void) memcpy(&cpFiles[iOff], cpPtr, iLen);
                                (void) memset(&cpFiles[iOff+iLen], ' ',
                                                *ipMaxLen - iLen);
                                iOff += *ipMaxLen;
                                iNnam++;
                            }
                        }
                    }
                    else            /* EOF occurred */
                    {
                        iEof = 1;
                    }
                }
            /*
             *  Close the pipe
             */
                (void) pclose(spFp);
                *ipNfile = iNnam;
            /*
             *  Redirect stderr to tty
             */
                (void) freopen("/dev/tty", "w", stderr);
            }
            else                /* One or more inputs are invalid */
            {
                *ipNfile = 0;
            }
        }
    }
