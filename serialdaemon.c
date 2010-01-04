#include <fcntl.h> 
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <termios.h> 
#include <sys/time.h>
#include <sys/stat.h> 
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>

#define NOTHING 0
#define BUFFER_SIZE 1024

/* File Descriptors */
int sockfd = NOTHING;
int auxfd = NOTHING;
int auxfd_pre = NOTHING;
int serialfd = NOTHING;
int sockfd_pre = NOTHING;

/* Debugging values */
int INDEBUG = 0;
int OUTDEBUG = 0;

/* baudrate settings are defined in <asm/termbits.h>, 
 * which is included by <termios.h> 
 */ 

char *see_speed(speed_t speed) 
    {
    static char   SPEED[20];
    switch (speed) 
        {
        case B9600:     strcpy(SPEED, "B9600");
            break;
        case B19200:    strcpy(SPEED, "B19200");
            break;
        case B38400:    strcpy(SPEED, "B38400");
            break;
        case B115200:   strcpy(SPEED, "B115200");
            break;
        case B230400:   strcpy(SPEED, "B230400");
            break;
#ifndef __APPLE__
        case B460800:   strcpy(SPEED, "B460800");
            break;
        case B500000:   strcpy(SPEED, "B500000");
            break;
#endif
        default:        sprintf(SPEED, "unknown (%d)", (int) speed);
        }
    return SPEED;
    }

int parseBaudRates(char* baudChars)
    {
    int result = 0;
    int tmp = atoi(baudChars);
    switch (tmp) 
        {
#ifndef __APPLE__
        case 500000:
            result = B500000;
            break;
        case 460800:
            result = B460800;
            break;
#endif
        case 230400:
            result = B230400;
            break;
        case 115200:
            result = B115200;
            break;
        case 57600:
            result = B57600;
            break;
        case 38400:
            result = B38400;
            break;
        case 19200:
            result = B19200;
            break;
        case 9600:
            result = B9600;
            break;
        default:
            printf("ERROR!: Unknown baud rate.\n");
            break;
        }
    return result;
    }

/* Print for Debugging */
void printDebugString(char* str, int len) 
    {
    int x;
    for(x=0;x < len;x++)
        {
        if (str[x] >= 32 && str[x] < 127)
            putchar(str[x]);
        else putchar('?');
        }
    printf("\n                 ");
    for(x=0; x < len;x++)
        {
        printf("%d ", str[x] >= 0 ? str[x] : str[x] + 256);
        }
    printf("\n");
    }

/*  Reads into ptr maxlen bytes from a socket or file descriptor
 *  or until encountering a '\n', whichever comes first.
 *  '\n' is included at the end of ptr, followed by '\0' (of course).
 *
 *  read fills ptr with the data, and returns either
 *  a read error (<0) or the number of bytes that were
 *  actually read (if this differs from maxlen, it's because
 *  an eof was encountered, or because a '\n' was found).
 *
 *  fd is the file or socket descriptor
 *  ptr is the data to write, maxlen bytes long
 *  maxlen is the maximum size of the data
 *
 *  Stolen straight from Stallings 
 */
int readline (int fd, char* ptr, int maxlen)
    {
    int n, rc;
    char c;
    for (n=1;n<maxlen;n++)
        {
        if ((rc=read(fd,&c,1)) == 1)
            {
            *ptr++=c;
            if (c=='\n') break;        /*  End Of Line */
            }
        else if (rc == 0)
            {
            if (n==1) return 0;        /*  EOF, No Data Read */
            else break;                /*  EOF, Some Data Was Read */
            }
        else return -1;                /*  Error! */
        }
    *ptr = 0;
    return n;
    }
    
int makeSocket(int port)
    {
    int    sockfd,sd,childpid;
    struct sockaddr_in serv_addr;
           
    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        {
        fprintf(stderr,"Server Error:  Can't open stream socket.\n");
        return -1;
        }
        
    bzero((char*) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family        =AF_INET;
    serv_addr.sin_addr.s_addr   =htonl(INADDR_ANY);
    serv_addr.sin_port          =htons(port);
    
    if (bind(sockfd, (struct sockaddr*) &serv_addr, sizeof(serv_addr))<0)
        {
        fprintf(stderr,"Server Error:  Can't bind to local address.\n");
        return -1;
        }

    listen(sockfd,5);
    return sockfd;
    }

int makeSerialPortFd(char port[],int baud)
    {
    int fd; 
    struct termios newtio; 
    fd = open(port,O_RDWR); // open up the port on read / write mode
    if (fd == -1)
        return(-1); // Opps. We just has an error
        
    /* Save the current serial port settings */
    tcgetattr(fd, &newtio);
        
    /* Set the input/output baud rates for this device */
    cfsetispeed(&newtio, baud);
    cfsetospeed(&newtio, baud);

    /* CLOCAL:      Local connection (no modem control) */
    /* CREAD:       Enable the receiver */
    newtio.c_cflag |= (CLOCAL | CREAD);

    /* PARENB:      Use NO parity */
    /* CSTOPB:      Use 1 stop bit */
    /* CSIZE:       Next two constants: */
    /* CS8:         Use 8 data bits */
    newtio.c_cflag &= ~PARENB;
    newtio.c_cflag &= ~CSTOPB;
    newtio.c_cflag &= ~CSIZE;
    newtio.c_cflag |= CS8;

    /* Disable hardware flow control */
    // BAD:  newtio.c_cflag &= ~(CRTSCTS);

    /* ICANON:      Disable Canonical mode */
    /* ECHO:        Disable echoing of input characters */
    /* ECHOE:       Echo erase characters as BS-SP-BS */
    /* ISIG:        Disable status signals */
    // BAD: newtio.c_lflag = (ECHOK);

    /* IGNPAR:      Ignore bytes with parity errors */
    /* ICRNL:       Map CR to NL (otherwise a CR input on the other computer will not terminate input) */
    // BAD:  newtio.c_iflag |= (IGNPAR | ICRNL);
    newtio.c_iflag |= (IGNPAR | IGNBRK); 
        
    /* NO FLAGS AT ALL FOR OUTPUT CONTROL  -- Sean */
    newtio.c_oflag = 0;

    /* IXON:        Disable software flow control (incoming) */
    /* IXOFF:       Disable software flow control (outgoing) */
    /* IXANY:       Disable software flow control (any character can start flow control */
    newtio.c_iflag &= ~(IXON | IXOFF | IXANY);

    /* NO FLAGS AT ALL FOR LFLAGS  -- Sean*/
    newtio.c_lflag = 0;

    /*** The following settings are deprecated and we are no longer using them (~Peter) ****/
    // cam_data.newtio.c_lflag &= ~(ICANON && ECHO && ECHOE && ISIG); 
    // cam_data.newtio.c_lflag = (ECHO);
    // cam_data.newtio.c_iflag = (IXON | IXOFF);
    /* Raw output */
    // cam_data.newtio.c_oflag &= ~OPOST;

    /* Clean the modem line and activate new port settings */
    tcflush(fd, TCIOFLUSH);
    tcsetattr(fd, TCSANOW, &newtio);

    return(fd);
    }

int waitOnSocket(int sockfd)
    {
    struct sockaddr_in  cli_addr;
    unsigned int clilen = sizeof(cli_addr);
    int sd;

    sd = accept(sockfd, (struct sockaddr *) &cli_addr, &clilen);
    
    if (sd <0)
        {
        fprintf(stderr,"Server Error:  Accept error.\n");
        return -1;
        }

    return sd;    
    }

void auxShiftBaud(char* cmd)
    {
    int newBaud = parseBaudRates(cmd);

    /* Save the current serial port settings */
    struct termios newtio; 
    tcgetattr(serialfd, &newtio);

    /* Set the input/output baud rates for this device */
    cfsetispeed(&newtio, newBaud);
    cfsetospeed(&newtio, newBaud);

    /* Clean the modem line and activate new port settings */
    tcflush(serialfd, TCIOFLUSH);
    tcsetattr(serialfd, TCSANOW, &newtio);

    if (tcgetattr(serialfd, &newtio) != 0)
        {
        printf("ERROR: Bad termoios; Rate change may have failed?\n");
        return;
        }

    if (OUTDEBUG)
        {
        printf("DEBUG: changed to %s\n",see_speed(cfgetispeed(&newtio)));
        }
    }

int closeAll()
    {
    if (sockfd     != NOTHING) close(sockfd);
    if (sockfd_pre != NOTHING) close(sockfd_pre);
    if (auxfd      != NOTHING) close(auxfd);
    if (auxfd_pre  != NOTHING) close(auxfd_pre);
    if (serialfd   != NOTHING) close(serialfd);
    }

int max(int a, int b)
    {
    return (a > b ? a : b);
    }

int main(int argc, char *argv[])
    {
    fd_set rset;
    struct timeval timeout;
    char c[BUFFER_SIZE];
    int csize;

    int mode;
    int x;
    int y;
    int args = 0;
    int tmp;
    int result;
    int blockerror = 0;

    char argSerial[] = "-serial";
    char argPort[]   = "-port";
    char argStrip[]  = "-strip";
    char argBaud[]   = "-baud";
    char argDebug[]  = "-debug";
    char argInDebug[]  = "-indebug";
    char argOutDebug[]  = "-outdebug";
    char argNonblock[]  = "-nonblock";
    char argAux[]  = "-aux";

    int SOCKET_PORT = 0;
    int BAUD = 0;
    int STRIP = 0;
    int NONBLOCK = 0;
    int AUX_PORT = 0;
    char SERIAL[100];
        
    for (x=1; x<argc; x++)
        {
        if (!strcmp(argSerial,argv[x]))
            {
            strcpy(SERIAL,argv[x+1]);
            x++;
            args++;
            }
        else if (!strcmp(argPort,argv[x]))
            {
            SOCKET_PORT = atoi(argv[x+1]);
            x++;
            args++;
            }
        else if (!strcmp(argBaud,argv[x]))
            {
            BAUD = parseBaudRates(argv[x+1]);
            if (!BAUD) return 1;
            x++;
            args++;
            }
        else if (!strcmp(argAux, argv[x]))
            {
            AUX_PORT = atoi(argv[x+1]);
            x++;
            }
        else if (!strcmp(argStrip,argv[x]))
            STRIP = 1;
        else if (!strcmp(argInDebug,argv[x]))
            INDEBUG = 1;
        else if (!strcmp(argOutDebug,argv[x]))
            OUTDEBUG = 1;
        else if (!strcmp(argDebug,argv[x]))
            INDEBUG = OUTDEBUG = 1;
        else if (!strcmp(argNonblock,argv[x]))
            NONBLOCK = 1;
        else { 
            printf("ERROR!: Unknown argument %s\n", argv[x]); 
            return 1; 
            }
        }

    if (args < 3)
        {
        printf("--------------------------------------------------------------\n");
        printf("------------------  GMU SerialDaemon  ------------------------\n");
        printf("--------------------------------------------------------------\n");
        printf("Usage:\n");
        printf("\tserialdaemon\n");
        printf("\t\t-serial [serialPort]\n");
        printf("\t\t-port   [TCP/IP Port]\n");
        printf("\t\t-aux    [auxiliary TCP/IP Port]\n");
        printf("\t\t-baud   [baudRate]\n");
        printf("\t\t\t500000    (available on Linux only)\n");
        printf("\t\t\t460800    (available on Linux only)\n");
        printf("\t\t\t230400\n");
        printf("\t\t\t115200\n");
        printf("\t\t\t57600\n");
        printf("\t\t\t38400\n");
        printf("\t\t\t19200\n");
        printf("\t\t\t9600\n");
        printf("\t\t-strip\n");
        printf("\t\t-indebug\n");
        printf("\t\t-outdebug\n");
        printf("\t\t-debug\n");
        printf("\t\t-nonblock\n");
        printf("\n");
        printf("Notes:\n");
        printf("1) If you have declared an auxiliary port, your client program\n");
        printf("   must connect to the primary TCP/IP port, THEN the auxiliary\n"); 
		printf("   port, and both must be connected before any traffic is sent\n");
        printf("2) Baud rates 460800 and 500000 are not available on OS/X\n");
        return(1);
        }

    if (INDEBUG || OUTDEBUG)
        printf ("DEBUG: debug mode on!\n");

    sockfd_pre = makeSocket(SOCKET_PORT);
    if (sockfd_pre <= 0)
        { 
        printf("ERROR: couldn't make TCP/IP socket!\n"); 
        closeAll(); 
        return; 
        }

    if (AUX_PORT != 0)
        {
        auxfd_pre = makeSocket(AUX_PORT);
        if (auxfd_pre <= 0)
            { 
            printf("ERROR: couldn't make TCP/IP socket!\n"); 
            closeAll(); 
            return; 
            }
        }

    serialfd = makeSerialPortFd(SERIAL, BAUD);
    if (serialfd <= 0)
        { 
        printf("ERROR: couldn't open serial port!\n"); 
        closeAll(); 
        return; 
        }

    if (argc <= 1)
        mode = 0;
    else
        mode = atoi(argv[1]);

    printf("Listening for data connections on port: %i\n",SOCKET_PORT);
    if (AUX_PORT != 0)
        {
        printf("Listening for aux  connections on port: %i\n",AUX_PORT);
        }

    while(1)
        {
        /* Wait for connection on data socket */
        sockfd = waitOnSocket(sockfd_pre);
        if (INDEBUG || OUTDEBUG)
            printf("DEBUG: New data socket opened.\n");
        if (sockfd < 0)
            {
            closeAll(); 
            return;
            }

        /* Set data socket to non-blocking */
        if (NONBLOCK)
            {
            if (fcntl(sockfd, F_SETFL, O_NONBLOCK) != 0)
                {
                printf("ERROR: couldn't make TCP/IP socket non-blocking!\n"); 
                closeAll(); 
                return; 
                }
            }

        /* Wait for connection on AUX socket (if specified) */
        if (auxfd_pre != NOTHING)
            {
            auxfd = waitOnSocket(auxfd_pre);
            if (INDEBUG || OUTDEBUG)
                printf("DEBUG: New aux  socket opened.\n");
            if (auxfd < 0)
                {
                closeAll(); 
                return;
                }
            }

        /* Must have a serial file descriptor (else, why are we running?) */
        if (serialfd < 0)
            {
            closeAll();
            return;
            }
    
        FD_ZERO(&rset);

        /* Main Loop */
        while(1)
            {
            /* Add connections to set */
            FD_SET(sockfd,&rset);
            if (auxfd!=NOTHING) FD_SET(auxfd,&rset);
            FD_SET(serialfd,&rset);

            /* Select on connection */
            select(max(max(sockfd,auxfd),serialfd)+1,&rset,NULL,NULL,NULL);
                        
            /* There's stuff to read on AUX */
            if (FD_ISSET(auxfd,&rset))
                { 
                if ((csize = readline(auxfd, c, BUFFER_SIZE)) >= 1)
                    {
                    c[csize] = '\0';  // after length, so no problem
                    char cmd = c[0];
                    if (c[1] != ' ')
                        printf("ERROR!: Malformed AUX command; ignoring\n");
                    char* data = &c[2];
                    switch (cmd)
                        {
                        case 'B':
                            if (INDEBUG)
                                {
                                printf("DEBUG: AUX baud change\n");
                                }
                            auxShiftBaud(data);
                            break;
                        default:
                            printf("ERROR!: Unknown AUX command; ignoring\n");
                            break;
                        }                                                       
                    }
                else break;             /* Failed */
                }
                        
            /* There's stuff to read on SOCKET */
            if (FD_ISSET(sockfd,&rset))
                {
                if ((csize= read(sockfd, c, BUFFER_SIZE)) >= 1)
                    {
                    y = csize;
                    if (STRIP==1)
                        {
                        for(x = 0, y = 0 ; x < csize; x++, y++)
                            {
                            if (c[x] == '\n')  { // get rid of it
                                y--;
                                if (OUTDEBUG) printf ("DEBUG: **STRIPPED**\n");
                                }
                            else c[y] = c[x];
                            }
                        }
                    if (OUTDEBUG)
                        {
                        c[y] = '\0';  // after length, so no problem
                        printf("DEBUG: serial <==");
                        printDebugString(c, y);
                        }
                    result = write(serialfd, c, y);
                    if (OUTDEBUG)
                        {
                        printf("DEBUG: wrote %d/%d\n", result, y);
                        }
                    }
                else break;             /* Failed */
                }
                        
            /* There's stuff to read on SERIAL */
            if (FD_ISSET(serialfd,&rset))
                {
                if ((csize = read(serialfd, c, BUFFER_SIZE)) >= 1)
                    {
                    if (STRIP==1)
                        {
                        for(x = 0 ; x < csize; x++)
                            {
                            if (c[x] == '\r' )  { // get rid of it
                                c[x] = '\n';
                                if (OUTDEBUG) printf ("DEBUG: **STRIPPED**\n");
                                }
                            else c[y] = c[x];
                            }
                        }
                    if (INDEBUG)
                        {
                        c[csize] = '\0';  // after length, so no problem
                        printf("DEBUG: serial ==>");
                        printDebugString(c, csize);
                        }
                    result = write(sockfd, c, csize);
                    if (result == EWOULDBLOCK)
                        {
                        if (!blockerror)
                            { 
                            blockerror = 1; 
                            printf("ERROR: dropping bytes writing to socket\n"); 
                            }
                        }
                    else if (INDEBUG)
                        {
                        printf("DEBUG: read %d/%d\n", result, csize);
                        }
                    }
                else break;             /* Failed */     
                }
            }

        /* Restart connection-wait loop */
        printf("Restarting\n");
        close(sockfd);  /* clean up */
        }
    }
