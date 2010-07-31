/* port_lib.c */
#include <stdio.h>
#include <string.h>

/* Message data are all unsigned bytes */
typedef unsigned char byte;

/* Read the 2 length bytes (MSB first), then the data. */
read_cmd(buf)
byte *buf;
{
    int len;

    if (read_exact(buf, 2) != 2)
        return(-1);
    len = (buf[0] << 8) | buf[1];
    return read_exact(buf, len);
}

/* Pack the 2  bytes length (MSB first) and send it */
write_cmd(buf, len)
byte *buf;
int len;
{
    byte str[2];

    put_int16(len, str);
    if (write_exact(str, 2) != 2)
        return(-1);
    return write_exact(buf, len);
}

/*  [read|write]_exact are used since they may return
 *  BEFORE all bytes have been transmitted
 */
read_exact(buf, len)
byte *buf;
int  len;
{
    int i, got = 0;

    do {
        if ((i = read(0, buf+got, len-got)) <= 0)
          return (i);
        got += i;
    } while (got < len);
    return (len);
}

write_exact(buf, len)
byte *buf;
int  len;
{
    int i, wrote = 0;

    do {
        if ((i = write(1, buf+wrote, len-wrote)) <= 0)
          return (i);
        wrote += i;
    } while (wrote < len);
    return (len);
}

put_int16(i, s)
byte *s;
{
    *s = (i >> 8) & 0xff;
    s[1] = i & 0xff;
}

int get_int16(byte *s)
{
   int i;
   i = *s << 8 | *(s+1);
   return(i);
}

