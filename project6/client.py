import asyncio
import time
import logging

async def tcp_echo_client(message, loop):
    # Connect to Goloman
    reader, writer = await asyncio.open_connection('127.0.0.1', 12080, loop=loop)
    # Connect to Holiday
    reader, writer = await asyncio.open_connection('127.0.0.1', 12082, loop=loop)
    #connect to welsh
    #reader, writer = await asyncio.open_connection('127.0.0.1', 12083, loop=loop)
    logging.info("Sending:" + message)
    writer.write(message.encode())
    data = await reader.read(100000)
    logging.info("Received:" + data.decode())


# writer.close()

def main():
    logging.basicConfig(filename='client' + "_log.txt", level=logging.DEBUG)
    # message = "\t\t\t\t\t    \f\f\fIAMAT\v\v\v\v\v   \t\fkiwi.cs.ucla.edu -33.86705222+151.1957 {0}\f\r\f\f\t\t\r\n".format(time.time())
    message = "IAMAT stupid.cs.ucla.edu -32.53+152.22 {0}\n".format(time.time())

    #message = "WHATSAT kiwi.cs.ucla.edu 20 10\n"
    #message = "WHATSAT stupid.cs.ucla.edu 20 10\n"
    loop = asyncio.get_event_loop()
    loop.run_until_complete(tcp_echo_client(message, loop))
    loop.close()


if __name__ == '__main__':
    main()
