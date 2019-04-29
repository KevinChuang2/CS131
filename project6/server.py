import aiohttp
import asyncio
import sys
import logging
import time
import json
key = "AIzaSyB2YYoHuj26jt4P9qEzgzwzSi-isa7Tgug"
ports = {
    "Goloman" : 12080,
    "Hands": 12081,
    "Holiday": 12082,
    "Welsh": 12083,
    "Wilkes": 12084

}
Goloman, Hands, Holiday, Welsh, Wilkes = 12080, 12081, 12082, 12083, 12084
comm = {
    'Goloman': ['Hands', 'Holiday', 'Wilkes'],
    'Hands': ['Goloman', 'Wilkes'],
    'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
    'Welsh': ['Holiday'],
    'Wilkes': ['Goloman', 'Hands', 'Holiday']
}
clients = {}
def process_latlong(lat_long):
    indices = []
    for num, char in enumerate(lat_long):
        if char == "+" or char == "-":
            indices.append(num)
    if len(indices)!= 2:
        return None
    if indices[0] != 0 or indices[1] == len(lat_long)-1:
        return None
    return float(lat_long[indices[0]:indices[1]]), float(lat_long[indices[1]:])


async def generate_iamat_out(msg, time_received):
    clients[msg[1]] = [sys.argv[1], process_latlong(msg[2]), float(msg[3]), time_received]
    return_msg = "AT " + sys.argv[1] + ' '
    time = time_received - float(msg[3])
    if time>0:
        time = "+" + str(time)
    return_msg += time + ' '
    return_msg += msg[1] + ' '
    return_msg += msg[2] + ' '
    return_msg += msg[3]
    return return_msg

def print_clients():
    return_str = ''
    for x in clients:
        return_str+=x + ": "
        for y in clients[x]:
            return_str+= str(y) + ","
    return return_str

def is_valid_input(msg):
    if msg:
        if msg[0] == "IAMAT":
            if len(msg)!=4:
                return False
            return True
        elif msg[0] == "AT":
            if len(msg) != 6:
                return False
            return True
        elif msg[0] == "WHATSAT":
            if len(msg) != 4:
                return False
            return True
        return False


async def generate_whatsat(msg, time_received):
    if msg[1] not in clients:
        logging.error("? ERROR: Not a client")
        return error_msg(msg)
    client = clients[msg[1]]
    #radius must be less than 51, more than 0
    if float(msg[2]) > 50 or float(msg[2]) <=0:
        logging.error("? Error, incorrect radius")
        return error_msg(msg)
    if float(msg[3]) >20 or float(msg[3]) <=0:
        logging.error("? Error, incorrect info bound")
        return error_msg(msg)
    url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'
    url += f'key={key}&'
    if not msg[1] in clients:
        logging.error("? Error: not a valid client")
        return error_msg(msg)
    url += f'location={str(client[1]).strip("()")}&'
    url += f'radius={float(msg[2])*1000}'
    time_diff = float(client[3])-float(client[2])
    if time_diff>0:
        time_diff = "+" + str(time_diff)
    return_msg = "AT " + client[0] + " "+ time_diff +  " " + str(client[1][0]) + " " +  str(client[1][1]) + " " + str(client[3]) + '\n'
    logging.info(url)
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as resp:
            response = await resp.json()
            response['results'] = response['results'][:int(msg[3])]
            return_msg += json.dumps(response, indent=3)
            return_msg += "\n\n"
    return return_msg

def change_msg_to_string(msg):
    return_str = ""
    for m in msg:
        return_str+= str(m) + " "
    return_str += '\n'
    return return_str


def error_msg(msg):
    string = change_msg_to_string(msg)
    return "? " + string


async def handle_input(reader, writer):
    # Read the buffer data
    data = await reader.readline()
    time_received = time.time()
    msg = data.decode()
    # The data already ends in a newline
    logging.info("RECEIVED: " + msg)
    msg = msg.strip().split()
    out_msg = ''
    if is_valid_input(msg):
        if msg[0] == "AT":
            if "+" in msg[2]:
                timevar = float(msg[2][1:])
            else:
                timevar = - float(msg[2][1:])
            if msg[3] not in clients:
                clients[msg[3]] = [msg[1], process_latlong(msg[4]), float(msg[5]), timevar + float(msg[5])]
                asyncio.ensure_future(flood(change_msg_to_string(msg)))
            else:
                if float(msg[5]) > clients[msg[3]][2]:
                    clients[msg[3]] = [msg[1], process_latlong(msg[4]), float(msg[5]), timevar + float(msg[5])]
                    asyncio.ensure_future(flood(change_msg_to_string(msg)))
        else:
            if msg[0] == "IAMAT":
                logging.info("IAMAT MSG")
                out_msg = await generate_iamat_out(msg, time_received)
                asyncio.ensure_future(flood(out_msg))

            elif msg[0] == "WHATSAT":
                logging.info("WHATSAT MSG")
                out_msg = await generate_whatsat(msg,time_received)
            logging.info("sending: " + out_msg)
            writer.write(out_msg.encode())
            await writer.drain()
    else:
        out_msg = error_msg(msg)
        logging.error("Error: " + out_msg)
        writer.write(out_msg.encode())
        await writer.drain()


async def flood(msg):
    logging.info("FLOODING")
    for serv in comm[sys.argv[1]]:
        logging.info(f"Connecting to server {serv} that has port # {ports[serv]}")
        try:
            logging.info(f"Sending {msg} to {serv}")
            reader, writer = await asyncio.open_connection('127.0.0.1', ports[serv], loop=loop)
            writer.write(msg.encode())
            await writer.drain()
            writer.close()
        except Exception as e:
            # Could not connect
            logging.info(f"Failed to Connect to {serv}")


def main():
    if len(sys.argv)!=2:
        print("Not right num of arguments")
        sys.exit(1)
    if not sys.argv[1] in ports:
        print("not valid server name")
        sys.exit(1)
    server_name = sys.argv[1]
    logging.basicConfig(filename=server_name + "_log.txt", level=logging.DEBUG)
    global loop
    loop = asyncio.get_event_loop()
    coro = asyncio.start_server(handle_input, '127.0.0.1', ports[sys.argv[1]], loop=loop)
    logging.info(server_name + " started")
    server = loop.run_until_complete(coro)

    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()
    logging.info(server_name + " closed")




if __name__ == '__main__':
    main()
