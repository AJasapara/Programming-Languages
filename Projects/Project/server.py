import asyncio
import time
import aiohttp
import json
import logging
import sys

# defining this globally prevents infinite looping
client_data = dict()
logging.basicConfig(filename='active_server.log',level=logging.DEBUG)

# Here's the server class that I decided to use. The option was to go 
# with either Protocol or Stream and I opted to go with Protocol
class EchoServerClientProtocol(asyncio.Protocol):
    def __init__(self, serverID, port, loop):
        self.serverID = serverID
        self.port = port
        self.apiKey = "AIzaSyBtr4jGS0XXXXXXXizHeMRTazDCiV9wkO4"
        self.loop = loop
        self.conn = {
            'Goloman': ['Hands', 'Holiday', 'Wilkes'],
            'Hands': ['Goloman', 'Wilkes'],
            'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
            'Welsh': ['Holiday'],
            'Wilkes': ['Goloman', 'Hands', 'Holiday']
        }
        self.serverPortConnection = {
            'Goloman': 11925,
            'Hands': 11926,
            'Holiday': 11927,
            'Welsh': 11928,
            'Wilkes': 11929
        }
    
    # the function used when we create a connection, it simply 
    # says a connection has been made between our client and
    # our server   
    def connection_made(self, transport):
        self.transport = transport
        self.peername = transport.get_extra_info('peername')
        madeConnectionMessage = '{0} (client) connecting to {1} server'.format(self.peername, self.serverID)
        logging.info(madeConnectionMessage)
        
    # just a function that returns that the connection is lost using the peername
    def connection_lost(self, exc):
        connectionLostMessage = '{0} lost connection of {1}'.format(self.serverID, self.peername)
        logging.info(connectionLostMessage)
        self.transport.close()

    # this function is used to to analyze the data that we receive
    # from the client and output the corresponding correct message
    # back to the client. The function validates for IAMAT, WHATSAT
    # and AT messages and checks for invalid responses as well
    def data_received(self, data):
        receivedMessage = data.decode()
        listReceivedMessage = receivedMessage.split()
        logging.info("{0} server received \"{1} message\"".format(self.serverID, listReceivedMessage))
        invalidResponse = "? " + receivedMessage
        while True:
            if listReceivedMessage[0] == "IAMAT":
                client_name = listReceivedMessage[1]
                message_coords = listReceivedMessage[2]
                message_time = listReceivedMessage[3]
                if ((client_name in client_data) and (float(client_data[client_name][1]) == float(message_time))):
                    break
                client_data[client_name] = [message_coords, message_time, self.serverID]
                for loc in message_coords[1:]:
                    if loc == '+' or loc == '-':
                        message_coords = message_coords[1:].split(loc)
                if float(message_coords[0]) > 180 or float(message_coords[1]) > 180:
                    logging.error("Coordinates are invalid, here's the response")
                    logging.error(invalidResponse)
                    self.transport.write(invalidResponse.encode())
                    break
                self.send_message(client_data, client_name)
                asyncio.ensure_future(
                    self.flood(self.create_response(client_data, client_name)))
                break
            elif listReceivedMessage[0] == "WHATSAT":
                client_name = listReceivedMessage[1]
                if client_name not in client_data:
                    logging.error("IAMAT was never sent by the client, here's the response")
                    logging.error(invalidResponse)
                    self.transport.write(invalidResponse.encode())
                    break
                client_location = client_data[client_name][0]
                for loc in client_location[1:]:
                    if loc == '-' or loc == '+':
                        client_location = ("," + loc).join(client_location[1:].split(loc))
                required_radius = listReceivedMessage[2]
                if float(required_radius) > 50 or float(required_radius) < 0:
                    logging.error("The radius is out of range, here's the response")
                    logging.error(invalidResponse)
                    self.transport.write(invalidResponse.encode())
                    break
                num_res = listReceivedMessage[3]
                if int(num_res) > 20 or int(num_res) < 0:
                    logging.error("The number of results is out of range, here's the response")
                    logging.error(invalidResponse)
                    self.transport.write(invalidResponse.encode())
                    break
                self.send_message(client_data, client_name)
                asyncio.ensure_future(self.query(required_radius, client_location, num_res))
                break
            elif listReceivedMessage[0] == "AT":
                message_coords = listReceivedMessage[4]
                message_time = listReceivedMessage[5]
                original_server = listReceivedMessage[1]
                client_name = listReceivedMessage[3]
                if ((float(client_data[client_name][1]) == float(message_time)) and (client_name in client_data)):
                    break
                client_data[client_name] = [message_coords, message_time, original_server]
                asyncio.ensure_future(self.flood(self.create_response(client_data, client_name)))
            else:
                logging.error("An incorrect command was given, here's the response")
                logging.error(invalidResponse)
                self.transport.write(invalidResponse.encode())
                break

    # This function helps in formatting the AT message that responds to
    # the IAMAT and WHATSAT messages
    def create_response(self, client_data, server_name):
        diff_time = time.time() - float(client_data[server_name][1])
        server_response_one = "AT " + client_data[server_name][2]
        server_response_two = " "
        if diff_time > 0:
            server_response_two = " +"
        server_response_three = (str(diff_time) + " " + server_name + " " + client_data[server_name][0] + " " + client_data[server_name][1])
        server_response = server_response_one + server_response_two + server_response_three
        return server_response

    # This function is used to send the correct message back to the client
    def send_message(self, client_data, server_name):
        server_response = self.create_response(client_data, server_name)
        logging.info("{0} server writes \"{1}\" message to client".format(self.serverID, server_response))
        self.transport.write(server_response.encode())
        self.transport.write("\n".encode())
        
    # This function was created to query the Google Places API. The function
    # is asynchronous and takes in the query and outputs the JSON object. The
    # good thing about the asyncio module is that it contains helper functions
    # for such tasks with json as it has load and dump that would help us in not
    # parsing through the JSON object manually
    async def query(self, radius, coordinates, num_res):
        coordinates_index = coordinates.rfind('-') if coordinates.rfind('-') != -1 else coordinates.rfind('+')
        coordinates = coordinates[:coordinates_index+1] + "," + coordinates[coordinates_index:]
        radius = str(int(radius) * 1000)
        url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=" + coordinates + "&radius=" + radius + "&key=" + self.apiKey
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.text = (await response.text())
                res = json.loads(response.text)
                res['results'] = res['results'][0:int(num_res) - 1]
                dumped_results = json.dumps(res, indent = 3)
                logging.info("{0} server writes the JSON to client:\n{1}".format(self.serverID, dumped_results))
                dumped_results += "\n"
                self.transport.write(dumped_results.encode())

    # This function is used to flood the message from the server to all other
    # servers that it talks to or is connected to. The function is also asynchronous
    # and uses the await keyword as well as the drain function
    async def flood(self, message):
        for server_index in self.conn[self.serverID]:
            connectionOpenedMessage = "{0} server opened connection to {1} server".format(self.serverID, server_index)
            logging.info(connectionOpenedMessage)
            try:
                reader, writer = await asyncio.open_connection('127.0.0.1', self.serverPortConnection[server_index], loop=self.loop)
                writer.write(message.encode())
                logging.info("{0} server write \"{1}\" message to {2} server".format(self.serverID, message, server_index))
                await writer.drain()
                closedConnectionMessage = "{0} server closed connection to {1} server".format(self.serverID, server_index)
                logging.info(closedConnectionMessage)
                writer.close()
            except ConnectionRefusedError:
                connection_error = "{0} server could not connect to {1} server".format(self.serverID, server_index)
                logging.warning(connection_error)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(1)
    serverID = sys.argv[1]
    loop = asyncio.get_event_loop()
    if serverID == "Goloman":
        port = 11925
    elif serverID == "Hands":
        port = 11926
    elif serverID == "Holiday":
        port = 11927
    elif serverID == "Welsh":
        port = 11928
    elif serverID == "Wilkes":
        port = 11929
    else:
        sys.exit(1)
    coroutine = loop.create_server(lambda: EchoServerClientProtocol(serverID, port, loop),'127.0.0.1', port)
    active_server = loop.run_until_complete(coroutine)
    server_message = 'Serving {0} on {1}'.format(serverID, active_server.sockets[0].getsockname())
    logging.info(server_message)
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass
    active_server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()
