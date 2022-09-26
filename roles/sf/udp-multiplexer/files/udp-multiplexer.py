#!/usr/bin/env python3

import argparse
import socket
import sys


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('listen_port', type=int, default=sys.argv[1],
                        help='A port that host will listen to')
    parser.add_argument('dest1_host', type=str, default=sys.argv[2],
                        help='Host1 destination IP address or domain')
    parser.add_argument('dest1_port', type=int, default=sys.argv[3],
                        help='Host1 destination port')
    parser.add_argument('dest2_host', type=str, default=sys.argv[4],
                        help='Host2 destination IP address or domain')
    parser.add_argument('dest2_port', type=int, default=sys.argv[5],
                        help='Host2 destination port')
    parser.add_argument('--prom-port', type=int, default=9108,
                        help='A port that Prometheus will check backend')
    return parser.parse_known_args(sys.argv[1:])[0]


def _check_connection(sock, host, port):
    return True if sock.connect_ex((host, port)) == 0 else False


def forward(data, port, args):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    for host, port in [(args.dest1_host, args.dest1_port),
                       (args.dest2_host, args.dest2_port)]:
        try:
            if _check_connection(sock, host, port):
                sock.sendto(data, (host, port))
            else:
                print("Couldn't connect to", host, port)
        except socket.gaierror:
            print("Some problems occured on sending package to  host %s:%s" % (
                  host, port))
        except ConnectionRefusedError:
            print("Connection refused to host %s:%s" % (host, port))
        except Exception as e:
            print(e)


def listen(host, port, args):
    listenSocket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    print("listening to", host, port)
    listenSocket.bind((host, port))
    while True:
        data, addr = listenSocket.recvfrom(1024)
        forward(data, addr[1], args)


if __name__ == "__main__":
    args = get_args()
    listen("127.0.0.1", args.listen_port, args)
