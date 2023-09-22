"""Test interacting with manage apps"""
import requests

ip = 'localhost'
ip = '172.16.42.52'
port = 4006

def post(endpoint, **kwargs):
    """Send data using an HTTP post"""
    url = f'http://{ip}:{port}/{endpoint}'
    r = requests.post(url, **kwargs)
    if r.ok:
        print(r.json())
    else:
        print(r.text)

def upload(filename):
    with open(filename, 'rb') as f:
        post('upload', files={filename: f})

def start():
    post('start')

def stop():
    post('stop')

def info():
    post('info')
        
if __name__ == '__main__':
    filename = 'main.cpp'
    #filename = 'test.py'
    upload(filename)
