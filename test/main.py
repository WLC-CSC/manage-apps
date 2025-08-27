from rgbmatrix import RGBMatrix, RGBMatrixOptions

import sys

# Configuration for the matrix
options = RGBMatrixOptions()
options.rows = 32
options.cols = 64
options.chain_length = 4
options.hardware_mapping = "adafruit-hat-pwm"
options.pixel_mapper_config = "V-mapper:Rotate:90"
options.gpio_slowdown = 4
options.drop_privileges = True

matrix = RGBMatrix(options=options)
canvas = matrix.CreateFrameCanvas()

height = options.cols * options.chain_length
width = options.rows * options.chain_length
x, y = 0, 0
while True:
    canvas.Clear()
    canvas.SetPixel(y, x, 255, 0, 0)
    x = (x+1) % width
    canvas = matrix.SwapOnVSync(canvas)
