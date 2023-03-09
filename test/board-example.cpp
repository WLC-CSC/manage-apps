#include "led-matrix.h"

using rgb_matrix::RGBMatrix;

int main() {
    RGBMatrix::Options options;
    options.rows = 32;
    options.cols = 64;
    options.hardware_mapping = "adafruit-hat-pwm";
    options.chain_length = 4;
    options.pixel_mapper_config = "V-mapper;Rotate:90";

    rgb_matrix::RuntimeOptions runtime;
    runtime.drop_privileges = 1;
    runtime.gpio_slowdown = 4;

    RGBMatrix* matrix = RGBMatrix::CreateFromOptions(options, runtime);
    if (matrix == nullptr) {
        return 1;
    }

    auto canvas = matrix->CreateFrameCanvas();

    for (int i = 0; i < 10000; ++i) {
        canvas->Fill(255, 0, 0);
        canvas = matrix->SwapOnVSync(canvas);
    }

    delete matrix;
}
