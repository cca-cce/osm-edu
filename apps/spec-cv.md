# SPEC: Edge detector

## 1. Purpose
Show how edge detection works: upload a picture, adjust two thresholds, and see the
edges appear.

## 2. Input
- An image uploaded by the user (JPG or PNG).
- If none is uploaded, a sample image drawn in code (shapes and text on a gradient),
  so the app works without any files.

## 3. Processing (OpenCV)
1. Convert to grayscale.
2. Smooth with a Gaussian blur (kernel size from a slider: 1, 3, 5 or 7).
3. Canny edge detection with a low and a high threshold (sliders, 0-255).
4. Optionally draw the edges in colour on top of the original image.

## 4. Interface (Gradio, shown in the notebook)
- Image upload, three sliders (blur, low threshold, high threshold), an overlay checkbox.
- The original and the edge image side by side, updated when a slider moves.
- The share of edge pixels shown as a percentage.
- A button to download the edge image as PNG.

## 5. Constraints
- One notebook cell: %pip install opencv-python-headless gradio
- Use opencv-python-headless (no desktop windows) so it runs in Colab.
- Run with demo.launch() so the interface appears below the cell.

## 6. Acceptance checks
- A1 Without an upload, the sample image and its edges are shown.
- A2 Raising the low threshold gives fewer edge pixels.
- A3 If the low threshold is above the high one, the app explains the problem instead
  of failing.
- A4 The downloaded PNG matches the edge image shown.
