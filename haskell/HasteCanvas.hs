module Main where

--
-- Basic example of how to draw on an HTML5 Canvas using Haskell
-- compiled to Javascript with the Haste compiler (https://haste-lang.org/)
--
-- Build with something like:
--
--     hastec --make HasteCanvas.hs -o haste-canvas.js
--
-- Then use in HTML5 page something like:
--
--     <canvas id="the-canvas" width=640 height=200></canvas>
--     <button id="draw-button">Draw</button>
--     <script src="haste-canvas.js"></script>
--

import Haste.DOM              -- https://haste-lang.org/docs/haddock/0.5.5/Haste-DOM.html
import Haste.Events           -- https://haste-lang.org/docs/haddock/0.5.5/Haste-Events.html
import Haste.Graphics.Canvas  -- https://haste-lang.org/docs/haddock/0.5.5/Haste-Graphics-Canvas.html


main = do
    Just drawButtonElem <- elemById "draw-button"
    onEvent drawButtonElem Click handleClick
    where
        handleClick _ = do
            Just canvasElem <- getCanvasById "the-canvas"
            render canvasElem $ makePicture
            return ()

makePicture = do
    setStrokeColor $ RGB 255 0 0
    stroke $ line (0, 0) (100, 100)
