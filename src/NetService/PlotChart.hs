{-# LANGUAGE RecordWildCards #-}
module NetService.PlotChart
     where
import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import qualified Data.Maybe                             as Maybe
import qualified Data.Time                              as Time
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Lib                                    (tzAsiaTokyo)
import qualified Model

-- |
--
data ChartData = ChartData
    { cSize   :: (Int, Int)
    , cTitle  :: String
    , cOhlcvs :: [Model.Ohlcv]
    }

-- |
--
chart :: ChartData -> Renderable ()
chart chartData =
    toRenderable layout
    where
    layout = layout_title .~ cTitle chartData
           $ layout_background .~ solidFillStyle (withOpacity white 0.9)
           $ layout_left_axis_visibility . axis_show_ticks .~ False
           $ layout_plots .~ [toPlot candle]
           $ def



    candle  = plot_candle_line_style  .~ lineStyle 1 black
            $ plot_candle_fill        .~ True
            $ plot_candle_rise_fill_style .~ solidFillStyle (opaque blue)
            $ plot_candle_fall_fill_style .~ solidFillStyle (opaque red)
            $ plot_candle_tick_length .~ 0
            $ plot_candle_width       .~ 2
            $ plot_candle_values      .~ mapOhlcvs (cOhlcvs chartData)
            $ def

    mapOhlcvs :: [Model.Ohlcv] -> [Candle Time.LocalTime Double]
    mapOhlcvs = Maybe.mapMaybe fromOhlcv

    fromOhlcv :: Model.Ohlcv -> Maybe (Candle Time.LocalTime Double)
    fromOhlcv Model.Ohlcv{..} = do
        candle_x    <- Just (Time.utcToLocalTime tzAsiaTokyo ohlcvAt)
        candle_low  <- ohlcvLow
        candle_open <- ohlcvOpen
        candle_mid  <- Just 0
        candle_close<- ohlcvClose
        candle_high <- ohlcvHigh
        Just Candle{..}

    lineStyle n colour = line_width .~ n
                       $ line_color .~ opaque colour
                       $ def

-- |
--
plotSVG :: FilePath -> ChartData -> IO (PickFn ())
plotSVG fpath chartData =
    renderableToFile (FileOptions (cSize chartData) SVG) fpath $ chart chartData

-- |
--
plotPNG :: FilePath -> ChartData -> IO (PickFn ())
plotPNG fpath chartData =
    renderableToFile (FileOptions (cSize chartData) PNG) fpath $ chart chartData


