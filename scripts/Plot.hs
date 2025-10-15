#! /usr/bin/env cabal
{- cabal:
   build-depends: base, bytestring, cassava, Chart, Chart-diagrams, containers, split, text, vector
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

import qualified Data.ByteString.Lazy as BL
import           Data.Csv hiding ((.=))
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List.Split (splitOn)
import           Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import           Data.Functor (void)
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Backend.Diagrams

-- | Represents a single data point extracted from the CSV.
data PlotPoint = PlotPoint
  { ppExperiment :: Text   -- ^ Experiment name (find, insert, etc).
  , ppBaseline   :: Text   -- ^ Baseline name.
  , ppN          :: Double -- ^ Number of elements in the experiment (X-axis).
  , ppValue      :: Double -- ^ Time measurement (Y-axis).
  , ppStdev      :: Double -- ^ Standard deviation of the measurement.
  } deriving (Show)

-- | A helper type to decode the one row (= 7 columns) of the CSV.
type CsvRow = (Text, Double, Double, Double, Double, Double, Double)

type SeriesData = (Double, Double, Double) -- (n, time, stdev)

-- | Parses the "Comparison/..." string and creates a PlotPoint.
-- Returns Nothing if the string format is invalid.
parsePlotPoint :: CsvRow -> Maybe PlotPoint
parsePlotPoint (key, val, _, _, stdev, _, _) =
  case splitOn "/" (T.unpack key) of
    ["Comparison", nStr, expName, baseName] ->
      case reads nStr of
        [(n, "")] -> Just $ PlotPoint (T.pack expName) (T.pack baseName) n val stdev
        _         -> Nothing -- 'n' is not a valid integer
    _ -> Nothing -- The key format is wrong

-- | Reads and parses the CSV file from the given path.
-- It only decodes the first two columns and skips rows that don't match the format.
readCsvData :: FilePath -> IO [PlotPoint]
readCsvData path = do
  csvData <- BL.readFile path
  case decode HasHeader csvData of
    Left err -> do
      putStrLn $ "Error parsing CSV: " ++ err
      return []
    Right (rows :: V.Vector CsvRow) ->
      return $ mapMaybe parsePlotPoint (V.toList rows)

-- | Groups the data points first by experiment name, then by baseline name.
-- The result is a map where each key is an experiment, and the value is
-- another map of baselines to their corresponding (x, y) data series.
groupData :: [PlotPoint] -> Map.Map Text (Map.Map Text [SeriesData])
groupData = foldr addPoint Map.empty
  where
    addPoint (PlotPoint expName baseName n val stdev) =
      Map.alter (Just . addBaselineData) expName
      where
        addBaselineData Nothing = Map.singleton baseName [(n, val, stdev)]
        addBaselineData (Just baselineMap) =
          Map.alter (Just . addDataPoint) baseName baselineMap
        addDataPoint Nothing     = [(n, val, stdev)]
        addDataPoint (Just series) = (n, val, stdev) : series

mycolors :: String -> Colour Double
mycolors = \case
  "hashtables" -> green
  "hashtables basic" -> darkgreen
  "vector-hashtables" -> orange
  "vector-hashtables unboxed" -> red
  "vector-hashtables unboxed keys" -> indianred
  "vector-hashtables boxed" -> darkkhaki
  "vector-hashtables (frozen)" -> sienna
  "mutable vector boxed" -> dodgerblue
  "mutable vector" -> darkorchid
  b -> error $ "mycolors: Unknown baseline: " ++ show b

-- | Generates a plot for a single experiment and saves it to a PNG file.
generatePlot :: Text -> Map.Map Text [SeriesData] -> IO ()
generatePlot experimentName baselineData = do
  let fileName = T.unpack (sanitizeName experimentName) ++ ".svg"
  toFile (def {_fo_format = SVG_EMBEDDED} {- default is PNG -}) fileName $ do
    layout_title .= T.unpack experimentName
    layout_x_axis . laxis_title .= "# elements"
    layout_x_axis . laxis_generate .= autoScaledLogAxis def
    layout_y_axis . laxis_title .= "time (s)"
    layout_y_axis . laxis_generate .= autoScaledLogAxis def

    setShapes $ repeat PointShapeCross
    -- Create a line plot for each baseline
    mapM_ (\(baseline, series') -> do
                let color = mycolors (T.unpack baseline)
                    series = map (\(n,t,_)->(n,t)) series'
                setColors [opaque color]
                plot (line (T.unpack baseline) [series])
                plot $ liftEC do
                        plot_fillbetween_style .= solidFillStyle (withOpacity color 0.4)
                        plot_fillbetween_values .= [(n, (time - stdev, time + stdev)) | (n, time, stdev) <- series']
                        plot_fillbetween_title .= (T.unpack baseline) ++ " (st. dev.)"

                setColors [opaque red]
                plot (points "" series)
          )
          (Map.toList baselineData)

sanitizeName :: Text -> Text
sanitizeName
  = T.replace " " "_"
  . T.replace "(" "_"
  . T.replace ")" "_"

main :: IO ()
main = do
  putStrLn "Processing CSV file: results.csv"
  points <- readCsvData "results.csv"

  if null points
    then putStrLn "No valid data points found. Exiting."
    else do
      let grouped = groupData points
      putStrLn $ "Found data for experiments: " ++ show (Map.keys grouped)

      void $ Map.traverseWithKey generatePlot grouped

      putStrLn "Plots generated successfully."
