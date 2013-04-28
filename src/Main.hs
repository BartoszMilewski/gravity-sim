{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
module Main where

import Yesod
import Data.Aeson
import System.Environment (getEnv)
import qualified Control.Exception as E

import Json
import World
import Physics
import Simulation

-- to do:
-- add Pause button
-- set maximum time for simulation
-- make curWorld a local variable

data Gravity = Gravity

instance Yesod Gravity

mkYesod "Gravity" [parseRoutes|
  /        HomeR    GET
  /advance AdvanceR POST
  /solar   SolarR   GET
  /world4  World4R  GET
|]

boxColor   = "#000"::String
bodyColor  = "#333"::String
boxSizeX    = 600::Int
boxSizeY    = 600::Int
framesPerS = 16::Int

getHomeR = defaultLayout $ do
  setTitle "Gravity"

  [whamlet|
    <div #box>
      <p>
      <canvas #sky width=#{boxSizeX} height=#{boxSizeY}> 
         Your browser doesn't support HTML 5
      <p>
        Gravitational interaction demo based on one of 
        <a href="http://www.cse.unsw.edu.au/~chak/" target="_blank">Manuel Chakravarty</a>'s 
        Haskell course exercises. The simulation is done in Haskell on the server. 
        Client code uses HTML 5 to display instantaneous positions of bodies. 
        It communicates with the (stateless) server using JSON. The web site is written in 
        <a href="http://www.yesodweb.com/" target="_blank">Yesod</a>.
        <div>
          <button #reset>Reset
          <select>
            <option value="solar"> Inner planets
            <option value="world4"> Four stars
  |]

  toWidget [cassius|
    #box
      width:#{show boxSizeX}px
      height:#{show boxSizeY}px
      margin-left:auto
      margin-right:auto
    canvas
      background-color:#{boxColor}
    body
      background-color:#{bodyColor}
      color:#eee
      font-family:Arial,Helvetica,sans-serif
      font-size:small
    a
      text-decoration:none
      color:#bdf
    #sky
      border:1px solid #888
  |]

  addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
  toWidget [julius|
    var urls = { // map from simulation names to urls
      solar:   "@{SolarR}", 
      world4:  "@{World4R}" 
    };
    // Important: changes to global variables
    // may only happen inside JSON handlers
    var interval = 1000.0 / #{toJSON framesPerS};
    var curSimName = null;
    var curWorld = null; // constantly changing world
    var skip = 0; // skip n frames if congested

    $(document).ready(function() {
        $("#reset").click(function() {reset(curSimName);});
        $("select").change(function() {
            var str = $("select option:selected").val();
            reset(str);
        });
        $("select option[value='solar']").attr('selected', 'selected');
        reset("solar");
        setInterval(advance, interval);
    });

    function reset(simName) {
        if (!urls[simName]) 
            alert("Error: invalid simulation: " + simName);
        $.getJSON(urls[simName], function(newWorld){
            newWorld.seqNum = curWorld? curWorld.seqNum + 1: 0;
            curSimName = simName;
            curWorld = newWorld;
        });
    }

    function advance() {
        if (skip == 0) {
            draw();
            refresh();
        } else
            skip -= 1;
    }

    function refresh(simName) {
        $.ajax(
         {
           "data"    : JSON.stringify(curWorld),
           "type"    : "POST",
           "url"     : "@{AdvanceR}",
           "success" : updateWorld
         });
    }

    function updateWorld(newWorld)
    {
       if(!curWorld) alert("null world!");
       var lag = curWorld.seqNum - newWorld.seqNum;
       if (lag == 0) {
           curWorld = newWorld;
           curWorld.seqNum += 1;
       } else if (lag > 0)
           skip = lag;
       else
           alert("Time travel discovered!")
    }

    var dimX = #{toJSON boxSizeX};
    var dimY = #{toJSON boxSizeY};

    function draw() {
        if (!curWorld) return true; // might happen

        var canvas = document.getElementById('sky');
        var ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, dimX, dimY);
        ctx.fillStyle = "white";

        // Draw particles
        var partsInView = 0;
        for (var j = 0; j < curWorld.parts.length; j++) {
            var part = curWorld.parts[j];
          var size = Math.log(part.mass/curWorld.pixInKg) / Math.LN10;
          if (size < 2) size = 2;
          var x = dimX/2 + curWorld.pixInM * part.pos.x;
          var y = dimY/2 + curWorld.pixInM * part.pos.y;
          if ( x > -10 && x < dimX + 10 && y > -10 && y < dimY + 10) {
              partsInView += 1;
              ctx.beginPath(); 
              ctx.arc(x, y, size/2, 0, Math.PI * 2, true); 
              ctx.fill();
          }
      }
      return partsInView != 0;
    }
  |]


-- Server side logic

postAdvanceR :: Handler RepJson
postAdvanceR = do
    world <- parseJsonBody_
    -- user time in seconds
    let userTime = 1.0 / fromIntegral framesPerS
    let worldTime = userTime * usrToWrldTime world
    jsonToRepJson $ advanceWorld worldTime world

getSolarR  :: Handler RepJson
getSolarR  = jsonToRepJson solarWorld
getWorld4R :: Handler RepJson
getWorld4R = jsonToRepJson world4


main = do
    portEither <- getPortEither
    let port = case portEither of
                        Right val -> read val
                        Left _    -> 3000
    warp port Gravity
  where
    getPortEither :: IO (Either IOError String)
    getPortEither = E.try (getEnv "PORT")