<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
<head>
<!-- Generated by HsColour, http://code.haskell.org/~malcolm/hscolour/ -->
<title>System/Hardware/Arduino/SamplePrograms/Blink.hs</title>
<link type='text/css' rel='stylesheet' href='hscolour.css' />
</head>
<body>
<pre><a name="line-1"></a><span class='hs-comment'>-------------------------------------------------------------------------------</span>
<a name="line-2"></a><span class='hs-comment'>-- |</span>
<a name="line-3"></a><span class='hs-comment'>-- Module      :  System.Hardware.Arduino.SamplePrograms.Blink</span>
<a name="line-4"></a><span class='hs-comment'>-- Copyright   :  (c) Levent Erkok</span>
<a name="line-5"></a><span class='hs-comment'>-- License     :  BSD3</span>
<a name="line-6"></a><span class='hs-comment'>-- Maintainer  :  erkokl@gmail.com</span>
<a name="line-7"></a><span class='hs-comment'>-- Stability   :  experimental</span>
<a name="line-8"></a><span class='hs-comment'>--</span>
<a name="line-9"></a><span class='hs-comment'>-- The /hello world/ of the arduino world, blinking the led.</span>
<a name="line-10"></a><span class='hs-comment'>-------------------------------------------------------------------------------</span>
<a name="line-11"></a>
<a name="line-12"></a><span class='hs-keyword'>module</span> <span class='hs-conid'>System</span><span class='hs-varop'>.</span><span class='hs-conid'>Hardware</span><span class='hs-varop'>.</span><span class='hs-conid'>Arduino</span><span class='hs-varop'>.</span><span class='hs-conid'>SamplePrograms</span><span class='hs-varop'>.</span><span class='hs-conid'>Blink</span> <span class='hs-keyword'>where</span>
<a name="line-13"></a>
<a name="line-14"></a><span class='hs-keyword'>import</span> <span class='hs-conid'>Control</span><span class='hs-varop'>.</span><span class='hs-conid'>Monad</span>       <span class='hs-layout'>(</span><span class='hs-varid'>forever</span><span class='hs-layout'>)</span>
<a name="line-15"></a>
<a name="line-16"></a><span class='hs-keyword'>import</span> <span class='hs-conid'>System</span><span class='hs-varop'>.</span><span class='hs-conid'>Hardware</span><span class='hs-varop'>.</span><span class='hs-conid'>Arduino</span>
<a name="line-17"></a>
<a name="line-18"></a><a name="blink"></a><span class='hs-comment'>-- | Blink the led connected to port 13 on the Arduino UNO board.</span>
<a name="line-19"></a><span class='hs-comment'>--</span>
<a name="line-20"></a><span class='hs-comment'>-- Note that you do not need any other components to run this example: Just hook</span>
<a name="line-21"></a><span class='hs-comment'>-- up your Arduino to the computer and make sure StandardFirmata is running on it.</span>
<a name="line-22"></a><span class='hs-comment'>-- However, you can connect a LED between Pin13 and GND if you want to blink an</span>
<a name="line-23"></a><span class='hs-comment'>-- external led as well, as depicted below:</span>
<a name="line-24"></a><span class='hs-comment'>--</span>
<a name="line-25"></a><span class='hs-comment'>--  &lt;&lt;<a href="http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Blink.png">http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Blink.png</a>&gt;&gt;</span>
<a name="line-26"></a><span class='hs-definition'>blink</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
<a name="line-27"></a><span class='hs-definition'>blink</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>withArduino</span> <span class='hs-conid'>False</span> <span class='hs-str'>"/dev/cu.usbmodemFD131"</span> <span class='hs-varop'>$</span> <span class='hs-keyword'>do</span>
<a name="line-28"></a>           <span class='hs-keyword'>let</span> <span class='hs-varid'>led</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>digital</span> <span class='hs-num'>13</span>
<a name="line-29"></a>           <span class='hs-varid'>setPinMode</span> <span class='hs-varid'>led</span> <span class='hs-conid'>OUTPUT</span>
<a name="line-30"></a>           <span class='hs-varid'>forever</span> <span class='hs-varop'>$</span> <span class='hs-keyword'>do</span> <span class='hs-varid'>digitalWrite</span> <span class='hs-varid'>led</span> <span class='hs-conid'>True</span>
<a name="line-31"></a>                        <span class='hs-varid'>delay</span> <span class='hs-num'>1000</span>
<a name="line-32"></a>                        <span class='hs-varid'>digitalWrite</span> <span class='hs-varid'>led</span> <span class='hs-conid'>False</span>
<a name="line-33"></a>                        <span class='hs-varid'>delay</span> <span class='hs-num'>1000</span>
</pre></body>
</html>