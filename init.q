////////////////////////////////////////////////////////////////////////////////////
// init.q - this is the init script to load the QR library
///

// loading the QR library
init:`:./QR 2:(`QR__init;1);
QRLib:init[];
value each {x, "::QRLib[", "`", x, "]"} each QRLib[`.qr.toString] each QRLib[`.qr.initAPI][];
 .z.ts:{show .z.p; QRLib[`.qr.util.timer.tick][];};

/
// init your rserver and qml here
// QHOME proivdes functionaltiy of dynamic loading your Q library, by following the QR framework
// R server provides fuctionality to called R within Q (embededR)
// QML provides functionality to call LAPAC (QML)
QRLib[`.qr.initQHOME][`$"{Your Q HOME}"];
QRLib[`.qr.initR][`$"{Your R Server}"];
QRLib[`.qr.initQML][`$"{Your QML}"];