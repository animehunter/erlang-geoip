rem replace C:\Program Files\erl5.7.3\erts-5.7.3\include with your latest version of erlang

cl /MT /LD /O2 /Ox /Ot /Fegeoip.dll /I"C:\Program Files\erl5.7.3\erts-5.7.3\include" GeoIP.c geoiperl.cpp ws2_32.lib
pause