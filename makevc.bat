rem replace C:\Program Files\erl5.7.1\erts-5.7.1\include with your latest version of erlang

cl /MT /LD /O2 /Ox /Ot /Fegeoip.dll /I"C:\Program Files\erl5.7.1\erts-5.7.1\include" GeoIP.c geoiperl.cpp ws2_32.lib
pause