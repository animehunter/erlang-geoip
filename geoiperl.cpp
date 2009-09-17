#include <cstdio>
#include <cstring>
#include <string>
#include <sstream>
#include "erl_driver.h"
#include "GeoIP.h"

#define DRIVER_NAME geoip
#define DRIVER_NAME_STRING "geoip"


enum { EVENT_OPEN=0, EVENT_COUNTRY_BY_IP };


class GeoIPErl 
{
	std::string dbName;
	GeoIP *geoip;
	ErlDrvPort port;

	
	inline void errorMsg(char* msg)
	{
		driver_failure_atom(port, msg);
	}
	inline void msg(char* msg)
	{
		driver_output(port, (char*)msg, std::strlen(msg));
	}
	
public:
	GeoIPErl() :
		geoip(0), port(0)
	{
		
	}
	~GeoIPErl() {}
	

public:
	static ErlDrvData onStart(ErlDrvPort port, char *buff)
	{
		GeoIPErl* d = new GeoIPErl;
		d->port = port;
		
		return (ErlDrvData)d;
	}

	static void onStop(ErlDrvData handle)
	{
		GeoIPErl* d = reinterpret_cast<GeoIPErl*>(handle);
		if(d->geoip)
			GeoIP_delete(d->geoip);
		
		delete d;
	}

	static void onOutput(ErlDrvData handle, char *str, int len)
	{
		GeoIPErl* d = reinterpret_cast<GeoIPErl*>(handle);

		//param: char EventType, Params...
		switch(str[0])
		{
			//param: char Type, char *Name
			case EVENT_OPEN:
				if(d->geoip)
					GeoIP_delete(d->geoip);
				
				d->dbName = str+2;
				
				d->geoip = GeoIP_open(d->dbName.c_str(),str[1]);
				if(d->geoip == 0)
				{
					d->errorMsg("error opening geoip database");
					return; 
				}
				
				break;
			
			//param: char *IP 		
			case EVENT_COUNTRY_BY_IP:
				if(d->geoip != 0)
				{
					const char *country = GeoIP_country_name_by_addr(d->geoip, str+1);
					if(country)
						d->msg((char*)country);
					else
						d->msg("");
				}
				else
				{
					d->errorMsg("Geoip has not been initialized!");
				}
				
				break;
				
			default:
				break;
		}

	}
};




// C code goes below
using namespace std;

static 
ErlDrvEntry driver = {
    NULL,               /* F_PTR init, N/A */
    &GeoIPErl::onStart,        /* L_PTR start, called when port is opened */
    &GeoIPErl::onStop,        /* F_PTR stop, called when port is closed */
    &GeoIPErl::onOutput,      /* F_PTR output, called when erlang has sent */
    NULL,               /* F_PTR ready_input */
    NULL,               /* F_PTR ready_output */
    DRIVER_NAME_STRING, /* char *driver_name, the argument to open_port */
    NULL,               /* F_PTR finish, called when unloaded */
    NULL,               /* Not used */
    NULL,                       /* F_PTR control, port_command callback */
    NULL,               /* F_PTR timeout, reserved */
    NULL,   /* F_PTR outputv, reserved */
    NULL,               /* F_PTR ready_async */
    NULL,               /* F_PTR flush */
    NULL,               /* F_PTR call */
    NULL,               /* F_PTR event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,               /* Reserved -- Used by emulator internally */
    NULL,               /* F_PTR process_exit */
};

extern "C" 
{
	DRIVER_INIT(DRIVER_NAME) /* must match name in driver_entry */
	{
		return &driver;
	}
}
