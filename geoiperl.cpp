#include <cstdio>
#include <cstring>
#include <string>
#include <sstream>
#include "erl_driver.h"
#include "GeoIP.h"

#define DRIVER_NAME geoip
#define DRIVER_NAME_STRING "geoip"

using namespace std;

enum { EVENT_OPEN=0, EVENT_USE_BINARY, EVENT_USE_STRING, EVENT_COUNTRY_BY_IP };


class GeoIPErl 
{
	std::string dbName;
	GeoIP *geoip;
	ErlDrvPort port;
	int returnType; //0 for string, PORT_CONTROL_FLAG_BINARY for binary
	
public:
	GeoIPErl() :
		geoip(0), port(0), returnType(0)
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

	static inline void makeMsg(int returnType, const char *msg, char **rbuf, int bufLen, int &rlen)
	{
		rlen = strlen(msg);
		if(rlen <= bufLen)
		{ 
			std::memcpy(*rbuf,msg,rlen);
		}
		else
		{
			//alloc some extra mem
			if(returnType == 0)
			{
				*rbuf = (char*)driver_alloc(rlen);
				std::memcpy(*rbuf,msg,rlen);
			}
			else
			{
				ErlDrvBinary *buf = driver_alloc_binary(rlen);
				*rbuf = (char*)buf;
				std::memcpy(buf->orig_bytes,msg,rlen);
			}
			
		}
		
	}
	
	static inline void makeEmptyMsg(char **rbuf, int &rlen)
	{
		*rbuf = NULL;
		rlen = 0;
	}
	
	//static void onOutput(ErlDrvData handle, char *str, int len)
	static int onControl(ErlDrvData handle, unsigned int command, char *str, int len, char **rbuf, int rlen)
	{
		GeoIPErl* d = reinterpret_cast<GeoIPErl*>(handle);
		register int ret;
		switch(command)
		{
			//param: char Type, char *Name
			case EVENT_OPEN:
				if(d->geoip)
					GeoIP_delete(d->geoip);
				
				d->dbName = str+1;
				
				d->geoip = GeoIP_open(d->dbName.c_str(),str[0]);
				if(d->geoip == 0)
				{
					char *errorMsg = "error opening geoip database";
					makeMsg(d->returnType,errorMsg, rbuf, rlen, ret);
				}
				else
				{
					makeEmptyMsg(rbuf,ret);
				}
				
				break;
				
			case EVENT_USE_STRING:
				set_port_control_flags(d->port, 0);
				d->returnType = 0;
				makeEmptyMsg(rbuf,ret);
				break;
				
			case EVENT_USE_BINARY:
				set_port_control_flags(d->port, PORT_CONTROL_FLAG_BINARY);
				d->returnType = PORT_CONTROL_FLAG_BINARY;
				makeEmptyMsg(rbuf,ret);
				break;

				
			//param: char *IP 		
			case EVENT_COUNTRY_BY_IP:
				if(d->geoip != 0)
				{
					const char *country = GeoIP_country_name_by_addr(d->geoip, str);
					if(country)
					{
						//d->msg((char*)country);
						makeMsg(d->returnType,country, rbuf, rlen, ret);
					}
					else
					{
						makeEmptyMsg(rbuf,ret);
					}
				}
				else
				{
					char *errorMsg = "Geoip has not been initialized!";
					makeMsg(d->returnType,errorMsg, rbuf, rlen, ret);
				}
				
				break;
				
			default:
				makeEmptyMsg(rbuf,ret);
		}
		
		return ret;

	}
};




// C code goes below


static 
ErlDrvEntry driver = {
    NULL,               /* F_PTR init, N/A */
    &GeoIPErl::onStart,        /* L_PTR start, called when port is opened */
    &GeoIPErl::onStop,        /* F_PTR stop, called when port is closed */
    NULL,      /* F_PTR output, called when erlang has sent */
    NULL,               /* F_PTR ready_input */
    NULL,               /* F_PTR ready_output */
    DRIVER_NAME_STRING, /* char *driver_name, the argument to open_port */
    NULL,               /* F_PTR finish, called when unloaded */
    NULL,               /* Not used */
    &GeoIPErl::onControl,                       /* F_PTR control, port_command callback */
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
	NULL				 /* stop_select -- Called to close an event object */
};

extern "C" 
{
	DRIVER_INIT(DRIVER_NAME) /* must match name in driver_entry */
	{
		return &driver;
	}
}
