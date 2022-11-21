/*
	This header file contains general definitions that
	are common to several libraries.  This header must
	be included in any source code using these libraries.
	At present the included list of libraries are:

			gclib
			dblib
			lists
			utils

	Developed October 6, 1995: kam. 
	
	Ken Masarie
	Cooperative Institute for Research in Environmental Sciences
	University of Colorado
	National Oceanic and Atmospheric Administration

	kmasarie@cmdl.noaa.gov
*/
/*
General include file
*/
#include 	<stdio.h>
#include 	<stdlib.h>
#include	<string.h>
#include	<math.h>
#include	<ctype.h>
#include	<sys/types.h>
#include	<sys/time.h>
#include	<sys/stat.h>
#include	<dirent.h>
#include	<values.h>
#include	<time.h>

/************************************
	GENERAL Definitions
*************************************/

#define		DEFAULT		-999.99

#define		REC_LEN		256

#define		SMSIZE		128
#define		MINSIZE		256
#define		MIDSIZE		1024
#define		MAXSIZE		30000

#define		LAT		0
#define		LON		1
	
#define		CH4		0
#define		CO		1
#define		H2		2
#define		N2O		3
#define		SF6		4
#define		CO2		5
#define		C13CO2		6
#define		O18CO2		7
#define		C13CH4		8

#define		NO		0
#define		YES		1

#define		FALSE		0
#define		TRUE		1

#define		BCLEN		5
#define		FLAGLEN		4
#define		IDLEN		9
#define		PORTLEN		3
#define		STALEN		4
#define		SITELEN		67+1
#define		INSTLEN		3

typedef char	reclen[REC_LEN];

typedef	char	bclen[BCLEN];
typedef	char	stalen[STALEN];
typedef	char	flaglen[FLAGLEN];
typedef	char	idlen[IDLEN];
typedef	char	portlen[PORTLEN];
typedef	char	rawlen[MIDSIZE];
typedef char	sitelen[SITELEN];
typedef char	instlen[INSTLEN];

static	char	*defhead=
		"selected on 1900 12 31 thru sample 12345-78 adate 2001 03 15";

static int 	dim[2][13] = {{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
			 {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}};

static char 	*month_name[] = {"N/A", "JAN", "FEB", "MAR", "APR", "MAY",
				 "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", 
				 "DEC"};

static int 	diy[2][13] = {
		{ -9, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 },
		{ -9, 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 }};

static	char	*mode_str[]={"single","double","multi"};

/************************************
	GCLIB Definitions
************************************/

#define			NUM_MODES	3
	
#define			SINGLE		0
#define			DOUBLE		1
#define			MULTI		2

#define			HEIGHT		0
#define			AREA		1

typedef	struct {
	char		inst[MIDSIZE];
	int		nrefs;
	float		*refval;
	idlen		*refid;
	int		nbclist;
	bclen		*bclist;
} headinfo;
	
typedef	struct {
	stalen		sta;
	idlen		id;
	char		meth;
	portlen		port;
	int		yr,mo,dy,hr,mn;
	int		ayr,amo,ady,ahr,amn;
	double		pkht,pkar;
	float		rt;
	bclen		bc;
	rawlen		diag;
	float		ar_mr,ht_mr;
	flaglen		ar_flag,ht_flag;
} rawdata;
	
typedef struct {
	int		nraw;
	rawdata		*data;
} rawinfo;

typedef	struct {
	double		pk;
	bclen		bc;
	int		flag;
} sampdata;

typedef	struct {
	double		pk;
	bclen		bc;
	idlen		id;
	int		flag;
} refdata;

typedef	struct {
	int		nspeaks;
	int		nrpeaks;
	sampdata	*sdata;
	refdata		*rdata;
	flaglen		flag;
	float		mr;
} setinfo;

void			freeraw();
void			freeheader();

void			read_gcflaskraw();
void			read_gcinsituraw();

void			compute_mr();

void			create_flask_str();
void			create_insitu_str();

/************************************
	DBLIB Definitions
************************************/

static	char		*networkdir=
			"/home/geo/masarie/projects/network/flask/site/";

typedef	struct {
	stalen		sta;
	idlen		id;
	char		meth;
	float		mr;
	flaglen		flag;
	instlen		inst;
	int		yr,mo,dy,hr,mn;
	int		ayr,amo,ady,ahr,amn;
} sitedata;
	
typedef struct {
	int		nsite;
	sitelen		header;
	sitedata	*data;
} siteinfo;

typedef struct {
	int		yr,mo,dy,hr,mn;
	float		ht_mr,ar_mr;
	int		port;
	instlen		inst;
	char		ht_flag,ar_flag;
} monthdata;

typedef struct {
	int		nmonth;
	monthdata	*data;
} monthinfo;

typedef	struct {
	stalen		sta;
	idlen		id;
	char		meth;
	int		yr,mo,dy,hr,mn;
	float		lat,lon;
	int		alt,wd;
	float		ws;
} netdata;
	
typedef struct {
	int		nnet;
	netdata		*data;
} netinfo;

/*
Forward declaration
of all functions in
dblib.c file.
*/
void			update_flask_site();
void			update_insitu_month();

void			read_flasksite();
void			read_insitumonth();
void			read_flasknetwork();

void   	 		chk_gcflaskraw_gcflasksite();
void   	 		chk_gcflasksite_gcflaskraw();
void			chk_gcflaskraw_networksite();

void			freesite();
void			freemonth();
void			freenet();

char			*getkey();
char			*buildkey();
char   	 		*build_flasksite_str();

/************************************
	UTILS Definitions
************************************/

char			*to_upper();
char			*to_lower();
char			*strip();
char			*get_sys_date();
char			*get_sys_year();
char			*date_conv1();
char			*date_conv2();
char			*date_conv3();
char			*date_conv4();
char			*date_conv5();
char			*julian2date();
int			date2julian();
double			date2dec();
float 			pround();
int			flaskread();
int			frcomp();
int			lines_in_file();
void			permissions();
char   		 	*dec_to_deg();
double			mean();
double			stdev();

/************************************
	LISTS Definitions
************************************/

typedef char   		minrec[MINSIZE];
typedef char		midrec[MIDSIZE];
typedef char		maxrec[MAXSIZE];

static	char		*loopdbfile="/home/geo/masarie/projects/network/loopdb";
static	char		*sitedbfile="/home/geo/masarie/projects/network/ccgfm/ccgfm.sites";
static	char		*loopdbtitle="CCG LOOPDB FILE";
static	char		*sitedbtitle="CCG COOPERATIVE AIR SAMPLING NETWORK";

int			network_site();
int			flask_site();
int			flask_raw();
int			loopdb();
int			sitedb();
