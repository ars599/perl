/*
	NOAA/CMDL Carbon Cycle Group general purpose utility
	routines - developed OCT 1995: kam. 
	
		Compile:  make -f make.utils

		compile object code in makefile.
		include utils.h.

	Ken Masarie
	Cooperative Institute for Research in Environmental Sciences
	University of Colorado
	National Oceanic and Atmospheric Administration

	kmasarie@cmdl.noaa.gov
*/
#include	"ccg.h"

char 	*to_lower(string)
char	*string;
{
	/*
	Change passed string
	to lower-case.
	*/
	int 		i;
	static	char	temp[1000];

	strcpy(temp, string);
	for (i=0; temp[i]!='\0'; i++) temp[i]=tolower(temp[i]);
	return(temp);
}

char 	*to_upper(string)
char	*string;
{
	/*
	Change passed string
	to upper-case.
	*/
	int 		i;
	static	char	temp[1000];

	strcpy(temp, string);
	for (i=0; temp[i]!='\0'; i++) temp[i]=toupper(temp[i]);
	return(temp);
}

char	*strip(str)
char 	str[];	
{
	/*
	Remove the leading white space from
	the passed string.
	*/
	int 		i,j;
	static	char 	temp[1000];

	for (i=0,j=0; str[i]!='\0'; i++) {
		if (str[i] != ' ') temp[j++] = str[i];
	}
	temp[j]='\0';
	return(temp);
}

char 	*get_sys_date()
{
	/*
	Get current system date.
	Return format: 16 Feb 1993.
	*/
	time_t		seconds;
	char		*lt;
	static	char	temp[20];

	seconds=time((long*)0);
	lt=asctime(localtime(&seconds));
	sprintf(temp,"%2.2s %.3s %4.4s",lt+8,lt+4,lt+20);
	return(temp);
	
	/*
	HP-UX
	return(nl_cxtime(&seconds,"%d %b %Y"));
	*/
}

char 	*get_sys_date2()
{
	/*
	Get current system date.
	Return format: 1993 02 16.
	*/
	time_t		seconds;
	struct	tm	*lt;
	static	char	temp[20];

	seconds=time((long*)0);
	lt=localtime(&seconds);
	sprintf(temp,"%4.4d %2.2d %2.2d",
		lt->tm_year+1900,lt->tm_mon,lt->tm_mday);
	return(temp);
	/*
	HP-UX
	return(nl_cxtime(&seconds,"%Y %m %d"));
	*/
}

char 	*get_sys_year()
{
        /*
        Get current system year.
        Return format: 1993.
        */
	time_t		seconds;
	struct	tm	*lt;
	static	char	temp[20];

	seconds=time((long*)0);
	lt=localtime(&seconds);
	sprintf(temp,"%4.4d",lt->tm_year+1900);
	return(temp);
	/*
	HP-UX
        return(nl_cxtime(&seconds,"%Y"));
	*/
}

char	*date_conv1(date)
char	date[];
{
	/*
	Convert date from 1992 01 12 
	to 12JAN1992.
	*/
	int 		yr,mo,dy;
	static	char	temp[12];

	sscanf(date, "%4d", &yr);
	sscanf(date+3, "%2d", &mo);
	sscanf(date+6, "%2d", &dy);
	sprintf(temp, "%02d%.3s%4d",dy,month_name[mo],yr);
	return(temp);
}

char	*date_conv2(date)
char	date[];
{
	/*
	Convert date from 12JAN1992 
	to 1992 01 12.
	*/
	int 		yr,mo,dy;
	static	char	temp[12];

	sscanf(date, "%2d", &dy);
	sscanf(date+5, "%4d", &yr);

	for (mo=1; mo<13; mo++)
		if (strstr(to_upper(date),month_name[mo])!=NULL) break;

	sprintf(temp, "%4d %02d %02d",yr,mo,dy);
	return(temp);
}

char	*date_conv3(date)
char	date[];
{
	/*
	Convert date from 12 JAN 1992 
	to 1992 01 12.
	*/
	int 		yr,mo,dy;
	static	char	temp[11];

	sscanf(date, "%2d", &dy);
	sscanf(date+7, "%4d", &yr);

	for (mo=1; mo<13; mo++)
		if (strstr(to_upper(date),month_name[mo])!=NULL) break;

	sprintf(temp, "%4d %02d %02d",yr,mo,dy);
	return(temp);
}

char	*date_conv4(date)
char	date[];
{
	/*
	Convert date from 1992 01 12
	to 12 JAN 1992.
	*/
	int 		yr,mo,dy;
	static	char	temp[12];

	sscanf(date, "%4d", &yr);
	sscanf(date+5, "%2d", &mo);
	sscanf(date+8, "%2d", &dy);

	sprintf(temp, "%02d %s %d",dy,month_name[mo],yr);
	return(temp);
}

char 	*julian2date(julian)
int	julian;
{
	/*
	Convert julian date (1992051) 
	to 21FEB1992 format.
	*/
	int 		leap,yr,mo,dy,doy;
	static	char	temp[12];

	yr = julian/1000;
	leap = (yr%4==0 && yr%100!=0) || yr%400==0;
	doy = julian%1000;

	for (mo=1; mo<13; mo++)
		if (diy[leap][mo] >= doy) break;
	mo-=1;
	dy=doy-diy[leap][mo];

	sprintf(temp, "%02d%s%4d", dy,month_name[mo],yr);
	return(temp);
}

int	date2julian(date,adj)
char	date[];
int	adj;
{
	/*
	Convert 21FEB1992 date to 
	julian date (1992051) format.
	*/
	int		i,leap;
	int 		yr,dy,doy;

	sscanf(date+5, "%4d", &yr);
	sscanf(date, "%2d", &dy);

	leap = (yr%4==0 && yr%100!=0) || yr%400==0;

	for (i=1; i<13; i++)
		if (strstr(to_upper(date),month_name[i])!=NULL) break;
	doy = diy[leap][i]+dy;

	doy+=adj;
	if (doy>(365+leap)) {
		yr+=1;
		doy=1;
	} else if (doy==0) {
		yr-=1;
		leap = (yr%4 == 0);
		doy=365+leap;
	}
	return(yr*1000+doy);
}

double 	date2dec(yr,mo,dy,hr,mn)
int	yr,mo,dy,hr,mn;	
{
	/*
	Convert passed year, month,
	day, hour, and minute to
	decimal date format.
	(ex)	1992,02,03,12,30
		1992.091586
	*/
	int		i,leap;

	leap = (yr%4==0 && yr%100 != 0) || yr%400 == 0;

	for (i=1; i<mo; i++)
		dy+=dim[leap][i];

	return(yr+((dy-1)*24.0+hr+(mn/60.0))/((365+leap)*24.0));
}

float 	pround(n, power)
float 	n;
int 	power;

/* round the number n to the specified power of 10 */
/* written by kwt */

#define NINT(t) ((int) (t=(t<0) ? t-.5 : t+.5))
{
   	double p,pow();
  	float t, t1;
   
  	p=(double) power;
   	t=n*pow(10.,-p);
   	if (t > (float) MAXINT )
      		return (n);
   	else {
      		t1= NINT(t);
      		return (t1*pow(10.,p));
   	}
}

int		lines_in_file(f)
char		*f;
{
	/*
	How many lines
	are there in the
	passed file?
	*/
	int		i;
	char		*temp,*tfile;
	struct stat     b, *buf = &b;
	FILE		*fp;
	
	if (stat(f,buf)!=0) return(0);

	temp=(char*)malloc(sizeof(char)*200);
	tfile=(char*)malloc(sizeof(char)*200);

	tmpnam(tfile);
	sprintf(temp,"wc -l %s > %s",f,tfile);
	system(temp);
	fp=fopen(tfile,"r");
	fscanf(fp,"%d",&i);
	fclose(fp);
      	unlink(tfile);
	free(tfile);
	free(temp);
	return(i);
}

void  	permissions(f)
char   		*f;
{
        /*
        Change permissions of
        passed file to 664.
        */
        chmod(f,S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH);
}

char	*dec_to_deg(val,type)
float	val;
int	type;
{
	/*
	Convert latitude or longitude represented
	in decimal notation to degree notation,
	i.e., -45.25 -> 45 15S.
	*/
	int		deg,min;
	char		c;
	static	char	result[1000];

	deg=(int)val;

	sprintf(result, "%.2f",val);
	min=(int)pround((val-deg)*60,0);
	switch(type) {
	case LAT:
		if (!strcmp(result, "-99.99")) return("99 99S");
		if (val>=0) c='N';
		else	    c='S';
		sprintf(result, "%2d %02d%c", abs(deg),abs(min),c);
		return(result);
	case LON:
		if (!strcmp(result, "-999.99")) return ("999 99W");
		if (val>=0) c='E';
		else	    c='W';
		sprintf(result, "%3d %02d%c", abs(deg),abs(min),c);
		return(result);
	}
	return("");
}
double	mean(sum,total)
double	sum;
int     total;
{
	/*
	Calculate the arithmetic mean.
	*/
        if (total)	return(sum/(double)total);
        else 		return(-999.99);
}

double  stdev(sum,var,total)
double  sum,var;
int     total;
{
	/*
	Calculate the standard deviation
	of the arithmetic mean.
	*/
        if (total>1) 	return(sqrt((total*var-(sum*sum))/(total*(total-1))));
        else 		return(-9.99);
}
