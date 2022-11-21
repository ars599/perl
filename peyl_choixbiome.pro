;==================================================================
;
;            Prgramme pour regenerer les indices...
;                 
;==================================================================
PRO INDICE_BUILD_BIOME



;---------------------------------------------------------------
;                  Read biome file
;---------------------------------------------------------------
biome_file='/home/data02/peylin/SIB/Other/biome'
grid = peyl_choixgrid('SIB54',/noinfo)
nlon = grid.nlon
nlat = grid.nlat
nmon = 12
biome=dblarr(nlon,nlat)
openr,u,biome_file,/get_lun
readf,u,biome
free_lun,u
;;print,'EXTRACT SPECIFIC BIOME...'


;---------------------------------------------------------------
;              Define indice for specific biomes...
;---------------------------------------------------------------

bb=biome
bb(30:*,*)=15 & bb(*,27:*)=15 
bb(22:23,23:24)=1 & bb(20,20)=1
ii_sa1=where(bb eq 1)
name_sa1 = 'S_AMERICA : Trop_forest'

;---------------------------------------------------------------
bb=biome
bb(30:*,*)=15 & bb(*,21:*)=15 & bb(0:22,*)=15 
bb(20,20)=15
ii_sa6=where(bb eq 6 or bb eq 8)
name_sa6 = 'S_AMERICA : tree+ground_cover'

;---------------------------------------------------------------
bb=biome
bb(26:*,*)=15 & bb(*,19:*)=15 
ii_sa12=where(bb eq 12 or bb eq 11 or bb eq 3 or bb eq 2 or bb eq 8)
name_sa12 = 'S_AMERICA : Deciduous_forest'

;---------------------------------------------------------------
bb=biome
bb(*,0:34)=15 
ii_na10=where(bb eq 10 or bb eq 11)
name_na10 = 'N_AMERICA : tundra'

;---------------------------------------------------------------
bb=biome
bb(26:*,*)=15 & bb(*,0:32)=15   
ii_na4=where(bb eq 4 or bb eq 3)
name_na4 = 'N_AMERICA : conif'

;---------------------------------------------------------------
bb=biome
bb(26:*,*)=15 & bb(*,0:25)=15 
bb(*,33:*)=15 & bb(15:18,33)=12 & bb(14:16,34)=12 
bb(14,35)=12 
ii_na12=where(bb eq 12 or bb eq 7 or bb eq 9 or bb eq 6 or bb eq 4 or bb eq 8)
name_na12 = 'N_AMERICA : Deciduous_forest'

;---------------------------------------------------------------
bb=biome
bb(0:32,*)=15 & bb(47:*,*)=15 & bb(*,31:*)=15 
ii_af1=where(bb eq 1)
name_af1 = 'AFRIQUE : Trop_forest'

;---------------------------------------------------------------
bb=biome
bb(0:32,*)=15 & bb(49:*,*)=15 & bb(*,31:*)=15 
bb(*,0:21)=15 
bb(35:36,30)=15 & bb(44:*,30)=15 & bb(46:*,29)=15
ii_af6n=where(bb eq 6 or bb eq 7 or bb eq 9 or bb eq 12 or bb eq 11)
name_af6n = 'AFRIQUE : nord-tree+ground_cover'

;---------------------------------------------------------------
bb=biome
bb(0:32,*)=15 & bb(49:*,*)=15 & bb(*,22:*)=15 
ii_af6s=where(bb eq 6 or bb eq 7 or bb eq 9 or bb eq 12 or bb eq 11)
name_af6s = 'AFRIQUE : sud-tree+ground_cover'

;---------------------------------------------------------------
bb=biome
bb(0:32,*)=15 & bb(51:*,*)=15 & bb(*,0:34)=15 
ii_eu4=where(bb eq 3 or bb eq 4 or bb eq 5)
name_eu4 = 'EUROPE : conif'

;---------------------------------------------------------------
bb=biome
bb(0:32,*)=15 & bb(51:*,*)=15 & bb(*,0:28)=15 
bb(47:*,31:33)=15 & bb(49:*,28:30)=15 
bb(38:*,36:*)=15 & bb(41:43,35)=15
ii_eu12=where(bb eq 6 or bb eq 3 or bb eq 9 or bb eq 12 or bb eq 2)
name_eu12 = 'EUROPE : Deciduous_forest'

;---------------------------------------------------------------
bb=biome
bb(0:50,*)=15 & bb(*,0:33)=15 
bb(*,0:31)=15
ii_as4=where(bb eq 4 or bb eq 5)
name_as4 = 'ASIE : conif'

;---------------------------------------------------------------
bb=biome
bb(0:50,*)=15 & bb(*,0:26)=15 
bb(ii_as4)=15 & bb(0:56,0:31)=15
ii_as12=where(bb eq 12 or bb eq 9 or bb eq 2 or bb eq 3 or bb eq 4 or bb eq 6)
name_as12 = 'ASIE : Deciduous_forest'

;---------------------------------------------------------------
bb=biome
bb(0:46,*)=15 & bb(*,0:28)=15 & bb(*,34:*)=15
bb(49:50,28)=12 
bb(0:48,0:30)=15 & bb(ii_as12)=15
ii_as7=where(bb eq 6 or bb eq 7 or bb eq 9 or bb eq 11 or bb eq 12)
name_as7 = 'ASIE : tree+ground_cover'

;---------------------------------------------------------------
bb=biome
bb(0:50,*)=15 & bb(*,0:18)=15
bb(ii_as12)=15 & bb(ii_as7)=15 & bb(ii_as4)=15 
ii_as1=where(bb eq 1 or bb eq 12 or bb eq 4 or bb eq 6)
name_as1 = 'ASIE : Trop_forest'

;---------------------------------------------------------------
bb=biome
bb(0:58,*)=15 & bb(*,19:*)=15
bb(*,0:9)=15 
ii_au6=where(bb ne 15)
name_au6 = 'AUSTRALIE : tree+ground_cover'


;---------------------------------------------------------------
;          Save indices in a file
;---------------------------------------------------------------
; 
openw,u,'/home/geo/peylin/idl/lib/PEYL/peyl_choixbiome.ind',/get_lun
printf,u,name_SA1 
printf,u,format='(1000I5)',ii_sa1

printf,u,name_SA6
printf,u,format='(1000I5)',ii_sa6

printf,u,name_SA12
printf,u,format='(1000I5)',ii_sa12

printf,u,name_NA10 
printf,u,format='(1000I5)',ii_na10

printf,u,name_NA4 
printf,u,format='(1000I5)',ii_na4

printf,u,name_NA12 
printf,u,format='(1000I5)',ii_na12

printf,u,name_AF1 
printf,u,format='(1000I5)',ii_af1

printf,u,name_AF6n 
printf,u,format='(1000I5)',ii_af6n

printf,u,name_AF6s 
printf,u,format='(1000I5)',ii_af6s

printf,u,name_EU4 
printf,u,format='(1000I5)',ii_eu4

printf,u,name_EU12 
printf,u,format='(1000I5)',ii_eu12

printf,u,name_AS4  
printf,u,format='(1000I5)',ii_as4

printf,u,name_AS12  
printf,u,format='(1000I5)',ii_as12

printf,u,name_AS7  
printf,u,format='(1000I5)',ii_as7

printf,u,name_AS1  
printf,u,format='(1000I5)',ii_as1

printf,u,name_AU6  
printf,u,format='(1000I5)',ii_au6

free_lun,u

END
;+
;============================================================================
;		Find indices for specific biome(s).
;============================================================================
FUNCTION peyl_choixbiome, selection
;@map2d_com

;---------------------------------------------------------------
;                  Build the indices if desired...
;---------------------------------------------------------------

;;;INDICE_BUILD_BIOME



;---------------------------------------------------------------
;              READ indices for specific biomes...
;---------------------------------------------------------------

;;openr,u,'/home/geo/peylin/idl/lib/PEYL/peyl_choixbiome.ind',/get_lun
;;lect=''

;;readf,u,lect
;;readf,u,ii_sa1
;;name_sa1=lect
ii_sa1=[1319,1320,1389,1390,1391,1392,1393,1460,1461,1462,1463,1464,1465,1466,1532,1533,1534,1535,1536,1537,1538,1539,1605,1606,1607,1608,1609,1677,1678,1679,1680,1748,1750,1751,1818,1819,1889,1890,1894]
name_sa1 = 'S_AMERICA : Trop_forest'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_sa6
;;name_sa6=lect
ii_sa6=[1032,1104,1105,1106,1176,1177,1178,1179,1247,1248,1249,1250,1251,1252,1321,1322,1323,1324,1394,1395,1396,1467,1468,1469]
name_sa6 = 'S_AMERICA : tree+ground_cover'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_sa12
;;name_sa12=lect
ii_sa12=[598, 669, 670, 741, 742, 813, 814, 815, 886, 887, 888, 958, 959, 960, 961,1030,1031,1033,1102,1103,1174,1175,1246,1317,1318]
name_sa12 = 'S_AMERICA : Deciduous_forest'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_na10
;;name_na10=lect
ii_na10=[2523,2588,2597,2613,2660,2680,2681,2685,2732,2733,2734,2736,2737,2738,2739,2750,2751,2752,2753,2754,2755,2756,2758,2759,2760,2775,2806,2807,2812,2813,2814,2815,2816,2820,2821,2822,2823,2824,2825,2826,2827,2828,2829,2830,2848,2849,2850,2858,2859,2860,2861,2862,2863,2876,2892,2893,2894,2895,2896,2897,2898,2899,2912,2926,2927,2934,2935,2936,2937,2938,2966,2967,2968,2969,2970,2971]
name_na10 = 'N_AMERICA : tundra'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_na4
;;name_na4=lect
ii_na4=[2388,2389,2390,2396,2397,2398,2399,2459,2460,2461,2465,2466,2467,2468,2469,2470,2471,2473,2530,2531,2532,2533,2535,2536,2537,2538,2539,2541,2542,2543,2544,2601,2602,2603,2604,2605,2606,2607,2608,2609,2614,2615,2668,2669,2670,2671,2672,2673,2674,2675,2676,2677,2678,2679,2740,2741,2742,2743,2744,2745,2746,2747,2748,2749]
name_na4 = 'N_AMERICA : conif'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_na12
;;name_na12=lect
ii_na12=[1888,1959,1960,1964,2030,2031,2032,2101,2102,2103,2104,2105,2108,2173,2174,2175,2176,2177,2178,2179,2180,2244,2245,2246,2247,2248,2249,2250,2251,2252,2316,2317,2318,2319,2320,2321,2322,2323,2324,2325,2391,2392,2393,2394,2462,2463,2464,2534]
name_na12 = 'N_AMERICA : Deciduous_forest'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_af1
;;name_af1=lect
ii_af1=[1342,1550,1552,1553,1622,1623,1624,1625,1626,1691,1692,1693,1694]
name_af1 = 'AFRIQUE : Trop_forest'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_af6n
;;name_af6n=lect
ii_af6n=[1627,1628,1629,1695,1696,1697,1698,1699,1700,1701,1762,1763,1764,1765,1766,1767,1768,1769,1770,1771,1772,1773,1774,1833,1834,1835,1836,1837,1838,1839,1840,1841,1842,1843,1844,1845,1905,1906,1907,1908]
ii_af6n=[ii_af6n,1909,1910,1911,1912,1913,1914,1915,1917,1918,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,2050,2051,2052,2053,2054,2055,2056,2057,2058,2060,2061,2063,2064,2122,2123,2124,2125,2126,2127,2128,2129,2130,2131,2132,2133,2197,2198]
name_af6n = 'AFRIQUE : nord-tree+ground_cover'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_af6s
;;name_af6s=lect
ii_af6s=[976,1048,1049,1050,1119,1120,1121,1122,1191,1192,1193,1194,1195,1197,1263,1264,1265,1266,1267,1269,1335,1336,1337,1338,1339,1407,1408,1409,1410,1411,1479,1480,1481,1482,1483,1484,1551,1554,1555,1556]
name_af6s = 'AFRIQUE : sud-tree+ground_cover'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_eu4
;;name_eu4=lect
ii_eu4=[2561,2562,2563,2631,2633,2634,2635,2636,2637,2638,2639,2640,2641,2642,2702,2703,2705,2706,2707,2708,2709,2710,2711,2712,2713,2714,2774,2776,2777,2778,2779,2780,2781,2782,2783,2784,2785,2786]
name_eu4 = 'EUROPE : conif'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_eu12
;;name_eu12=lect
ii_eu12=[2134,2135,2136,2195,2196,2204,2205,2206,2207,2208,2266,2267,2272,2274,2275,2276,2277,2278,2339,2340,2343,2344,2345,2348,2349,2412,2413,2414,2415,2416,2417,2418,2419,2420,2421,2485,2486,2487,2488,2489,2490,2491,2492,2493,2494,2495,2496,2497,2498,2555,2556,2558,2559,2560,2564,2565,2566,2567,2568,2569,2570,2627]
name_eu12 = 'EUROPE : Deciduous_forest'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_as4
;;name_as4=lect
ii_as4=[2508,2509,2510,2511,2512,2574,2575,2576,2577,2579,2580,2581,2582,2583,2584,2643,2644,2645,2646,2647,2648,2649,2650,2651,2652,2653,2654,2655,2656,2657,2658,2659,2715,2716,2717,2718,2719,2720,2721,2722,2723,2724,2725,2726,2727,2728,2729,2730,2731,2787,2788,2789,2790,2791,2792,2793,2794,2795,2796,2797,2798,2799,2800,2801,2802,2803,2804,2805,2864,2865,2866,2867,2868,2869,2870,2871,2872,2873,2874,2875]
name_as4 = 'ASIE : conif'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_as12
;;name_as12=lect
ii_as12=[2002,2073,2074,2075,2145,2146,2147,2148,2217,2218,2219,2223,2291,2293,2296,2355,2356,2357,2361,2362,2363,2364,2365,2366,2369,2427,2428,2429,2430,2431,2432,2433,2434,2435,2436,2437,2438,2439,2499,2500,2501,2502,2503,2504,2505,2506,2507,2571,2572,2573,2578]
name_as12 = 'ASIE : Deciduous_forest'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_as7
;;name_as7=lect
ii_as7=[2065,2066,2137,2138,2139,2140,2141,2142,2143,2144,2209,2210,2211,2212,2213,2214,2215,2216,2279,2280,2281,2282,2283,2284,2285,2286,2287,2288,2289,2290,2351,2352,2353,2354,2358,2359,2360,2423,2425,2426]
name_as7 = 'ASIE : tree+ground_cover'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_as1
;;name_as1=lect
ii_as1=[1434,1504,1505,1569,1571,1640,1643,1712,1780,1785,1789,1851,1852,1856,1857,1860,1923,1924,1927,1928,1929,1995,1996,1997,1999,2000,2001,2067,2068,2069,2070,2071,2072]
name_as1 = 'ASIE : Trop_forest'

;---------------------------------------------------------------
;;readf,u,lect
;;readf,u,ii_au6
;;name_au6=lect
ii_au6=[857, 862, 935, 996,1000,1001,1002,1068,1069,1070,1071,1072,1073,1074,1139,1140,1141,1142,1143,1144,1145,1146,1211,1212,1213,1214,1215,1216,1217,1285,1286,1287,1288,1289,1358,1359]
name_au6 = 'AUSTRALIE : tree+ground_cover'

;;free_lun,u

;-----------------------------------------------------------------
;                 Loop over all selected biomes...
;---------------------------------------------------------------
;
nb_select = n_elements(selection)
name=''
ii=-1   ;--- artificial for the loop...

for nn=0, nb_select-1 do begin

case selection(nn) of
    'sa1' : begin & ii=[ii,ii_sa1] & name=name+name_sa1 & end 
    'sa6' : begin & ii=[ii,ii_sa6] & name=name+name_sa6 & end
    'sa12' : begin & ii=[ii,ii_sa12] & name=name+name_sa12 & end
    'na10' : begin & ii=[ii,ii_na10] & name=name+name_na10 & end
    'na4' : begin & ii=[ii,ii_na4] & name=name+name_na4 & end
    'na12' : begin & ii=[ii,ii_na12] & name=name+name_na12 & end
    'af1' : begin & ii=[ii,ii_af1] & name=name+name_af1 & end
    'af6n' : begin & ii=[ii,ii_af6n] & name=name+name_af6n & end 
    'af6s' : begin & ii=[ii,ii_af6s] & name=name+name_af6s & end
    'eu4' : begin & ii=[ii,ii_eu4] & name=name+name_eu4 & end
    'eu12' : begin & ii=[ii,ii_eu12] & name=name+name_eu12 & end
    'as4' : begin & ii=[ii,ii_as4] & name=name+name_as4 & end
    'as12' : begin & ii=[ii,ii_as12] & name=name+name_as12 & end
    'as1' : begin & ii=[ii,ii_as1] & name=name+name_as1 & end
    'as7' : begin & ii=[ii,ii_as7] & name=name+name_as7 & end
    'au6' : begin & ii=[ii,ii_au6] & name=name+name_au6 & end
    else : begin & print,'not a biome...' & stop & end
endcase

if nn lt  (nb_select-1) then name=name+'+'
endfor      ;---- end of loop over selected biomes...

;----- Get ridd of the first value : -1 
ii=ii(1:*)
nb_point = n_elements(ii)
if nb_point le 0 then begin
   print,'no biome defined...' & stop
endif


;---------------------------------------------------------------
;                         Define output
;---------------------------------------------------------------

nlon=72
nlat=44
nmon=12

;------- Set 1D indices 
iilat     = ii/nlon
iilon     = ii - iilat*nlon

;------- Set 2D indices 
ii2d      = ii
foo       = lindgen(long(nlon)*long(nlat)) 
foo(ii2d) = -1
ii2d_inv  = where (foo ne -1) 

;------- Set 3D indices 
foo2      = intarr(nlon,nlat)
foo2(ii2d)= -1
foo3      = intarr(nlon,nlat,nmon)
for n=0,nmon-1 do foo3(*,*,n)=foo2
ii3d      = where(foo3 eq -1)
foo       = lindgen(long(nlon)*long(nlat)*long(nmon))
foo(ii3d) = -1
ii3d_inv  = where(foo ne -1)


;---------------------------------------------------------------
;                         Set output
;---------------------------------------------------------------

ind={lon:iilon, lat:iilat, $
     ii2d:ii2d, ii2d_inv:ii2d_inv, $
     ii3d:ii3d, ii3d_inv:ii3d_inv, $
     name:name}


;---------------------------------------------------------------
;                         Visualisation...
;---------------------------------------------------------------

;---- Visualize biome selected
if 0 then begin
   data=fltarr(nlon,nlat)
   data(ii2d)=999
   peyl_initmap2d
   map2d
endif

;---- Visualize all biomes
if 0 then begin
   k=0
   data=fltarr(nlon,nlat)
   data(*)=0
   data(ii_sa1)=1 & k=k+n_elements(ii_sa1)
   data(ii_sa6)=12 & k=k+n_elements(ii_sa6)
   data(ii_sa12)=8 & k=k+n_elements(ii_sa12)
   data(ii_na10)=4 & k=k+n_elements(ii_na10)
   data(ii_na4)=5 & k=k+n_elements(ii_na4)
   data(ii_na12)=9 & k=k+n_elements(ii_na12)
   data(ii_af1)=2 & k=k+n_elements(ii_af1)
   data(ii_af6n)=13 & k=k+n_elements(ii_af6n)
   data(ii_af6s)=14 & k=k+n_elements(ii_af6s)
   data(ii_eu4)=6 & k=k+n_elements(ii_eu4)
   data(ii_eu12)=10 & k=k+n_elements(ii_eu12)
   data(ii_as4)=7 & k=k+n_elements(ii_as4)
   data(ii_as12)=11 & k=k+n_elements(ii_as12)
   data(ii_as1)=3 & k=k+n_elements(ii_as1)
   data(ii_as7)=16 & k=k+n_elements(ii_as7)
   data(ii_au6)=15 & k=k+n_elements(ii_au6)

;------------------------------------------------------
;   data(ii_sa1)=1 
;   data(ii_sa6)=4 
;   data(ii_sa12)=4 
;   data(ii_na10)=13 
;   data(ii_na4)=5 
;   data(ii_na12)=8 
;   data(ii_af1)=1 
;   data(ii_af6n)=3 
;   data(ii_af6s)=9
;   data(ii_eu4)=6
;   data(ii_eu12)=10
;   data(ii_as4)=14
;   data(ii_as12)=2
;   data(ii_as1)=7
;   data(ii_as7)=2
;   data(ii_au6)=11 

   peyl_initmap2d
   map2d
   stop
endif

return,ind
end
;-








