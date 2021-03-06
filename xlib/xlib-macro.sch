 ;; manually produced declarations (for macro functions) from
 ;; include file Xlib.h
 (extern
   (macro connectionnumber::int       (display*)      "ConnectionNumber")
   (macro rootwindow::int             (display* int)  "RootWindow")
   (macro defaultscreen::int          (display*)      "DefaultScreen")
   (macro defaultrootwindow::int      (display*)      "DefaultRootWindow")
   (macro defaultvisual::visual*      (display* int)  "DefaultVisual")
   (macro defaultgc::gc               (display* int)  "DefaultGC")
   (macro blackpixel::ulong           (display* int)  "BlackPixel")
   (macro whitepixel::ulong           (display* int)  "WhitePixel")
   (macro qlength::int                (display*)      "QLength")
   (macro displaywidth::int           (display* int)  "DisplayWidth")
   (macro displayheight::int          (display* int)  "DisplayHeight")
   (macro displaywidthmm::int         (display* int)  "DisplayWidthMM")
   (macro displayheightmm::int        (display* int)  "DisplayHeightMM")
   (macro displayplanes::int          (display* int)  "DisplayPlanes")
   (macro displaycells::int           (display* int)  "DisplayCells")
   (macro screencount::int            (display*)      "ScreenCount")
   (macro servervendor::string        (display*)      "ServerVendor")
   (macro protocolversion::int        (display*)      "ProtocolVersion")
   (macro protocolrevision::int       (display*)      "ProtocolRevision")
   (macro VendorRelease::int          (display*)      "VendorRelease")
   (macro displaystring::string       (display*)      "DisplayString")
   (macro defaultdepth::int           (display* int)  "DefaultDepth")
   (macro defaultcolormap::colormap   (display* int)  "DefaultColormap")
   (macro bitmapunit::int             (display*)      "BitmapUnit")
   (macro bitmapbitorder::int         (display*)      "BitmapBitOrder")
   (macro bitmappad::int              (display*)      "BitmapPad")
   (macro imagebyteorder::int         (display*)      "ImageByteOrder")
   (macro nextrequest::ulong          (display*)      "NextRequest")
   (macro lastknownrequestprocessed::ulong (display*) "LastKnownRequestProcessed")
   
   (macro screenofdisplay::screen*    (display* int) "ScreenOfDisplay")
   (macro defaultscreenofdisplay::int (display*)     "DefaultScreenOfDisplay")
   (macro displayofscreen::display*   (screen*)      "DisplayOfScreen")
   (macro rootwindowofscreen::window* (screen*)      "RootWindowOfScreen")
   (macro blackpixelofscreen::ulong   (screen*)      "BlackPixelOfScreen")
   (macro whitepixelofscreen::ulong   (screen*)      "WhitePixelOfScreen")
   (macro defaultcolormapofscreen::colormap (screen*) "DefaultColormapOfScreen")
   (macro defaultdepthofscreen::int   (screen*)      "DefaultDepthOfScreen") 
   (macro defaultgcofscreen::gc       (screen*)      "DefaultGCOfScreen")    
   (macro defaultvisualofscreen::visual* (screen*)   "DefaultVisualOfScreen")
   (macro widthofscreen::int          (screen*)      "WidthOfScreen")        
   (macro heightofscreen::int         (screen*)      "HeightOfScreen")       
   (macro widthmmofscreen::int        (screen*)      "WidthMMOfScreen")      
   (macro heightmmofscreen::int       (screen*)      "HeightMMOfScreen")     
   (macro planesofscreen::int         (screen*)      "PlanesOfScreen")       
   (macro cellsofscreen::int          (screen*)      "CellsOfScreen")        
   (macro mincmapsofscreen::int       (screen*)      "MinCmapsOfScreen")     
   (macro maxcmapsofscreen::int       (screen*)      "MaxCmapsOfScreen")     
   (macro doessaveunders::bool        (screen*)      "DoesSaveUnders")       
   (macro doesbackingstore::int       (screen*)      "DoesBackingStore")     
   (macro eventmaskofscreen::long     (screen*)      "EventMaskOfScreen"))

