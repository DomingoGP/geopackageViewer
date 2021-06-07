{Pascal wrappper for proj library Supports only from the version 6 API.}
{This License applies only to this wrapper and not the proj itself}
{For the latest proj visit https://proj.org/   }
{* Author: Domingo Galmes <dgalmesp@gmail.com>  29-11-2020
******************************************************************************
* Copyright (c) 2020,2021 Domingo Galmés
*
* Permission is hereby granted, free of charge, to any person obtaining a
* copy of this software and associated documentation files (the "Software"),
* to deal in the Software without restriction, including without limitation
* the rights to use, copy, modify, merge, publish, distribute, sublicense,
* and/or sell copies of the Software, and to permit persons to whom the
* Software is furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included
* in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO COORD SHALL
* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
* DEALINGS IN THE SOFTWARE.
*****************************************************************************}
{ original proj.h license }
  {*****************************************************************************
   * Project:  PROJ.4
   * Purpose:  Revised, experimental API for PROJ.4, intended as the foundation
   *           for added geodetic functionality.
   *
   *           The original proj API (defined previously in projects.h) has grown
   *           organically over the years, but it has also grown somewhat messy.
   *
   *           The same has happened with the newer high level API (defined in
   *           proj_api.h): To support various historical objectives, proj_api.h
   *           contains a rather complex combination of conditional defines and
   *           typedefs. Probably for good (historical) reasons, which are not
   *           always evident from today's perspective.
   *
   *           This is an evolving attempt at creating a re-rationalized API
   *           with primary design goals focused on sanitizing the namespaces.
   *           Hence, all symbols exposed are being moved to the proj_ namespace,
   *           while all data types are being moved to the PJ_ namespace.
   *
   *           Please note that this API is *orthogonal* to  the previous APIs:
   *           Apart from some inclusion guards, projects.h and proj_api.h are not
   *           touched - if you do not include proj.h, the projects and proj_api
   *           APIs should work as they always have.
   *
   *           A few implementation details:
   *
   *           Apart from the namespacing efforts, I'm trying to eliminate three
   *           proj_api elements, which I have found especially confusing.
   *
   *           FIRST and foremost, I try to avoid typedef'ing away pointer
   *           semantics. I agree that it can be occasionally useful, but I
   *           prefer having the pointer nature of function arguments being
   *           explicitly visible.
   *
   *           Hence, projCtx has been replaced by PJ_CONTEXT *.
   *           and    projPJ  has been replaced by PJ *
   *
   *           SECOND, I try to eliminate cases of information hiding implemented
   *           by redefining data types to void pointers.
   *
   *           I prefer using a combination of forward declarations and typedefs.
   *           Hence:
   *               typedef void *projCtx;
   *           Has been replaced by:
   *               struct projCtx_t;
   *               typedef struct projCtx_t PJ_CONTEXT;
   *           This makes it possible for the calling program to know that the
   *           PJ_CONTEXT data type exists, and handle pointers to that data type
   *           without having any idea about its internals.
   *
   *           (obviously, in this example, struct projCtx_t should also be
   *           renamed struct pj_ctx some day, but for backwards compatibility
   *           it remains as-is for now).
   *
   *           THIRD, I try to eliminate implicit type punning. Hence this API
   *           introduces the PJ_COORD union data type, for generic 4D coordinate
   *           handling.
   *
   *           PJ_COORD makes it possible to make explicit the previously used
   *           "implicit type punning", where a XY is turned into a LP by
   *           re#defining both as UV, behind the back of the user.
   *
   *           The PJ_COORD union is used for storing 1D, 2D, 3D and 4D coordinates.
   *
   *           The bare essentials API presented here follows the PROJ.4
   *           convention of sailing the coordinate to be reprojected, up on
   *           the stack ("call by value"), and symmetrically returning the
   *           result on the stack. Although the PJ_COORD object is twice as large
   *           as the traditional XY and LP objects, timing results have shown the
   *           overhead to be very reasonable.
   *
   *           Contexts and thread safety
   *           --------------------------
   *
   *           After a year of experiments (and previous experience from the
   *           trlib transformation library) it has become clear that the
   *           context subsystem is unavoidable in a multi-threaded world.
   *           Hence, instead of hiding it away, we move it into the limelight,
   *           highly recommending (but not formally requiring) the bracketing
   *           of any code block calling PROJ.4 functions with calls to
   *           proj_context_create(...)/proj_context_destroy()
   *
   *           Legacy single threaded code need not do anything, but *may*
   *           implement a bit of future compatibility by using the backward
   *           compatible call proj_context_create(0), which will not create
   *           a new context, but simply provide a pointer to the default one.
   *
   *           See proj_4D_api_test.c for examples of how to use the API.
   *
   * Author:   Thomas Knudsen, <thokn@sdfe.dk>
   *           Benefitting from a large number of comments and suggestions
   *           by (primarily) Kristian Evers and Even Rouault.
   *
   ******************************************************************************
   * Copyright (c) 2016, 2017, Thomas Knudsen / SDFE
   * Copyright (c) 2018, Even Rouault
   *
   * Permission is hereby granted, free of charge, to any person obtaining a
   * copy of this software and associated documentation files (the "Software"),
   * to deal in the Software without restriction, including without limitation
   * the rights to use, copy, modify, merge, publish, distribute, sublicense,
   * and/or sell copies of the Software, and to permit persons to whom the
   * Software is furnished to do so, subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included
   * in all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO COORD SHALL
   * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   * DEALINGS IN THE SOFTWARE.
   **************************************************************************** }
   
unit proj;

{$IFDEF FPC}
{$PACKRECORDS C}
{$MINENUMSIZE 4}
{$ENDIF}

interface

uses
    math;

//  {.$MACRO ON}
//  //ONLY ONE.
//  {.$DEFINE USE_VERSION:=70100}
//  {$DEFINE USE_VERSION:=80100}
  const
   //only one
   //USE_VERSION=70100;
   USE_VERSION=80100;


    const
      HUGE_VAL=Infinity;
      { The version numbers should be updated with every release! * }
      //#define PROJ_COMPUTE_VERSION(maj,min,patch) ((maj)*10000+(min)*100+(patch))
   {$IF USE_VERSION=70100}
      PROJ_VERSION_MAJOR = 7;
      PROJ_VERSION_MINOR = 1;
      PROJ_VERSION_PATCH = 0;
      PROJ_VERSION_NUMBER = 70100;
   {$ELSE}
     PROJ_VERSION_MAJOR = 8;
     PROJ_VERSION_MINOR = 1;
     PROJ_VERSION_PATCH = 0;
     PROJ_VERSION_NUMBER = 80100;
   {$ENDIF}
//todo: CHANGE DLL NAME.
      //last precompiled dll available from: https://trac.osgeo.org/osgeo4w/
      {$IF Defined(MSWINDOWS)}
        External_Dll ='proj_6_3.dll';
      {$ELSEIF Defined(DARWIN)}
        External_Dll ='proj_6_3.dylib';
        {$linklib proj}
      {$ELSEIF Defined(UNIX)}
        External_Dll ='proj_6_3.so';
        {$ELSE}
           {$ERROR 'platform not supported'}
      {$IFEND}

  Type

    PPJ  = ^PJ;
    PPJ_AREA  = ^PJ_AREA;
    PPJ_CONTEXT  = ^PJ_CONTEXT;

  { first forward declare everything needed  }
  { Data type for generic geodetic 3D data plus epoch information  }

    PJ_AREA = record
        {undefined structure}
      end;

    P5_FACTORS = record                   { Common designation  }
        meridional_scale : double;        { h  }
        parallel_scale : double;          { k  }
        areal_scale : double;             { s  }
        angular_distortion : double;      { omega  }
        meridian_parallel_angle : double; { theta-prime  }
        meridian_convergence : double;    { alpha  }
        tissot_semimajor : double;        { a  }
        tissot_semiminor : double;        { b  }
        dx_dlam : double;
        dx_dphi : double;
        dy_dlam : double;
        dy_dphi : double;
      end;

  PJ_FACTORS=P5_FACTORS;
  //{ Data type for projection/transformation information  }
  PJconsts = record
        {undefined structure}
  end;
  PJ=PJconsts;

  { the PJ object herself  }
  { Data type for library level information  }

  { Data types for list of operations, ellipsoids, datums and units used in PROJ.4  }

    PPJ_LIST = ^PJ_LIST;
    PJ_LIST = record
        id : PAnsiChar;     { projection keyword  }
        proj : function (_para1:PPJ):PPJ;cdecl;    { projection entry point  }
        descr : ^PAnsiChar;  { description text  }
      end;
   PJ_OPERATIONS=PJ_LIST;
   PPJ_OPERATIONS=^PJ_OPERATIONS;

    PPJ_ELLPS=^PJ_ELLPS;
    PJ_ELLPS = record
        id : PAnsiChar;     { ellipse keyword name  }
        major : PAnsiChar;  { a= value  }
        ell : PAnsiChar;    { elliptical parameter  }
        name : PAnsiChar;   { comments  }
      end;

    PPJ_UNITS=^PJ_UNITS;
    PJ_UNITS = record
        id : PAnsiChar;       { units keyword  }
        to_meter : PAnsiChar; { multiply by value to get meters  }
        name : PAnsiChar;     { comments  }
        factor : double;  { to_meter factor in actual numbers  }
      end;

    PPJ_PRIME_MERIDIANS=^PJ_PRIME_MERIDIANS;
    PJ_PRIME_MERIDIANS = record
        id : PAnsiChar;     { prime meridian keyword  }
        defn : PAnsiChar;   { offset from greenwich in DMS format.  }
      end;

  { Geodetic, mostly spatiotemporal coordinate types  }

    PJ_XYZT = record
        x : double;
        y : double;
        z : double;
        t : double;
      end;

    PJ_UVWT = record
        u : double;
        v : double;
        w : double;
        t : double;
      end;

    PJ_LPZT = record
        lam : double;
        phi : double;
        z : double;
        t : double;
      end;

    PJ_OPK = record    { Rotations: omega, phi, kappa  }
        o : double;
        p : double;
        k : double;
      end;


    PJ_ENU = record    { East, North, Up  }
        e : double;
        n : double;
        u : double;
      end;


    PJ_GEOD = record     { Geodesic length, fwd azi, rev azi  }
        s : double;
        a1 : double;
        a2 : double;
      end;

  { Classic proj.4 pair/triplet types - moved into the PJ_ name space  }

    PJ_UV = record
        u : double;
        v : double;
      end;

    PJ_XY = record
        x : double;
        y : double;
      end;

    PJ_LP = record
        lam : double;
        phi : double;
      end;

    PJ_XYZ = record
        x : double;
        y : double;
        z : double;
      end;

    PJ_UVW = record
        u : double;
        v : double;
        w : double;
      end;

    PJ_LPZ = record
        lam : double;
        phi : double;
        z : double;
      end;
	  
  { Avoid preprocessor renaming and implicit type-punning: Use a union to make it explicit  }
  { First and foremost, it really is "just 4 numbers in a vector"  }
    PPJ_COORD = ^PJ_COORD;
    PJ_COORD = record
        case longint of
          0 : ( v : array[0..3] of double );
          1 : ( xyzt : PJ_XYZT );
          2 : ( uvwt : PJ_UVWT );
          3 : ( lpzt : PJ_LPZT );
          4 : ( geod : PJ_GEOD );
          5 : ( opk : PJ_OPK );
          6 : ( enu : PJ_ENU );
          7 : ( xyz : PJ_XYZ );
          8 : ( uvw : PJ_UVW );
          9 : ( lpz : PJ_LPZ );
          10 : ( xy : PJ_XY );
          11 : ( uv : PJ_UV );
          12 : ( lp : PJ_LP );
        end;

    PJ_INFO = record
        major : longint;      { Major release number                  }
        minor : longint;      { Minor release number                  }
        patch : longint;      { Patch level                           }
        release : PAnsiChar;      { Release info. Version + date          }
        version : PAnsiChar;      { Full version number                   }
        searchpath : PAnsiChar;   { Paths where init and grid files are   }
                              { looked for. Paths are separated by    }
                              { semi-colons on Windows, and colons    }
                              { on non-Windows platforms.             }
        paths : ^PAnsiChar;
        path_count : size_t;
      end;

    PJ_PROJ_INFO = record
        id : PAnsiChar;             { Name of the projection in question                        }
        description : PAnsiChar;    { Description of the projection                             }
        definition : PAnsiChar;     { Projection definition                                     }
        has_inverse : longint;  { 1 if an inverse mapping exists, 0 otherwise               }
        accuracy : double;      { Expected accuracy of the transformation. -1 if unknown.   }
      end;

    PJ_GRID_INFO = record
        gridname : array[0..31] of char;  { name of grid                          }
        filename : array[0..259] of char; { full path to grid                     }
        format : array[0..7] of char;     { file format of grid                   }
        lowerleft : PJ_LP;                { Coordinates of lower left corner      }
        upperright : PJ_LP;               { Coordinates of upper right corner     }
        n_lon, nlat: longint;             { Grid size                             }
        cs_lon, cs_lat : double;          { Cell size of grid                     }
      end;

    PJ_INIT_INFO = record
        name : array[0..31] of char;       { name of init file                         }
        filename : array[0..259] of char;  { full path to the init file.               }
        version : array[0..31] of char;    { version of the init file                  }
        origin : array[0..31] of char;     { origin of the file, e.g. EPSG             }
        lastupdate : array[0..15] of char; { Date of last update in YYYY-MM-DD format  }
      end;

    PJ_LOG_LEVEL = (
	  PJ_LOG_NONE := 0,
	  PJ_LOG_ERROR := 1,
          PJ_LOG_DEBUG := 2,
	  PJ_LOG_TRACE := 3,
          PJ_LOG_TELL := 4,
	  PJ_LOG_DEBUG_MAJOR := 2,    { for proj_api.h compatibility  }
          PJ_LOG_DEBUG_MINOR := 3);   { for proj_api.h compatibility  }

    PJ_LOG_FUNCTION = procedure (_para1:pointer; _para2:longint; _para3:PAnsiChar);cdecl;
  { The context type - properly namespaced synonym for projCtx  }
  {$IF USE_VERSION >= 80000}
  pj_ctx = record
     {undefined structure}
  end;
  PJ_CONTEXT = pj_ctx;
  {$ELSE}
  projCtx_t = record
     {undefined structure}
  end;
  PJ_CONTEXT = projCtx_t;
  {$ENDIF}

  { A P I  }
  {*
   * The objects returned by the functions defined in this section have minimal
   * interaction with the functions of the
   * \ref iso19111_functions section, and vice versa. See its introduction
   * paragraph for more details.
    }
  { Functionality for handling thread contexts  }

  const
    PJ_DEFAULT_CTX = nil;

    function proj_context_create:PPJ_CONTEXT;cdecl;external External_Dll;
    function proj_context_destroy(ctx:PPJ_CONTEXT):PPJ_CONTEXT;cdecl;external External_Dll;
    {$IF USE_VERSION >= 80000}
    function proj_context_clone(ctx:PPJ_CONTEXT):PPJ_CONTEXT;cdecl;external External_DLL;
    {$ENDIF}

  {* Callback to resolve a filename to a full path  }
  type
    Pproj_file_finder = ^proj_file_finder;
    proj_file_finder = function (ctx:PPJ_CONTEXT; _para2:PAnsiChar; user_data:pointer):PAnsiChar;cdecl;
    procedure proj_context_set_file_finder(ctx:PPJ_CONTEXT;finder:proj_file_finder;user_data:Pointer);cdecl;external External_Dll;
    procedure proj_context_set_search_paths(ctx:PPJ_CONTEXT;count_paths:integer;paths:PPAnsiChar);cdecl;external External_Dll;
    {$IF USE_VERSION >= 80000}
    procedure proj_context_set_ca_bundle_path(ctx:PPJ_CONTEXT;path:PAnsiChar);cdecl;external External_DLL;
    {$ENDIF}

    procedure proj_context_use_proj4_init_rules(ctx:PPJ_CONTEXT;enable:integer);cdecl;external External_Dll;
    function proj_context_get_use_proj4_init_rules(ctx:PPJ_CONTEXT;from_legacy_code_path:integer):integer;cdecl;external External_Dll;

  {* Opaque structure for PROJ for a file handle. Implementations might cast it to their
   * structure/class of choice.  }
  {* Open access / mode  }
  {* Read-only access. Equivalent to "rb"  }
  {* Read-update access. File should be created if not existing. Equivalent to "r+b"  }
  {* Create access. File should be truncated to 0-byte if already existing. Equivalent to "w+b"  }
  type
    PPROJ_FILE_HANDLE = ^PROJ_FILE_HANDLE;
    PROJ_FILE_HANDLE = record
        {undefined structure}
    end;

    PROJ_OPEN_ACCESS = (PROJ_OPEN_ACCESS_READ_ONLY, PROJ_OPEN_ACCESS_READ_UPDATE, PROJ_OPEN_ACCESS_CREATE);
    {* File API callbacks  }
    PPROJ_FILE_API=^PROJ_FILE_API;
    PROJ_FILE_API = record
        version : longint;     {* Version of this structure. Should be set to 1 currently.  }
        {* Open file. Return NULL if error  }
        open_cbk : function (ctx:PPJ_CONTEXT; filename:PAnsiChar; access:PROJ_OPEN_ACCESS; user_data:pointer):PPROJ_FILE_HANDLE;cdecl;
        {* Read sizeBytes into buffer from current position and return number of bytes read  }
        read_cbk : function (ctx:PPJ_CONTEXT; _para2:PPROJ_FILE_HANDLE; buffer:pointer; sizeBytes:size_t; user_data:pointer):size_t;cdecl;
        {* Write sizeBytes into buffer from current position and return number of bytes written  }
        write_cbk : function (ctx:PPJ_CONTEXT; _para2:PPROJ_FILE_HANDLE; buffer:pointer; sizeBytes:size_t; user_data:pointer):size_t;cdecl;
        {* Seek to offset using whence=SEEK_SET/SEEK_CUR/SEEK_END. Return TRUE in case of success  }
        seek_cbk : function (ctx:PPJ_CONTEXT; _para2:PPROJ_FILE_HANDLE; offset:int64; whence:longint; user_data:pointer):longint;cdecl;
        {* Return current file position  }
        tell_cbk : function (ctx:PPJ_CONTEXT; _para2:PPROJ_FILE_HANDLE; user_data:pointer):qword;cdecl;
        {* Close file  }
        close_cbk : procedure (ctx:PPJ_CONTEXT; _para2:PPROJ_FILE_HANDLE; user_data:pointer);cdecl;
        {* Return TRUE if a file exists  }
        exists_cbk : function (ctx:PPJ_CONTEXT; filename:PAnsiChar; user_data:pointer):longint;cdecl;
        {* Return TRUE if directory exists or could be created   }
        mkdir_cbk : function (ctx:PPJ_CONTEXT; filename:PAnsiChar; user_data:pointer):longint;cdecl;
        {* Return TRUE if file could be removed   }
        unlink_cbk : function (ctx:PPJ_CONTEXT; filename:PAnsiChar; user_data:pointer):longint;cdecl;
        {* Return TRUE if file could be renamed   }
        rename_cbk : function (ctx:PPJ_CONTEXT; oldPath:PAnsiChar; newPath:PAnsiChar; user_data:pointer):longint;cdecl;
      end;
   function proj_context_set_fileapi(ctx:PPJ_CONTEXT; fileapi:PPROJ_FILE_API;user_data:Pointer):integer;cdecl;external External_Dll;
   procedure proj_context_set_sqlite3_vfs_name(ctx:PPJ_CONTEXT;name:PAnsiChar);cdecl;external External_Dll;


  {* Opaque structure for PROJ for a network handle. Implementations might cast it to their
   * structure/class of choice.  }
type
   PPROJ_NETWORK_HANDLE=^PROJ_NETWORK_HANDLE;
   PROJ_NETWORK_HANDLE = record
       {undefined structure}
   end;

  {* Network access: open callback
   * 
   * Should try to read the size_to_read first bytes at the specified offset of
   * the file given by URL url,
   * and write them to buffer. *out_size_read should be updated with the actual
   * amount of bytes read (== size_to_read if the file is larger than size_to_read).
   * During this read, the implementation should make sure to store the HTTP
   * headers from the server response to be able to respond to
   * proj_network_get_header_value_cbk_type callback.
   *
   * error_string_max_size should be the maximum size that can be written into
   * the out_error_string buffer (including terminating nul character).
   *
   * @return a non-NULL opaque handle in case of success.
    }

   Psize_t=^size_t;
   proj_network_open_cbk_type = function (ctx:PPJ_CONTEXT; url:PAnsiChar; offset:qword; size_to_read:size_t; buffer:pointer;
                 out_size_read:Psize_t; error_string_max_size:size_t; out_error_string:PAnsiChar; user_data:pointer):PPROJ_NETWORK_HANDLE;cdecl;
  {* Network access: close callback  }

    proj_network_close_cbk_type = procedure (ctx:PPJ_CONTEXT; handle:PPROJ_NETWORK_HANDLE; user_data:pointer);cdecl;
  {* Network access: get HTTP headers  }

    Pproj_network_get_header_value_cbk_type = ^proj_network_get_header_value_cbk_type;
    proj_network_get_header_value_cbk_type = function (ctx:PPJ_CONTEXT; handle:PPROJ_NETWORK_HANDLE; header_name:PAnsiChar; user_data:pointer):PAnsiChar;cdecl;
  {* Network access: read range
   *
   * Read size_to_read bytes from handle, starting at offset, into
   * buffer.
   * During this read, the implementation should make sure to store the HTTP
   * headers from the server response to be able to respond to
   * proj_network_get_header_value_cbk_type callback.
   *
   * error_string_max_size should be the maximum size that can be written into
   * the out_error_string buffer (including terminating nul character).
   *
   * @return the number of bytes actually read (0 in case of error)
    }

    proj_network_read_range_type = function (ctx:PPJ_CONTEXT; handle:PPROJ_NETWORK_HANDLE; offset:qword; size_to_read:size_t; buffer:pointer; 
                 error_string_max_size:size_t; out_error_string:PAnsiChar; user_data:pointer):size_t;cdecl;
    proj_download_progress_cbk_type= function (ctx:PPJ_CONTEXT;pct:double;user_data:pointer):integer;cdecl;

function proj_context_set_network_callbacks(
    open_cbk:proj_network_open_cbk_type;
    close_cbk:proj_network_close_cbk_type;
    get_header_value_cbk:proj_network_get_header_value_cbk_type;
    read_range_cbk:proj_network_read_range_type;
    user_data:Pointer):integer;cdecl;external External_Dll;
function proj_context_set_enable_network(ctx:PPJ_CONTEXT; enabled:integer):integer;cdecl;external External_Dll;
function proj_context_is_network_enabled(ctx:PPJ_CONTEXT):integer;cdecl;external External_Dll;
procedure proj_context_set_url_endpoint(ctx:PPJ_CONTEXT; url:PAnsiChar);cdecl;external External_Dll;
function proj_context_get_url_endpoint(ctx:PPJ_CONTEXT):PAnsiChar;cdecl;external External_Dll;
function proj_context_get_user_writable_directory(ctx:PPJ_CONTEXT;create:integer):PAnsiChar;cdecl;external External_Dll;
procedure proj_grid_cache_set_enable(ctx:PPJ_CONTEXT; enabled:integer);cdecl;external External_Dll;
procedure proj_grid_cache_set_filename(ctx:PPJ_CONTEXT; fullname:PAnsiChar);cdecl;external External_Dll;
procedure proj_grid_cache_set_max_size(ctx:PPJ_CONTEXT;max_size_MB:integer);cdecl;external External_Dll;
procedure proj_grid_cache_set_ttl(ctx:PPJ_CONTEXT;ttl_seconds:integer);cdecl;external External_Dll;
procedure proj_grid_cache_clear(ctx:PPJ_CONTEXT);cdecl;external External_Dll;
function proj_is_download_needed(ctx:PPJ_CONTEXT;url_or_filename:PAnsiChar;ignore_ttl_setting:integer):integer;cdecl;external External_Dll;
function proj_download_file(ctx:PPJ_CONTEXT;url_or_filename:PAnsiChar;ignore_ttl_setting:integer;
                                progress_cbk:proj_download_progress_cbk_type;
                                user_data:Pointer):integer;cdecl;external External_Dll;
{ Manage the transformation definition object PJ  }
function proj_create (ctx:PPJ_CONTEXT;definition:PAnsiChar):PPJ;cdecl;external External_Dll;
function proj_create_argv(ctx:PPJ_CONTEXT;argc:integer;argv:PPAnsiChar):PPJ;cdecl;external External_Dll;
function proj_create_crs_to_crs(ctx:PPJ_CONTEXT; const source_crs:PAnsiChar;const target_crs:PAnsiChar;area: PPJ_AREA):PPJ;cdecl;external External_Dll;
function proj_create_crs_to_crs_from_pj(ctx:PPJ_CONTEXT;
                                            const source_crs:PPJ;
                                            const target_crs:PPJ;
                                            area:PPJ_AREA;
                                            options:PPAnsiChar):PPJ;cdecl;external External_Dll;
function proj_normalize_for_visualization(ctx:PPJ_CONTEXT;const obj:PPJ):PPJ;cdecl;external External_Dll;
procedure proj_assign_context(pj:PPJ;ctx:PPJ_CONTEXT);cdecl;external External_Dll;
function proj_destroy (P:PPJ):PPJ;cdecl;external External_Dll;
function proj_area_create:PPJ_AREA;cdecl;external External_Dll;
procedure proj_area_set_bbox(area:PPJ_AREA;
                                 west_lon_degree:double;
                                 south_lat_degree:double;
                                 east_lon_degree:double;
                                 north_lat_degree:double);cdecl;external External_Dll;
procedure proj_area_destroy(area:PPJ_AREA);cdecl;external External_Dll;

  { Apply transformation to observation - in forward or inverse direction  }
  { Forward     }
  { Do nothing  }
  { Inverse     }
type
    PJ_DIRECTION = (PJ_FWD := 1,PJ_IDENT := 0,PJ_INV := -(1) );

function proj_angular_input(P:PPJ;dir:PJ_DIRECTION):integer;cdecl;external External_Dll;
function proj_angular_output(P:PPJ; dir:PJ_DIRECTION):integer;cdecl;external External_Dll;
function proj_degree_input(P:PPJ; dir:PJ_DIRECTION):integer;cdecl;external External_Dll;
function proj_degree_output(P:PPJ; dir:PJ_DIRECTION):integer;cdecl;external External_Dll;
function proj_trans (P:PPJ;direction:PJ_DIRECTION;coord:PJ_COORD):PJ_COORD;cdecl;external External_Dll;
function proj_trans_array (P:PPJ;direction:PJ_DIRECTION;n:size_t;coord:PPJ_COORD):integer;cdecl;external External_Dll;
function proj_trans_generic (direction:PJ_DIRECTION;
    x:Pdouble;sx:size_t;nx:size_t;
    y:Pdouble;sy:size_t;ny:size_t;
    z:Pdouble;sz:size_t;nz:size_t;
    t:Pdouble;st:size_t;nt:size_t):size_t;cdecl;external External_Dll;
  { Initializers  }
function proj_coord(x:double;y:double;z:double;t:double):PJ_COORD;cdecl;external External_Dll;
  { Measure internal consistency - in forward or inverse direction  }
function proj_roundtrip(P:PPJ;direction:PJ_DIRECTION;n:integer;coord:PPJ_COORD):double;cdecl;external External_Dll;
  { Geodesic distance between two points with angular 2D coordinates  }
function proj_lp_dist(P:PPJ;a:PJ_COORD;b:PJ_COORD):double;cdecl;external External_Dll;
  { The geodesic distance AND the vertical offset  }
function proj_lpz_dist(P:PPJ;a:PJ_COORD;b:PJ_COORD):double;cdecl;external External_Dll;
{ Euclidean distance between two points with linear 2D coordinates  }
function proj_xy_dist(a:PJ_COORD;b:PJ_COORD):double;cdecl;external External_Dll;
  { Euclidean distance between two points with linear 3D coordinates  }
function proj_xyz_dist(a:PJ_COORD;b:PJ_COORD):double;cdecl;external External_Dll;
  { Geodesic distance (in meter) + fwd and rev azimuth between two points on the ellipsoid  }
function proj_geod(P:PPJ;a:PJ_COORD;b:PJ_COORD):PJ_COORD;cdecl;external External_Dll;

{/* PROJ error codes */

/** Error codes typically related to coordinate operation initialization
 * Note: some of them can also be emitted during coordinate transformation,
 * like PROJ_ERR_INVALID_OP_FILE_NOT_FOUND_OR_INVALID in case the resource loading
 * is deferred until it is really needed.
 */
 }
const

PROJ_ERR_INVALID_OP=                           1024;                        //* other/unspecified error related to coordinate operation initialization */
PROJ_ERR_INVALID_OP_WRONG_SYNTAX=              (PROJ_ERR_INVALID_OP+1);     //* invalid pipeline structure, missing +proj argument, etc */
PROJ_ERR_INVALID_OP_MISSING_ARG=               (PROJ_ERR_INVALID_OP+2);     //* missing required operation parameter */
PROJ_ERR_INVALID_OP_ILLEGAL_ARG_VALUE=         (PROJ_ERR_INVALID_OP+3);     //* one of the operation parameter has an illegal value */
PROJ_ERR_INVALID_OP_MUTUALLY_EXCLUSIVE_ARGS=   (PROJ_ERR_INVALID_OP+4);     //* mutually exclusive arguments */
PROJ_ERR_INVALID_OP_FILE_NOT_FOUND_OR_INVALID= (PROJ_ERR_INVALID_OP+5);     //* file not found (particular case of PROJ_ERR_INVALID_OP_ILLEGAL_ARG_VALUE) */

//** Error codes related to transformation on a specific coordinate */
PROJ_ERR_COORD_TRANSFM=                           2048;                           //* other error related to coordinate transformation */
PROJ_ERR_COORD_TRANSFM_INVALID_COORD=             (PROJ_ERR_COORD_TRANSFM+1);     //* for e.g lat > 90deg */
PROJ_ERR_COORD_TRANSFM_OUTSIDE_PROJECTION_DOMAIN= (PROJ_ERR_COORD_TRANSFM+2);     //* coordinate is outside of the projection domain. e.g approximate mercator with |longitude - lon_0| > 90deg, or iterative convergence method failed */
PROJ_ERR_COORD_TRANSFM_NO_OPERATION=              (PROJ_ERR_COORD_TRANSFM+3);     //* no operation found, e.g if no match the required accuracy, or if ballpark transformations were asked to not be used and they would be only such candidate */
PROJ_ERR_COORD_TRANSFM_OUTSIDE_GRID=              (PROJ_ERR_COORD_TRANSFM+4);     //* point to transform falls outside grid or subgrid */
PROJ_ERR_COORD_TRANSFM_GRID_AT_NODATA=            (PROJ_ERR_COORD_TRANSFM+5);     //* point to transform falls in a grid cell that evaluates to nodata */

//** Other type of errors */
PROJ_ERR_OTHER=                                   4096;
PROJ_ERR_OTHER_API_MISUSE=                        (PROJ_ERR_OTHER+1);             //* error related to a misuse of PROJ API */
PROJ_ERR_OTHER_NO_INVERSE_OP=                     (PROJ_ERR_OTHER+2);             //* no inverse method available */
PROJ_ERR_OTHER_NETWORK_ERROR=                     (PROJ_ERR_OTHER+3);             //* failure when accessing a network resource */


  { Set or read error level  }
function proj_context_errno(ctx:PPJ_CONTEXT):integer;cdecl;external External_Dll;
function proj_errno (P:PPJ):integer;cdecl;external External_Dll;
function proj_errno_set(P:PPJ;err:integer):integer;cdecl;external External_Dll;
function proj_errno_reset(P:PPJ):integer;cdecl;external External_Dll;
function proj_errno_restore(P:PPJ;err:integer):integer;cdecl;external External_Dll;
function proj_errno_string (err:integer):PAnsiChar;cdecl;external External_Dll; {$IF USE_VERSION>=80000}deprecated 'use proj_context_errno_string'; {$ENDIF}
{$IF USE_VERSION >= 80000}
function proj_context_errno_string (ctx:PPJ_CONTEXT;err:integer):PAnsiChar;cdecl;external External_Dll;
{$ENDIF}

function proj_log_level(ctx:PPJ_CONTEXT;log_level:PJ_LOG_LEVEL):PJ_LOG_LEVEL;cdecl;external External_Dll;
procedure proj_log_func(ctx:PPJ_CONTEXT;app_data:Pointer;logf:PJ_LOG_FUNCTION);cdecl;external External_Dll;
{ Scaling and angular distortion factors  }
function proj_factors(P:PPJ;lp:PJ_COORD):PJ_FACTORS;cdecl;external External_Dll;
{ Info functions - get information about various PROJ.4 entities  }
function proj_info:PJ_INFO;cdecl;external External_Dll;
function proj_pj_info(p:PPJ):PJ_INFO;cdecl;external External_Dll;
function proj_init_info(initname:PAnsiChar):PJ_INIT_INFO;cdecl;external External_Dll;
{ List functions:  }
{ Get lists of operations, ellipsoids, units and prime meridians.  }
function proj_list_operations:PPJ_OPERATIONS;cdecl;external External_Dll;
function proj_list_ellps:PPJ_ELLPS;cdecl;external External_Dll;
function proj_list_units:PPJ_UNITS;cdecl;external External_Dll;deprecated 'Deprecated by proj_get_units_from_database';
function proj_list_angular_units:PPJ_UNITS;cdecl;external External_Dll;deprecated 'Deprecated by proj_get_units_from_database';
function proj_list_prime_meridians:PPJ_PRIME_MERIDIANS;cdecl;external External_Dll;
  { These are trivial, and while occasionally useful in real code, primarily here to       }
  { simplify demo code, and in acknowledgement of the proj-internal discrepancy between    }
  { angular units expected by classical proj, and by Charles Karney's geodesics subsystem  }
function proj_torad(angle_in_degrees:double):double;cdecl;external External_Dll;
function proj_todeg(angle_in_radians:double):double;cdecl;external External_Dll;
function proj_dmstor(is_:PAnsiChar;rs:PPAnsiChar):double;cdecl;external External_Dll;
function proj_rtodms(s:PAnsiChar;r:double;pos:integer;neg:integer):PAnsiChar;cdecl;external External_Dll;
procedure proj_cleanup;cdecl;external External_Dll;

type
  { -------------------------------------------------------------------------  }
  { Binding in C of C++ API  }
  { -------------------------------------------------------------------------  }
  {* @defgroup iso19111_types Data types for ISO19111 C API
   *  Data types for ISO19111 C API
   *  @
    }
  {* \brief Type representing a NULL terminated list of NULL-terminate strings.  }

    PROJ_STRING_LIST = ^PAnsiChar;
    PPROJ_STRING_LIST = ^PROJ_STRING_LIST;

  {* \brief Guessed WKT "dialect".  }
  {* \ref WKT2_2019  }
  {* Deprecated alias for PJ_GUESSED_WKT2_2019  }
  {* \ref WKT2_2015  }
  {* \ref WKT1  }
  {* ESRI variant of WKT1  }
  {* Not WKT / unrecognized  }

    PJ_GUESSED_WKT_DIALECT = (PJ_GUESSED_WKT2_2019,PJ_GUESSED_WKT2_2018 := PJ_GUESSED_WKT2_2019,
      PJ_GUESSED_WKT2_2015,PJ_GUESSED_WKT1_GDAL,
      PJ_GUESSED_WKT1_ESRI,PJ_GUESSED_NOT_WKT
      );
  {* \brief Object category.  }

    PJ_CATEGORY = (PJ_CATEGORY_ELLIPSOID,PJ_CATEGORY_PRIME_MERIDIAN,
      PJ_CATEGORY_DATUM,PJ_CATEGORY_CRS,PJ_CATEGORY_COORDINATE_OPERATION
      {$IF USE_VERSION >= 80000}
      ,PJ_CATEGORY_DATUM_ENSEMBLE
      {$ENDIF}
      );
  {* \brief Object type.  }
  {* Abstract type, not returned by proj_get_type()  }
  {* proj_get_type() will never return that type, but
       * PJ_TYPE_GEOGRAPHIC_2D_CRS or PJ_TYPE_GEOGRAPHIC_3D_CRS.  }

    PPJ_TYPE = ^PJ_TYPE;
    PJ_TYPE = (PJ_TYPE_UNKNOWN,PJ_TYPE_ELLIPSOID,PJ_TYPE_PRIME_MERIDIAN,
      PJ_TYPE_GEODETIC_REFERENCE_FRAME,PJ_TYPE_DYNAMIC_GEODETIC_REFERENCE_FRAME,
      PJ_TYPE_VERTICAL_REFERENCE_FRAME,PJ_TYPE_DYNAMIC_VERTICAL_REFERENCE_FRAME,
      PJ_TYPE_DATUM_ENSEMBLE,PJ_TYPE_CRS,PJ_TYPE_GEODETIC_CRS,
      PJ_TYPE_GEOCENTRIC_CRS,PJ_TYPE_GEOGRAPHIC_CRS,
      PJ_TYPE_GEOGRAPHIC_2D_CRS,PJ_TYPE_GEOGRAPHIC_3D_CRS,
      PJ_TYPE_VERTICAL_CRS,PJ_TYPE_PROJECTED_CRS,
      PJ_TYPE_COMPOUND_CRS,PJ_TYPE_TEMPORAL_CRS,
      PJ_TYPE_ENGINEERING_CRS,PJ_TYPE_BOUND_CRS,
      PJ_TYPE_OTHER_CRS,PJ_TYPE_CONVERSION,
      PJ_TYPE_TRANSFORMATION,PJ_TYPE_CONCATENATED_OPERATION,
      PJ_TYPE_OTHER_COORDINATE_OPERATION
      {$IF USE_VERSION >= 80000}
      ,PJ_TYPE_TEMPORAL_DATUM,
      PJ_TYPE_ENGINEERING_DATUM,
      PJ_TYPE_PARAMETRIC_DATUM
      {$ENDIF}
      );
  {* Comparison criterion.  }
  {* All properties are identical.  }
  {* The objects are equivalent for the purpose of coordinate
      * operations. They can differ by the name of their objects,
      * identifiers, other metadata.
      * Parameters may be expressed in different units, provided that the
      * value is (with some tolerance) the same once expressed in a
      * common unit.
       }
  {* Same as EQUIVALENT, relaxed with an exception that the axis order
      * of the base CRS of a DerivedCRS/ProjectedCRS or the axis order of
      * a GeographicCRS is ignored. Only to be used
      * with DerivedCRS/ProjectedCRS/GeographicCRS  }

    PJ_COMPARISON_CRITERION = (PJ_COMP_STRICT,PJ_COMP_EQUIVALENT,PJ_COMP_EQUIVALENT_EXCEPT_AXIS_ORDER_GEOGCRS
      );
  {* \brief WKT version.  }
  {* cf osgeo::proj::io::WKTFormatter::Convention::WKT2  }
  {* cf osgeo::proj::io::WKTFormatter::Convention::WKT2_SIMPLIFIED  }
  {* cf osgeo::proj::io::WKTFormatter::Convention::WKT2_2019  }
  {* Deprecated alias for PJ_WKT2_2019  }
  {* cf osgeo::proj::io::WKTFormatter::Convention::WKT2_2019_SIMPLIFIED  }
  {* Deprecated alias for PJ_WKT2_2019  }
  {* cf osgeo::proj::io::WKTFormatter::Convention::WKT1_GDAL  }
  {* cf osgeo::proj::io::WKTFormatter::Convention::WKT1_ESRI  }

    PJ_WKT_TYPE = (PJ_WKT2_2015,PJ_WKT2_2015_SIMPLIFIED,
      PJ_WKT2_2019,PJ_WKT2_2018 := PJ_WKT2_2019,PJ_WKT2_2019_SIMPLIFIED,
      PJ_WKT2_2018_SIMPLIFIED := PJ_WKT2_2019_SIMPLIFIED,PJ_WKT1_GDAL,
      PJ_WKT1_ESRI);
  {* Specify how source and target CRS extent should be used to restrict
    * candidate operations (only taken into account if no explicit area of
    * interest is specified.  }
  {* Ignore CRS extent  }
  {* Test coordinate operation extent against both CRS extent.  }
  {* Test coordinate operation extent against the intersection of both
          CRS extent.  }
  {* Test coordinate operation against the smallest of both CRS extent.  }

    PROJ_CRS_EXTENT_USE = (PJ_CRS_EXTENT_NONE,PJ_CRS_EXTENT_BOTH,
      PJ_CRS_EXTENT_INTERSECTION,PJ_CRS_EXTENT_SMALLEST
      );
  {* Describe how grid availability is used.  }
  {* Grid availability is only used for sorting results. Operations
          * where some grids are missing will be sorted last.  }
  {* Completely discard an operation if a required grid is missing.  }
  {* Ignore grid availability at all. Results will be presented as if
          * all grids were available.  }
  {* Results will be presented as if grids known to PROJ (that is
      * registered in the grid_alternatives table of its database) were
      * available. Used typically when networking is enabled.
       }

    PROJ_GRID_AVAILABILITY_USE = (PROJ_GRID_AVAILABILITY_USED_FOR_SORTING,
      PROJ_GRID_AVAILABILITY_DISCARD_OPERATION_IF_MISSING_GRID,
      PROJ_GRID_AVAILABILITY_IGNORED,PROJ_GRID_AVAILABILITY_KNOWN_AVAILABLE
      );
  {* \brief PROJ string version.  }
  {* cf osgeo::proj::io::PROJStringFormatter::Convention::PROJ_5  }
  {* cf osgeo::proj::io::PROJStringFormatter::Convention::PROJ_4  }

    PJ_PROJ_STRING_TYPE = (PJ_PROJ_5,PJ_PROJ_4);
  {* Spatial criterion to restrict candidate operations.  }
  {* The area of validity of transforms should strictly contain the
          * are of interest.  }
  {* The area of validity of transforms should at least intersect the
          * area of interest.  }

    PROJ_SPATIAL_CRITERION = (PROJ_SPATIAL_CRITERION_STRICT_CONTAINMENT,
      PROJ_SPATIAL_CRITERION_PARTIAL_INTERSECTION
      );
  {* Describe if and how intermediate CRS should be used  }
  {* Always search for intermediate CRS.  }
  {* Only attempt looking for intermediate CRS if there is no direct
          * transformation available.  }
  { Do not attempt looking for intermediate CRS.  }

    PROJ_INTERMEDIATE_CRS_USE = (PROJ_INTERMEDIATE_CRS_USE_ALWAYS,PROJ_INTERMEDIATE_CRS_USE_IF_NO_DIRECT_TRANSFORMATION,
      PROJ_INTERMEDIATE_CRS_USE_NEVER);
  {* Type of coordinate system.  }

    PJ_COORDINATE_SYSTEM_TYPE = (PJ_CS_TYPE_UNKNOWN,PJ_CS_TYPE_CARTESIAN,
      PJ_CS_TYPE_ELLIPSOIDAL,PJ_CS_TYPE_VERTICAL,
      PJ_CS_TYPE_SPHERICAL,PJ_CS_TYPE_ORDINAL,
      PJ_CS_TYPE_PARAMETRIC,PJ_CS_TYPE_DATETIMETEMPORAL,
      PJ_CS_TYPE_TEMPORALCOUNT,PJ_CS_TYPE_TEMPORALMEASURE
      );
  {* \brief Structure given overall description of a CRS.
   *
   * This structure may grow over time, and should not be directly allocated by
   * client code.
  }
    PPROJ_CRS_INFO = ^PROJ_CRS_INFO;
    PPPROJ_CRS_INFO = ^PPROJ_CRS_INFO;
    PROJ_CRS_INFO = record
        auth_name : PAnsiChar;         {* Authority name.  }
        code : PAnsiChar;              {* Object code.  }
        name : PAnsiChar;              {* Object name.  }
        _type : PJ_TYPE;           {* Object type.  }
        deprecated : longint;      {* Whether the object is deprecated  }
        bbox_valid : longint;      {* Whereas the west_lon_degree, south_lat_degree, east_lon_degree and
                                    * north_lat_degree fields are valid.  }
        west_lon_degree : double;  {* Western-most longitude of the area of use, in degrees.  }
        south_lat_degree : double; {* Southern-most latitude of the area of use, in degrees.  }
        east_lon_degree : double;  {* Eastern-most longitude of the area of use, in degrees.  }
        north_lat_degree : double; {* Northern-most latitude of the area of use, in degrees.  }
        area_name : PAnsiChar;         {* Name of the area of use.  }
        projection_method_name : PAnsiChar;  {* Name of the projection method for a projected CRS. Might be NULL even
                                          *for projected CRS in some cases.  }
      end;
  {* \brief Structure describing optional parameters for proj_get_crs_list();
   *
   * This structure may grow over time, and should not be directly allocated by
   * client code.
    }
  {* Array of allowed object types. Should be NULL if all types are allowed }
(* Const before type ignored *)
  {* Size of types. Should be 0 if all types are allowed }
  {* If TRUE and bbox_valid == TRUE, then only CRS whose area of use
       * entirely contains the specified bounding box will be returned.
       * If FALSE and bbox_valid == TRUE, then only CRS whose area of use
       * intersects the specified bounding box will be returned.
        }
  {* To set to TRUE so that west_lon_degree, south_lat_degree,
       * east_lon_degree and north_lat_degree fields are taken into account.  }
  {* Western-most longitude of the area of use, in degrees.  }
  {* Southern-most latitude of the area of use, in degrees.  }
  {* Eastern-most longitude of the area of use, in degrees.  }
  {* Northern-most latitude of the area of use, in degrees.  }
  {* Whether deprecated objects are allowed. Default to FALSE.  }

    PPROJ_CRS_LIST_PARAMETERS = ^PROJ_CRS_LIST_PARAMETERS;
    PROJ_CRS_LIST_PARAMETERS = record
        types : PPJ_TYPE;
        typesCount : size_t;
        crs_area_of_use_contains_bbox : longint;
        bbox_valid : longint;
        west_lon_degree : double;
        south_lat_degree : double;
        east_lon_degree : double;
        north_lat_degree : double;
        allow_deprecated : longint;
      end;
  {* \brief Structure given description of a unit.
   *
   * This structure may grow over time, and should not be directly allocated by
   * client code.
   * @since 7.1
    }
  {* Authority name.  }
  {* Object code.  }
  {* Object name. For example "metre", "US survey foot", etc.  }
  {* Category of the unit: one of "linear", "linear_per_time", "angular",
       * "angular_per_time", "scale", "scale_per_time" or "time"  }
  {* Conversion factor to apply to transform from that unit to the
       * corresponding SI unit (metre for "linear", radian for "angular", etc.).
       * It might be 0 in some cases to indicate no known conversion factor.  }
  {* PROJ short name, like "m", "ft", "us-ft", etc... Might be NULL  }
  {* Whether the object is deprecated  }

    PPROJ_UNIT_INFO=^PROJ_UNIT_INFO;
    PPPROJ_UNIT_INFO= ^PPROJ_UNIT_INFO;
    PROJ_UNIT_INFO = record
        auth_name : PAnsiChar;
        code : PAnsiChar;
        name : PAnsiChar;
        category : PAnsiChar;
        conv_factor : double;
        proj_short_name : PAnsiChar;
        deprecated : longint;
      end;

      PPJ_OBJ_LIST = ^PJ_OBJ_LIST;
      PJ_OBJ_LIST = record
          {undefined structure}
      end;
     PPInteger=^PInteger;

  {*@ }
  {*
   * \defgroup iso19111_functions Binding in C of basic methods from the C++ API
   *  Functions for ISO19111 C API
   *
   * The PJ* objects returned by proj_create_from_wkt(),
   * proj_create_from_database() and other functions in that section
   * will have generally minimal interaction with the functions declared in the
   * upper section of this header file (calling those functions on those objects
   * will either return an error or default/non-sensical values). The exception is
   * for ISO19111 objects of type CoordinateOperation that can be exported as a
   * valid PROJ pipeline. In this case, the PJ objects will work for example with
   * proj_trans_generic().
   * Conversely, objects returned by proj_create() and proj_create_argv(), which
   * are not of type CRS, will return an error when used with functions of this section.
   * @
    }
procedure proj_string_list_destroy(list:PROJ_STRING_LIST);cdecl;external External_Dll;
procedure proj_context_set_autoclose_database(ctx:PPJ_CONTEXT;autoclose:integer);cdecl;external External_Dll;
function proj_context_set_database_path(ctx:PPJ_CONTEXT;dbPath:PAnsiChar;auxDbPaths:PPAnsiChar;options:PPAnsiChar):integer;cdecl;external External_Dll;
function proj_context_get_database_path(ctx:PPJ_CONTEXT):PAnsiChar;cdecl;external External_Dll;
function proj_context_get_database_metadata(ctx:PPJ_CONTEXT;key:PAnsiChar):PAnsiChar;cdecl;external External_Dll;
function proj_context_guess_wkt_dialect(ctx:PPJ_CONTEXT;wkt:PAnsiChar):PJ_GUESSED_WKT_DIALECT;cdecl;external External_Dll;
function proj_create_from_wkt(ctx:PPJ_CONTEXT;wkt:PAnsiChar;options:PPAnsiChar;
                                          out_warnings:PPROJ_STRING_LIST;
                                          out_grammar_errors:PPROJ_STRING_LIST):PPJ;cdecl;external External_Dll;
function proj_create_from_database(ctx:PPJ_CONTEXT;auth_name:PAnsiChar;code:PAnsiChar;
                                               category:PJ_CATEGORY;
                                               usePROJAlternativeGridNames:integer;
                                               options:PPAnsiChar):PPJ;cdecl;external External_Dll;
function proj_uom_get_info_from_database(ctx:PPJ_CONTEXT;
                               auth_name:PAnsiChar;
                               code:PAnsiChar;
                               out_name:PPAnsiChar;
                               out_conv_factor:PDouble;
                               out_category:PPAnsiChar):integer;cdecl;external External_Dll;
function proj_grid_get_info_from_database(ctx:PPJ_CONTEXT;
                              grid_name:PAnsiChar;
                              out_full_name:PPAnsiChar;
                              out_package_name:PPAnsiChar;
                              out_url:PPAnsiChar;
                              out_direct_download:PInteger;
                              out_open_license:PInteger;
                              out_available:PInteger):integer;cdecl;external External_Dll;
function proj_clone(ctx:PPJ_CONTEXT;obj:PPJ):PPJ;cdecl;external External_Dll;
function proj_create_from_name(ctx:PPJ_CONTEXT; auth_name:PAnsiChar;
                                                searchedName:PAnsiChar;
                                                types:PPJ_TYPE;
                                                typesCount:size_t;
                                                approximateMatch:integer;
                                                limitResultCount:size_t;
                                                options:PPAnsiChar):PPJ_OBJ_LIST;cdecl;external External_Dll;
function proj_get_type(obj:PPJ):PJ_TYPE;cdecl;external External_Dll;
function proj_is_deprecated(obj:PPJ):integer;cdecl;external External_Dll;
function proj_get_non_deprecated(ctx:PPJ_CONTEXT;obj:PPJ):PPJ_OBJ_LIST;cdecl;external External_Dll;
function proj_is_equivalent_to(obj:PPJ;other:PPJ;criterion:PJ_COMPARISON_CRITERION):integer;cdecl;external External_Dll;
function proj_is_equivalent_to_with_ctx(ctx:PPJ_CONTEXT;obj:PPJ;other:PPJ;criterion:PJ_COMPARISON_CRITERION):integer;cdecl;external External_Dll;
function proj_is_crs(obj:PPJ):integer;cdecl;external External_Dll;
function proj_get_name(obj:PPJ):PAnsiChar;cdecl;external External_Dll;
function proj_get_id_auth_name(obj:PPJ;index:integer):PAnsiChar;cdecl;external External_Dll;
function proj_get_id_code(obj:PPJ;index:integer):PAnsiChar;cdecl;external External_Dll;
function proj_get_remarks(obj:PPJ):PAnsiChar;cdecl;external External_Dll;
function proj_get_scope(obj:PPJ):PAnsiChar;cdecl;external External_Dll;
function proj_get_area_of_use(ctx:PPJ_CONTEXT; bj:PPJ;
                                      out_west_lon_degree:PDouble;
                                      out_south_lat_degree:PDouble;
                                      out_east_lon_degree:PDouble;
                                      out_north_lat_degree:PDouble;
                                      out_area_name:PPAnsiChar):integer;cdecl;external External_Dll;
function proj_as_wkt(ctx:PPJ_CONTEXT; obj:PPJ;type_:PJ_WKT_TYPE; options:PPAnsiChar):PAnsiChar;cdecl;external External_Dll;
function proj_as_proj_string(ctx:PPJ_CONTEXT; obj:PPJ; type_:PJ_PROJ_STRING_TYPE;
                                             options:PPAnsiChar):PAnsiChar;cdecl;external External_Dll;
function proj_as_projjson(ctx:PPJ_CONTEXT; obj:PPJ; options:PPAnsiChar):PAnsiChar;cdecl;external External_Dll;
function proj_get_source_crs(ctx:PPJ_CONTEXT;obj:PPJ):PPJ;cdecl;external External_Dll;
function proj_get_target_crs(ctx:PPJ_CONTEXT;obj:PPJ):PPJ;cdecl;external External_Dll;
function proj_identify(ctx:PPJ_CONTEXT;obj:PPJ;auth_name:PAnsiChar;options:PPAnsiChar;
                                       out_confidence:PPInteger):PPJ_OBJ_LIST;cdecl;external External_Dll;
procedure proj_int_list_destroy(list:PInteger);cdecl;external External_Dll;
  { -------------------------------------------------------------------------  }
function proj_get_authorities_from_database(ctx:PPJ_CONTEXT):PROJ_STRING_LIST;cdecl;external External_Dll;
function proj_get_codes_from_database(ctx:PPJ_CONTEXT; auth_name:PAnsiChar;type_:PJ_TYPE;
                                             allow_deprecated:integer):PROJ_STRING_LIST;cdecl;external External_Dll;
function proj_get_crs_list_parameters_create:PPROJ_CRS_LIST_PARAMETERS;cdecl;external External_Dll;
procedure proj_get_crs_list_parameters_destroy( params:PPROJ_CRS_LIST_PARAMETERS);cdecl;external External_Dll;
function proj_get_crs_info_list_from_database(auth_name:PAnsiChar;
                                      params:PPROJ_CRS_LIST_PARAMETERS;
                                      out_result_count:PInteger):PPPROJ_CRS_INFO;cdecl;external External_Dll;
procedure proj_crs_info_list_destroy(list:PPPROJ_CRS_INFO);cdecl;external External_Dll;
function proj_get_units_from_database(auth_name:PAnsiChar; category:PAnsiChar;allow_deprecated:integer;
                                            out_result_count:PInteger):PPPROJ_UNIT_INFO;cdecl;external External_Dll;
procedure proj_unit_list_destroy(list:PPPROJ_UNIT_INFO);cdecl;external External_Dll;


//* ------------------------------------------------------------------------- */
//*! @cond Doxygen_Suppress */
{$IF USE_VERSION >= 80000}
type
PPJ_INSERT_SESSION = ^PJ_INSERT_SESSION;
PJ_INSERT_SESSION = record
    {undefined structure}
end;

function proj_insert_object_session_create(ctx:PPJ_CONTEXT):PPJ_INSERT_SESSION;cdecl;external External_Dll;
procedure proj_insert_object_session_destroy(ctx:PPJ_CONTEXT;session:PPJ_INSERT_SESSION);cdecl;external External_Dll;
function proj_get_insert_statements(ctx:PPJ_CONTEXT;session:PPJ_INSERT_SESSION;
                                            object1:PPJ;
                                            authority:PAnsiChar;
                                            code:PAnsiChar;
                                            numeric_codes:integer;
                                            allowed_authorities:PPAnsiChar;
                                            options:PPAnsiChar):PROJ_STRING_LIST;cdecl;external External_Dll;
function proj_suggests_code_for(ctx:PPJ_CONTEXT;object1:PPJ;authority:PAnsiChar;numeric_code:integer;
                                    options:PPAnsiChar):PAnsiChar;cdecl;external External_Dll;
procedure proj_string_destroy(str:PAnsiChar);cdecl;external External_Dll;
{$ENDIF}
//* ------------------------------------------------------------------------- */
//*! @cond Doxygen_Suppress */
  { -------------------------------------------------------------------------  }
type
PPJ_OPERATION_FACTORY_CONTEXT = ^PJ_OPERATION_FACTORY_CONTEXT;
PJ_OPERATION_FACTORY_CONTEXT = record
    {undefined structure}
end;

function proj_create_operation_factory_context(authority:PAnsiChar):PPJ_OPERATION_FACTORY_CONTEXT;cdecl;external External_Dll;
procedure proj_operation_factory_context_destroy(ctx:PPJ_OPERATION_FACTORY_CONTEXT);cdecl;external External_Dll;
procedure proj_operation_factory_context_set_desired_accuracy(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
                                            accuracy:double);cdecl;external External_Dll;
procedure  proj_operation_factory_context_set_area_of_interest(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
                                             west_lon_degree:double;
                                             south_lat_degree:double;
                                             east_lon_degree:double;
                                             north_lat_degree:double);cdecl;external External_Dll;
procedure  proj_operation_factory_context_set_crs_extent_use(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
                                            use:PROJ_CRS_EXTENT_USE);cdecl;external External_Dll;
procedure  proj_operation_factory_context_set_spatial_criterion(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
                                           criterion:PROJ_SPATIAL_CRITERION);cdecl;external External_Dll;
procedure  proj_operation_factory_context_set_grid_availability_use(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
                                           use:PROJ_GRID_AVAILABILITY_USE);cdecl;external External_Dll;
procedure  proj_operation_factory_context_set_use_proj_alternative_grid_names(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
     usePROJNames:integer);cdecl;external External_Dll;
procedure  proj_operation_factory_context_set_allow_use_intermediate_crs(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
    use:PROJ_INTERMEDIATE_CRS_USE );cdecl;external External_Dll;
procedure  proj_operation_factory_context_set_allowed_intermediate_crs(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
   list_of_auth_name_codes:PPAnsiChar);cdecl;external External_Dll;
procedure  proj_operation_factory_context_set_discard_superseded(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
    discard:integer);cdecl;external External_Dll;
procedure  proj_operation_factory_context_set_allow_ballpark_transformations(factory_ctx:PPJ_OPERATION_FACTORY_CONTEXT;
    allow:integer);cdecl;external External_Dll;

  { -------------------------------------------------------------------------  }
function proj_create_operations(source_crs:PPJ; target_crs:PPJ;
                            operationContext:PPJ_OPERATION_FACTORY_CONTEXT):PPJ_OBJ_LIST;cdecl;external External_Dll;
function proj_list_get_count(result_:PPJ_OBJ_LIST):integer;cdecl;external External_Dll;
function proj_list_get(ctx:PPJ_CONTEXT; result_:PPJ_OBJ_LIST; index:integer):PPJ;cdecl;external External_Dll;
procedure proj_list_destroy(result_:PPJ_OBJ_LIST);cdecl;external External_Dll;
function  proj_get_suggested_operation(ctx:PPJ_CONTEXT; operations:PPJ_OBJ_LIST; direction:PJ_DIRECTION;
                                          coord:PJ_COORD):integer;cdecl;external External_Dll;

  { -------------------------------------------------------------------------  }
function proj_crs_get_geodetic_crs(ctx:PPJ_CONTEXT; crs:PPJ):PPJ;cdecl;external External_Dll;
function proj_crs_get_horizontal_datum(ctx:PPJ_CONTEXT; crs:PPJ):PPJ;cdecl;external External_Dll;
function proj_crs_get_sub_crs(ctx:PPJ_CONTEXT; crs:PPJ; index:integer):PPJ;cdecl;external External_Dll;
function proj_crs_get_datum(ctx:PPJ_CONTEXT; crs:PPJ):PPJ;cdecl;external External_Dll;
{$IF USE_VERSION >= 80000}
function proj_crs_is_derived(ctx:PPJ_CONTEXT; crs:PPJ):integer;cdecl;external External_Dll;
function proj_crs_get_datum_ensemble(ctx:PPJ_CONTEXT; crs:PPJ):PPJ;cdecl;external External_Dll;
function proj_crs_get_datum_forced(ctx:PPJ_CONTEXT; crs:PPJ):PPJ;cdecl;external External_Dll;
function proj_datum_ensemble_get_member_count(ctx:PPJ_CONTEXT;datum_ensemble:PPJ):integer;cdecl;external External_Dll;
function proj_datum_ensemble_get_accuracy(ctx:PPJ_CONTEXT;datum_ensemble:PPJ):double;cdecl;external External_Dll;
function proj_datum_ensemble_get_member(ctx:PPJ_CONTEXT;datum_ensemble:PPJ;member_index:integer):PPJ;cdecl;external External_Dll;
function proj_dynamic_datum_get_frame_reference_epoch(ctx:PPJ_CONTEXT;datum:PPJ):double;cdecl;external External_Dll;
{$ENDIF}
function proj_crs_get_coordinate_system(ctx:PPJ_CONTEXT; crs:PPJ):PPJ;cdecl;external External_Dll;
function proj_cs_get_type(ctx:PPJ_CONTEXT; cs:PPJ):PJ_COORDINATE_SYSTEM_TYPE;cdecl;external External_Dll;
function  proj_cs_get_axis_count(ctx:PPJ_CONTEXT; cs:PPJ):integer;cdecl;external External_Dll;
function  proj_cs_get_axis_info(ctx:PPJ_CONTEXT; cx:PPJ;index:integer;
                                       out_name:PPAnsiChar;
                                       out_abbrev:PPAnsiChar;
                                       out_direction:PPAnsiChar;
                                       out_unit_conv_factor:PDouble;
                                       out_unit_name:PPAnsiChar;
                                       out_unit_auth_name:PPAnsiChar;
                                       out_unit_code:PPAnsiChar):integer;cdecl;external External_Dll;
function proj_get_ellipsoid(ctx:PPJ_CONTEXT; obj:PPJ):PPJ;cdecl;external External_Dll;
function  proj_ellipsoid_get_parameters(ctx:PPJ_CONTEXT; ellipsoid:PPJ;
                                       out_semi_major_metre:PDouble;
                                       out_semi_minor_metre:PDouble;
                                       out_is_semi_minor_computed:PInteger;
                                       out_inv_flattening:PDouble):integer;cdecl;external External_Dll;
function proj_get_prime_meridian(ctx:PPJ_CONTEXT; obj:PPJ):PPJ;cdecl;external External_Dll;
function  proj_prime_meridian_get_parameters(ctx:PPJ_CONTEXT;prime_meridian:PPJ;
                                               out_longitude:PDouble;
                                               out_unit_conv_factor:PDouble;
                                               out_unit_name:PPAnsiChar):integer;cdecl;external External_Dll;
function proj_crs_get_coordoperation(ctx:PPJ_CONTEXT; crs:PPJ):PPJ;cdecl;external External_Dll;
function  proj_coordoperation_get_method_info(ctx:PPJ_CONTEXT; coordoperation:PPJ;
                                                 out_method_name:PPAnsiChar;
                                                 out_method_auth_name:PPAnsiChar;
                                                 out_method_code:PPAnsiChar):integer;cdecl;external External_Dll;
function  proj_coordoperation_is_instantiable(ctx:PPJ_CONTEXT;coordoperation:PPJ):integer;cdecl;external External_Dll;
function  proj_coordoperation_has_ballpark_transformation(ctx:PPJ_CONTEXT; coordoperation:PPJ):integer;cdecl;external External_Dll;
function  proj_coordoperation_get_param_count(ctx:PPJ_CONTEXT; coordoperation:PPJ):integer;cdecl;external External_Dll;
function  proj_coordoperation_get_param_index(ctx:PPJ_CONTEXT; coordoperation:PPJ;name:PAnsiChar):integer;cdecl;external External_Dll;
function  proj_coordoperation_get_param(ctx:PPJ_CONTEXT; coordoperation:PPJ;
                                           index:integer;
                                           out_name:PPAnsiChar;
                                           out_auth_name:PPAnsiChar;
                                           out_code:PPAnsiChar;
                                           out_value:PDouble;
                                           out_value_string:PPAnsiChar;
                                           out_unit_conv_factor:PDouble;
                                           out_unit_name:PPAnsiChar;
                                           out_unit_auth_name:PPAnsiChar;
                                           out_unit_code:PPAnsiChar;
                                           out_unit_category:PPAnsiChar):integer;cdecl;external External_Dll;
function  proj_coordoperation_get_grid_used_count(ctx:PPJ_CONTEXT;coordoperation:PPJ):integer;cdecl;external External_Dll;
function  proj_coordoperation_get_grid_used(ctx:PPJ_CONTEXT;coordoperation:PPJ;
                                           index:integer;
                                           out_short_name:PPAnsiChar;
                                           out_full_name:PPAnsiChar;
                                           out_package_name:PPAnsiChar;
                                           out_url:PPAnsiChar;
                                           out_direct_download:PInteger;
                                           out_open_license:PInteger;
                                           out_available:PInteger):integer;cdecl;external External_Dll;
function proj_coordoperation_get_accuracy(ctx:PPJ_CONTEXT; obj:PPJ):double;cdecl;external External_Dll;
function  proj_coordoperation_get_towgs84_values(ctx:PPJ_CONTEXT; coordoperation:PPJ;
                                           out_values:PDouble;
                                           value_count:integer;
                                           emit_error_if_incompatible:integer):integer;cdecl;external External_Dll;
function proj_coordoperation_create_inverse(ctx:PPJ_CONTEXT; obj:PPJ):PPJ;cdecl;external External_Dll;
function  proj_concatoperation_get_step_count(ctx:PPJ_CONTEXT; concatoperation:PPJ):integer;cdecl;external External_Dll;
function proj_concatoperation_get_step(ctx:PPJ_CONTEXT; concatoperation:PPJ;i_step:integer):PPJ;cdecl;external External_Dll;
function proj_get_pj_release:AnsiString;

function PROJ_COMPUTE_VERSION(maj:integer;min:integer;patch:integer):integer;
function PROJ_AT_LEAST_VERSION(maj:integer;min:integer;patch:integer):boolean;

implementation

var
  //extern char const PROJ_DLL pj_release[]; /* global release id string */
  pj_release:array [0..0] of AnsiChar;external External_Dll;

function proj_get_pj_release:AnsiString;
begin
  result:=PChar(@pj_release[0]);
end;

//* Note: the following 3 defines have been introduced in PROJ 8.0.1 */
//* Macro to compute a PROJ version number from its components */
function PROJ_COMPUTE_VERSION(maj:integer;min:integer;patch:integer):integer;
begin
  Result := maj * 10000 + min * 100 + patch;
end;

//* Macro that returns true if the current PROJ version is at least the version specified by (maj,min,patch) */
function PROJ_AT_LEAST_VERSION(maj:integer;min:integer;patch:integer):boolean;
begin
  Result := PROJ_VERSION_NUMBER >= PROJ_COMPUTE_VERSION(maj,min,patch);
end;

end.
