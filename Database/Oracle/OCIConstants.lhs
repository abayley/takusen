
|
Module      :  Database.Oracle.OCIConstants
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable
 
Contains CInt equivalents of the #defines in the oci library headers.
This is not a complete set; just enough to get the Haskell libraries working.
This also might not be particularly portable, but I don't think Oracle are going
to change these in a hurry (that would break compiled programs).


> module Database.Oracle.OCIConstants where

> import Foreign.C.Types


** Used all over the place:

> oci_DEFAULT :: CInt
> oci_DEFAULT = 0


** Handle types:

> oci_HTYPE_ENV :: CInt
> oci_HTYPE_ENV = 1
> oci_HTYPE_ERROR :: CInt
> oci_HTYPE_ERROR = 2
> oci_HTYPE_SVCCTX :: CInt
> oci_HTYPE_SVCCTX = 3
> oci_HTYPE_STMT :: CInt
> oci_HTYPE_STMT = 4
> oci_HTYPE_BIND :: CInt
> oci_HTYPE_BIND = 5
> oci_HTYPE_DEFINE :: CInt
> oci_HTYPE_DEFINE = 6
> oci_HTYPE_DESCRIBE :: CInt
> oci_HTYPE_DESCRIBE = 7
> oci_HTYPE_SERVER :: CInt
> oci_HTYPE_SERVER = 8
> oci_HTYPE_SESSION :: CInt
> oci_HTYPE_SESSION = 9
> oci_HTYPE_TRANS :: CInt
> oci_HTYPE_TRANS = 10


** Error code types:

> oci_SUCCESS :: CInt
> oci_SUCCESS = 0
> oci_SUCCESS_WITH_INFO :: CInt
> oci_SUCCESS_WITH_INFO = 1
> oci_RESERVED_FOR_INT_USE :: CInt
> oci_RESERVED_FOR_INT_USE = 200
> oci_NO_DATA :: CInt
> oci_NO_DATA = 100
> oci_ERROR :: CInt
> oci_ERROR = -1
> oci_INVALID_HANDLE :: CInt
> oci_INVALID_HANDLE = -2
> oci_NEED_DATA :: CInt
> oci_NEED_DATA = 99
> oci_STILL_EXECUTING :: CInt
> oci_STILL_EXECUTING = -3123
> oci_CONTINUE :: CInt
> oci_CONTINUE = -24200



** Attribute types:

> oci_ATTR_ENV :: CInt
> oci_ATTR_ENV = 5
> oci_ATTR_SERVER :: CInt
> oci_ATTR_SERVER = 6
> oci_ATTR_SESSION :: CInt
> oci_ATTR_SESSION = 7
> oci_ATTR_TRANS :: CInt
> oci_ATTR_TRANS = 8
> oci_ATTR_ROW_COUNT :: CInt
> oci_ATTR_ROW_COUNT = 9
> oci_ATTR_PREFETCH_ROWS :: CInt
> oci_ATTR_PREFETCH_ROWS = 11



** Syntax types (i.e. does the DBMS understand v7 or v8 syntax, etc):

> oci_NTV_SYNTAX :: CInt
> oci_NTV_SYNTAX = 1



** Scrollable Cursor Options:

> oci_FETCH_NEXT :: CInt
> oci_FETCH_NEXT = 2
> oci_FETCH_FIRST :: CInt
> oci_FETCH_FIRST = 4
> oci_FETCH_LAST :: CInt
> oci_FETCH_LAST = 8
> oci_FETCH_PRIOR :: CInt
> oci_FETCH_PRIOR = 16
> oci_FETCH_ABSOLUTE :: CInt
> oci_FETCH_ABSOLUTE = 32
> oci_FETCH_RELATIVE :: CInt
> oci_FETCH_RELATIVE = 64
> oci_FETCH_RESERVED :: CInt
> oci_FETCH_RESERVED = 128




** OCI datatypes:

> oci_SQLT_CHR :: CInt
> oci_SQLT_CHR = 1
> oci_SQLT_NUM :: CInt
> oci_SQLT_NUM = 2
> oci_SQLT_INT :: CInt
> oci_SQLT_INT = 3
> oci_SQLT_FLT :: CInt
> oci_SQLT_FLT = 4
> oci_SQLT_STR :: CInt
> oci_SQLT_STR = 5
> oci_SQLT_VNU :: CInt
> oci_SQLT_VNU = 6
> oci_SQLT_LNG :: CInt
> oci_SQLT_LNG = 8
> oci_SQLT_VCS :: CInt
> oci_SQLT_VCS = 9
> oci_SQLT_RID :: CInt
> oci_SQLT_RID = 11
> oci_SQLT_DAT :: CInt
> oci_SQLT_DAT = 12
> oci_SQLT_VBI :: CInt
> oci_SQLT_VBI = 15
> oci_SQLT_BIN :: CInt
> oci_SQLT_BIN = 23
> oci_SQLT_LBI :: CInt
> oci_SQLT_LBI = 24
> oci_SQLT_UIN :: CInt
> oci_SQLT_UIN = 68
> oci_SQLT_LVC :: CInt
> oci_SQLT_LVC = 94
> oci_SQLT_LVB :: CInt
> oci_SQLT_LVB = 95
> oci_SQLT_AFC :: CInt
> oci_SQLT_AFC = 96
> oci_SQLT_AVC :: CInt
> oci_SQLT_AVC = 96



**Transaction types; parameters for ociTransStart.

There are more than this, but they're related to complicated transaction-management stuff
in the OCI libraries that I don't understand.
These should be sufficient to support the simple transaction model
understood by most developers.

> oci_TRANS_READONLY :: CInt
> oci_TRANS_READONLY = 0x00000100  -- 256
> oci_TRANS_READWRITE :: CInt
> oci_TRANS_READWRITE = 0x00000200  -- 512
> oci_TRANS_SERIALIZABLE :: CInt
> oci_TRANS_SERIALIZABLE = 0x00000400  -- 1024
