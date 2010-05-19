; Win32 API  -  basic declarations
(module win32-base
  (extern
   (include "windows.h")
   ; ***** types *****
   (type BYTE uchar "BYTE")
   (type LPBYTE (pointer BYTE) "LPBYTE")
   (type LONG long "LONG")
   (type DWORD ulong "DWORD")
   (type LPDWORD (pointer DWORD) "LPDWORD")
   (type LPSTR string "LPSTR")
   (type LPCSTR string "LPCSTR")
   (type LPVOID (pointer void) "LPVOID")
   (type LPVOIDARRAY (array LPVOID) "LPVOID*")
   (type FILETIME
         (struct (dwLowDateTime::DWORD "dwLowDateTime")
                 (dwHighDateTime::DWORD "dwHighDateTime"))
         "FILETIME")
   (type PFILETIME FILETIME* "PFILETIME")
   ; ***** functions *****
   (macro FormatMessage::DWORD (dwFlags::DWORD lpSource::LPVOID dwMessageId::DWORD dwLanguageId::DWORD lpBuffer::LPSTR nSize::DWORD Arguments::LPVOIDARRAY) "FormatMessageA")
   (macro GetLastError::DWORD () "GetLastError")
   (macro GetTempPath::DWORD (nBufferLength::DWORD lpBuffer::LPSTR) "GetTempPathA")
   ; ***** FormatMessage flags *****
   (macro FORMAT_MESSAGE_ALLOCATE_BUFFER::DWORD "FORMAT_MESSAGE_ALLOCATE_BUFFER")
   (macro FORMAT_MESSAGE_IGNORE_INSERTS::DWORD "FORMAT_MESSAGE_IGNORE_INSERTS")
   (macro FORMAT_MESSAGE_FROM_STRING::DWORD "FORMAT_MESSAGE_FROM_STRING")
   (macro FORMAT_MESSAGE_FROM_HMODULE::DWORD "FORMAT_MESSAGE_FROM_HMODULE")
   (macro FORMAT_MESSAGE_FROM_SYSTEM::DWORD "FORMAT_MESSAGE_FROM_SYSTEM")
   (macro FORMAT_MESSAGE_ARGUMENT_ARRAY::DWORD "FORMAT_MESSAGE_ARGUMENT_ARRAY")
   ; ***** error codes *****
   (macro ERROR_MORE_DATA::LONG "ERROR_MORE_DATA")
   (macro ERROR_NO_MORE_ITEMS::LONG "ERROR_NO_MORE_ITEMS")
   (macro ERROR_SUCCESS::LONG "ERROR_SUCCESS")))
