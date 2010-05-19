; Win32 API  -  registry declarations
(module win32-registry
  (use (win32-base))
  (extern
   ; ***** types *****
   (type REGSAM (opaque) "REGSAM")
   (type HKEY (opaque) "HKEY")
   (type PHKEY (pointer HKEY) "PHKEY")
   ; ***** key hive constants *****
   (macro HKEY_CLASSES_ROOT::HKEY "HKEY_CLASSES_ROOT")
   (macro HKEY_CURRENT_CONFIG::HKEY "HKEY_CURRENT_CONFIG")
   (macro HKEY_CURRENT_USER::HKEY "HKEY_CURRENT_USER")
   (macro HKEY_LOCAL_MACHINE::HKEY "HKEY_LOCAL_MACHINE")
   (macro HKEY_USERS::HKEY "HKEY_USERS")
   (macro KEY_QUERY_VALUE::REGSAM "KEY_QUERY_VALUE")
   ; ***** functions *****
   (macro RegCloseKey::LONG (hKey::HKEY) "RegCloseKey")
   (macro RegEnumKeyEx::LONG (hKey::HKEY dwIndex::DWORD lpName::LPSTR lpcName::LPDWORD lpReserved::LPDWORD lpClass::LPSTR lpcClass::LPDWORD lpftLastWriteTime::PFILETIME) "RegEnumKeyExA")
   (macro RegOpenKeyEx::LONG (hKey::HKEY lpSubKey::LPCSTR ulOptions::DWORD samDesired::REGSAM phkResult::PHKEY) "RegOpenKeyExA")
   (macro RegQueryValueEx::LONG (hKey::HKEY lpValueName::LPCSTR lpReserved::LPDWORD lpType::LPDWORD lpData::LPCSTR lpcbData::LPDWORD) "RegQueryValueExA")))
