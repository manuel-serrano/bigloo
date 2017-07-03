(directives
 (extern
  ;; beginning of connection.h
  (macro setPathConnection::obj ($busp string) "setPathConnection")
  (macro setInterfaceConnection::obj ($busp string) "setInterfaceConnection")
  (macro connectBus::$busp ($dbustype) "connectBus")
  (macro connectRemoteBus::$busp (string) "connectRemoteBus")
  (macro closeBus::void ($busp) "closeBus")
  (macro isConnected::obj ($busp) "isConnected")
  (macro serviceNameHasOwner::obj ($busp string) "serviceNameHasOwner")
  (macro handleService::$servicep ($busp string) "handleService")
  (macro handleObject::$objectp ($servicep string string) "handleObject")
  (macro unsubscribe::obj (string $busp) "unsubscribe")
  (macro subscribe::obj (string $busp) "subscribe")
  (macro releaseServiceName::obj ($busp) "releaseServiceName")
  (macro setServiceName::obj ($busp string uint) "setServiceName")))

