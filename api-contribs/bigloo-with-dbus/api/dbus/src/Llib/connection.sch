(directives
 (extern
  ;; beginning of connection.h
  (macro setPathConnection::int ($busp string) "setPathConnection")
  (macro setInterfaceConnection::int ($busp string) "setInterfaceConnection")
  (macro connectBus::$busp ($dbustype) "connectBus")
  (macro connectRemoteBus::$busp (string) "connectRemoteBus")
  (macro closeBus::void ($busp) "closeBus")
  (macro isConnected::int ($busp) "isConnected")
  (macro serviceNameHasOwner::int ($busp string) "serviceNameHasOwner")
  (macro handleService::$servicep ($busp string) "handleService")
  (macro handleObject::$objectp ($servicep string string) "handleObject")
  (macro unsubscribe::int (string $busp) "unsubscribe")
  (macro subscribe::int (string $busp) "subscribe")
  (macro releaseServiceName::int ($busp) "releaseServiceName")
  (macro setServiceName::int ($busp string uint) "setServiceName")))

