(directives
 (extern
  ;; beginning of service.h
  (macro getMethodObject::obj ($objectp int) "getMethodObject")
  (macro sendMethodCallReturn::int ($busp $messagep obj) "sendMethodCallReturn")
  (macro sendError::int ($busp $messagep string string obj) "sendError")
  (macro sendMethodCall::$messagep ($objectp string int int obj) "sendMethodCall")
  (macro sendIntrospectReturn::int ($busp string $messagep) "sendIntrospectReturn")
  (macro getMember::obj ($messagep) "getMember")
  (macro getSender::obj ($messagep) "getSender")))

