(directives
 (extern
  ;; beginning of message.h
  (macro getArgsMessage::obj ($messagep) "getArgsMessage")
  (macro receiveMessageFromObject::$messagep ($objectp int) "receiveMessageFromObject")
  (macro receiveMessageFromBus::$messagep ($busp int) "receiveMessageFromBus")
  (macro receiveMessageWithRule::$messagep ($busp int string) "receiveMessageWithRule")
  (macro sendSignal::int ($busp string obj) "sendSignal")
  (macro getPath::obj ($messagep) "getMessagePath")
  (macro getMessageType::obj ($messagep) "getMessageType")))

