;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/avahi/src/Llib/avahi.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 20 14:46:34 2011                          */
;*    Last change :  Mon Feb  6 10:39:47 2017 (serrano)                */
;*    Copyright   :  2011-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    C avahi functions                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (extern
      (include "avahi-client/client.h")
      (include "avahi-client/lookup.h")
      (include "avahi-client/publish.h")
      (include "avahi-common/simple-watch.h")
      (include "avahi-common/thread-watch.h")
      (include "avahi-common/error.h")
      (include "avahi-common/alternative.h")
      (include "bavahi.h")

      ;; misc
      (macro $avahi-strerror::string
	 (::int)
	 "(char *)avahi_strerror")
      (infix macro $string-null::string
	 ()
	 "(char *)0L")
      
      ($avahi-invoke-callbacks::void () "bgl_avahi_invoke_callbacks")
      (macro $avahi-alternative-host-name::string
	 (::string)
	 "avahi_alternative_host_name")
      (macro $avahi-alternative-service-name::string
	 (::string)
	 "avahi_alternative_service_name")

      ;; errors
      (macro $avahi_ok::int "AVAHI_OK")
      (macro $avahi-err-failure::int "AVAHI_ERR_FAILURE")
      (macro $avahi-err-bad_state::int "AVAHI_ERR_BAD_STATE")
      (macro $avahi-err-invalid-host-name::int "AVAHI_ERR_INVALID_HOST_NAME")
      (macro $avahi-err-invalid-domain-name::int "AVAHI_ERR_INVALID_DOMAIN_NAME")
      (macro $avahi-err-no-network::int "AVAHI_ERR_NO_NETWORK")
      (macro $avahi-err-invalid-ttl::int "AVAHI_ERR_INVALID_TTL")
      (macro $avahi-err-is-pattern::int "AVAHI_ERR_IS_PATTERN")
      (macro $avahi-err-collision::int "AVAHI_ERR_COLLISION")
      (macro $avahi-err-invalid-record::int "AVAHI_ERR_INVALID_RECORD")
      (macro $avahi-err-invalid-service-name::int "AVAHI_ERR_INVALID_SERVICE_NAME")
      (macro $avahi-err-invalid-service-type::int "AVAHI_ERR_INVALID_SERVICE_TYPE")
      (macro $avahi-err-invalid-port::int "AVAHI_ERR_INVALID_PORT")
      (macro $avahi-err-invalid-key::int "AVAHI_ERR_INVALID_KEY")
      (macro $avahi-err-invalid-address::int "AVAHI_ERR_INVALID_ADDRESS")
      (macro $avahi-err-timeout::int "AVAHI_ERR_TIMEOUT")
      (macro $avahi-err-too_many-clients::int "AVAHI_ERR_TOO_MANY_CLIENTS")
      (macro $avahi-err-too_many-objects::int "AVAHI_ERR_TOO_MANY_OBJECTS")
      (macro $avahi-err-too_many-entries::int "AVAHI_ERR_TOO_MANY_ENTRIES")
      (macro $avahi-err-os::int "AVAHI_ERR_OS")
      (macro $avahi-err-access-denied::int "AVAHI_ERR_ACCESS_DENIED")
      (macro $avahi-err-invalid-operation::int "AVAHI_ERR_INVALID_OPERATION")
      (macro $avahi-err-dbus-error::int "AVAHI_ERR_DBUS_ERROR")
      (macro $avahi-err-disconnected::int "AVAHI_ERR_DISCONNECTED")
      (macro $avahi-err-no-memory::int "AVAHI_ERR_NO_MEMORY")
      (macro $avahi-err-invalid-object::int "AVAHI_ERR_INVALID_OBJECT")
      (macro $avahi-err-no-daemon::int "AVAHI_ERR_NO_DAEMON")
      (macro $avahi-err-invalid-interface::int "AVAHI_ERR_INVALID_INTERFACE")
      (macro $avahi-err-invalid-protocol::int "AVAHI_ERR_INVALID_PROTOCOL")
      (macro $avahi-err-invalid-flags::int "AVAHI_ERR_INVALID_FLAGS")
      (macro $avahi-err-not-found::int "AVAHI_ERR_NOT_FOUND")
      (macro $avahi-err-invalid-config::int "AVAHI_ERR_INVALID_CONFIG")
      (macro $avahi-err-version-mismatch::int "AVAHI_ERR_VERSION_MISMATCH")
      (macro $avahi-err-invalid-service-subtype::int "AVAHI_ERR_INVALID_SERVICE_SUBTYPE")
      (macro $avahi-err-invalid-packet::int "AVAHI_ERR_INVALID_PACKET")
      (macro $avahi-err-invalid-dns-error::int "AVAHI_ERR_INVALID_DNS_ERROR")
      (macro $avahi-err-dns-formerr::int "AVAHI_ERR_DNS_FORMERR")
      (macro $avahi-err-dns-servfail::int "AVAHI_ERR_DNS_SERVFAIL")
      (macro $avahi-err-dns-nxdomain::int "AVAHI_ERR_DNS_NXDOMAIN")
      (macro $avahi-err-dns-notimp::int "AVAHI_ERR_DNS_NOTIMP")
      (macro $avahi-err-dns-refused::int "AVAHI_ERR_DNS_REFUSED")
      (macro $avahi-err-dns-yxdomain::int "AVAHI_ERR_DNS_YXDOMAIN")
      (macro $avahi-err-dns-yxrrset::int "AVAHI_ERR_DNS_YXRRSET")
      (macro $avahi-err-dns-nxrrset::int "AVAHI_ERR_DNS_NXRRSET")
      (macro $avahi-err-dns-notauth::int "AVAHI_ERR_DNS_NOTAUTH")
      (macro $avahi-err-dns-notzone::int "AVAHI_ERR_DNS_NOTZONE")
      (macro $avahi-err-invalid-rdata::int "AVAHI_ERR_INVALID_RDATA")
      (macro $avahi-err-invalid-dns-class::int "AVAHI_ERR_INVALID_DNS_CLASS")
      (macro $avahi-err-invalid-dns-type::int "AVAHI_ERR_INVALID_DNS_TYPE")
      (macro $avahi-err-not-supported::int "AVAHI_ERR_NOT_SUPPORTED")
      (macro $avahi-err-not-permitted::int "AVAHI_ERR_NOT_PERMITTED")
      (macro $avahi-err-invalid-argument::int "AVAHI_ERR_INVALID_ARGUMENT")
      (macro $avahi-err-is-empty::int "AVAHI_ERR_IS_EMPTY")
      (macro $avahi-err-no-change::int "AVAHI_ERR_NO_CHANGE")
      (macro $avahi-err-max::int "AVAHI_ERR_MAX")
      
      ;; avahi-simple-poll
      (type $avahi-simple-poll void* "AvahiSimplePoll *")
      (infix macro $avahi-simple-poll-nil::$avahi-simple-poll () "0L")
      
      (macro $bgl-avahi-simple-poll-new::void
	 (::avahi-simple-poll)
	 "bgl_avahi_simple_poll_new")
      (macro $bgl-avahi-simple-poll-close::void
	 (::avahi-simple-poll)
	 "bgl_avahi_simple_poll_close")
      (macro $avahi-simple-poll-loop::void
	 (::$avahi-simple-poll)
	 "avahi_simple_poll_loop")
      (macro $avahi-simple-poll-quit::void
	 (::$avahi-simple-poll)
	 "avahi_simple_poll_quit")
      (macro $bgl-avahi-simple-poll-timeout::void
	 (::$avahi-simple-poll ::elong ::procedure)
	 "bgl_avahi_simple_poll_timeout")

      ;; avahi-threaded-poll
      (type $avahi-threaded-poll void* "AvahiThreadedPoll *")
      (infix macro $avahi-threaded-poll-nil::$avahi-threaded-poll () "0L")
      
      (macro $bgl-avahi-threaded-poll-new::void
	 (::avahi-threaded-poll)
	 "bgl_avahi_threaded_poll_new")
      (macro $bgl-avahi-threaded-poll-close::void
	 (::avahi-threaded-poll)
	 "bgl_avahi_threaded_poll_close")
      (macro $avahi-threaded-poll-loop::void
	 (::$avahi-threaded-poll)
	 "avahi_threaded_poll_start")
      (macro $avahi-threaded-poll-quit::void
	 (::$avahi-threaded-poll)
	 "avahi_threaded_poll_stop")
      (macro $bgl-avahi-threaded-poll-timeout::void
	 (::$avahi-threaded-poll ::elong ::procedure)
	 "bgl_avahi_threaded_poll_timeout")
      (macro $avahi-threaded-poll-lock::void
	 (::$avahi-threaded-poll)
	 "avahi_threaded_poll_lock")
      (macro $avahi-threaded-poll-unlock::void
	 (::$avahi-threaded-poll)
	 "avahi_threaded_poll_unlock")

      ;; avahi-client
      (type $avahi-client void* "AvahiClient *")
      (infix macro $avahi-client-nil::$avahi-client () "0L")
      (infix macro $avahi-client-nil?::bool (::$avahi-client) " == 0L")

      (macro $bgl-avahi-client-new::void
	 (::avahi-client)
	 "bgl_avahi_client_new")
      (macro $bgl-avahi-client-close::void
	 (::avahi-client)
	 "bgl_avahi_client_close")
      (macro $avahi-client-errno::int
	 (::$avahi-client)
	 "avahi_client_errno")
      (macro $avahi-client-get-state::$avahi-client-state
	 (::$avahi-client)
	 "avahi_client_get_state")
      (macro $avahi-client-get-version-string::string
	 (::$avahi-client)
	 "avahi_client_get_version_string")
      (macro $avahi-client-get-host-name::string
	 (::$avahi-client)
	 "avahi_client_get_host_name")
      (macro $avahi-client-set-host-name::string
	 (::$avahi-client ::string)
	 "avahi_client_set_host_name")
      (macro $avahi-client-get-domain-name::string
	 (::$avahi-client)
	 "avahi_client_get_domain_name")
      (macro $avahi-client-get-host-name-fqdn::string
	 (::$avahi-client)
	 "avahi_client_get_host_name_fqdn")

      (type $avahi-client-flag long "AvahiClientFlags")
      (macro $avahi-client-flag-ignore-user-config::$avahi-client-flag
	 "AVAHI_CLIENT_IGNORE_USER_CONFIG")
      (macro $avahi-client-flag-no-fail::$avahi-client-flag
	 "AVAHI_CLIENT_NO_FAIL")

      (type $avahi-client-state long "AvahiClientState")
      (macro $avahi-client-state-registering::$avahi-client-state
	 "AVAHI_CLIENT_S_REGISTERING")
      (macro $avahi-client-state-runnning::$avahi-client-state
	 "AVAHI_CLIENT_S_RUNNING")
      (macro $avahi-client-state-collision::$avahi-client-state
	 "AVAHI_CLIENT_S_COLLISION")
      (macro $avahi-client-state-failure::$avahi-client-state
	 "AVAHI_CLIENT_FAILURE")
      (macro $avahi-client-state-connecting::$avahi-client-state
	 "AVAHI_CLIENT_CONNECTING")

      ;; avahi-entry-group
      (type $avahi-entry-group void* "AvahiEntryGroup *")
      (infix macro $avahi-entry-group-nil::$avahi-entry-group () "0L")

      (macro $bgl-avahi-entry-group-new::void
	 (::avahi-entry-group)
	 "bgl_avahi_entry_group_new")
      (macro $bgl-avahi-entry-group-close::void
	 (::avahi-entry-group)
	 "bgl_avahi_entry_group_close")
      (macro $avahi-entry-group-empty?::void
	 (::$avahi-entry-group)
	 "avahi_entry_group_is_empty")
      (macro $avahi-entry-group-commit::int
	 (::$avahi-entry-group)
	 "avahi_entry_group_commit")
      (macro $avahi-entry-group-reset::int
	 (::$avahi-entry-group)
	 "avahi_entry_group_reset")
      (macro $avahi-entry-group-add-service-subtype::int
	 (::$avahi-entry-group ::$avahi-if-index ::$avahi-protocol
	    ::$avahi-publish-flags ::string ::string ::string ::string)
	 "avahi_entry_group_add_service_subtype")
      (macro $avahi-entry-group-add-service-strlst::int
	 (::$avahi-entry-group ::$avahi-if-index ::$avahi-protocol
	    ::$avahi-publish-flags ::string ::string ::string ::string ::int
	    ::$avahi-string-list)
	 "avahi_entry_group_add_service_strlst")
      (macro $avahi-entry-group-add-service::int
	 (::$avahi-entry-group ::$avahi-if-index ::$avahi-protocol
	    ::$avahi-publish-flags ::string ::string ::string ::string ::int
	    ::string)
	 "avahi_entry_group_add_service")

      (type $avahi-entry-group-state long "AvahiEntryGroupState")
      (macro $avahi-entry-group-state-uncommited::$avahi-entry-group-state
	 "AVAHI_ENTRY_GROUP_UNCOMMITED")
      (macro $avahi-entry-group-state-registering::$avahi-entry-group-state
	 "AVAHI_ENTRY_GROUP_REGISTERING")
      (macro $avahi-entry-group-state-established::$avahi-entry-group-state
	 "AVAHI_ENTRY_GROUP_ESTABLISHED")
      (macro $avahi-entry-group-state-collision::$avahi-entry-group-state
	 "AVAHI_ENTRY_GROUP_COLLISION")
      (macro $avahi-entry-group-state-failure::$avahi-entry-group-state
	 "AVAHI_ENTRY_GROUP_FAILURE")

      (type $avahi-if-index long "AvahiIfIndex")
      (macro $avahi-if-unspec::$avahi-if-index
	 "AVAHI_IF_UNSPEC")

      (type $avahi-protocol long "AvahiProtocol")
      (macro $avahi-proto-inet::$avahi-protocol
	 "AVAHI_PROTO_INET")
      (macro $avahi-proto-inet6::$avahi-protocol
	 "AVAHI_PROTO_INET6")
      (macro $avahi-proto-unspec::$avahi-protocol
	 "AVAHI_PROTO_UNSPEC")

      (type $avahi-publish-flags long "AvahiPublishFlags")
      (macro $avahi-publish-unique::$avahi-publish-flags
	 "AVAHI_PUBLISH_UNIQUE")
      (macro $avahi-publish-probe::$avahi-publish-flags
	 "AVAHI_PUBLISH_PROBE")
      (macro $avahi-publish-no-announce::$avahi-publish-flags
	 "AVAHI_PUBLISH_NO_ANNOUNCE")
      (macro $avahi-publish-allow-multiple::$avahi-publish-flags
	 "AVAHI_PUBLISH_ALLOW_MULTIPLE")
      (macro $avahi-publish-update::$avahi-publish-flags
	 "AVAHI_PUBLISH_UPDATE")
      (macro $avahi-publish-none::$avahi-publish-flags
	 "0L")

      ;; avahi-service-browser
      (type $avahi-service-browser void* "AvahiServiceBrowser *")
      (infix macro $avahi-service-browser-nil::$avahi-service-browser () "0L")

      (macro $bgl-avahi-service-browser-new::void
	 (::avahi-service-browser)
	 "bgl_avahi_service_browser_new")
      (macro $bgl-avahi-service-browser-close::void
	 (::avahi-service-browser)
	 "bgl_avahi_service_browser_close")

      ;; avahi-service-type-browser
      (type $avahi-service-type-browser void* "AvahiServiceTypeBrowser *")
      (infix macro $avahi-service-type-browser-nil::$avahi-service-type-browser () "0L")

      (macro $bgl-avahi-service-type-browser-new::void
	 (::avahi-service-type-browser)
	 "bgl_avahi_service_type_browser_new")
      (macro $bgl-avahi-service-type-browser-close::void
	 (::avahi-service-type-browser)
	 "bgl_avahi_service-type_browser_close")

      ;; avahi-domain-browser
      (type $avahi-domain-browser void* "AvahiDomainBrowser *")
      (infix macro $avahi-domain-browser-nil::$avahi-domain-browser () "0L")

      (macro $bgl-avahi-domain-browser-new::void
	 (::avahi-domain-browser ::$avahi-domain-browser-type)
	 "bgl_avahi_domain_browser_new")
      (macro $bgl-avahi-domain-browser-close::void
	 (::avahi-domain-browser)
	 "bgl_avahi_domain_browser_close")


      ;; avahi-service-resolver
      (type $avahi-service-resolver void* "AvahiServiceResolver *")
      (infix macro $avahi-service-resolver-nil::$avahi-service-resolver () "0L")

      (macro $bgl-avahi-service-resolver-new::void
	 (::avahi-service-resolver)
	 "bgl_avahi_service_resolver_new")
      (macro $bgl-avahi-service-resolver-close::void
	 (::avahi-service-resolver)
	 "bgl_avahi_service_resolver_close")

      (type $avahi-browser-event long "AvahiBrowserEvent")
      (macro $avahi-browser-new::$avahi-browser-event
	 "AVAHI_BROWSER_NEW")
      (macro $avahi-browser-remove::$avahi-browser-event
	 "AVAHI_BROWSER_REMOVE")
      (macro $avahi-browser-cache-exhausted::$avahi-browser-event
	 "AVAHI_BROWSER_CACHE_EXHAUSTED")
      (macro $avahi-browser-all-for-now::$avahi-browser-event
	 "AVAHI_BROWSER_ALL_FOR_NOW")
      (macro $avahi-browser-failure::$avahi-browser-event
	 "AVAHI_BROWSER_FAILURE")

      (type $avahi-resolver-event long "AvahiResolverEvent")
      (macro $avahi-resolver-found::$avahi-resolver-event
	 "AVAHI_RESOLVER_FOUND")
      (macro $avahi-resolver-failure::$avahi-resolver-event
	 "AVAHI_RESOLVER_FAILURE")

      (type $avahi-lookup-flags long "AvahiLookupFlags")
      (macro $avahi-lookup-no-txt::$avahi-lookup-flags
	 "AVAHI_LOOKUP_NO_TXT")
      (macro $avahi-lookup-no-address::$avahi-lookup-flags
	 "AVAHI_LOOKUP_NO_ADDRESS")
      
      (type $avahi-lookup-result-flags long "AvahiLookupResultFlags")
      (macro $avahi-lookup-result-cached::$avahi-lookup-result-flags
	 "AVAHI_LOOKUP_RESULT_CACHED")
      (macro $avahi-lookup-result-wide-area::$avahi-lookup-result-flags
	 "AVAHI_LOOKUP_RESULT_WIDE_AREA")
      (macro $avahi-lookup-result-multicast::$avahi-lookup-result-flags
	 "AVAHI_LOOKUP_RESULT_MULTICAST")
      (macro $avahi-lookup-result-local::$avahi-lookup-result-flags
	 "AVAHI_LOOKUP_RESULT_LOCAL")
      (macro $avahi-lookup-result-our-own::$avahi-lookup-result-flags
	 "AVAHI_LOOKUP_RESULT_OUR_OWN")
      (macro $avahi-lookup-result-static::$avahi-lookup-result-flags
	 "AVAHI_LOOKUP_RESULT_STATIC")

      (type $avahi-domain-browser-type long "AvahiDomainBrowserType")
      (macro $avahi-domain-browser-browse::$avahi-domain-browser-type
	 "AVAHI_DOMAIN_BROWSER_BROWSE")
      (macro $avahi-domain-browser-browse-default::$avahi-domain-browser-type
	 "AVAHI_DOMAIN_BROWSER_BROWSE_DEFAULT")
      (macro $avahi-domain-browser-register::$avahi-domain-browser-type
	 "AVAHI_DOMAIN_BROWSER_REGISTER")
      (macro $avahi-domain-browser-register-default::$avahi-domain-browser-type
	 "AVAHI_DOMAIN_BROWSER_REGISTER_DEFAULT")
      (macro $avahi-domain-browser-browse-legacy::$avahi-domain-browser-type
	 "AVAHI_DOMAIN_BROWSER_BROWSE_LEGACY")
      (macro $avahi-domain-browser-max::$avahi-domain-browser-type
	 "AVAHI_DOMAIN_BROWSER_MAX")
      
      ;; AvahiStringList
      (type $avahi-string-list void* "AvahiStringList *")
      ($bgl-avahi-list->string-list::$avahi-string-list
	 (::pair-nil)
	 "bgl_avahi_list_to_string_list")
      (macro $avahi-string-list-free::void
	 (::$avahi-string-list)
	 "avahi_string_list_free")
      ))
    

