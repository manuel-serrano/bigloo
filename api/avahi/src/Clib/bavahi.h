/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/api/avahi/src/Clib/bavahi.h       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Oct 14 12:42:13 2020                          */
/*    Last change :  Fri Dec  8 17:43:34 2023 (serrano)                */
/*    Copyright   :  2020-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    External declarations for the Bigloo avahi binding               */
/*=====================================================================*/

/* Forward references */
struct BgL_avahizd2simplezd2pollz00_bgl;
struct BgL_avahizd2threadedzd2pollz00_bgl;
struct BgL_avahizd2clientzd2_bgl;
struct BgL_avahizd2entryzd2groupz00_bgl;
struct BgL_avahizd2servicezd2browserz00_bgl;
struct BgL_avahizd2servicezd2typezd2browserzd2_bgl;
struct BgL_avahizd2domainzd2browserz00_bgl;
struct BgL_avahizd2servicezd2resolverz00_bgl;

extern void bgl_avahi_simple_poll_new(struct BgL_avahizd2simplezd2pollz00_bgl *);
extern void bgl_avahi_simple_poll_timeout(AvahiSimplePoll *, long, obj_t, struct BgL_avahizd2simplezd2pollz00_bgl *);
extern void bgl_avahi_simple_poll_close(struct BgL_avahizd2simplezd2pollz00_bgl *);

extern void bgl_avahi_threaded_poll_new(struct BgL_avahizd2threadedzd2pollz00_bgl *);
extern void bgl_avahi_threaded_poll_timeout(AvahiThreadedPoll *, long, obj_t, struct BgL_avahizd2threadedzd2pollz00_bgl *);
extern void bgl_avahi_threaded_poll_close(struct BgL_avahizd2threadedzd2pollz00_bgl *);

extern void bgl_avahi_client_new(struct BgL_avahizd2clientzd2_bgl *);
extern void bgl_avahi_client_close(struct BgL_avahizd2clientzd2_bgl *);

extern void bgl_avahi_entry_group_new(struct BgL_avahizd2entryzd2groupz00_bgl *);
extern void bgl_avahi_entry_group_close(struct BgL_avahizd2entryzd2groupz00_bgl *);

extern void bgl_avahi_service_browser_new(struct BgL_avahizd2servicezd2browserz00_bgl *);
extern void bgl_avahi_service_browser_close(struct BgL_avahizd2servicezd2browserz00_bgl *);

extern void bgl_avahi_service_type_browser_new(struct BgL_avahizd2servicezd2typezd2browserzd2_bgl *);
extern void bgl_avahi_service_type_browser_close(struct BgL_avahizd2servicezd2typezd2browserzd2_bgl *);

extern void bgl_avahi_domain_browser_new(struct BgL_avahizd2domainzd2browserz00_bgl *, AvahiDomainBrowserType);
extern void bgl_avahi_domain_browser_close(struct BgL_avahizd2domainzd2browserz00_bgl *);

extern void bgl_avahi_service_resolver_new(struct BgL_avahizd2servicezd2resolverz00_bgl *);
extern void bgl_avahi_service_resolver_close(struct BgL_avahizd2servicezd2resolverz00_bgl *);

#define BGL_AVAHI_ENTRY_GROUP_ADD_SERVICE(a, b, c, d, e, f, h, i, j)	\
   avahi_entry_group_add_service(a, b, c, d, e, f, h, i, j, 0L)
