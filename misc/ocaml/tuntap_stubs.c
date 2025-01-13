#include <sys/param.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <net/if.h>
#include <linux/if_tun.h>
#include <sys/ioctl.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

CAMLprim value tuntap_create_tap_interface(value v_name) {
    CAMLparam1(v_name);
    CAMLlocal2(res, v_dev);

    struct ifreq ifr;
    int fd, err;

    if ((fd = open("/dev/net/tun", O_RDWR)) < 0) {
        uerror("open", Nothing);
    }

    memset(&ifr, 0, sizeof(ifr));
    ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
    size_t sz = MIN(IF_NAMESIZE, caml_string_length(v_name));
    strncpy(ifr.ifr_name, String_val(v_name), sz);

    if ((err = ioctl(fd, TUNSETIFF, (void *)&ifr)) < 0) {
        close(fd);
        uerror("ioctl(TUNSETIFF)", Nothing);
    }
    res = caml_alloc_tuple(2);
    v_dev = caml_copy_string(ifr.ifr_name);
    Store_field(res, 0, Val_int(fd));
    Store_field(res, 1, v_dev);
    CAMLreturn(res);
}
