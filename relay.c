#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <linux/if_tun.h>
#include <errno.h>
#include <sys/select.h>

#define BUF_SIZE 1600

// Function to create and configure a TAP interface
int create_tap_interface(const char *dev) {
    struct ifreq ifr;
    int fd, err;

    if ((fd = open("/dev/net/tun", O_RDWR)) < 0) {
        perror("Opening /dev/net/tun");
        return fd;
    }

    memset(&ifr, 0, sizeof(ifr));
    ifr.ifr_flags = IFF_TAP | IFF_NO_PI; // TAP device, no packet info
    if (*dev) {
        strncpy(ifr.ifr_name, dev, IFNAMSIZ);
    }

    if ((err = ioctl(fd, TUNSETIFF, (void *)&ifr)) < 0) {
        perror("ioctl(TUNSETIFF)");
        close(fd);
        return err;
    }

    printf("Created interface: %s\n", ifr.ifr_name);
    return fd;
}

// Function to relay packets between two TAP interfaces
void relay_packets(int fd1, int fd2) {
    char buffer[BUF_SIZE];
    int nread;
    fd_set fds;

    while (1) {
        FD_ZERO(&fds);
        FD_SET(fd1, &fds);
        FD_SET(fd2, &fds);

        int maxfd = (fd1 > fd2) ? fd1 : fd2;

        int ret = select(maxfd + 1, &fds, NULL, NULL, NULL);
        if (ret < 0) {
            perror("select()");
            break;
        }

        // Check if there's data to read from fd1
        if (FD_ISSET(fd1, &fds)) {
            nread = read(fd1, buffer, BUF_SIZE);
            if (nread < 0) {
                perror("Reading from interface 1");
                break;
            }
            write(fd2, buffer, nread);
        }

        // Check if there's data to read from fd2
        if (FD_ISSET(fd2, &fds)) {
            nread = read(fd2, buffer, BUF_SIZE);
            if (nread < 0) {
                perror("Reading from interface 2");
                break;
            }
            write(fd1, buffer, nread);
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <tap-interface-1> <tap-interface-2>\n", argv[0]);
        exit(1);
    }

    int tap1 = create_tap_interface(argv[1]);
    int tap2 = create_tap_interface(argv[2]);

    if (tap1 < 0 || tap2 < 0) {
        fprintf(stderr, "Error creating TAP interfaces\n");
        exit(1);
    }

    printf("Relaying packets between %s and %s...\n", argv[1], argv[2]);
    relay_packets(tap1, tap2);

    close(tap1);
    close(tap2);

    return 0;
}
