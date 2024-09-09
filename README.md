# A simple firewall

This is a test unikernel to try out virtio multiple interface patch with GNS3.

After cloning this repository, you can run the following to build the unikernel:
```bash
opam pin solo5 https://github.com/palainp/solo5.git#update-virtio -y && \
mirage configure -t virtio && \
make depend && dune build
```

Then you just have to pass the different addresses at runtime (options `--public-ipv4=10.10.10.1/24 --private-ipv4=172.10.10.1/24` to start with two interfaces, one configured with `10.10.10.1`, the other with `172.10.10.1`, and it should filter out TCP traffic between `172.10.10.10` and `10.10.10.10`.
