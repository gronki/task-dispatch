FROM intel/oneapi-hpckit AS builder

WORKDIR /fpm
ADD https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 fpm.F90
RUN ifort -O0 fpm.F90 -o fpm && install fpm /usr/local/bin/

WORKDIR /source
COPY . .
ENV FPM_FC=ifort
RUN fpm install --profile debug --prefix /app

FROM intel/oneapi-runtime

COPY --from=builder /app/ /app/

ENTRYPOINT ["/app/bin/console"]