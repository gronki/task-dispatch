set -e

if [ $# -lt 1 ]; then
    echo "usage: $0 gcc_version [ubuntu_version]"
    echo "for example: $0 12"
    echo "if ubuntu version not given, the latest LTS tag will be pulled"
    exit 1
fi

GCC_VERSION="$1"

if [ -z "$2" ]; then
    UBUNTU_VERSION=$(( ( $(date --date=-5\ months +%y) / 2 ) * 2 )).04
else
    UBUNTU_VERSION="$2"
fi

IMAGE_NAME="gfortran:${GCC_VERSION}-${UBUNTU_VERSION}"

DOCKER=podman

${DOCKER} build \
    --build-arg "UBUNTU_TAG=${UBUNTU_VERSION}" \
    --build-arg "GCC_VERSION=${GCC_VERSION}" \
    -t "${IMAGE_NAME}" -f Dockerfile.test .

# ${DOCKER} run -it -v $(pwd):/source -v $(mktemp -d):/source/build \
#     --entrypoint /usr/local/bin/fpm $IMAGE_NAME \
#     test --profile debug

${DOCKER} run -it -v $(pwd):/source -v $(mktemp -d):/source/build \
    --entrypoint /bin/bash $IMAGE_NAME 