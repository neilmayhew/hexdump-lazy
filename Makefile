host:
	stack test

docker: docker-image
	stack --docker test

nix:
	stack --nix test

DOCKER_IMAGE_TAG := $(shell sed -n 's/^ *image: *//p' stack.yaml)

docker-image:
	docker build -t $(DOCKER_IMAGE_TAG) stack-base
