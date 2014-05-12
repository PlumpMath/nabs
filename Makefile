define nabs_script :=
(add-to-load-path ".")
(use-modules (nabs configure) (nabs predicates) (gnu make))
(add-search-path inst %load-path)
(declare inst "Installation directory" INST_DIR
         path-suffix /site (verifies (file-exists?)))
(configure-include)

*unspecified*
endef

$(guile $(nabs_script))

#all:
#	echo $(guile %load-path)

#.build-configuration: $(MAKEFILE_LIST)
#	echo Configuring the build...
#	$(guile (write-configuration \".build-configuration\"))
#
#include .build-configuration

all:
	@echo 'use "make install" to install nabs to ' $(INST_DIR)/nabs

$(INST_DIR)/nabs:
	mkdir -p $@

install: $(INST_DIR)/nabs
	cp -rav nabs/*.scm $<
