GUILE_LOAD_PATH:=$(guile %load-path)

define nabs_script :=
(add-to-load-path ".")
(use-modules (nabs configure) (nabs predicates) (gnu make) (nabs targets))
(add-search-path inst $(GUILE_LOAD_PATH))
(declare inst "Installation directory" INST_DIR
         path-suffix /site (verifies (file-exists?)))
(configure-include)
endef

$(guile $(nabs_script))

all:
	@echo 'use "make install" to install nabs to ' $(INST_DIR)/nabs

$(INST_DIR)/nabs:
	mkdir -p $@

install: $(INST_DIR)/nabs
	cp -rav nabs/*.scm $<
