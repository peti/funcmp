TOP=..
include $(TOP)/mk/boilerplate.mk

# ---------------------------------------------------------------

ALL_DIRS =      \
    FMP

PACKAGE		:= funcmp
RELEASEDAY	:= 2005-02-11
VERSION		:= 0.0-$(RELEASEDAY)
PACKAGE_DEPS	:= base haskell98

SRC_HADDOCK_OPTS += -t "Functional MetaPost ($(PACKAGE) package)"

# ---------------------------------------------------------------

-include $(TOP)/mk/crypto.mk
include $(TOP)/mk/target.mk
