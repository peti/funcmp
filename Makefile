TOP=..
include $(TOP)/mk/boilerplate.mk

# ---------------------------------------------------------------

ALL_DIRS =      \
    FMP

PACKAGE         := funcmp
RELEASEDAY      := 2005-02-14
VERSION         := 0.0-$(RELEASEDAY)
PACKAGE_DEPS    := base haskell98
SRC_HC_OPTS     += -fno-warn-name-shadowing -fno-warn-type-defaults

SRC_HADDOCK_OPTS += -t "Functional MetaPost ($(PACKAGE) package)"

# ---------------------------------------------------------------

-include $(TOP)/mk/crypto.mk
include $(TOP)/mk/target.mk
