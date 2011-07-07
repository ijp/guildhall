moddir=$(prefix)/share/guile/site

SOURCES =					\
	dorodango/actions.scm \
	dorodango/bundle.scm \
	dorodango/config.scm \
	dorodango/database.scm \
	dorodango/database/dependencies.scm \
	dorodango/destination.scm \
	dorodango/hooks.scm \
	dorodango/inventory.scm \
	dorodango/inventory/mapping.scm \
	dorodango/package.scm \
	dorodango/private/utils.scm \
	dorodango/private/zip.scm \
	dorodango/repository.scm \
	dorodango/solver.scm \
	dorodango/solver/choice.scm \
	dorodango/solver/dummy-db.scm \
	dorodango/solver/expression.scm \
	dorodango/solver/internals.scm \
	dorodango/solver/logging.scm \
	dorodango/solver/promotions.scm \
	dorodango/solver/search-graph.scm \
	dorodango/solver/universe.scm \
	dorodango/ui.scm \
	dorodango/ui/cmdline.scm \
	dorodango/ui/cmdline/base.scm \
	dorodango/ui/cmdline/dependencies.scm \
	dorodango/ui/cmdline/help.scm \
	dorodango/ui/formatters.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

CLEANFILES = $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"
