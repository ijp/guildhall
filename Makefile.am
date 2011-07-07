moddir=$(prefix)/share/guile/site

SOURCES =					\
	dorodango/actions.sls \
	dorodango/bundle.sls \
	dorodango/config.sls \
	dorodango/database.sls \
	dorodango/database/dependencies.sls \
	dorodango/destination.sls \
	dorodango/hooks.sls \
	dorodango/inventory.sls \
	dorodango/inventory/mapping.sls \
	dorodango/package.sls \
	dorodango/private/utils.sls \
	dorodango/private/zip.sls \
	dorodango/repository.sls \
	dorodango/solver.sls \
	dorodango/solver/choice.sls \
	dorodango/solver/dummy-db.sls \
	dorodango/solver/expression.sls \
	dorodango/solver/internals.sls \
	dorodango/solver/logging.sls \
	dorodango/solver/promotions.sls \
	dorodango/solver/search-graph.sls \
	dorodango/solver/universe.sls \
	dorodango/ui.sls \
	dorodango/ui/cmdline.sls \
	dorodango/ui/cmdline/base.sls \
	dorodango/ui/cmdline/dependencies.sls \
	dorodango/ui/cmdline/help.sls \
	dorodango/ui/formatters.sls

GOBJECTS = $(SOURCES:%.sls=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

CLEANFILES = $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .sls .go
.sls.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"
