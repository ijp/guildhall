moddir=$(prefix)/share/guile/site

SOURCES =					\
	ocelotl/wt-tree.scm \
	spells/algebraic-types.scm \
	spells/algebraic-types/helpers.scm \
	spells/alist.scm \
	spells/args-fold.scm \
	spells/cells.scm \
	spells/condition.scm \
	spells/define-values.scm \
	spells/delimited-readers.scm \
	spells/error.scm \
	spells/filesys.scm \
	spells/filesys/compat.scm \
	spells/finite-types.scm \
	spells/gc.scm \
	spells/gc/compat.scm \
	spells/hash-utils.scm \
	spells/include.scm \
	spells/include/compat.scm \
	spells/include/helpers.scm \
	spells/logging.scm \
	spells/match.scm \
	spells/misc.scm \
	spells/misc/compat.scm \
	spells/operations.scm \
	spells/opt-args.scm \
	spells/pathname.scm \
	spells/pathname/os-string.scm \
	spells/ports.scm \
	spells/procedure-annotations.scm \
	spells/process.scm \
	spells/process/compat.scm \
	spells/record-types.scm \
	spells/record-types/expand-drt.scm \
	spells/string-utils.scm \
	spells/syntax-utils.scm \
	spells/sysutils.scm \
	spells/sysutils/compat.scm \
	spells/test-runner.scm \
	spells/time-lib.scm \
	spells/tracing.scm \
	spells/tracing/compat.scm \
	spells/xvector.scm \
	spells/zipper-tree.scm \
	wak/fmt.scm \
	wak/fmt/c.scm \
	wak/fmt/color.scm \
	wak/fmt/js.scm \
	wak/foof-loop.scm \
	wak/foof-loop/nested.scm \
	wak/irregex.scm \
	wak/parscheme/matcher-combinators.scm \
	wak/parscheme/parse-errors.scm \
	wak/parscheme/parser-combinators.scm \
	wak/parscheme/text-matcher-combinators.scm \
	wak/parscheme/text-parser-combinators.scm \
	wak/private/define-values.scm \
	wak/private/include.scm \
	wak/private/include/compat.scm \
	wak/private/include/filesys.scm \
	wak/private/include/helper.scm \
	wak/private/include/utils.scm \
	wak/private/let-optionals.scm \
	wak/riastreams.scm \
	wak/syn-param.scm \
	wak/trc-testing.scm \
	wak/trc-testing/display-condition.scm \
	wak/trc-testing/limited-write.scm \
	wak/trc-testing/parameters.scm \
	wak/trc-testing/port-tracker.scm \
	wak/trc-testing/restart.scm \
	\
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


TESTS = \
	tests/actions.scm \
	tests/bundle.scm \
	tests/database.scm \
	tests/expression.scm \
	tests/hooks.scm \
	tests/inventory.scm \
	tests/package.scm \
	tests/solver.scm \
	tests/tests.scm \
	tests/utils.scm

TESTS_ENVIRONMENT=$(top_builddir)/env guile --no-auto-compile -s 
