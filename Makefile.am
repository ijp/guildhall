moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =					\
	sigil/ext/wt-tree.scm \
	sigil/spells/algebraic-types.scm \
	sigil/spells/algebraic-types/helpers.scm \
	sigil/spells/args-fold.scm \
	sigil/spells/condition.scm \
	sigil/spells/filesys.scm \
	sigil/spells/finite-types.scm \
	sigil/spells/hash-utils.scm \
	sigil/spells/logging.scm \
	sigil/spells/operations.scm \
	sigil/spells/pathname.scm \
	sigil/spells/process.scm \
	sigil/spells/process/compat.scm \
	sigil/spells/record-types.scm \
	sigil/spells/record-types/expand-drt.scm \
	sigil/spells/string-utils.scm \
	sigil/spells/syntax-utils.scm \
	sigil/spells/sysutils.scm \
	sigil/spells/xvector.scm \
	sigil/spells/zipper-tree.scm \
	sigil/ext/fmt.scm \
	sigil/ext/foof-loop.scm \
	sigil/ext/foof-loop/nested.scm \
	sigil/ext/irregex.scm \
	sigil/ext/define-values.scm \
	sigil/ext/let-optionals.scm \
	sigil/ext/trc-testing.scm \
	sigil/ext/trc-testing/display-condition.scm \
	sigil/ext/trc-testing/limited-write.scm \
	sigil/ext/trc-testing/parameters.scm \
	sigil/ext/trc-testing/port-tracker.scm \
	sigil/ext/trc-testing/restart.scm \
	sigil/weinholt/compression/bitstream.scm \
	sigil/weinholt/compression/huffman.scm \
	sigil/weinholt/compression/inflate.scm \
	sigil/weinholt/compression/sliding-buffer.scm \
	sigil/weinholt/compression/zip.scm \
	sigil/weinholt/compression/zip/extra.scm \
	sigil/weinholt/crypto/crc.scm \
	sigil/weinholt/struct/pack.scm \
	\
	sigil/actions.scm \
	sigil/bundle.scm \
	sigil/build-info.scm \
	sigil/config.scm \
	sigil/database.scm \
	sigil/database/dependencies.scm \
	sigil/destination.scm \
	sigil/hooks.scm \
	sigil/inventory.scm \
	sigil/inventory/mapping.scm \
	sigil/package.scm \
	sigil/private/utils.scm \
	sigil/private/zip.scm \
	sigil/repository.scm \
	sigil/solver.scm \
	sigil/solver/choice.scm \
	sigil/solver/dummy-db.scm \
	sigil/solver/expression.scm \
	sigil/solver/internals.scm \
	sigil/solver/logging.scm \
	sigil/solver/promotions.scm \
	sigil/solver/search-graph.scm \
	sigil/solver/universe.scm \
	sigil/cli.scm \
	sigil/cli/config.scm \
	sigil/cli/db.scm \
	sigil/cli/ui.scm \
	sigil/ui.scm \
	sigil/ui/cmdline/base.scm \
	sigil/ui/cmdline/dependencies.scm \
	sigil/ui/cmdline/help.scm \
	sigil/ui/formatters.scm \
	\
	scripts/clean.scm \
	scripts/config.scm \
	scripts/create-bundle.scm \
	scripts/list-packages.scm \
	scripts/install.scm \
	scripts/remove.scm \
	scripts/scan-bundles.scm \
	scripts/show.scm \
	scripts/show-bundle.scm \
	scripts/symlink-bundle.scm \
	scripts/update.scm \
	scripts/update-archive.scm \
	scripts/upgrade.scm

# These files are `include'd by other files.  Given up-to-date .go
# files, installing them is not strictly necessary.  However it is
# always nice to have source, so that's what we do.
#
NOCOMP_SOURCES = \
	sigil/ext/inc/foof-loop.scm \
	sigil/ext/inc/nested-foof-loop.scm \
	sigil/ext/inc/syn-param.scm \
	sigil/ext/inc/irregex-utils.scm \
	sigil/ext/inc/irregex-r6rs.scm \
	sigil/ext/inc/fmt-pretty.scm \
	sigil/ext/inc/mantissa.scm \
	sigil/ext/inc/fmt-column.scm \
	sigil/ext/inc/fmt.scm \
	sigil/ext/inc/test.scm \
	sigil/spells/private/condition.scm \
	sigil/spells/private/xvector.scm \
	sigil/private/hook-runner.sps

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

nobase_ccache_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_ccacheDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS) build-info.scm

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"


TESTS = \
	tests/utils.scm \
	tests/inventory.scm \
	tests/expression.scm \
	tests/package.scm \
	tests/bundle.scm \
	tests/hooks.scm \
	tests/database.scm \
	tests/solver.scm \
	tests/actions.scm

TESTDATA = \
	tests/bundle.zip \
	tests/repository/discrepancy.zip \
	tests/bundle/unsatisfied-depends/pkg-list.scm \
	tests/bundle/bar/b.scm \
	tests/bundle/bar/pkg-list.scm \
	tests/bundle/hook/pkg-list.scm \
	tests/bundle/multi/core.scm \
	tests/bundle/multi/pkg-list.scm \
	tests/bundle/multi/programs/multi.sps \
	tests/bundle/foo/libraries/a.scm \
	tests/bundle/foo/pkg-list.scm \
	tests/bundle/foo/programs/foo.sps \
	tests/bundle/file-conflict-foo/libraries/a.scm \
	tests/bundle/file-conflict-foo/pkg-list.scm \
	tests/repository/available.scm \
	tests/repository/discrepancy/pkg-list.scm

EXTRA_DIST += $(TESTS) $(TESTDATA)

TESTS_ENVIRONMENT=srcdir=$(abs_top_srcdir)/tests/ $(top_builddir)/env guile --no-auto-compile -s 

info_TEXINFOS = docs/sigil.texi
sigil_TEXINFOS = \
	docs/installation.texi \
	docs/quickstart.texi \
	docs/overview.texi \
	docs/reference.texi \
	docs/packaging.texi

EXTRA_DIST += $(sigil_TEXINFOS)

EXTRA_DIST += HACKING COPYING.GPL-3 COPYING.LGPL-3 COPYING.MIT COPYING.BSD
