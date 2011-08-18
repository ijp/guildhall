moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =					\
	guild/ext/wt-tree.scm \
	guild/spells/algebraic-types.scm \
	guild/spells/algebraic-types/helpers.scm \
	guild/spells/args-fold.scm \
	guild/spells/condition.scm \
	guild/spells/filesys.scm \
	guild/spells/finite-types.scm \
	guild/spells/hash-utils.scm \
	guild/spells/logging.scm \
	guild/spells/operations.scm \
	guild/spells/pathname.scm \
	guild/spells/process.scm \
	guild/spells/process/compat.scm \
	guild/spells/record-types.scm \
	guild/spells/record-types/expand-drt.scm \
	guild/spells/string-utils.scm \
	guild/spells/syntax-utils.scm \
	guild/spells/sysutils.scm \
	guild/spells/xvector.scm \
	guild/spells/zipper-tree.scm \
	guild/ext/fmt.scm \
	guild/ext/foof-loop.scm \
	guild/ext/foof-loop/nested.scm \
	guild/ext/irregex.scm \
	guild/ext/define-values.scm \
	guild/ext/let-optionals.scm \
	guild/ext/trc-testing.scm \
	guild/ext/trc-testing/display-condition.scm \
	guild/ext/trc-testing/limited-write.scm \
	guild/ext/trc-testing/parameters.scm \
	guild/ext/trc-testing/port-tracker.scm \
	guild/ext/trc-testing/restart.scm \
	guild/weinholt/compression/bitstream.scm \
	guild/weinholt/compression/huffman.scm \
	guild/weinholt/compression/inflate.scm \
	guild/weinholt/compression/sliding-buffer.scm \
	guild/weinholt/compression/zip.scm \
	guild/weinholt/compression/zip/extra.scm \
	guild/weinholt/crypto/crc.scm \
	guild/weinholt/struct/pack.scm \
	\
	guild/actions.scm \
	guild/bundle.scm \
	guild/build-info.scm \
	guild/config.scm \
	guild/database.scm \
	guild/database/dependencies.scm \
	guild/destination.scm \
	guild/hooks.scm \
	guild/inventory.scm \
	guild/inventory/mapping.scm \
	guild/package.scm \
	guild/private/utils.scm \
	guild/private/zip.scm \
	guild/repository.scm \
	guild/solver.scm \
	guild/solver/choice.scm \
	guild/solver/dummy-db.scm \
	guild/solver/expression.scm \
	guild/solver/internals.scm \
	guild/solver/logging.scm \
	guild/solver/promotions.scm \
	guild/solver/search-graph.scm \
	guild/solver/universe.scm \
	guild/cli.scm \
	guild/cli/config.scm \
	guild/cli/db.scm \
	guild/cli/ui.scm \
	guild/ui.scm \
	guild/ui/cmdline/base.scm \
	guild/ui/cmdline/dependencies.scm \
	guild/ui/cmdline/help.scm \
	guild/ui/formatters.scm \
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
	guild/ext/inc/foof-loop.scm \
	guild/ext/inc/nested-foof-loop.scm \
	guild/ext/inc/syn-param.scm \
	guild/ext/inc/irregex-utils.scm \
	guild/ext/inc/irregex-r6rs.scm \
	guild/ext/inc/fmt-pretty.scm \
	guild/ext/inc/mantissa.scm \
	guild/ext/inc/fmt-column.scm \
	guild/ext/inc/fmt.scm \
	guild/ext/inc/test.scm \
	guild/spells/private/condition.scm \
	guild/spells/private/xvector.scm \
	guild/private/hook-runner.sps

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
