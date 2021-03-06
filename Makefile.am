moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =					\
	guildhall/ext/wt-tree.scm \
	guildhall/spells/algebraic-types.scm \
	guildhall/spells/algebraic-types/helpers.scm \
	guildhall/spells/args-fold.scm \
	guildhall/spells/condition.scm \
	guildhall/spells/filesys.scm \
	guildhall/spells/finite-types.scm \
	guildhall/spells/hash-utils.scm \
	guildhall/spells/logging.scm \
	guildhall/spells/operations.scm \
	guildhall/spells/pathname.scm \
	guildhall/spells/process.scm \
	guildhall/spells/process/compat.scm \
	guildhall/spells/record-types.scm \
	guildhall/spells/record-types/expand-drt.scm \
	guildhall/spells/string-utils.scm \
	guildhall/spells/syntax-utils.scm \
	guildhall/spells/sysutils.scm \
	guildhall/spells/xvector.scm \
	guildhall/spells/zipper-tree.scm \
	guildhall/ext/fmt.scm \
	guildhall/ext/foof-loop.scm \
	guildhall/ext/foof-loop/nested.scm \
	guildhall/ext/irregex.scm \
	guildhall/ext/define-values.scm \
	guildhall/ext/let-optionals.scm \
	guildhall/ext/trc-testing.scm \
	guildhall/ext/trc-testing/display-condition.scm \
	guildhall/ext/trc-testing/limited-write.scm \
	guildhall/ext/trc-testing/parameters.scm \
	guildhall/ext/trc-testing/port-tracker.scm \
	guildhall/ext/trc-testing/restart.scm \
	guildhall/weinholt/compression/bitstream.scm \
	guildhall/weinholt/compression/huffman.scm \
	guildhall/weinholt/compression/inflate.scm \
	guildhall/weinholt/compression/sliding-buffer.scm \
	guildhall/weinholt/compression/zip.scm \
	guildhall/weinholt/compression/zip/extra.scm \
	guildhall/weinholt/crypto/crc.scm \
	guildhall/weinholt/struct/pack.scm \
	\
	guildhall/actions.scm \
	guildhall/bundle.scm \
	guildhall/build-info.scm \
	guildhall/config.scm \
	guildhall/database.scm \
	guildhall/database/dependencies.scm \
	guildhall/destination.scm \
	guildhall/destination/fhs.scm \
	guildhall/hooks.scm \
	guildhall/inventory.scm \
	guildhall/inventory/mapping.scm \
	guildhall/package.scm \
	guildhall/private/utils.scm \
	guildhall/private/zip.scm \
	guildhall/repository.scm \
	guildhall/solver.scm \
	guildhall/solver/choice.scm \
	guildhall/solver/dummy-db.scm \
	guildhall/solver/expression.scm \
	guildhall/solver/internals.scm \
	guildhall/solver/logging.scm \
	guildhall/solver/promotions.scm \
	guildhall/solver/search-graph.scm \
	guildhall/solver/universe.scm \
	guildhall/cli.scm \
	guildhall/cli/config.scm \
	guildhall/cli/db.scm \
	guildhall/cli/ui.scm \
	guildhall/ui.scm \
	guildhall/ui/cmdline/base.scm \
	guildhall/ui/cmdline/dependencies.scm \
	guildhall/ui/cmdline/help.scm \
	guildhall/ui/formatters.scm \
	\
	scripts/clean.scm \
	scripts/config.scm \
	scripts/create-bundle.scm \
	scripts/list-packages.scm \
	scripts/install.scm \
	scripts/reinstall.scm \
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
	guildhall/ext/inc/foof-loop.scm \
	guildhall/ext/inc/nested-foof-loop.scm \
	guildhall/ext/inc/syn-param.scm \
	guildhall/ext/inc/irregex-utils.scm \
	guildhall/ext/inc/irregex-r6rs.scm \
	guildhall/ext/inc/fmt-pretty.scm \
	guildhall/ext/inc/mantissa.scm \
	guildhall/ext/inc/fmt-column.scm \
	guildhall/ext/inc/fmt.scm \
	guildhall/ext/inc/test.scm \
	guildhall/spells/private/condition.scm \
	guildhall/spells/private/xvector.scm \
	guildhall/private/support-bundle/hook-runner.sps \
	guildhall/private/support-bundle/pkg-list.scm

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

info_TEXINFOS = docs/guildhall.texi
guildhall_TEXINFOS = \
	docs/installation.texi \
	docs/quickstart.texi \
	docs/overview.texi \
	docs/reference.texi \
	docs/packaging.texi

EXTRA_DIST += $(guildhall_TEXINFOS)

EXTRA_DIST += HACKING COPYING.GPL-3 COPYING.LGPL-3 COPYING.MIT COPYING.BSD
