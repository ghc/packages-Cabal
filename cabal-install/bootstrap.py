#!/usr/bin/env python
# -*- coding: utf-8 -*-

print("""
DO NOT use this script if you have another recent cabal-install available.
This script is intended only for bootstrapping cabal-install on new
architectures.
""")

import hashlib
import json
from pathlib import Path
import shutil
import subprocess
from enum import Enum
from typing import Set, Optional, Dict, List, Tuple, \
                   NewType, BinaryIO, NamedTuple

ghc_path = Path('ghc')
ghc_pkg_path = Path('ghc-pkg')

PACKAGES = Path('packages')
PKG_DB = PACKAGES / 'packages.conf'
BOOTSTRAP_DEPS_JSON = Path('bootstrap-deps.json')

PackageName = NewType('PackageName', str)
Version = NewType('Version', str)

class PackageSource(Enum):
    HACKAGE = 'hackage'
    PREEXISTING = 'pre-existing'
    LOCAL = 'local'

BootstrapDep = NamedTuple('BootstrapDep', [
    ('package', PackageName),
    ('version', Version),
    ('source', PackageSource),
    # only valid when source == HACKAGE
    ('revision', Optional[int]),
    ('sha256', Optional[bytes]),
])

class BadTarball(Exception):
    def __init__(self, path: str, expected_sha256: bytes, found_sha256: bytes):
        self.path = path
        self.expected_sha256 = expected_sha256
        self.found_sha256 = found_sha256

    def __str__(self):
        return '\n'.join([
            f'Bad tarball hash: {self.path}',
            f'  expected: {self.expected_sha256}',
            f'  found:    {self.found_sha256}',
        ])

def package_url(package: PackageName, version: Version) -> str:
    return f'https://hackage.haskell.org/package/{package}-{version}/{package}-{version}.tar.gz'

def package_cabal_url(package: PackageName, version: Version, revision: int) -> str:
    return f'https://hackage.haskell.org/package/{package}-{version}/revision/{revision}.cabal'

def fetch_package(package: PackageName,
                  version: Version,
                  revision: Optional[int],
                  sha256: bytes
                  ) -> Path:
    import urllib.request

    # Download source distribution
    out = PACKAGES / (f'{package}-{version}.tar.gz')
    if not out.exists():
        print(f'Fetching {package}-{version}...')
        out.parent.mkdir(parents=True, exist_ok=True)
        url = package_url(package, version)
        with urllib.request.urlopen(url) as resp:
            shutil.copyfileobj(resp, out.open('wb'))

    # Download revised cabal file
    cabal_file = PACKAGES / f'{package}.cabal'
    if revision is not None and not cabal_file.exists():
        url = package_cabal_url(package, version, revision)
        with urllib.request.urlopen(url) as resp:
            shutil.copyfileobj(resp, cabal_file.open('wb'))

    h = hash_file(hashlib.sha256(), out.open('rb'))
    if sha256 != h:
        raise BadTarball(out, sha256, h)

    return out

def read_bootstrap_deps() -> List[BootstrapDep]:
    deps = json.load(open('bootstrap-deps.json'))
    def from_json(o: object) -> BootstrapDep:
        o['source'] = PackageSource(o['source'])
        return BootstrapDep(**o)

    return [from_json(dep) for dep in deps]

def install_dep(dep: BootstrapDep) -> None:
    if dep.source == PackageSource.PREEXISTING:
        # We expect it to be in the compiler's bootstrap package set
        subprocess.run([str(ghc_pkg_path), 'describe', f'{dep.package}-{dep.version}'],
                       check=True, stdout=subprocess.DEVNULL)
        print(f'Using {dep.package}-{dep.version} from GHC...')
        return

    elif dep.source == PackageSource.HACKAGE:
        tarball = fetch_package(dep.package, dep.version, dep.revision, dep.sha256)
        subprocess.run(['tar', 'xf', tarball.resolve()],
                       cwd=PACKAGES, check=True)
        sdist_dir = PACKAGES / f'{dep.package}-{dep.version}'

        # Update cabal file with revision
        if dep.revision is not None:
            shutil.copyfile(PACKAGES / f'{dep.package}.cabal',
                            sdist_dir / f'{dep.package}.cabal')

    elif dep.source == PackageSource.LOCAL:
        if dep.package == 'Cabal':
            sdist_dir = Path('Cabal').resolve()
        elif dep.package == 'cabal-install':
            sdist_dir = Path('cabal-install').resolve()
        else:
            raise 'hi'

    install_sdist(sdist_dir)

def install_sdist(sdist_dir: Path):
    prefix = (PACKAGES / 'tmp').resolve()
    configure_args = [
        f'--package-db={PKG_DB.resolve()}',
        f'--prefix={prefix}',
        f'--with-compiler={ghc_path}',
        f'--with-hc-pkg={ghc_pkg_path}',
    ]

    def check_call(args: List[str]) -> None:
        subprocess.run(args, cwd=sdist_dir, check=True)

    check_call([str(ghc_path), '--make', 'Setup'])
    check_call(['./Setup', 'configure'] + configure_args)
    check_call(['./Setup', 'build'])
    check_call(['./Setup', 'install'])

def hash_file(h, f: BinaryIO) -> bytes:
    while True:
        d = f.read(1024)
        if len(d) == 0:
            return h.hexdigest()

        h.update(d)


# Cabal plan.json representation
UnitId = NewType('UnitId', str)
PlanUnit = NewType('PlanUnit', object)

def read_plan(project_dir: Path) -> Dict[UnitId, PlanUnit]:
    path = project_dir / 'dist-newstyle' / 'cache' / 'plan.json'
    plan = json.load(path.open('rb'))
    return {
        UnitId(c['id']): PlanUnit(c)
        for c in plan['install-plan']
    }

def extract_plan() -> None:
    units = read_plan(Path('.'))
    target_unit = [
        unit
        for unit in units.values()
        if unit['pkg-name'] == 'cabal-install'
        if unit['component-name'] == 'exe:cabal'
    ][0]

    def unit_to_bootstrap_dep(unit: PlanUnit) -> BootstrapDep:
        if 'pkg-src' in unit and unit['pkg-src']['type'] == 'local':
            source = PackageSource.LOCAL
        elif unit['type'] == 'configured':
            source = PackageSource.HACKAGE
        elif unit['type'] == 'pre-existing':
            source = PackageSource.PREEXISTING

        return BootstrapDep(package = unit['pkg-name'],
                            version = unit['pkg-version'],
                            source = source,
                            revision = None,
                            sha256 = unit.get('pkg-src-sha256'))

    def unit_ids_deps(unit_ids: List[UnitId]) -> List[BootstrapDep]:
        deps = []
        for unit_id in unit_ids:
            unit = units[unit_id]
            deps += unit_deps(unit)
            deps.append(unit_to_bootstrap_dep(unit))

        return deps

    def unit_deps(unit: PlanUnit) -> List[BootstrapDep]:
        if unit['type'] == 'pre-existing':
            return []

        deps = []
        if 'components' in unit:
            for comp_name, comp in unit['components'].items():
                deps += unit_ids_deps(comp['depends'])
        if 'depends' in unit:
            deps += unit_ids_deps(unit['depends'])

        return deps

    deps = remove_duplicates(unit_deps(target_unit))
    write_bootstrap_deps(deps)

def write_bootstrap_deps(deps: List[BootstrapDep]):
    def to_json(dep: BootstrapDep) -> object:
        return {
            'package': dep.package,
            'version': dep.version,
            'source': dep.source.value,
            'revision': dep.revision,
            'sha256': dep.sha256
        }

    json.dump([to_json(dep) for dep in deps],
              BOOTSTRAP_DEPS_JSON.open('w'),
              indent=2)

def remove_duplicates(xs: list) -> list:
    # it's easier to build lists and remove duplicates later than
    # to implement an order-preserving set.
    out = []
    for x in xs:
        if x not in out:
            out.append(x)

    return out

def bootstrap() -> None:
    if not PKG_DB.exists():
        print(f'Creating package database {PKG_DB}')
        PKG_DB.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run([ghc_pkg_path, 'init', PKG_DB])

    deps = read_bootstrap_deps()
    for dep in deps:
        install_dep(dep)

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--extract-plan', action='store_true',
                        help='generate bootstrap-deps.json from plan.json')
    args = parser.parse_args()

    if args.extract_plan:
        deps = extract_plan()
        write_bootstrap_deps(deps)
        print(f'dependencies written to {BOOTSTRAP_DEPS_JSON}')
    else:
        bootstrap()
        cabal_path = PACKAGES
        print(f'''
            Bootstrapping finished!

            The resulting cabal-install executable can be found in
            in {cabal_path}. You now should use this to build a full
            cabal-install distribution.
        ''')

if __name__ == '__main__':
    main()
